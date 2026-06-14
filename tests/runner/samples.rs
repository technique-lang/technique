use std::collections::HashSet;
use std::path::Path;

use technique::parsing;
use technique::program::Operation;
use technique::runner::{Appender, Environment, Headless, Library, Runner};
use technique::translation;

use crate::common::list_technique_documents;

// Operations the runner records a Result for: every Step (recursing into its
// substeps) and every Section scope. A subroutine's own scope is counted by
// the caller. Read straight off the translated Program, so it is an
// independent reference for what a clean run must produce.
fn count_results(op: &Operation) -> usize {
    match op {
        Operation::Step { body, .. } => 1 + count_results(body),
        Operation::Section { body, .. } => 1 + count_results(body),
        Operation::Sequence(ops) | Operation::List(ops) => ops
            .iter()
            .map(count_results)
            .sum(),
        // A foreach over a literal list unrolls to one body pass per element,
        // the loop adding no result of its own. A runtime iterable or a
        // `repeat` has no statically known count, so such a sample cannot be
        // checked this way (and `repeat` never terminates headless).
        Operation::Loop { over, body, .. } => {
            let iterations = match over {
                Some(over) => match over.as_ref() {
                    Operation::List(items) => items.len(),
                    _ => panic!("ensure_run sample loops over a non-literal iterable"),
                },
                None => panic!("ensure_run sample uses `repeat`, which never terminates headless"),
            };
            iterations * count_results(body)
        }
        Operation::Bind { value, .. } => count_results(value),
        _ => 0,
    }
}

/// Run every sample to completion headless and establish that each step had a
/// result. Each sample is a linear Technique (each declared procedure invoked
/// exactly once, no loops, no dead code), so the runner must settle a Result
/// for every operation the translated Program declares: each Step, each
/// Section, and each named procedure scope. The reference count is read
/// straight off the Program; the `Headless` driver counts the results it
/// actually settled, and the two must agree.
#[test]
fn ensure_run() {
    let dir = Path::new("tests/samples/runner/");
    let files = list_technique_documents(dir);

    let mut failures = Vec::new();

    for file in &files {
        let content = parsing::load(&file)
            .unwrap_or_else(|e| panic!("Failed to load file {:?}: {:?}", file, e));

        let document = match parsing::parse(&file, &content) {
            Ok(document) => document,
            Err(e) => {
                println!("File {:?} failed to parse: {:?}", file, e);
                failures.push(file.clone());
                continue;
            }
        };

        let program = match translation::translate(&document) {
            Ok(program) => program,
            Err(e) => {
                println!("File {:?} failed to translate: {:?}", file, e);
                failures.push(file.clone());
                continue;
            }
        };

        let declared: usize = program
            .subroutines
            .iter()
            .map(|sub| {
                let own = if sub
                    .name
                    .is_some()
                {
                    1
                } else {
                    0
                };
                own + count_results(&sub.body)
            })
            .sum();

        let mut library = Library::core();
        library.extend(Library::system());
        let mut runner = Runner::new(
            &program,
            Appender::sink(),
            HashSet::new(),
            Headless::new(),
            library,
        );
        if let Err(e) = runner.run(Environment::new()) {
            println!("File {:?} did not run cleanly: {:?}", file, e);
            failures.push(file.clone());
            continue;
        }

        let results = runner
            .into_driver()
            .results();
        if results != declared {
            println!(
                "File {:?}: runner settled {} results, expecting {}",
                file, results, declared
            );
            failures.push(file.clone());
        }
    }

    if !failures.is_empty() {
        panic!(
            "Sample files should run with a result for every operation, but {} files failed",
            failures.len()
        );
    }
}
