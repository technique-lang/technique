use std::collections::HashMap;
use std::fs;
use std::path::Path;

use technique::parsing;
use technique::runner::{
    Appender, Conclusion, Context, Environment, Headless, Library, Outcome, Runner,
};
use technique::translation;

use crate::common::list_technique_documents;

// Strip the volatile leading fields — timestamp and run-id — from each
// recorded PFFTT line, leaving the `<path> <state>` tail. That tail is what
// the expected trail pins; the timestamp and run-id vary from one run to the
// next.
fn strip_timestamp_and_runid(trail: &str) -> Vec<String> {
    trail
        .lines()
        .map(|line| {
            line.splitn(3, ' ')
                .nth(2)
                .unwrap_or(line)
                .to_string()
        })
        .collect()
}

/// Run every sample to completion headless, capturing the trail in memory,
/// and assert two things: the run finishes `Done`, and the recorded walk
/// matches the expected `.pfftt` checked in beside the sample. The walk
/// records pin each step's qualified path and outcome in walk order, so a
/// wrong path, a missing seal, a dropped iteration segment, or a reordered
/// walk is caught. The in-memory capture holds the walk and its closing
/// `Finish`, but not the opening `Start` (the store layer writes that), so the
/// expected file's first line is skipped when comparing. A sample without a
/// matching `.pfftt` also fails.
#[test]
fn ensure_run() {
    let dir = Path::new("tests/golden/runner/");
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

        let mut program = match translation::translate(&document) {
            Ok(program) => program,
            Err(e) => {
                println!("File {:?} failed to translate: {:?}", file, e);
                failures.push(file.clone());
                continue;
            }
        };

        if let Err(e) = technique::resolution::resolve(&mut program) {
            println!("File {:?} failed to resolve: {:?}", file, e);
            failures.push(file.clone());
            continue;
        }

        let mut library = Library::core();
        library.extend(Library::system());
        if let Err(e) = technique::linking::link(&mut program, &library) {
            println!("File {:?} failed to link: {:?}", file, e);
            failures.push(file.clone());
            continue;
        }
        let mut runner = Runner::new(
            &program,
            Appender::memory(),
            HashMap::new(),
            Headless::new(),
            library,
        )
        .with_context(Context::capture());
        let outcome = match runner.run(Environment::new()) {
            Ok(outcome) => outcome,
            Err(e) => {
                println!("File {:?} did not run cleanly: {:?}", file, e);
                failures.push(file.clone());
                continue;
            }
        };
        let recorded = strip_timestamp_and_runid(
            runner
                .into_appender()
                .contents(),
        );

        // The expected file is a complete, valid PFFTT trail; its first line
        // is the opening Start lifecycle record, which the in-memory walk
        // capture does not include, so skip the first line before comparing
        // the walk records.
        let expected_path = file.with_extension("pfftt");
        let expected_text = fs::read_to_string(&expected_path).unwrap_or_else(|e| {
            panic!(
                "missing expected trail {:?}: {:?} — add the .pfftt beside the sample",
                expected_path, e
            )
        });
        let expected: Vec<String> = strip_timestamp_and_runid(&expected_text)
            .into_iter()
            .skip(1)
            .collect();

        // A pure-prose procedure legitimately finishes Skip under the
        // automatic driver; only a Fail or Stopped run is a test failure.
        let finished = match outcome {
            Conclusion::Completed(Outcome::Done(_)) | Conclusion::Completed(Outcome::Skip(_)) => true,
            _ => {
                println!("File {:?} did not finish cleanly: {:?}", file, outcome);
                false
            }
        };

        if !finished || recorded != expected {
            println!("\nTrail mismatch for {:?}", file);
            println!("--- expected\n+++ recorded");
            let max = recorded
                .len()
                .max(expected.len());
            for i in 0..max {
                let e = expected
                    .get(i)
                    .map(String::as_str)
                    .unwrap_or("");
                let r = recorded
                    .get(i)
                    .map(String::as_str)
                    .unwrap_or("");
                if e != r {
                    println!("@@ line {} @@\n- {}\n+ {}", i + 1, e, r);
                }
            }
            failures.push(file.clone());
        }
    }

    if !failures.is_empty() {
        panic!(
            "Sample runs must complete, and must match expected results, but {} files failed",
            failures.len()
        );
    }
}
