use std::fs;
use std::path::Path;

use technique::engraving::{Appender, Ledger};
use technique::parsing;
use technique::runner::{Conclusion, Context, Environment, Headless, Library, Outcome, Runner};
use technique::translation;

use crate::common::list_technique_documents;

// Strip the volatile leading fields — timestamp and run-id — from each
// recorded PFFTT line, leaving the `<path> <state>` tail. That tail is what
// the expected journal pins; the timestamp and run-id vary from one run to the
// next.
fn strip_timestamp_and_runid(journal: &str) -> Vec<String> {
    journal
        .lines()
        .map(|line| {
            line.splitn(3, ' ')
                .nth(2)
                .unwrap_or(line)
                .to_string()
        })
        .collect()
}

// Rewrite an expected journal from a freshly captured walk. The head line and
// the run identifier are taken from the existing file; the timestamps are the
// ones the capture just wrote.
fn regenerate(path: &Path, existing: &str, captured: &str) {
    let head = existing
        .lines()
        .next()
        .expect("expected journal has a Start line");
    let run_id = head
        .split(' ')
        .nth(1)
        .expect("Start line carries a run identifier");
    let opening = head
        .find("/ Start ")
        .map(|i| &head[i..])
        .expect("Start line names the document");
    let opened = captured
        .lines()
        .next()
        .and_then(|line| {
            line.split(' ')
                .next()
        })
        .expect("capture has at least one record");

    let mut out = String::new();
    out.push_str(&format!("{} {} 000 {}\n", opened, run_id, opening));
    for line in captured.lines() {
        let mut fields = line.splitn(3, ' ');
        let recorded = fields
            .next()
            .unwrap_or_default();
        let _ = fields.next();
        let tail = fields
            .next()
            .unwrap_or_default();
        out.push_str(&format!("{} {} {}\n", recorded, run_id, tail));
    }
    fs::write(path, out).expect("rewrite expected journal");
    println!("regenerated {:?}", path);
}

/// Run every sample to completion headless, capturing the journal in memory,
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
            Ledger::new(),
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
        let captured = runner
            .into_appender()
            .contents()
            .to_string();
        let recorded = strip_timestamp_and_runid(&captured);

        // The expected file is a complete, valid PFFTT journal; its first line
        // is the opening Start lifecycle record, which the in-memory walk
        // capture does not include, so skip the first line before comparing
        // the walk records.
        let expected_path = file.with_extension("pfftt");
        let expected_text = fs::read_to_string(&expected_path).unwrap_or_else(|e| {
            panic!(
                "missing expected journal {:?}: {:?} — add the .pfftt beside the sample",
                expected_path, e
            )
        });
        // A change to what the walk records means every expected journal has to
        // be rewritten. Set TECHNIQUE_REGENERATE to have this test emit the
        // walk it just captured, keeping the journal's existing `Start` line and
        // run identifier so the file stays the run it has always been. Read
        // the diff before committing it.
        if std::env::var("TECHNIQUE_REGENERATE").is_ok() {
            regenerate(&expected_path, &expected_text, &captured);
            continue;
        }

        let expected: Vec<String> = strip_timestamp_and_runid(&expected_text)
            .into_iter()
            .skip(1)
            .collect();

        // A pure-prose procedure legitimately finishes Skip under the
        // automatic driver; only a Fail or Stopped run is a test failure.
        let finished = match outcome {
            Conclusion::Completed(Outcome::Done(_)) | Conclusion::Completed(Outcome::Skip(_)) => {
                true
            }
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
