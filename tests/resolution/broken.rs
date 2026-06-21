use std::path::Path;

use technique::parsing;
use technique::resolution;
use technique::translation;

use crate::common::list_technique_documents;

#[test]
fn ensure_fail() {
    let dir = Path::new("tests/broken/resolution/");
    let files = list_technique_documents(dir);

    let mut unexpected_successes = Vec::new();
    let mut earlier_failures = Vec::new();

    for file in &files {
        let content = parsing::load(&file)
            .unwrap_or_else(|e| panic!("Failed to load file {:?}: {:?}", file, e));

        // Resolution-failure fixtures must parse and translate cleanly first;
        // the failure is meant to come from the resolution phase.
        let document = match parsing::parse(&file, &content) {
            Ok(document) => document,
            Err(errors) => {
                println!("File {:?} unexpectedly failed to parse: {:?}", file, errors);
                earlier_failures.push(file.clone());
                continue;
            }
        };

        let mut program = match translation::translate(&document) {
            Ok(program) => program,
            Err(errors) => {
                println!(
                    "File {:?} unexpectedly failed to translate: {:?}",
                    file, errors
                );
                earlier_failures.push(file.clone());
                continue;
            }
        };

        if resolution::resolve(&mut program).is_ok() {
            println!("File {:?} unexpectedly resolved successfully", file);
            unexpected_successes.push(file.clone());
        }
    }

    if !earlier_failures.is_empty() {
        panic!(
            "Resolution-failure fixtures must parse and translate cleanly, but {} failed earlier",
            earlier_failures.len()
        );
    }

    if !unexpected_successes.is_empty() {
        panic!(
            "Broken files should not resolve successfully, but {} files passed",
            unexpected_successes.len()
        );
    }
}
