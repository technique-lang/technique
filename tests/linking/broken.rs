use std::path::Path;

use technique::linking;
use technique::parsing;
use technique::runner::Library;
use technique::translation;

use crate::common::list_technique_documents;

#[test]
fn ensure_fail() {
    let dir = Path::new("tests/broken/linking/");
    let files = list_technique_documents(dir);

    let library = Library::core();

    let mut unexpected_successes = Vec::new();
    let mut earlier_failures = Vec::new();

    for file in &files {
        let content = parsing::load(&file)
            .unwrap_or_else(|e| panic!("Failed to load file {:?}: {:?}", file, e));

        // Linking-failure fixtures must parse and translate cleanly first; the
        // failure is meant to come from the linking phase, not earlier ones.
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

        if linking::link(&mut program, &library).is_ok() {
            println!("File {:?} unexpectedly linked successfully", file);
            unexpected_successes.push(file.clone());
        }
    }

    if !earlier_failures.is_empty() {
        panic!(
            "Linking-failure fixtures must parse and translate cleanly, but {} failed earlier",
            earlier_failures.len()
        );
    }

    if !unexpected_successes.is_empty() {
        panic!(
            "Broken files should not link successfully, but {} files passed",
            unexpected_successes.len()
        );
    }
}
