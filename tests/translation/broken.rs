use std::path::Path;

use technique::parsing;
use technique::translation;

use crate::common::list_technique_documents;

#[test]
fn ensure_fail() {
    let dir = Path::new("tests/broken/translation/");
    let files = list_technique_documents(dir);

    let mut unexpected_successes = Vec::new();
    let mut parse_failures = Vec::new();

    for file in &files {
        let content = parsing::load(&file)
            .unwrap_or_else(|e| panic!("Failed to load file {:?}: {:?}", file, e));

        // Translation-failure fixtures must parse cleanly first; the failure
        // is meant to come from the translation phase, not the parser.
        let document = match parsing::parse(&file, &content) {
            Ok(document) => document,
            Err(errors) => {
                println!(
                    "File {:?} unexpectedly failed to parse: {:?}",
                    file, errors
                );
                parse_failures.push(file.clone());
                continue;
            }
        };

        if translation::translate(&document).is_ok() {
            println!("File {:?} unexpectedly translated successfully", file);
            unexpected_successes.push(file.clone());
        }
    }

    if !parse_failures.is_empty() {
        panic!(
            "Translation-failure fixtures must parse cleanly, but {} failed at the parser",
            parse_failures.len()
        );
    }

    if !unexpected_successes.is_empty() {
        panic!(
            "Broken files should not translate successfully, but {} files passed",
            unexpected_successes.len()
        );
    }
}
