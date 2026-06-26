use std::path::Path;

use technique::parsing;

use crate::common::list_technique_documents;

fn check_directory(dir: &Path) {
    let files = list_technique_documents(dir);

    let mut failures = Vec::new();

    for file in &files {
        let content = parsing::load(&file)
            .unwrap_or_else(|e| panic!("Failed to load file {:?}: {:?}", file, e));

        match parsing::parse(&file, &content) {
            Ok(_) => {}
            Err(e) => {
                println!("File {:?} failed to parse: {:?}", file, e);
                failures.push(file.clone());
            }
        }
    }

    if !failures.is_empty() {
        panic!(
            "Sample files should parse successfully, but {} files failed",
            failures.len()
        );
    }
}

#[test]
fn ensure_parse() {
    check_directory(Path::new("tests/samples/parsing/"));
    check_directory(Path::new("tests/samples/runner/"));
    check_directory(Path::new("examples/minimal/"));
}
