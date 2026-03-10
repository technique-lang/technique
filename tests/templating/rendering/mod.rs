use std::fs;
use std::path::Path;

use technique::parsing;
use technique::templating;

fn check_directory(dir: &Path, template: &impl templating::Template) {
    assert!(dir.exists(), "directory missing: {:?}", dir);

    let entries = fs::read_dir(dir).expect("Failed to read directory");

    let mut files = Vec::new();
    for entry in entries {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path
            .extension()
            .and_then(|s| s.to_str())
            == Some("tq")
        {
            files.push(path);
        }
    }

    assert!(!files.is_empty(), "No .tq files found in {:?}", dir);

    let mut failures = Vec::new();

    for file in &files {
        let source =
            parsing::load(file).unwrap_or_else(|e| panic!("Failed to load {:?}: {:?}", file, e));

        let doc = parsing::parse(file, &source)
            .unwrap_or_else(|e| panic!("Failed to parse {:?}: {:?}", file, e));

        let output = templating::render(template, &doc);

        if output.is_empty() {
            failures.push(file.clone());
        }
    }

    if !failures.is_empty() {
        panic!(
            "Template produced empty output for {} files: {:?}",
            failures.len(),
            failures
        );
    }
}

#[path = "procedure.rs"]
mod procedure;

#[path = "checklist.rs"]
mod checklist;
