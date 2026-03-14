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

    for file in &files {
        let source =
            parsing::load(file).unwrap_or_else(|e| panic!("Failed to load {:?}: {:?}", file, e));

        let technique = parsing::parse(file, &source)
            .unwrap_or_else(|e| panic!("Failed to parse {:?}: {:?}", file, e));

        // Exercise the markup path; panics surface as test failures
        let _ = template.markup(&technique);
    }
}

#[path = "procedure.rs"]
mod procedure;

#[path = "checklist.rs"]
mod checklist;
