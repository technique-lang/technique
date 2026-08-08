use std::fs;
use std::path::{Path, PathBuf};

pub fn list_technique_documents(dir: &Path) -> Vec<PathBuf> {
    list_files(dir, "tq")
}

pub fn list_files(dir: &Path, extension: &str) -> Vec<PathBuf> {
    assert!(dir.exists(), "samples directory missing");

    let entries = fs::read_dir(dir).expect("Failed to read samples directory");

    let mut files = Vec::new();
    for entry in entries {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path
            .extension()
            .and_then(|s| s.to_str())
            == Some(extension)
        {
            files.push(path);
        }
    }

    assert!(
        !files.is_empty(),
        "No .{} files found in directory",
        extension
    );
    files
}
