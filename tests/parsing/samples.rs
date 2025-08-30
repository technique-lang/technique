#[cfg(test)]
mod samples {
    use std::fs;
    use std::path::Path;

    use technique::parsing;

    #[test]
    fn ensure_samples_parse() {
        let dir = Path::new("tests/samples/");

        assert!(dir.exists(), "samples directory missing");

        let entries = fs::read_dir(dir).expect("Failed to read samples directory");

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

        assert!(!files.is_empty(), "No .tq files found in samples directory");

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
    fn ensure_broken_fail() {
        let dir = Path::new("tests/broken/");

        assert!(dir.exists(), "broken directory missing");

        let entries = fs::read_dir(dir).expect("Failed to read broken directory");

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

        assert!(!files.is_empty(), "No .tq files found in broken directory");

        let mut unexpected_successes = Vec::new();

        for file in &files {
            let content = parsing::load(&file)
                .unwrap_or_else(|e| panic!("Failed to load file {:?}: {:?}", file, e));

            match parsing::parse(&file, &content) {
                Ok(_) => {
                    println!("File {:?} unexpectedly parsed successfully", file);
                    unexpected_successes.push(file.clone());
                }
                Err(_) => {}
            }
        }

        if !unexpected_successes.is_empty() {
            panic!(
                "Broken files should not to parse successfully, but {} files passed",
                unexpected_successes.len()
            );
        }
    }
}
