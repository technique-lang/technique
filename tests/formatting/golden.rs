#[cfg(test)]
mod examples {
    use std::fs;
    use std::path::Path;

    use technique::formatting::*;
    use technique::parsing;

    /// Golden test for the format command
    ///
    /// This test:
    /// 1. Reads all .t files from examples/golden/
    /// 2. Runs the equivalent of the `format` command on each file
    /// 3. Compares the formatted output with the original input
    /// 4. Shows clear diffs when differences are found
    ///
    /// The test expects files to be in their canonical formatted form. If
    /// files fail this test, either the parser & formatter is wrong (a bug
    /// that needs to be fixed!) or possibly the example file is wrong
    /// (perhaps because of a deliberate style change, and they thus might
    /// need reformatting)

    /// Simple diff function to show line-by-line differences
    fn show_diff(original: &str, formatted: &str, file_path: &Path) {
        let original_lines: Vec<&str> = original
            .lines()
            .collect();
        let formatted_lines: Vec<&str> = formatted
            .lines()
            .collect();

        let max_lines = original_lines
            .len()
            .max(formatted_lines.len());
        let mut differences_found = false;

        println!("\nDifferences found in file: {:?}", file_path);
        println!("--- Original");
        println!("+++ Formatted");

        for i in 0..max_lines {
            let orig_line = original_lines
                .get(i)
                .unwrap_or(&"");
            let fmt_line = formatted_lines
                .get(i)
                .unwrap_or(&"");

            if orig_line != fmt_line {
                if !differences_found {
                    differences_found = true;
                }
                println!("@@ Line {} @@", i + 1);
                println!("- {}", orig_line);
                println!("+ {}", fmt_line);
            }
        }
    }

    #[test]
    fn ensure_identical_output() {
        // Read all .tq files from examples/prototype/
        let dir = Path::new("examples/golden");

        // Ensure the directory exists
        assert!(dir.exists(), "examples directory missing");

        // Get all .tq files in the directory
        let entries = fs::read_dir(dir).expect("Failed to read examples directory");

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

        // Ensure we found some test files
        assert!(!files.is_empty(), "No .tq files found in examples directory");

        let mut failures = Vec::new();

        // Test each file
        for file in &files {
            // Load the original content
            let original = parsing::load(&file)
                .unwrap_or_else(|e| panic!("Failed to load file {:?}: {:?}", file, e));

            // Parse the content into a Document
            let document = parsing::parse(&file, &original)
                .unwrap_or_else(|e| panic!("Failed to parse file {:?}: {:?}", file, e));

            // Format the document using the Identity renderer (no markup)
            // Using width 78 to match the default
            let result = render(&Identity, &document, 78);

            // Compare the formatted output with the original input
            // They should be identical for well-formed files
            if result != original {
                failures.push(file.clone());
            }
        }

        // If any files had differences, show detailed diffs and fail
        if !failures.is_empty() {
            for file_path in &failures {
                let content = parsing::load(&file_path).unwrap();
                let document = parsing::parse(&file_path, &content).unwrap();
                let output = render(&Identity, &document, 78);
                show_diff(&content, &output, &file_path);
            }

            panic!("All examples must format unchanged");
        }
    }
}
