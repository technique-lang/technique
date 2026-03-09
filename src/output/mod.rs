//! Output generation for the Technique CLI application

use owo_colors::OwoColorize;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use tracing::{debug, info};

pub fn via_typst(filename: &Path, markup: &str) {
    info!("Printing file: {}", filename.display());

    // Verify that the file actually exists
    if filename.to_str() == Some("-") {
        eprintln!(
            "{}: Unable to render to PDF from standard input.",
            "error".bright_red()
        );
        std::process::exit(1);
    }
    if !filename.exists() {
        panic!(
            "Supplied procedure file does not exist: {}",
            filename.display()
        );
    }

    let target = filename.with_extension("pdf");

    let mut child = Command::new("typst")
        .arg("compile")
        .arg("-")
        .arg(target)
        .stdin(Stdio::piped())
        .spawn()
        .expect("Failed to start external Typst process");

    // Write the markup to the process's stdin
    let mut stdin = child
        .stdin
        .take()
        .unwrap();

    stdin
        .write(markup.as_bytes())
        .expect("Write document to child process");

    drop(stdin);

    // Wait for the process to complete
    let output = child
        .wait_with_output()
        .expect("Failed to read stdout");

    // Log the output
    debug!("Process output: {:?}", output);
}
