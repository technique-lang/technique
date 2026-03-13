//! Output generation for the Technique CLI application

use owo_colors::OwoColorize;
use std::path::Path;
use std::process::Command;
use tracing::{debug, info};

/// Write the domain template and assembled document into a (hidden) file
/// beside the input source file, then compile to PDF using the external Typst
/// binary.
pub fn via_typst(filename: &Path, template: &str, domain: &str, document: &str) {
    info!("Printing file: {}", filename.display());

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

    let source_dir = filename
        .parent()
        .unwrap_or(Path::new("."));
    let stem = filename
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap();

    // Write domain template beside source
    let machinery = source_dir.join(format!(".{}.typ", domain));
    std::fs::write(&machinery, template).expect("Failed to write domain template");

    // Write assembled document beside source
    let target_typ = source_dir.join(format!(".{}.typ", stem));
    std::fs::write(&target_typ, document).expect("Failed to write generated document");

    let target_pdf = filename.with_extension("pdf");

    let status = Command::new("typst")
        .arg("compile")
        .arg("--root")
        .arg(".")
        .arg(&target_typ)
        .arg(&target_pdf)
        .status()
        .unwrap_or_else(|e| {
            eprintln!("{}: failed to start typst: {}", "error".bright_red(), e);
            std::process::exit(1);
        });

    if !status.success() {
        eprintln!("{}: typst compile failed", "error".bright_red());
        std::process::exit(1);
    }

    debug!("Wrote {}", target_pdf.display());
}
