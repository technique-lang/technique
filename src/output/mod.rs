//! Output generation for the Technique CLI application

use owo_colors::OwoColorize;
use std::path::Path;
use std::process::Command;
use tracing::{debug, info};

/// Generate the Typst document content that wires together the domain
/// template, optional user template, data literal, and render call.
pub fn document(domain: &str, data: &str, custom: Option<&str>) -> String {
    let mut doc = String::new();

    doc.push_str(&format!("#import \".{}.typ\": render, template\n", domain));
    if let Some(path) = custom {
        doc.push_str(&format!("#import \"/{}\": *\n", path));
    }
    doc.push_str("\n#show: template\n\n");
    doc.push_str(data);
    doc.push_str("\nrender(technique)\n");

    doc
}

/// Write the domain template and generated document beside the source
/// file, then compile to PDF via Typst.
pub fn via_typst(
    filename: &Path,
    template: &str,
    domain: &str,
    data: &str,
    custom: Option<&str>,
) {
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

    // Write generated document beside source
    let content = document(domain, data, custom);
    let document = source_dir.join(format!(".{}.typ", stem));
    std::fs::write(&document, &content).expect("Failed to write generated document");

    let target = filename.with_extension("pdf");

    let status = Command::new("typst")
        .arg("compile")
        .arg("--root")
        .arg(".")
        .arg(&document)
        .arg(&target)
        .status()
        .unwrap_or_else(|e| {
            eprintln!("{}: failed to start typst: {}", "error".bright_red(), e);
            std::process::exit(1);
        });

    if !status.success() {
        eprintln!("{}: typst compile failed", "error".bright_red());
        std::process::exit(1);
    }

    debug!("Wrote {}", target.display());
}
