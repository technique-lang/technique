//! Output generation for the Technique CLI application

use owo_colors::OwoColorize;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use tracing::{debug, info};

/// Compile a Typst document piped via stdin to a PDF file.
///
/// The template content, data literal, and render call are written
/// sequentially to the process's stdin. If `template` is `None` (as
/// with Source), `data` is already a complete Typst document.
///
/// The `root` path is passed as `--root` to Typst, controlling where
/// relative imports resolve from. For built-in templates pass `"."`.
pub fn via_typst(filename: &Path, template: Option<&str>, data: &str, root: &Path) {
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

    let target = filename.with_extension("pdf");

    let mut child = Command::new("typst")
        .arg("compile")
        .arg("--root")
        .arg(root)
        .arg("-")
        .arg(&target)
        .stdin(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| {
            eprintln!("{}: failed to start typst: {}", "error".bright_red(), e);
            std::process::exit(1);
        });

    let mut stdin = child
        .stdin
        .take()
        .unwrap();

    if let Some(tmpl) = template {
        stdin
            .write_all(tmpl.as_bytes())
            .expect("Failed attempting to write");
        stdin
            .write_all(b"\n")
            .expect("Failed attempting to write");
    }
    stdin
        .write_all(data.as_bytes())
        .expect("Write data");
    if template.is_some() {
        stdin
            .write_all(b"\n#render(technique)\n")
            .expect("Failed attempting to write");
    }

    drop(stdin);

    let status = child
        .wait()
        .expect("Failed to wait for Typst process");

    if !status.success() {
        eprintln!("{}: typst compile failed", "error".bright_red());
        std::process::exit(1);
    }

    debug!("Wrote {}", target.display());
}
