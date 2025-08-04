use serde::Serialize;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use tinytemplate::TinyTemplate;
use tracing::{debug, info};

use technique::language::Document;

mod source;

static TEMPLATE: &'static str = r#"
#show raw: set text(font: "Inconsolata")
#show raw.where(lang: "technique"): set raw(
        lang: "technique",
        syntaxes: "technique.sublime-syntax",
        theme: "technique.tmTheme",
    )

#show raw: set block(breakable: true)
#raw(
        block: true,
        lang: "technique",
        read("{filename}")
    )
"#;

#[derive(Serialize)]
struct Context {
    filename: String,
}

pub(crate) fn via_typst(source: &Path, document: &Document) {
    let filename = source.display();
    info!("Printing file: {}", filename);

    // Verify that the file actually exists
    if !source.exists() {
        panic!("Supplied procedure file does not exist: {}", filename);
    }

    let target = source.with_extension("pdf");

    let mut child = Command::new("typst")
        .arg("compile")
        .arg("-")
        .arg(target)
        .stdin(Stdio::piped())
        .spawn()
        .expect("Failed to start external Typst process");

    // Write the file contents to the process's stdin
    let mut stdin = child
        .stdin
        .take()
        .unwrap();

    let mut tt = TinyTemplate::new();
    tt.add_template("hello", TEMPLATE)
        .unwrap();

    let context = Context {
        filename: filename.to_string(),
    };

    let rendered = tt
        .render("hello", &context)
        .unwrap();
    stdin
        .write(rendered.as_bytes())
        .expect("Write header to child process");

    let markup = source::render(document);
    // write markup to stdin handle
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
