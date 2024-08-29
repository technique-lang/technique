use std::fs;
use std::io::Write;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::process::{Command, Stdio};
use tracing::{debug, info};

pub(crate) fn via_typst(filename: &Path) {
    info!("Printing file: {:?}", filename);

    // Verify that the file actually exists
    if !filename.exists() {
        panic!("Supplied procedure file does not exist: {:?}", filename);
    }

    // Read the file contents
    let contents = fs::read_to_string(filename).expect("Unable to read specified file");

    let mut child = Command::new("typst")
        .arg("compile")
        .arg("-")
        .arg("FIXME.pdf")
        .stdin(Stdio::piped())
        .spawn()
        .expect("Failed to start external Typst process");

    // Write the file contents to the process's stdin
    let mut stdin = child.stdin.take().unwrap();

    stdin
        .write(
            format!(
                r#"
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
        read("{}")
    )"#,
                filename.to_str().expect("Should be able to render filename")
            )
            .as_bytes(),
        )
        .expect("Write header to child prcess");

    // stdin
    //     .write_all(contents.as_bytes())
    //     .expect("Failed to write to stdin");

    //     stdin.write(filename.as_os_str().as_bytes()).expect("Write filename to child process");

    //     stdin.write(r#"")
    //     )
    // "#.as_bytes()).expect("Write tail to child process");

    drop(stdin);

    // Wait for the process to complete
    let output = child.wait_with_output().expect("Failed to read stdout");

    // Log the output
    debug!("Process output: {:?}", output);
}
