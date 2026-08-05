//! parser for the Technique language

use std::io::Read;
use std::path::Path;
use tracing::debug;

use crate::language::{Document, LoadingError, Technique};

mod parser;
mod scope;

// Export the actual public API
pub use parser::{Parser, ParsingError, parse_numeric, parse_with_recovery};

/// Convert a document written on Windows to have Unix line endings, allowing
/// the rest of the parser to only have to deal with \n characters. We check
/// the first line ending for a \r as a sentinel.
fn normalise(content: String) -> String {
    let crlf = match content.find('\n') {
        Some(i) => content[..i].ends_with('\r'),
        None => false,
    };

    if crlf {
        content.replace("\r\n", "\n")
    } else {
        content
    }
}

/// Read a file and return an owned String. We pass that ownership back to the
/// main function so that the Technique object created by parse() below can
/// have the same lifetime.
pub fn load(filename: &Path) -> Result<String, LoadingError<'_>> {
    if filename.to_str() == Some("-") {
        let mut buffer = String::new();
        match std::io::stdin().read_to_string(&mut buffer) {
            Ok(_) => return Ok(normalise(buffer)),
            Err(error) => {
                debug!(?error);
                return Err(LoadingError {
                    problem: "Failed reading from stdin".to_string(),
                    details: error.to_string(),
                    filename,
                });
            }
        }
    }

    match std::fs::read_to_string(filename) {
        Ok(content) => Ok(normalise(content)),
        Err(error) => {
            debug!(?error);
            match error.kind() {
                std::io::ErrorKind::NotFound => Err(LoadingError {
                    problem: "File not found".to_string(),
                    details: String::new(),
                    filename,
                }),
                _ => Err(LoadingError {
                    problem: "Failed reading".to_string(),
                    details: error
                        .kind()
                        .to_string(),
                    filename,
                }),
            }
        }
    }
}

/// Parse text into a Document object, or return the list of errors
/// encountered.
pub fn parse<'i>(filename: &'i Path, content: &'i str) -> Result<Document<'i>, Vec<ParsingError>> {
    let result = parser::parse_with_recovery(filename, content);

    match result {
        Ok(document) => {
            if let Some(body) = &document.body {
                match body {
                    Technique::Procedures(procedures) => {
                        debug!(
                            "Found {} procedure{}",
                            procedures.len(),
                            if procedures.len() == 1 { "" } else { "s" }
                        );
                    }
                    Technique::Steps(steps) => {
                        debug!(
                            "Found {} step{}",
                            steps.len(),
                            if steps.len() == 1 { "" } else { "s" }
                        );
                    }
                    Technique::Empty => {
                        debug!("Empty");
                    }
                }
            } else {
                debug!("No content found");
            }
            Ok(document)
        }
        Err(errors) => {
            debug!("errors: {}", errors.len());
            Err(errors)
        }
    }
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn carriage_returns_removed() {
        let content = "% technique v1\r\n\r\nalpha :\r\n".to_string();
        assert_eq!(normalise(content), "% technique v1\n\nalpha :\n");

        // content already written with newlines is handed back untouched
        let content = "% technique v1\n\nalpha :\n".to_string();
        assert_eq!(normalise(content), "% technique v1\n\nalpha :\n");
    }
}
