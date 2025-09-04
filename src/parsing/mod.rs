//! parser for the Technique language

use std::path::Path;
use tracing::debug;

use crate::language::{Document, LoadingError, Technique};
use crate::parsing::parser::ParsingError;

pub mod parser;
mod scope;

/// Read a file and return an owned String. We pass that ownership back to the
/// main function so that the Technique object created by parse() below can
/// have the same lifetime.
pub fn load(filename: &Path) -> Result<String, LoadingError<'_>> {
    match std::fs::read_to_string(filename) {
        Ok(content) => Ok(content),
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
