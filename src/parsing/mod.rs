//! parser for the Technique language

use std::path::Path;
use tracing::debug;

use crate::language::Technique;

pub mod parser;
mod scope;

/// Read a file and return an owned String. We pass that ownership back to the main function so that
/// the Technique object created by parse() below can have the same lifetime.
pub fn load(filename: &Path) -> String {
    let content = std::fs::read_to_string(filename).expect("Failed to read the source file");
    content
}

/// Parse text into a Technique object, or error out.
pub fn parse(content: &str) -> Technique {
    let result = parser::parse_via_taking(content);

    match result {
        Ok(technique) => {
            if let Some(procedures) = &technique.body {
                debug!(
                    "Found {} procedure{}",
                    procedures.len(),
                    if procedures.len() > 1 { "s" } else { "" }
                );
            } else {
                debug!("No procedures found");
            }

            technique
        }
        Err(error) => {
            eprintln!("error: {}", error);
            std::process::exit(1);
        }
    }
}
