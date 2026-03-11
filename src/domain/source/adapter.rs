//! Projects the AST into the source domain model.
//!
//! This runs the code formatter to produce syntax-tagged fragments,
//! then wraps them as domain types for serialization.

use crate::domain::Adapter;
use crate::formatting::formatter::format_with_renderer;
use crate::language;

use super::types::{Document, Fragment};

const WIDTH: u8 = 70;

pub struct SourceAdapter;

impl Adapter for SourceAdapter {
    type Model = Document;

    fn extract(&self, document: &language::Document) -> Document {
        let fragments = format_with_renderer(document, WIDTH);

        Document {
            fragments: fragments
                .into_iter()
                .map(|(syntax, content)| Fragment {
                    syntax: format!("{:?}", syntax),
                    content: content.into_owned(),
                })
                .collect(),
        }
    }
}
