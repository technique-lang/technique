//! Trait for transforming Technique documents and then rendering them.

use crate::language;

/// Templates transform Technique documents into Typst markup.
pub trait Template {
    /// Render a Technique document into Typst markup
    fn render(&self, document: &language::Document, width: u8) -> String;
}
