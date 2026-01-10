//! Template trait for rendering Technique documents

use crate::language::Document;

/// Trait for templates that transform Technique documents into Typst markup
pub trait Template {
    /// Render a Technique document into Typst markup
    fn render(&self, document: &Document, width: u8) -> String;
}
