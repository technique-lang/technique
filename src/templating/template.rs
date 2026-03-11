//! Traits for the templating pipeline.

use crate::language;

/// A template transforms a Technique document into output. Internally this
/// is split into two phases: an adapter, which takes the AST from the parser
/// and converts it to domain types, and rendering which converts that domain
/// into output. Not all templates make this split; `Source` is a special case
/// that delegates directly to the code formatting logic.

pub trait Template {
    /// Serialize the document as a Typst data literal.
    fn data(&self, document: &language::Document) -> String;

    /// Return the Typst source for this template, if any. Templates that
    /// generate complete Typst documents directly (like Source) return `None`.
    fn typst(&self) -> Option<&str> {
        None
    }
}
