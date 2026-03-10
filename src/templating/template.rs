//! Traits for the templating pipeline.

use crate::language;

/// A template transforms a Technique document into output. Internally this
/// is split into two phases: an adapter, which takes the AST from the parser
/// and converts it to domain types, and rendering which converts that domain
/// into output. Not all templates make this split; `Source` is a special case
/// that delegates directly to the code formatting logic.

pub trait Template {
    /// Render the document as complete Typst markup (for PDF generation).
    fn render(&self, document: &language::Document) -> String;

    /// Serialize the document as a Typst data literal.
    fn data(&self, document: &language::Document) -> String;
}

/// Adapters project the AST into a domain-specific model. Each template
/// defines its own model types (e.g. checklist::Document,
/// procedure::Document) reflecting how that domain thinks about the elements
/// of procedures as encoded in Technique.
pub trait Adapter {
    type Model;
    fn extract(&self, document: &language::Document) -> Self::Model;
}
