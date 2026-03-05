//! Traits for the templating pipeline.

use crate::language;

/// A template transforms a Technique document into Typst markup. Internally
/// this is split into two phases: an adapter, which takes the AST from the
/// parser and converts it to domain types, and a renderer which converts that
/// domain into Typst markup. Not all templates make this split; `Source` is a
/// special case that delegates directly to the code formatting logic.

pub trait Template {
    fn render(&self, document: &language::Document) -> String;
}

/// Adapters project the AST into a domain-specific model. Each template
/// defines its own model types (e.g. checklist::Document,
/// procedure::Document) reflecting how that domain thinks about the elements
/// of procedures as encoded in Technique.
pub trait Adapter {
    type Model;
    fn extract(&self, document: &language::Document) -> Self::Model;
}

/// Renderers convert from a domain model into Typst markup. Shared `typst`
/// primitives are made available as helpers to make for more consistent
/// output.
pub trait Renderer {
    type Model;
    fn render(&self, model: &Self::Model) -> String;
}
