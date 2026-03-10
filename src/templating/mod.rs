//! Render Technique documents into formatted output.
//!
//! The rendering pipeline has three layers:
//!
//! - The **Engine** contains accessors and helpers for working over the abstract
//! syntax tree types that emerge from the parser, providing convenient
//! iteration without exposing parser internals.
//!
//! - An **Adapter** trait projects the AST types into a domain-specific
//! model (e.g. checklist flattens to checkable items, procedure preserves
//! the full hierarchy). Finally,
//!
//! - The **Template** trait which acts as a top-level interface providing
//! `render()` for Typst markup (PDF path) and `data()` for Typst data
//! literals. Each domain template composes an adapter internally.

mod checklist;
mod engine;
mod procedure;
mod source;
mod template;
pub mod typst;

pub use checklist::Checklist;
pub use procedure::Procedure;
pub use source::Source;
pub use template::{Adapter, Template};

use crate::language;

/// Render a Technique document using the specified template.
pub fn render(template: &impl Template, document: &language::Document) -> String {
    template.render(document)
}

/// Serialize a Technique document as a Typst data literal.
pub fn data(template: &impl Template, document: &language::Document) -> String {
    template.data(document)
}
