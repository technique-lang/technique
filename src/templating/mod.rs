//! Render Technique documents into formatted output.
//!
//! The rendering pipeline has four layers:
//!
//! - **Engine** contains accessors and helpers for working over the abstract syntax
//! tree types that emerge from the parser, providing convenient iteration
//! without exposing parser internals.
//!  
//! - The **Adapter** trait that projects the AST types into a domain-specific
//! model (e.g. checklist flattens to checkable items, procedure preserves the
//! full hierarchy).
//!  
//! - The **Renderer** trait formats domain model types into Typst markup,
//! using the shared `typst` primitives. Finally,
//!  
//! - A **Template** trait which acts as a top-level interface that provides
//! `render()` as an entry point. Each domain template composes an adapter and
//! renderer internally.

mod checklist;
mod engine;
mod procedure;
mod source;
mod template;
pub mod typst;

pub use checklist::Checklist;
pub use procedure::Procedure;
pub use source::Source;
pub use template::{Adapter, Renderer, Template};

use crate::language;

/// Render a Technique document using the specified template
pub fn render(template: &impl Template, document: &language::Document) -> String {
    template.render(document)
}
