//! Render Technique documents into formatted output.
//!
//! The **Template** trait acts as a top-level interface providing `render()`
//! for Typst markup (PDF path) and `data()` for Typst data literals. Each
//! domain template composes an adapter from `crate::domain` internally.

mod checklist;
mod procedure;
mod source;
mod template;
pub mod typst;

pub use checklist::Checklist;
pub use procedure::Procedure;
pub use source::Source;
pub use template::Template;

use crate::language;

/// Render a Technique document using the specified template.
pub fn render(template: &impl Template, document: &language::Document) -> String {
    template.render(document)
}

/// Serialize a Technique document as a Typst data literal.
pub fn data(template: &impl Template, document: &language::Document) -> String {
    template.data(document)
}
