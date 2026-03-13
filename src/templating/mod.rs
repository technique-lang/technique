//! Render Technique documents into formatted output.
//!
//! The **Template** trait provides `markup()` for Typst function-call
//! output. Each domain template composes an adapter from `crate::domain`
//! internally.

mod checklist;
mod procedure;
mod source;
mod template;

pub use checklist::Checklist;
pub use procedure::Procedure;
pub use source::Source;
pub use template::Template;

use crate::language;

/// Render a Technique document as Typst function-call markup.
pub fn markup(template: &impl Template, document: &language::Document) -> String {
    template.markup(document)
}
