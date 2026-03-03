//! Templates for rendering Technique documents into formatted output

mod checklist;
mod engine;
mod source;
mod template;

pub use checklist::Checklist;
pub use source::Source;
pub use template::Template;

use crate::language;

/// Render a Technique document using the specified template
pub fn fill(template: &impl Template, document: &language::Document, width: u8) -> String {
    template.render(document, width)
}
