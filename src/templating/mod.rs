//! Templates for rendering Technique documents into formatted output

mod source;
mod template;

pub use source::Source;
pub use template::Template;

use crate::language::Document;

/// Render a Technique document using the specified template
pub fn fill(template: &impl Template, document: &Document, width: u8) -> String {
    template.render(document, width)
}
