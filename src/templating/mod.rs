//! Templates for rendering Technique documents into formatted output

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
