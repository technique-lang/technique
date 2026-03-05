//! Checklist template — flattens procedures into printable checklists.
//!
//! The checklist domain model is relatively flat: sections with headings,
//! steps with checkboxes, response options, and limited nesting. Role
//! assignments are inherited downward (an `@surgeon` scope annotates its
//! child steps) rather than forming structural containers.

mod adapter;
mod renderer;
pub mod types;

use crate::language;
use crate::templating::template::Template;

use crate::templating::template::Adapter;
use crate::templating::template::Renderer;
use adapter::ChecklistAdapter;
use renderer::ChecklistRenderer;

/// Checklist template: adapter + renderer composition.
pub struct Checklist;

impl Template for Checklist {
    fn render(&self, document: &language::Document) -> String {
        let model = ChecklistAdapter.extract(document);
        ChecklistRenderer.render(&model)
    }
}
