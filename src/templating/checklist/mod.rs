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
use crate::templating::template::{Adapter, Template};
use crate::templating::typst::{Data, Render};
use adapter::ChecklistAdapter;

/// Checklist template: adapter + renderer composition.
pub struct Checklist;

impl Template for Checklist {
    fn render(&self, document: &language::Document) -> String {
        let model = ChecklistAdapter.extract(document);
        renderer::markup(&model)
    }

    fn data(&self, document: &language::Document) -> String {
        let model = ChecklistAdapter.extract(document);
        let mut data = Data::new();
        model.render(&mut data);
        data.finish()
    }
}
