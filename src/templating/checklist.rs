//! Checklist domain — flattens procedures into printable checklists.
//!
//! The checklist domain model is relatively flat: sections with headings,
//! steps with checkboxes, response options, and limited nesting. Role
//! assignments are inherited downward (an `@surgeon` scope annotates its
//! child steps) rather than forming structural containers.

use crate::domain::checklist::adapter::ChecklistAdapter;
use crate::domain::typst::{Data, Render};
use crate::domain::Adapter;
use crate::language;
use crate::templating::template::Template;

pub static TEMPLATE: &str = include_str!("checklist.typ");

pub struct Checklist;

impl Template for Checklist {
    fn data(&self, document: &language::Document) -> String {
        let model = ChecklistAdapter.extract(document);
        let mut data = Data::new();
        model.render(&mut data);
        data.finish()
    }

    fn typst(&self) -> Option<&str> {
        Some(TEMPLATE)
    }
}
