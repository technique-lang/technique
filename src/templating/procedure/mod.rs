//! Renders procedures preserving the full hierarchy described by the source
//! Technique document.
//!
//! Unlike the checklist template (which flattens structure), the procedure
//! domain model preserves hierarchy. Sections with ordinals, role groups as
//! distinct items rather than step annotations, and nested children.

mod adapter;
mod renderer;
pub mod types;

use crate::language;
use crate::templating::template::{Adapter, Template};
use crate::templating::typst::{Data, Render};
use adapter::ProcedureAdapter;

/// Procedure template: adapter + renderer composition.
pub struct Procedure;

impl Template for Procedure {
    fn render(&self, document: &language::Document) -> String {
        let model = ProcedureAdapter.extract(document);
        renderer::markup(&model)
    }

    fn data(&self, document: &language::Document) -> String {
        let model = ProcedureAdapter.extract(document);
        let mut data = Data::new();
        model.render(&mut data);
        data.finish()
    }
}
