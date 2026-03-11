//! Renders procedures preserving the full hierarchy described by the source
//! Technique document.
//!
//! Unlike the checklist template (which flattens structure), the procedure
//! domain model preserves hierarchy. Sections with ordinals, role groups as
//! distinct items rather than step annotations, and nested children.

mod renderer;

use crate::domain::procedure::adapter::ProcedureAdapter;
use crate::domain::typst::{Data, Render};
use crate::domain::Adapter;
use crate::language;
use crate::templating::template::Template;

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
