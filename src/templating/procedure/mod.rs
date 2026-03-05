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
use crate::templating::template::Template;

use crate::templating::template::Adapter;
use crate::templating::template::Renderer;
use adapter::ProcedureAdapter;
use renderer::ProcedureRenderer;

/// Procedure template: adapter + renderer composition.
pub struct Procedure;

impl Template for Procedure {
    fn render(&self, document: &language::Document) -> String {
        let model = ProcedureAdapter.extract(document);
        ProcedureRenderer.render(&model)
    }
}
