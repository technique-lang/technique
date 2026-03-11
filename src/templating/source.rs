//! Source domain — renders Technique source code with syntax highlighting.
//!
//! The source domain model is a flat sequence of syntax-tagged fragments
//! produced by the code formatter. The Typst template maps each syntax
//! tag to a colour and weight.

use crate::domain::source::adapter::SourceAdapter;
use crate::domain::typst::{Data, Render};
use crate::domain::Adapter;
use crate::language;
use crate::templating::template::Template;

pub static TEMPLATE: &str = include_str!("source.typ");

pub struct Source;

impl Template for Source {
    fn data(&self, document: &language::Document) -> String {
        let model = SourceAdapter.extract(document);
        let mut data = Data::new();
        model.render(&mut data);
        data.finish()
    }

    fn typst(&self) -> &str {
        TEMPLATE
    }
}
