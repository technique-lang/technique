//! Source domain — renders Technique source code with syntax highlighting.
//!
//! The source domain model is a flat sequence of syntax-tagged fragments
//! produced by the code formatter. The Typst template maps each syntax
//! tag to a colour and weight.

use crate::domain::serialize::{Markup, Render};
use crate::domain::source::adapter::SourceAdapter;
use crate::domain::Adapter;
use crate::language;
use crate::templating::template::Template;

pub static TEMPLATE: &str = include_str!("source.typ");

pub struct Source;

impl Template for Source {
    fn markup(&self, document: &language::Document) -> String {
        let model = SourceAdapter.extract(document);
        let mut out = Markup::new();
        model.render(&mut out);
        out.finish()
    }

    fn typst(&self) -> &str {
        TEMPLATE
    }

    fn domain(&self) -> &str {
        "source"
    }
}
