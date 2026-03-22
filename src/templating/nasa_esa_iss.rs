//! NASA/ESA ISS Crew Procedure domain — renders Technique documents in the
//! style of ISS crew procedures, with bordered tables, role designators, and
//! structured command/verification layout.

use crate::domain::nasa_esa_iss::adapter::NasaEsaIssAdapter;
use crate::domain::serialize::{Markup, Render};
use crate::domain::Adapter;
use crate::language;
use crate::templating::template::Template;

pub static TEMPLATE: &str = include_str!("nasa_esa_iss.typ");

pub struct NasaEsaIss;

impl Template for NasaEsaIss {
    fn markup(&self, document: &language::Document) -> String {
        let model = NasaEsaIssAdapter.extract(document);
        let mut out = Markup::new();
        model.render(&mut out);
        out.finish()
    }

    fn typst(&self) -> &str {
        TEMPLATE
    }

    fn domain(&self) -> &str {
        "nasa-esa-iss"
    }
}
