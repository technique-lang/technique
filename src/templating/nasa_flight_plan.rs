//! NASA flight plan domain — renders Technique documents in the style of ISS
//! flight procedures, with bordered tables, role designators, and structured
//! command/verification layout.

use crate::domain::nasa_flight_plan::adapter::NasaFlightPlanAdapter;
use crate::domain::serialize::{Markup, Render};
use crate::domain::Adapter;
use crate::language;
use crate::templating::template::Template;

pub static TEMPLATE: &str = include_str!("nasa_flight_plan.typ");

pub struct NasaFlightPlan;

impl Template for NasaFlightPlan {
    fn markup(&self, document: &language::Document) -> String {
        let model = NasaFlightPlanAdapter.extract(document);
        let mut out = Markup::new();
        model.render(&mut out);
        out.finish()
    }

    fn typst(&self) -> &str {
        TEMPLATE
    }

    fn domain(&self) -> &str {
        "nasa-flight-plan"
    }
}
