//! Domain types for checklists
//!
//! A checklist is moderately structured and relatively flat: sections with
//! headings, steps with checkboxes, response options, and limited nesting.

use crate::domain::typst::{Data, Render};

/// A checklist is a document of sections containing steps.
pub struct Document {
    pub sections: Vec<Section>,
}

impl Document {
    pub fn new() -> Self {
        Document {
            sections: Vec::new(),
        }
    }
}

impl Render for Document {
    fn render(&self, data: &mut Data) {
        data.open();
        data.list("sections", &self.sections);
        data.close();
    }
}

/// A section within a checklist.
pub struct Section {
    pub ordinal: Option<String>,
    pub heading: Option<String>,
    pub steps: Vec<Step>,
}

impl Render for Section {
    fn render(&self, data: &mut Data) {
        data.open();
        data.field("ordinal", &self.ordinal);
        data.field("heading", &self.heading);
        data.list("steps", &self.steps);
        data.close();
    }
}

/// A step within a checklist section.
pub struct Step {
    #[allow(dead_code)]
    pub name: Option<String>,
    pub ordinal: Option<String>,
    pub title: Option<String>,
    pub body: Vec<String>,
    pub role: Option<String>,
    pub responses: Vec<Response>,
    pub children: Vec<Step>,
}

impl Render for Step {
    fn render(&self, data: &mut Data) {
        data.open();
        data.field("ordinal", &self.ordinal);
        data.field("title", &self.title);
        data.list("body", &self.body);
        data.field("role", &self.role);
        data.list("responses", &self.responses);
        data.list("children", &self.children);
        data.close();
    }
}

/// A response option with an optional condition.
pub struct Response {
    pub value: String,
    pub condition: Option<String>,
}

impl Render for Response {
    fn render(&self, data: &mut Data) {
        data.open();
        data.field("value", &self.value);
        data.field("condition", &self.condition);
        data.close();
    }
}
