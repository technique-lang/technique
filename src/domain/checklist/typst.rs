//! Typst serialization for checklist domain types.

use crate::domain::serialize::{Markup, Render};

use super::types::{Document, Response, Section, Step};

impl Render for Document {
    fn render(&self, out: &mut Markup) {
        if self.name.is_some() || self.title.is_some() {
            out.call("render-document");
            out.param_opt("name", &self.name);
            out.param_opt("title", &self.title);
            out.close();
        }
        for section in &self.sections {
            section.render(out);
        }
    }
}

impl Render for Section {
    fn render(&self, out: &mut Markup) {
        out.call("render-section");
        out.param_opt("ordinal", &self.ordinal);
        out.param_opt("heading", &self.heading);
        if !self
            .steps
            .is_empty()
        {
            out.content_open("children");
            for step in &self.steps {
                step.render(out);
            }
            out.content_close();
        }
        out.close();
    }
}

impl Render for Step {
    fn render(&self, out: &mut Markup) {
        if self.name.is_some() {
            out.call("render-procedure");
            out.param_opt("name", &self.name);
            out.param_opt("title", &self.title);
        } else {
            out.call("render-step");
            out.param_opt("ordinal", &self.ordinal);
            out.param_opt("title", &self.title);
        }
        out.param_list("body", &self.body);
        out.param_opt("role", &self.role);
        if !self
            .responses
            .is_empty()
        {
            out.content_open("responses");
            for r in &self.responses {
                r.render(out);
            }
            out.content_close();
        }
        if !self
            .children
            .is_empty()
        {
            out.content_open("children");
            for child in &self.children {
                child.render(out);
            }
            out.content_close();
        }
        out.close();
    }
}

impl Render for Response {
    fn render(&self, out: &mut Markup) {
        out.call("render-response");
        out.param("value", &self.value);
        out.param_opt("condition", &self.condition);
        out.close();
    }
}
