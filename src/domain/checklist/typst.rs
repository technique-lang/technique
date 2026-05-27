//! Typst serialization for checklist domain types.

use crate::domain::serialize::{render_prose_list, Markup, Render};

use super::types::{Document, Item, Procedure, Response, Section, Step};

impl Render for Document {
    fn render(&self, out: &mut Markup) {
        out.call("render-document");
        if !self
            .items
            .is_empty()
        {
            out.content_open("children");
            for item in &self.items {
                item.render(out);
            }
            out.content_close();
        }
        out.close();
    }
}

impl Render for Item {
    fn render(&self, out: &mut Markup) {
        match self {
            Item::Step(s) => s.render(out),
            Item::Procedure(p) => p.render(out),
            Item::Section(s) => s.render(out),
        }
    }
}

impl Render for Procedure {
    fn render(&self, out: &mut Markup) {
        out.call("render-procedure");
        out.param("name", &self.name);
        out.param_opt("title", &self.title);
        render_prose_list(out, "description", &self.description);
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

impl Render for Section {
    fn render(&self, out: &mut Markup) {
        out.call("render-section");
        out.param_opt("ordinal", &self.ordinal);
        out.param_opt("heading", &self.heading);
        if !self
            .items
            .is_empty()
        {
            out.content_open("children");
            for item in &self.items {
                item.render(out);
            }
            out.content_close();
        }
        out.close();
    }
}

impl Render for Step {
    fn render(&self, out: &mut Markup) {
        out.call("render-step");
        out.param_opt("ordinal", &self.ordinal);
        out.param_opt("title", &self.title);
        render_prose_list(out, "body", &self.body);
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
