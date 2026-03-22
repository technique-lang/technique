//! Typst serialization for recipe domain types.

use crate::domain::serialize::{render_prose_list, Markup, Render};

use super::types::{Document, Ingredient, Ingredients, Step};

impl Render for Document {
    fn render(&self, out: &mut Markup) {
        out.call("render-document");
        out.param_opt("title", &self.title);
        render_prose_list(out, "description", &self.description);
        if !self
            .ingredients
            .is_empty()
        {
            out.content_open("ingredients");
            for group in &self.ingredients {
                group.render(out);
            }
            out.content_close();
        }
        if !self
            .steps
            .is_empty()
        {
            out.content_open("method");
            for step in &self.steps {
                step.render(out);
            }
            out.content_close();
        }
        out.close();
    }
}

impl Render for Ingredients {
    fn render(&self, out: &mut Markup) {
        out.call("render-ingredients");
        out.param_opt("heading", &self.heading);
        render_prose_list(out, "description", &self.description);
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

impl Render for Ingredient {
    fn render(&self, out: &mut Markup) {
        out.call("render-ingredient");
        out.param("label", &self.label);
        out.param("quantity", &self.quantity);
        out.param_opt("source", &self.source);
        out.close();
    }
}

impl Render for Step {
    fn render(&self, out: &mut Markup) {
        out.call("render-step");
        out.param_opt("ordinal", &self.ordinal);
        out.param_opt("title", &self.title);
        render_prose_list(out, "description", &self.description);
        out.param_opt("role", &self.role);
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
