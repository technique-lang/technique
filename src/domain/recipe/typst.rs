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
            // Check whether children have multiple distinct named roles.
            let mixed = has_mixed_roles(&self.children);

            out.content_open("children");
            let mut prev: Option<&String> = None;
            for child in &self.children {
                if mixed
                    && child
                        .role
                        .as_ref()
                        != prev
                {
                    if let Some(name) = &child.role {
                        if name != "*" {
                            out.call("render-role-heading");
                            out.param("name", name);
                            out.close();
                        }
                    }
                    prev = child
                        .role
                        .as_ref();
                }
                child.render(out);
            }
            out.content_close();
        }
        out.close();
    }
}

fn has_mixed_roles(children: &[Step]) -> bool {
    let mut seen: Option<&str> = None;
    for child in children {
        if let Some(r) = &child.role {
            if r != "*" {
                match seen {
                    None => seen = Some(r),
                    Some(prev) if prev != r => return true,
                    _ => {}
                }
            }
        }
    }
    false
}
