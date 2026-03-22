//! Recipe domain — extracts ingredients and method steps from a Technique
//! document describing a recipe.

use crate::domain::recipe::adapter::RecipeAdapter;
use crate::domain::serialize::{Markup, Render};
use crate::domain::Adapter;
use crate::language;
use crate::templating::template::Template;

pub static TEMPLATE: &str = include_str!("recipe.typ");

pub struct Recipe;

impl Template for Recipe {
    fn markup(&self, document: &language::Document) -> String {
        let model = RecipeAdapter.extract(document);
        let mut out = Markup::new();
        model.render(&mut out);
        out.finish()
    }

    fn typst(&self) -> &str {
        TEMPLATE
    }

    fn domain(&self) -> &str {
        "recipe"
    }
}
