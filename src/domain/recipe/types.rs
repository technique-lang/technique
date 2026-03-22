//! Domain types for recipes.
//!
//! A recipe has a title, descriptive introduction, a consolidated list of
//! ingredients grouped by sub-recipe, and method steps describing the
//! preparation. Ingredients are extracted from tablets found in procedures;
//! method steps come from the remaining procedural content.

pub use crate::domain::engine::{Inline, Prose};

/// A document describing a recipe.
pub struct Document {
    pub title: Option<String>,
    pub description: Vec<Prose>,
    pub ingredients: Vec<Ingredients>,
    pub steps: Vec<Step>,
}

impl Document {
    pub fn new() -> Self {
        Document {
            title: None,
            description: Vec::new(),
            ingredients: Vec::new(),
            steps: Vec::new(),
        }
    }
}

/// A group of ingredients, typically corresponding to a sub-recipe or
/// component (e.g. "Turkey", "Stuffing", "Breadsauce").
pub struct Ingredients {
    pub heading: Option<String>,
    pub items: Vec<Ingredient>,
}

/// A single ingredient with its quantity and optional source.
pub struct Ingredient {
    pub label: String,
    pub quantity: String,
    pub source: Option<String>,
}

/// A step within the recipe method.
pub struct Step {
    pub ordinal: Option<String>,
    pub text: Option<String>,
    pub role: Option<String>,
    pub children: Vec<Step>,
}
