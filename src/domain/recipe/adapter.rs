//! Projects the parser's AST into the recipe domain model.

use crate::domain::Adapter;
use crate::language;

use super::types::Document;

pub struct RecipeAdapter;

impl Adapter for RecipeAdapter {
    type Model = Document;

    fn extract(&self, _document: &language::Document) -> Document {
        Document::new()
    }
}
