// Translation of the internal parser abstract syntax tree to an internal
// intermediate representation.

use crate::language::Document;

use super::types::{Program, TranslationError};

pub fn translate<'i>(_document: &Document<'i>) -> Result<Program<'i>, Vec<TranslationError<'i>>> {
    Ok(Program::empty())
}
