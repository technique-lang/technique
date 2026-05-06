// Translation of the internal parser abstract syntax tree to an internal
// intermediate representation.

use crate::language;
use crate::language::Document;

use super::types::Program;

pub fn translate<'i>(_document: &Document<'i>) -> Result<Program<'i>, Vec<TranslationError<'i>>> {
    Ok(Program::empty())
}

#[derive(Debug, Eq, PartialEq)]
pub enum TranslationError<'i> {
    DuplicateProcedure(language::Identifier<'i>),
    DuplicateTitle(language::Identifier<'i>),
    InterleavedDescription(language::Identifier<'i>),
    OrphanResponse,
}
