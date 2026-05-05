//! Translation from the surface AST (`crate::language`) to an Intermediate
//! Representation suitable for a runner.

mod passes;
mod types;

pub use passes::translate;
pub use types::{Block, Operation, Procedure, ProcedureId, Program, TranslationError};

#[cfg(test)]
#[path = "checks/translate.rs"]
mod check;

#[cfg(test)]
#[path = "checks/errors.rs"]
mod errors;
