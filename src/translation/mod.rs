//! Translation from the surface AST (`crate::language`) to an Intermediate
//! Representation suitable for an interpreter.

mod translator;
mod types;

pub use translator::{translate, TranslationError};
pub use types::{
    Entry, Executable, Fragment, Invocable, Operation, Ordinal, Program, Subroutine, SubroutineId,
    SubroutineRef,
};

#[cfg(test)]
#[path = "checks/translate.rs"]
mod check;

#[cfg(test)]
#[path = "checks/errors.rs"]
mod errors;
