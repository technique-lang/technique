//! Translation from the surface AST (`crate::language`) to an Intermediate
//! Representation suitable for an interpreter.

mod translator;

pub use translator::{translate, TranslationError};

#[cfg(test)]
#[path = "checks/translate.rs"]
mod check;

#[cfg(test)]
#[path = "checks/errors.rs"]
mod errors;
