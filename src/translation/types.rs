// Intermediate Representation types for translated Technique procedures.
//
// These types complement those found in `crate::language::types` for the
// parser's abstract syntax tree but lifts and reshapes constructs toward a
// form that can be walked - and subsequently executed.

use crate::language::Identifier;

/// Top-level Technique translated to a runnable program.
#[derive(Debug, Eq, PartialEq, Default)]
pub struct Program<'i> {
    /// All procedures declared in the input document, in source order. If an
    /// anonymous wrapper for a top-level `Technique::Steps`-only document was
    /// created it will be at index 0.
    pub procedures: Vec<Procedure<'i>>,
}

impl<'i> Program<'i> {
    pub fn empty() -> Self {
        Program {
            procedures: Vec::new(),
        }
    }
}

/// Index of a procedure in `Program.procedures`. Used as the resolved form
/// of a procedure invocation.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct ProcedureId(pub usize);

#[derive(Debug, Eq, PartialEq)]
pub struct Procedure<'i> {
    /// If this is a synthetic wrapper around a top-level `Technique::Steps`
    /// then `None`, otherwise all procedures have names.
    pub name: Option<Identifier<'i>>,
    pub body: Block<'i>,
}

/// A sequence of operations forming any scope, be it procedure body, a step,
/// the body of a control structure creating a loop, etc.
#[derive(Debug, Eq, PartialEq, Default)]
pub struct Block<'i> {
    pub operations: Vec<Operation<'i>>,
}

/// One unit of work in a `Block`.
#[derive(Debug, Eq, PartialEq)]
pub enum Operation<'i> {
    Placeholder(std::marker::PhantomData<&'i ()>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum TranslationError<'i> {
    Placeholder(std::marker::PhantomData<&'i ()>),
}
