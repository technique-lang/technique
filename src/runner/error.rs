//! Errors raised while preparing or running a Technique.

use std::io;

use crate::engraving::StoreError;

/// Anything that can go wrong while preparing or running a Technique.
#[derive(Debug)]
pub enum RunnerError {
    Store(StoreError),
    MissingEntryProcedure,
    UnboundVariable(String),
    BindArityMismatch {
        expected: usize,
        actual: usize,
    },
    BindNotTuple {
        expected: usize,
    },
    NotIterable,
    InvalidCost,
    InvalidArgument {
        function: &'static str,
        expected: &'static str,
    },
    UnknownFunction(String),
    FunctionArityMismatch {
        function: &'static str,
        expected: usize,
        actual: usize,
    },
    ExecError(io::Error),
    CommandFailed(i32),
    IncompatibleCombination {
        left: &'static str,
        right: &'static str,
    },
    ParameterArityMismatch {
        procedure: String,
        parameters: Vec<String>,
        actual: usize,
    },
    ParameterUnexpected {
        procedure: String,
        actual: usize,
    },
    MalformedArgument {
        parameter: String,
        argument: String,
    },
    MalformedList {
        text: String,
    },
    RecursionLimit {
        procedure: String,
        depth: usize,
    },
    TerminalRequired,
}

impl From<StoreError> for RunnerError {
    fn from(error: StoreError) -> Self {
        RunnerError::Store(error)
    }
}
