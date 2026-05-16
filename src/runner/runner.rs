//! Interactive walker over a translated Program.

use std::io;
use std::path::PathBuf;

use super::evaluator::Environment;
use super::path::{PathSegment, QualifiedPath};
use super::prompt::{Prompt, UserInput};
use super::state::{Appender, Outcome as RecordOutcome, Record, RecordError, RunId};
use crate::program::{Operation, Ordinal, Program};
use crate::value::Value;

/// What executing an Operation (or evaluating a Step at any scale)
/// produced. `Done(Value)` is the natural success — for a leaf Step
/// the operator's recorded value, for a Sequence / Section / procedure
/// body the unit value once the whole subtree is finished. `Skipped` and
/// `Failed` are operator verdicts on individual Steps. `Quit` is a
/// control signal that propagates immediately up the call stack:
/// nothing is recorded, a `technique resume` would pick up where
/// this run paused.
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Outcome {
    Done(Value),
    Skipped,
    Failed(Failure),
    Quit,
}

/// Why a Step failed.
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Failure {
    Aborted(String),
}

/// Anything that can go wrong while preparing or running a Technique.
/// Variants are populated as the implementing steps land; the formatter
/// in `crate::problem` knows how to render each one.
#[allow(dead_code)]
#[derive(Debug)]
pub enum RunnerError {
    NoSuchRun(RunId),
    StoreError { path: PathBuf, error: io::Error },
    MalformedRecord { run: RunId, error: RecordError },
    ManifestMissing(RunId),
    InvalidRunId(String),
    MissingEntryProcedure,
    UnboundVariable(String),
    BindArityMismatch { expected: usize, actual: usize },
    UserQuit,
}
