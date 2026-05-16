//! Interactive walker over a translated Program.

use std::io;
use std::path::PathBuf;

use super::state::{RecordError, RunId};

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
