//! Interactive runner that walks a translated Program step-by-step,
//! prompting the operator and recording each completed step to a state store
//! so a run can be resumed after interruption.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::program::Program;

mod evaluator;
mod manifest;
mod path;
mod prompt;
mod runner;
mod state;

pub use runner::{Outcome, RunnerError};
pub use state::{RecordError, RunId};

use prompt::Console;
use runner::{now_iso8601, Runner};
use state::{construct_state_path, Appender, Store};

const STORE_ROOT: &str = ".store";

/// Allocate a new run, persist its manifest, and walk the program to
/// completion or until the user signals they are quitting.
pub fn start<'i>(
    document: &Path,
    program: &'i Program<'i>,
) -> Result<(RunId, Outcome), RunnerError> {
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (id, run_dir, _manifest) = store.create(document, now_iso8601())?;
    let pfftt = construct_state_path(&run_dir, document);
    let appender = Appender::open(pfftt)?;
    let mut runner = Runner::with_pieces(program, appender, HashSet::new(), Console::new());
    let outcome = runner.run()?;
    Ok((id, outcome))
}

/// Read the manifest of an existing run, returning the source document
/// path so the caller can load and re-translate it before resuming.
pub fn locate(id: RunId) -> Result<PathBuf, RunnerError> {
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (manifest, _, _) = store.open(id)?;
    Ok(manifest.document)
}

/// Open an existing run and walk the given program, short-circuiting
/// any step whose FQN has already been recorded.
pub fn resume<'i>(
    document: &Path,
    program: &'i Program<'i>,
    id: RunId,
) -> Result<Outcome, RunnerError> {
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (_manifest, completed, run_dir) = store.open(id)?;
    let pfftt = construct_state_path(&run_dir, document);
    let appender = Appender::open(pfftt)?;
    let mut runner = Runner::with_pieces(program, appender, completed, Console::new());
    runner.run()
}
