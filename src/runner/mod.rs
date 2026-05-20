//! Interactive runner that walks a translated Program step-by-step,
//! prompting the operator and recording each completed step to a state store
//! so a run can be resumed after interruption.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::program::Program;

mod evaluator;
mod path;
mod prompt;
mod runner;
mod state;

pub use runner::{Outcome, RunnerError};
pub use state::{RecordError, RunId};

use prompt::Console;
use runner::{now_iso8601, Runner};
use state::{construct_state_path, Appender, Record, State, Store};

const STORE_ROOT: &str = ".store";

/// Allocate a new run, write the opening `Start` record, and walk the program
/// to completion or until the user interrupts by signalling they are pausing
/// or quitting.
pub fn start<'i>(
    document: &Path,
    program: &'i Program<'i>,
) -> Result<(RunId, Outcome), RunnerError> {
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (run_id, run_dir) = store.create(document, now_iso8601())?;
    let pfftt = construct_state_path(&run_dir, document);
    let appender = Appender::open(pfftt, run_id)?;
    let mut runner = Runner::with_pieces(program, appender, HashSet::new(), Console::new());
    let outcome = runner.run()?;
    Ok((run_id, outcome))
}

/// Read the opening `Start` record of an existing run, returning the
/// source document path so the caller can load and re-translate it
/// before resuming.
pub fn locate(run_id: RunId) -> Result<PathBuf, RunnerError> {
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (document, _, _) = store.open(run_id)?;
    Ok(document)
}

/// Open an existing run and walk the given program, short-circuiting
/// any step whose FQN has already been recorded. Appends a `Resume`
/// record at the root path before walking.
pub fn resume<'i>(run_id: RunId, program: &'i Program<'i>) -> Result<Outcome, RunnerError> {
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (document, completed, run_dir) = store.open(run_id)?;
    let pfftt = construct_state_path(&run_dir, &document);
    let mut appender = Appender::open(pfftt, run_id)?;
    let record = Record {
        recorded: now_iso8601(),
        run_id,
        path: "/".to_string(),
        state: State::Resume,
    };
    appender.append(&record)?;
    let mut runner = Runner::with_pieces(program, appender, completed, Console::new());
    runner.run()
}
