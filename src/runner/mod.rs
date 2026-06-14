//! Interactive runner that walks a translated Program step-by-step,
//! prompting the operator and recording each completed step to a state store
//! so a run can be resumed after interruption.

use std::collections::HashSet;
use std::io::IsTerminal;
use std::path::{Path, PathBuf};

use crate::program::Program;

mod context;
mod driver;
mod evaluator;
mod library;
mod path;
mod runner;
mod state;

pub use context::Context;
pub use driver::{Headless, Mode};
pub use evaluator::Environment;
pub use library::{Builtin, Library, Native};
pub use runner::{Outcome, Runner, RunnerError};
pub use state::{Appender, RecordError, RunId};

use driver::{Automatic, Console};
use runner::{bind_parameters, now_iso8601};
use state::{construct_state_path, Record, State, Store};

const STORE_ROOT: &str = ".store";

/// Allocate a new run, write the opening `Start` record, and walk the program
/// to completion or until the user interrupts by signalling they are pausing
/// or quitting. Command-line arguments are bound to the entry procedure's
/// parameters before the beginning the walk.
pub fn start<'i>(
    mode: Mode,
    document: &Path,
    program: &'i Program<'i>,
    arguments: &[String],
    library: Library,
) -> Result<(RunId, Outcome), RunnerError> {
    let env = bind_parameters(program, arguments)?;
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (run_id, run_dir) = store.create(document, now_iso8601())?;
    let pfftt = construct_state_path(&run_dir, document);
    let appender = Appender::open(pfftt, run_id)?;
    let outcome = match mode {
        Mode::Interactive => {
            if !std::io::stdout().is_terminal() {
                return Err(RunnerError::TerminalRequired);
            }
            let mut runner =
                Runner::new(program, appender, HashSet::new(), Console::new(), library);
            runner.run(env)?
        }
        Mode::Automatic => {
            let mut runner =
                Runner::new(program, appender, HashSet::new(), Automatic::new(), library);
            runner.run(env)?
        }
    };
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
pub fn resume<'i>(
    run_id: RunId,
    program: &'i Program<'i>,
    library: Library,
) -> Result<Outcome, RunnerError> {
    if !std::io::stdout().is_terminal() {
        return Err(RunnerError::TerminalRequired);
    }
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
    let mut runner = Runner::new(program, appender, completed, Console::new(), library);
    let env = Environment::new();
    runner.run(env)
}
