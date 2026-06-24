//! Interactive runner that walks a translated Program step-by-step,
//! prompting the operator and recording each completed step to a state store
//! so a run can be resumed after interruption.

use std::collections::HashMap;
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
pub use library::{library_for, Builtin, Library, Native};
pub use runner::{Outcome, Runner, RunnerError};
pub use state::{Appender, RecordError, RunId};

use driver::{Automatic, Console, Transcript};
use runner::{bind_parameters, now_iso8601};
use state::{construct_state_path, Record, State, Store};

const STORE_ROOT: &str = ".store";

/// Allocate a new run, write the opening `Start` record, and walk the program
/// to completion or until the user interrupts by signalling they are pausing
/// or quitting. Command-line arguments are bound to the entry procedure's
/// parameters before the beginning the walk. `quiet` runs non-interactively
/// with the `Headless` driver, suppressing all chrome so only executed
/// commands' output reaches the terminal.
pub fn start<'i>(
    mode: Mode,
    quiet: bool,
    colour: bool,
    document: &Path,
    program: &'i Program<'i>,
    arguments: &[String],
    library: Library,
    libraries: &[String],
) -> Result<(RunId, Outcome), RunnerError> {
    let env = bind_parameters(program, arguments)?;
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (run_id, run_dir) = store.create(document, now_iso8601(), libraries)?;
    let pfftt = construct_state_path(&run_dir, document);
    let appender = Appender::open(pfftt, run_id)?;
    let completed = HashMap::new();
    let label = document_label(document);
    if quiet {
        let mut runner =
            Runner::new(program, appender, completed, Headless::new(), library).with_document(label);
        let outcome = runner.run(env)?;
        return Ok((run_id, outcome));
    }
    let outcome = match mode {
        Mode::Interactive => {
            if !std::io::stdout().is_terminal() {
                return Err(RunnerError::TerminalRequired);
            }
            let mut runner = Runner::new(program, appender, completed, Console::new(), library)
                .with_document(label);
            runner.run(env)?
        }
        Mode::Automatic => {
            let mut runner = Runner::new(
                program,
                appender,
                completed,
                Automatic::new(colour),
                library,
            )
            .with_document(label);
            runner.run(env)?
        }
    };
    Ok((run_id, outcome))
}

/// Walk the program with the mode's driver wrapped in a `Transcript`, which
/// streams the value trace to stderr while the wrapped driver runs as usual.
/// Records nothing. Backs `run --output=native`, orthogonal to `--mode`.
pub fn inspect<'i>(
    mode: Mode,
    colour: bool,
    program: &'i Program<'i>,
    arguments: &[String],
    library: Library,
) -> Result<Outcome, RunnerError> {
    let env = bind_parameters(program, arguments)?;
    match mode {
        Mode::Interactive => {
            if !std::io::stdout().is_terminal() {
                return Err(RunnerError::TerminalRequired);
            }
            let appender = Appender::sink();
            let completed = HashMap::new();
            let driver = Transcript::new(Console::new());
            let mut runner = Runner::new(program, appender, completed, driver, library);
            runner.run(env)
        }
        Mode::Automatic => {
            let appender = Appender::sink();
            let completed = HashMap::new();
            let driver = Transcript::new(Automatic::new(colour));
            let mut runner = Runner::new(program, appender, completed, driver, library);
            runner.run(env)
        }
    }
}

/// Read the opening `Start` record of an existing run, returning the source
/// document path and the libraries it was run with so the caller can load,
/// re-translate, and re-link it before resuming.
pub fn locate(run_id: RunId) -> Result<(PathBuf, Vec<String>), RunnerError> {
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (document, libraries, _, _, _) = store.open(run_id)?;
    Ok((document, libraries))
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
    let (document, _, completed, inputs, run_dir) = store.open(run_id)?;
    let pfftt = construct_state_path(&run_dir, &document);
    let mut appender = Appender::open(pfftt, run_id)?;
    let record = Record {
        recorded: now_iso8601(),
        run_id,
        path: "/".to_string(),
        state: State::Resume,
    };
    appender.append(&record)?;
    let mut runner = Runner::new(program, appender, completed, Console::new(), library)
        .with_inputs(inputs)
        .with_document(document_label(&document));
    let env = Environment::new();
    runner.run(env)
}

// The boundary trace lines name the document by its file stem (`NetworkProbe`
// for `NetworkProbe.tq`), matching the PFFTT file the run writes to.
fn document_label(document: &Path) -> String {
    document
        .file_stem()
        .map(|s| {
            s.to_string_lossy()
                .into_owned()
        })
        .unwrap_or_default()
}
