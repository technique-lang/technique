//! Interactive runner that walks a translated Program step-by-step,
//! prompting the user and recording each completed step to a state store
//! so a run can be resumed after interruption.

use std::io::IsTerminal;
use std::path::{Path, PathBuf};

use crate::program::Program;

mod context;
mod driver;
mod evaluator;
mod library;
mod path;
mod runner;

pub use context::Context;
pub use driver::{Headless, Intent, Mode, intent};
pub use evaluator::Environment;
pub use library::{Builtin, Library, Native, library_for};
pub use runner::{Conclusion, Outcome, Runner, RunnerError};

use crate::engraving::{
    Appender, Ledger, Record, RunId, Serial, State, Store, construct_state_path,
};
use driver::{Automatic, Console, Driver, Transcript};
use runner::{bind_parameters, now_iso8601};

const STORE_ROOT: &str = ".store";

/// Allocate a new run, write the opening `Start` record, and walk the program
/// to completion or until the user interrupts by signalling they are pausing
/// or quitting. Command-line arguments are bound to the entry procedure's
/// parameters before the beginning the walk. `Mode::Quiet` runs
/// non-interactively with the `Headless` driver, suppressing all chrome so only
/// executed commands' output reaches the terminal.
pub fn start<'i>(
    mode: Mode,
    colour: bool,
    document: &Path,
    program: &'i Program<'i>,
    arguments: &[String],
    library: Library,
    libraries: &[String],
) -> Result<(RunId, Conclusion), RunnerError> {
    let env = bind_parameters(program, arguments)?;
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (run_id, run_dir) = store.create(document, now_iso8601(), libraries)?;
    // The opening `Start` is written by the store, not the walk, so read it
    // back: it is the root position review climbs out to.
    let opening = store.read(run_id)?;
    let pfftt = construct_state_path(&run_dir, document);
    let appender = Appender::open(pfftt, run_id)?;
    let ledger = Ledger::new();
    let label = document_label(document);
    let outcome = match mode {
        Mode::Quiet => drive(
            Runner::new(program, appender, ledger, Headless::new(), library)
                .with_records(opening.clone())
                .with_context(Context::native(colour))
                .with_document(label),
            env,
        )?,
        Mode::Interactive => {
            if !std::io::stdout().is_terminal() {
                return Err(RunnerError::TerminalRequired);
            }
            drive(
                Runner::new(program, appender, ledger, Console::new(), library)
                    .with_records(opening.clone())
                    .with_context(Context::native(colour))
                    .with_document(label),
                env,
            )?
        }
        Mode::Automatic => drive(
            Runner::new(program, appender, ledger, Automatic::new(colour), library)
                .with_records(opening.clone())
                .with_context(Context::native(colour))
                .with_document(label),
            env,
        )?,
    };
    Ok((run_id, outcome))
}

/// Walk the program, starting again from the top each time the user amends a
/// recorded value. Every mode goes through here, so a restart is handled in
/// one place rather than at each of them. The fresh walk takes a fresh
/// environment: the entry procedure's arguments come back from its own
/// recorded `Begin`, as every other replayed value does.
fn drive<'i, D: Driver>(
    mut runner: Runner<'i, D>,
    env: Environment,
) -> Result<Conclusion, RunnerError> {
    let mut env = env;
    loop {
        let conclusion = runner.run(env)?;
        if let Conclusion::Restarting = conclusion {
            runner = runner.restart();
            env = Environment::new();
        } else {
            return Ok(conclusion);
        }
    }
}

/// Walk the program with the mode's driver wrapped in a `Transcript`, which
/// streams the value trail to stderr while the wrapped driver runs as usual.
/// Records nothing. Backs `run --output=native`, orthogonal to `--mode`.
pub fn inspect<'i>(
    mode: Mode,
    colour: bool,
    program: &'i Program<'i>,
    arguments: &[String],
    library: Library,
) -> Result<Conclusion, RunnerError> {
    let env = bind_parameters(program, arguments)?;
    match mode {
        Mode::Interactive => {
            if !std::io::stdout().is_terminal() {
                return Err(RunnerError::TerminalRequired);
            }
            let appender = Appender::sink();
            let ledger = Ledger::new();
            let driver = Transcript::new(Console::new());
            drive(
                Runner::new(program, appender, ledger, driver, library)
                    .with_context(Context::native(colour)),
                env,
            )
        }
        Mode::Automatic => {
            let appender = Appender::sink();
            let ledger = Ledger::new();
            let driver = Transcript::new(Automatic::new(colour));
            drive(
                Runner::new(program, appender, ledger, driver, library)
                    .with_context(Context::native(colour)),
                env,
            )
        }
        Mode::Quiet => {
            let appender = Appender::sink();
            let ledger = Ledger::new();
            let driver = Transcript::new(Headless::new());
            drive(
                Runner::new(program, appender, ledger, driver, library)
                    .with_context(Context::native(colour)),
                env,
            )
        }
    }
}

/// Read the opening `Start` record of an existing run, returning the source
/// document path and the libraries it was run with so the caller can load,
/// re-translate, and re-link it before resuming.
pub fn locate(run_id: RunId) -> Result<(PathBuf, Vec<String>), RunnerError> {
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (document, libraries, _, _) = store.open(run_id)?;
    Ok((document, libraries))
}

/// Load an existing run's recorded journal back into memory.
pub fn load(run_id: RunId) -> Result<Vec<Record>, RunnerError> {
    let store = Store::new(PathBuf::from(STORE_ROOT));
    Ok(store.read(run_id)?)
}

/// Open an existing run and walk the given program, short-circuiting
/// any step whose FQN has already been recorded. Appends a `Resume`
/// record at the root path before walking.
pub fn resume<'i>(
    run_id: RunId,
    program: &'i Program<'i>,
    library: Library,
) -> Result<Conclusion, RunnerError> {
    if !std::io::stdout().is_terminal() {
        return Err(RunnerError::TerminalRequired);
    }
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (document, _, ledger, run_dir) = store.open(run_id)?;
    let mut records = store.read(run_id)?;
    let pfftt = construct_state_path(&run_dir, &document);
    let mut appender = Appender::open(pfftt, run_id)?;
    let record = Record {
        recorded: now_iso8601(),
        run_id,
        serial: Serial::LIFECYCLE,
        path: "/".to_string(),
        state: State::Resume,
    };
    appender.append(&record)?;
    records.push(record);
    drive(
        Runner::new(program, appender, ledger, Console::new(), library)
            .with_records(records)
            .with_context(Context::native(std::io::stdout().is_terminal()))
            .with_document(document_label(&document)),
        Environment::new(),
    )
}

// The boundary trail lines name the document by its file stem (`NetworkProbe`
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
