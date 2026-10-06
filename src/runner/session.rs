//! A session: one `run` or `resume` against a run's journal. The procedure is
//! walked from the top, and walked again after each amendment made in review.

use std::io::IsTerminal;
use std::path::{Path, PathBuf};

use super::context::Context;
use super::driver::{
    Automatic, Console, Driver, Event, Frame, Headless, Marker, Mode, Offer, Review, Transcript,
    Verdict,
};
use super::error::RunnerError;
use super::evaluator;
use super::library::{Library, now_iso8601};
use super::walker::{self, Amendment, Change, Halt, Outcome, reason_of, verdict_of};
use crate::engraving::{
    Appender, History, InvokeTarget, Journal, Position, Record, RunId, Serial, State, Store,
    Supplied, construct_source_path, construct_state_path,
};
use crate::program::Program;
use crate::value::Value;

const STORE_ROOT: &str = ".store";

/// How a session ended.
#[derive(Debug, Clone, PartialEq)]
pub enum Conclusion {
    Completed(Outcome),
    Stopping,
}

/// How the user left review.
pub(super) enum Reviewed {
    Leave,
    Quit,
    /// The position to amend; its `Revoke` is held back, not yet written.
    Amend(Amendment),
}

pub struct Runner<'i, D: Driver> {
    pub(super) program: &'i Program<'i>,
    appender: Appender,
    pub(super) records: Vec<Record>,
    /// How many of the last `records` are not yet written to the file.
    pub(super) pending: usize,
    wrote: bool,
    pub(super) driver: D,
    pub(super) library: Library,
    pub(super) context: Context,
    document: Option<String>,
}

impl<'i, D: Driver> Runner<'i, D> {
    /// `records` is the journal so far: empty, the opening `Start`, or all of
    /// a run being resumed.
    pub fn new(
        program: &'i Program<'i>,
        appender: Appender,
        records: Vec<Record>,
        driver: D,
        library: Library,
    ) -> Self {
        Runner {
            program,
            appender,
            records,
            pending: 0,
            wrote: false,
            driver,
            library,
            context: Context::native(false),
            document: None,
        }
    }

    pub fn with_context(mut self, context: Context) -> Self {
        self.context = context;
        self
    }

    /// Name the source document, so the trail opens and closes with it.
    pub fn with_document(mut self, document: String) -> Self {
        self.document = Some(document);
        self
    }

    pub fn into_driver(self) -> D {
        self.driver
    }

    pub fn into_appender(self) -> Appender {
        self.appender
    }

    /// Walk until the entry procedure finishes or the user quits. `arguments`
    /// supply the entry procedure's `Begin` if the journal has none.
    pub fn run(&mut self, arguments: Vec<Supplied>) -> Result<Conclusion, RunnerError> {
        let label = self
            .document
            .as_ref()
            .map(|document| {
                format!(
                    "/ {},1 #{}",
                    document,
                    self.appender
                        .run_id()
                        .render()
                )
            });
        if let Some(label) = &label {
            self.driver
                .show(Event::Commence { label });
        }
        if let Some(metadata) = self
            .program
            .prelude
        {
            let header = crate::formatting::render_header(
                metadata,
                self.driver
                    .renderer(),
            );
            self.driver
                .show(Event::Display(&header));
        }

        let history = History::new(&self.records);
        let mut amendment = None;
        // A finished run replays its trail, writing nothing, then opens
        // review.
        let mut replaying = history.finished();
        if !replaying
            && !history
                .roots()
                .is_empty()
        {
            self.defer(Serial::ROOT, "/", State::Resume);
        }

        loop {
            let history = History::new(&self.records);
            match walker::walk(self, &history, amendment.take(), &arguments) {
                Ok(outcome) if replaying => match self.review(None, &[])? {
                    Reviewed::Amend(chosen) => {
                        replaying = false;
                        amendment = Some(chosen);
                        self.driver
                            .show(Event::Restart);
                    }
                    Reviewed::Leave | Reviewed::Quit => {
                        return Ok(Conclusion::Completed(outcome));
                    }
                },
                Ok(outcome) => {
                    self.append(Serial::ROOT, "/", State::Finish)?;
                    if let Some(label) = &label {
                        self.driver
                            .show(Event::Conclude {
                                label,
                                verdict: &verdict_of(&outcome),
                            });
                    }
                    return Ok(Conclusion::Completed(outcome));
                }
                Err(Halt::Stop) => return Ok(Conclusion::Stopping),
                Err(Halt::Restart(chosen)) => {
                    replaying = false;
                    amendment = Some(chosen);
                    self.driver
                        .show(Event::Restart);
                }
                Err(Halt::Error(error)) => return Err(error),
            }
        }
    }

    /// Write a record to the file, after any held back by `defer()`.
    pub(super) fn append(
        &mut self,
        serial: Serial,
        path: &str,
        state: State,
    ) -> Result<(), RunnerError> {
        let record = self.record(serial, path, state);
        let n = self
            .records
            .len()
            - self.pending;
        for held in &self.records[n..] {
            self.appender
                .append(held)?;
        }
        self.pending = 0;
        self.appender
            .append(&record)?;
        self.wrote = true;
        self.records
            .push(record);
        Ok(())
    }

    /// Add a record to the journal in memory, holding it back from the file
    /// until `append()` writes something else.
    pub(super) fn defer(&mut self, serial: Serial, path: &str, state: State) {
        let record = self.record(serial, path, state);
        self.records
            .push(record);
        self.pending += 1;
    }

    /// Discard the records held back, and write `Stop` only if this session
    /// wrote anything.
    pub(super) fn stop(&mut self) -> Result<(), RunnerError> {
        let n = self
            .records
            .len()
            - self.pending;
        self.records
            .truncate(n);
        self.pending = 0;
        if self.wrote {
            self.append(Serial::ROOT, "/", State::Stop)?;
        }
        Ok(())
    }

    fn record(&self, serial: Serial, path: &str, state: State) -> Record {
        Record {
            recorded: now_iso8601(),
            run_id: self
                .appender
                .run_id(),
            serial,
            path: path.to_string(),
            state,
        }
    }

    /// Move the review cursor over the journal until the user leaves, quits,
    /// or amends a position. `prompt` is the scope whose prompt the user came
    /// from, `None` for a finished run; `asked` are the invocations whose
    /// arguments were prompted for.
    pub(super) fn review(
        &mut self,
        prompt: Option<Serial>,
        asked: &[Serial],
    ) -> Result<Reviewed, RunnerError> {
        let history = History::new(&self.records);
        let journal = Journal::new(&self.records, prompt);
        let Some(mut at) = journal.opening() else {
            return Ok(Reviewed::Leave);
        };
        let (serial, path, change) = loop {
            let Position::At(i) = at else {
                return Ok(Reviewed::Leave);
            };
            let record = &self.records[i];
            let verdict = verdict_at(&record.state);
            let encloses = match history.get(record.serial) {
                Some(a) => !a
                    .children
                    .is_empty(),
                None => false,
            };
            let offers = offers_at(record, verdict.as_ref(), encloses, asked);
            let bound = names_bound(&record.state);
            let reply = self
                .driver
                .review(Frame {
                    marker: marker_of(record),
                    path: &record.path,
                    bound: &bound,
                    verdict: verdict.as_ref(),
                    offers: &offers,
                });
            let change = match reply {
                Review::Move(motion) => {
                    match journal.step(at, motion) {
                        Some(Position::Live) => return Ok(Reviewed::Leave),
                        Some(next) => at = next,
                        None => {}
                    }
                    continue;
                }
                Review::Leave => return Ok(Reviewed::Leave),
                Review::Quit | Review::Chose(Offer::Quit) => return Ok(Reviewed::Quit),
                Review::Chose(Offer::Edit) => match verdict {
                    Some(_) => Change::Redo,
                    None => Change::Reask,
                },
                Review::Chose(Offer::Skip) => Change::Skip,
                Review::Chose(Offer::Override) => Change::Override,
                Review::Chose(Offer::Fail) => Change::Fail(String::new()),
                Review::Reason(reason) => Change::Fail(reason),
            };
            break (
                record.serial,
                record
                    .path
                    .clone(),
                change,
            );
        };
        // A finished run ended with `Finish`, so amending it resumes it
        // first.
        if history.finished() {
            self.defer(Serial::ROOT, "/", State::Resume);
        }
        self.defer(serial, &path, State::Revoke);
        Ok(Reviewed::Amend(Amendment { serial, change }))
    }
}

// The verdict an outcome record carries, `None` for any other record.
fn verdict_at(state: &State) -> Option<Verdict> {
    match state {
        State::Done(value) => Some(Verdict::Done(match value {
            Some(value) => value.clone(),
            None => Value::Unitus,
        })),
        State::Skip => Some(Verdict::Skip),
        State::Fail(reason) => Some(Verdict::Fail(reason_of(reason))),
        _ => None,
    }
}

// What review offers at a record: Edit at the outcome of a step enclosing
// nothing, or at the `Begin` of a call whose arguments were prompted for;
// Skip and Fail at any outcome, Override at a Fail; Quit everywhere.
fn offers_at(
    record: &Record,
    verdict: Option<&Verdict>,
    encloses: bool,
    asked: &[Serial],
) -> Vec<Offer> {
    let mut offers = Vec::new();
    if let Some(verdict) = verdict {
        if !encloses && marker_of(record) == Marker::Step {
            offers.push(Offer::Edit);
        }
        offers.push(Offer::Skip);
        offers.push(Offer::Fail);
        if let Verdict::Fail(_) = verdict {
            offers.push(Offer::Override);
        }
    } else if let State::Begin(_) = record.state {
        if asked.contains(&record.serial) {
            offers.push(Offer::Edit);
        }
    }
    offers.push(Offer::Quit);
    offers
}

/// The marker the live trail drew a record with: its path's last segment says
/// what kind of scope it is, and its state whether it was entered or left.
fn marker_of(record: &Record) -> Marker {
    if let State::Invoke(target) = &record.state {
        return match target {
            InvokeTarget::Uri(_) => Marker::Depart,
            InvokeTarget::Procedure(_) => Marker::Step,
        };
    }
    let leaving = match record.state {
        State::Done(_) | State::Skip | State::Fail(_) | State::Finish | State::Stop => true,
        _ => false,
    };
    // An external's last segment is a `<URI>`, which may contain slashes.
    let edge = match record
        .path
        .rfind("/<")
    {
        Some(i)
            if record
                .path
                .ends_with('>') =>
        {
            &record.path[i + 1..]
        }
        _ => record
            .path
            .rsplit('/')
            .next()
            .unwrap_or(""),
    };
    if record.path == "/" || (edge.starts_with('<') && edge.ends_with('>')) {
        return if leaving {
            Marker::Return
        } else {
            Marker::Depart
        };
    }
    let nests = edge.ends_with(':')
        || (edge.starts_with('[') && edge.ends_with(']'))
        || (!edge.is_empty()
            && edge
                .chars()
                .all(|c| "IVXLCDM".contains(c)));
    match (nests, leaving) {
        (true, true) => Marker::Close,
        (true, false) => Marker::Enter,
        (false, _) => Marker::Step,
    }
}

// `~ a, b` for a `Bind`, empty for anything else.
fn names_bound(state: &State) -> String {
    let State::Bind(bound) = state else {
        return String::new();
    };
    let names: Vec<&str> = bound
        .iter()
        .filter_map(|item| match &item.name {
            Some(name) => Some(name.as_str()),
            None => None,
        })
        .collect();
    format!("~ {}", names.join(", "))
}

/// Allocate a new run, write its `Start`, and walk it to completion or until
/// the user quits.
pub fn start<'i>(
    mode: Mode,
    colour: bool,
    document: &Path,
    source: &str,
    program: &'i Program<'i>,
    arguments: &[String],
    library: Library,
    libraries: &[String],
) -> Result<(RunId, Conclusion), RunnerError> {
    let supplied = bind_parameters(program, arguments)?;
    if let Mode::Interactive = mode {
        if !std::io::stdout().is_terminal() {
            return Err(RunnerError::TerminalRequired);
        }
    }
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (run_id, _) = store.create(document, source, now_iso8601(), libraries)?;
    let conclusion = walk(run_id, mode, colour, program, library, supplied)?;
    Ok((run_id, conclusion))
}

/// Walk an existing run again from the top against its journal. A finished
/// run opens in review.
pub fn resume<'i>(
    run_id: RunId,
    mode: Mode,
    colour: bool,
    program: &'i Program<'i>,
    library: Library,
) -> Result<Conclusion, RunnerError> {
    if let Mode::Interactive = mode {
        if !std::io::stdout().is_terminal() {
            return Err(RunnerError::TerminalRequired);
        }
    }
    walk(run_id, mode, colour, program, library, Vec::new())
}

fn walk<'i>(
    run_id: RunId,
    mode: Mode,
    colour: bool,
    program: &'i Program<'i>,
    library: Library,
    arguments: Vec<Supplied>,
) -> Result<Conclusion, RunnerError> {
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (document, _, run_dir) = store.open(run_id)?;
    let (appender, records) = Appender::open(construct_state_path(&run_dir, &document), run_id)?;
    let label = document_label(&document);
    let context = Context::native(colour);
    match mode {
        Mode::Interactive => drive(
            Runner::new(program, appender, records, Console::new(), library),
            context,
            label,
            arguments,
        ),
        Mode::Automatic => drive(
            Runner::new(program, appender, records, Automatic::new(colour), library),
            context,
            label,
            arguments,
        ),
        Mode::Quiet => drive(
            Runner::new(program, appender, records, Headless::new(), library),
            context,
            label,
            arguments,
        ),
    }
}

fn drive<'i, D: Driver>(
    runner: Runner<'i, D>,
    context: Context,
    label: String,
    arguments: Vec<Supplied>,
) -> Result<Conclusion, RunnerError> {
    runner
        .with_context(context)
        .with_document(label)
        .run(arguments)
}

/// Walk with the mode's driver inside a `Transcript` streaming the value
/// trail to stderr, recording nothing. Backs `run --output=native`.
pub fn inspect<'i>(
    mode: Mode,
    colour: bool,
    program: &'i Program<'i>,
    arguments: &[String],
    library: Library,
) -> Result<Conclusion, RunnerError> {
    let supplied = bind_parameters(program, arguments)?;
    let context = Context::native(colour);
    match mode {
        Mode::Interactive => {
            if !std::io::stdout().is_terminal() {
                return Err(RunnerError::TerminalRequired);
            }
            let driver = Transcript::new(Console::new());
            Runner::new(program, Appender::sink(), Vec::new(), driver, library)
                .with_context(context)
                .run(supplied)
        }
        Mode::Automatic => {
            let driver = Transcript::new(Automatic::new(colour));
            Runner::new(program, Appender::sink(), Vec::new(), driver, library)
                .with_context(context)
                .run(supplied)
        }
        Mode::Quiet => {
            let driver = Transcript::new(Headless::new());
            Runner::new(program, Appender::sink(), Vec::new(), driver, library)
                .with_context(context)
                .run(supplied)
        }
    }
}

/// The copy of the source document kept in an existing run's directory, and
/// the libraries the run was started with.
pub fn locate(run_id: RunId) -> Result<(PathBuf, Vec<String>), RunnerError> {
    let store = Store::new(PathBuf::from(STORE_ROOT));
    let (document, libraries, run_dir) = store.open(run_id)?;
    Ok((construct_source_path(&run_dir, &document), libraries))
}

/// An existing run's journal.
pub fn load(run_id: RunId) -> Result<Vec<Record>, RunnerError> {
    let store = Store::new(PathBuf::from(STORE_ROOT));
    Ok(store.read(run_id)?)
}

/// The entry procedure's arguments from the command line, each parsed to its
/// natural value. A wildcard parameter's value has no name.
pub fn bind_parameters(
    program: &Program<'_>,
    arguments: &[String],
) -> Result<Vec<Supplied>, RunnerError> {
    let entry = program
        .subroutines
        .first()
        .ok_or(RunnerError::MissingEntryProcedure)?;
    let params = &entry.parameters;
    let procedure = entry
        .name
        .as_ref()
        .map(|n| n.value)
        .unwrap_or("the entry procedure")
        .to_string();
    if params.is_empty() && !arguments.is_empty() {
        return Err(RunnerError::ParameterUnexpected {
            procedure,
            actual: arguments.len(),
        });
    }
    let formae = walker::render_parameter_formae(entry.signature);
    if params.len() != arguments.len() {
        return Err(RunnerError::ParameterArityMismatch {
            procedure,
            parameters: describe_parameters(params, &formae),
            actual: arguments.len(),
        });
    }
    let mut supplied = Vec::with_capacity(params.len());
    for (i, (bind, argument)) in params
        .iter()
        .zip(arguments)
        .enumerate()
    {
        let value =
            evaluator::parse_value(argument).ok_or_else(|| RunnerError::MalformedArgument {
                parameter: bind
                    .clone()
                    .or_else(|| {
                        formae
                            .get(i)
                            .cloned()
                    })
                    .unwrap_or_else(|| "?".to_string()),
                argument: argument.to_string(),
            })?;
        supplied.push(Supplied {
            value,
            name: bind.clone(),
        });
    }
    Ok(supplied)
}

// `name : Type` for each expected parameter, for an arity mismatch error.
fn describe_parameters(params: &[Option<String>], formae: &[String]) -> Vec<String> {
    let count = params
        .len()
        .max(formae.len());
    (0..count)
        .map(|i| {
            let name = params
                .get(i)
                .and_then(|bind| bind.clone());
            match (name, formae.get(i)) {
                (Some(n), Some(t)) => format!("{} : {}", n, t),
                (Some(n), None) => n,
                (None, Some(t)) => t.clone(),
                (None, None) => "?".to_string(),
            }
        })
        .collect()
}

// The trail names the document by its file stem, as the PFFTT file is named.
fn document_label(document: &Path) -> String {
    document
        .file_stem()
        .map(|s| {
            s.to_string_lossy()
                .into_owned()
        })
        .unwrap_or_default()
}

#[cfg(test)]
#[path = "checks/session.rs"]
mod check;
