//! On-disk state store and run identifiers.

use std::collections::HashSet;
use std::io;
use std::path::{Path, PathBuf};

use super::runner::RunnerError;

/// Monotonic identifier for a run. Conventionally rendered and stored as a
/// six-digit zero-padded string.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct RunId(pub u32);

impl RunId {
    /// Parse a run identifier. Both unpadded (`7`) and zero-padded
    /// (`000007`) decimal forms are accepted.
    pub fn parse(text: &str) -> Result<RunId, RunnerError> {
        text.parse::<u32>()
            .map(RunId)
            .map_err(|_| RunnerError::InvalidRunId(text.to_string()))
    }

    /// Render as a six-digit zero-padded decimal string.
    pub fn render(self) -> String {
        format!("{:06}", self.0)
    }
}

/// Errors raised if a PFFTT file is malformed or invalid.
#[derive(Debug, Eq, PartialEq)]
pub enum RecordError {
    MalformedRecord,
    MalformedState,
    UnknownState(String),
}

/// One record line on disk.
#[allow(dead_code)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record {
    pub recorded: String,
    pub run_id: RunId,
    pub path: String,
    pub state: State,
}

/// A lifecycle or step-outcome event; the keyword written into each PFFTT
/// record line. `Start`, `Pause`, and `Resume` are run-lifecycle events
/// emitted at the root path `/`; `Begin` marks the moment work starts
/// on a step (paired with the eventual `Done`, `Skip`, or `Fail`).
/// `Invoke` records dispatch into another procedure (the return is
/// implicit — the next event's path reveals the resumed procedure).
/// `Execute` records a host-function call from inside a step body.
#[allow(dead_code)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum State {
    Start { uri: String },
    Pause,
    Resume,
    Invoke(InvokeTarget),
    Execute { function: String },
    Begin,
    Done(Option<Value>),
    Skip,
    Fail(Option<Value>),
}

/// The target of an `Invoke`: either a named procedure (rendered as
/// `name:`) or a URI to an external technique.
#[allow(dead_code)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum InvokeTarget {
    Procedure(String),
    Uri(String),
}

/// A `Value` carried by a Done or Fail state. Three on-disk forms,
/// handled by `format_value` / `parse_value` below: `unit` (`()`), a
/// double-quoted `literal` (the form a chosen response records as), and a
/// `tablet`; tablets currently round-trip as opaque text until tablet
/// typing in the runner lands.
#[allow(dead_code)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Unit,
    Literal(String),
    Tablet(String),
}

/// On-disk store of runs, rooted at some base directory (conventionally
/// `.store/` relative to the operator's current directory).
#[allow(dead_code)]
pub struct Store {
    base: PathBuf,
}

// Cap the number of times the allocator retries when another process has
// taken the identifier we just computed. The race window is small; a
// handful of retries is more than enough in practice.
#[allow(dead_code)]
const ALLOCATE_RETRIES: usize = 4;

#[allow(dead_code)]
impl Store {
    /// Build a handle to a store rooted at `base`. No I/O happens here; the
    /// directory is created on the first call to `allocate`.
    pub fn new(base: PathBuf) -> Self {
        Store { base }
    }

    /// Allocate a new run identifier and create its directory. Returns the
    /// identifier and the path of the new directory.
    pub fn allocate(&self) -> Result<(RunId, PathBuf), RunnerError> {
        // Make sure the store root exists before scanning for siblings.
        if let Err(error) = std::fs::create_dir_all(&self.base) {
            return Err(RunnerError::StoreError {
                path: self
                    .base
                    .clone(),
                error,
            });
        }

        for _ in 0..ALLOCATE_RETRIES {
            let next = self.next_identifier()?;
            let path = self
                .base
                .join(next.render());
            match std::fs::create_dir(&path) {
                Ok(()) => return Ok((next, path)),
                Err(error) if error.kind() == io::ErrorKind::AlreadyExists => continue,
                Err(error) => return Err(RunnerError::StoreError { path, error }),
            }
        }

        Err(RunnerError::StoreError {
            path: self
                .base
                .clone(),
            error: io::Error::new(
                io::ErrorKind::AlreadyExists,
                "exhausted retries allocating a run identifier",
            ),
        })
    }

    /// Allocate a new run and write its opening `Start` record. The PFFTT
    /// file is named after the source document's basename (e.g.
    /// `NetworkProbe.pfftt`).
    pub fn create(
        &self,
        document: &Path,
        started: String,
    ) -> Result<(RunId, PathBuf), RunnerError> {
        let absolute = std::path::absolute(document).map_err(|error| RunnerError::StoreError {
            path: document.to_path_buf(),
            error,
        })?;
        let (run_id, run_dir) = self.allocate()?;
        let pfftt = construct_state_path(&run_dir, &absolute);
        let record = Record {
            recorded: started,
            run_id,
            path: "/".to_string(),
            state: State::Start {
                uri: format!("file://{}", absolute.display()),
            },
        };
        std::fs::write(&pfftt, format_record(&record))
            .map_err(|error| RunnerError::StoreError { path: pfftt, error })?;
        Ok((run_id, run_dir))
    }

    /// Open an existing run. Parses the leading `Start` record to recover
    /// the source document, then replays `Done` / `Skip` / `Fail` records
    /// into a set of completed step paths. `Pause` and `Resume` records
    /// are passed over.
    pub fn open(&self, run_id: RunId) -> Result<(PathBuf, HashSet<String>, PathBuf), RunnerError> {
        let run_dir = self
            .base
            .join(run_id.render());
        if !run_dir.is_dir() {
            return Err(RunnerError::NoSuchRun(run_id));
        }
        let pfftt = find_pfftt_file(&run_dir, run_id)?;
        let content = std::fs::read_to_string(&pfftt).map_err(|error| RunnerError::StoreError {
            path: pfftt.clone(),
            error,
        })?;

        let mut lines = content
            .lines()
            .filter(|line| {
                !line
                    .trim()
                    .is_empty()
            });

        let first = lines
            .next()
            .ok_or(RunnerError::StartMissing(run_id))?;
        let head =
            parse_record(first).map_err(|error| RunnerError::MalformedRecord { run_id, error })?;
        let document = match head.state {
            State::Start { uri, .. } => {
                let stripped = uri
                    .strip_prefix("file://")
                    .unwrap_or(&uri);
                PathBuf::from(stripped)
            }
            _ => return Err(RunnerError::StartMissing(run_id)),
        };

        let mut completed = HashSet::new();
        for line in lines {
            let record = parse_record(line)
                .map_err(|error| RunnerError::MalformedRecord { run_id, error })?;
            match record.state {
                State::Done(_) | State::Skip | State::Fail(_) => {
                    completed.insert(record.path);
                }
                State::Start { .. }
                | State::Pause
                | State::Resume
                | State::Invoke(_)
                | State::Execute { .. }
                | State::Begin => {}
            }
        }
        Ok((document, completed, run_dir))
    }

    // Scan the store for the highest existing run identifier and return
    // one more. Entries whose names are not valid decimal integers are
    // ignored, which keeps the allocator robust against editor scratch
    // files left in `.store/`.
    fn next_identifier(&self) -> Result<RunId, RunnerError> {
        let mut max: u32 = 0;
        let entries = std::fs::read_dir(&self.base).map_err(|error| RunnerError::StoreError {
            path: self
                .base
                .clone(),
            error,
        })?;
        for entry in entries {
            let entry = entry.map_err(|error| RunnerError::StoreError {
                path: self
                    .base
                    .clone(),
                error,
            })?;
            if let Some(name) = entry
                .file_name()
                .to_str()
            {
                if let Ok(n) = name.parse::<u32>() {
                    if n > max {
                        max = n;
                    }
                }
            }
        }
        Ok(RunId(max + 1))
    }
}

// Compute the on-disk PFFTT file path for a run, named using the source
// document's stem.
pub(crate) fn construct_state_path(run_dir: &Path, document: &Path) -> PathBuf {
    let stem = document
        .file_stem()
        .map(|s| s.to_os_string())
        .unwrap_or_default();
    let mut name = PathBuf::from(stem);
    name.set_extension("pfftt");
    run_dir.join(name)
}

/// Append-only writer for a PFFTT file. Used by the runner to append a
/// record for each step boundary and lifecycle event. Carries the
/// `RunId` so callers can stamp it onto records without plumbing it
/// through every layer.
#[allow(dead_code)]
pub struct Appender {
    file: std::fs::File,
    path: PathBuf,
    run_id: RunId,
}

#[allow(dead_code)]
impl Appender {
    /// Open the PFFTT file for append. The file must already exist (the
    /// runner writes the opening `Start` record first via `Store::create`).
    pub fn open(path: PathBuf, run_id: RunId) -> Result<Self, RunnerError> {
        let file = std::fs::OpenOptions::new()
            .append(true)
            .open(&path)
            .map_err(|error| RunnerError::StoreError {
                path: path.clone(),
                error,
            })?;
        Ok(Appender { file, path, run_id })
    }

    /// The `RunId` this Appender is writing records for.
    pub fn run_id(&self) -> RunId {
        self.run_id
    }

    /// Append one record line. Flushes are left to the OS; on Quit the
    /// runner drops the Appender, which closes the file.
    pub fn append(&mut self, record: &Record) -> Result<(), RunnerError> {
        use std::io::Write;
        let text = format_record(record);
        self.file
            .write_all(text.as_bytes())
            .map_err(|error| RunnerError::StoreError {
                path: self
                    .path
                    .clone(),
                error,
            })
    }
}

// Locate the single `*.pfftt` file in a run directory.
fn find_pfftt_file(run_dir: &Path, run_id: RunId) -> Result<PathBuf, RunnerError> {
    let entries = std::fs::read_dir(run_dir).map_err(|error| RunnerError::StoreError {
        path: run_dir.to_path_buf(),
        error,
    })?;
    for entry in entries {
        let entry = entry.map_err(|error| RunnerError::StoreError {
            path: run_dir.to_path_buf(),
            error,
        })?;
        let path = entry.path();
        if path
            .extension()
            .and_then(|s| s.to_str())
            == Some("pfftt")
        {
            return Ok(path);
        }
    }
    Err(RunnerError::StartMissing(run_id))
}

// Serialize a Record in PFFTT line form. The format is:
// Timestamp RunId Path (State Value) followed by a newline.
#[allow(dead_code)]
pub(crate) fn format_record(record: &Record) -> String {
    let mut text = String::new();
    text.push_str(&record.recorded);
    text.push(' ');
    text.push_str(
        &record
            .run_id
            .render(),
    );
    text.push(' ');
    text.push_str(&record.path);
    text.push(' ');
    format_state(&mut text, &record.state);
    text.push('\n');
    text
}

fn format_state(out: &mut String, state: &State) {
    match state {
        State::Start { uri } => {
            out.push_str("Start ");
            out.push_str(uri);
        }
        State::Pause => out.push_str("Pause"),
        State::Resume => out.push_str("Resume"),
        State::Invoke(target) => {
            out.push_str("Invoke ");
            match target {
                InvokeTarget::Procedure(name) => {
                    out.push_str(name);
                    out.push(':');
                }
                InvokeTarget::Uri(uri) => out.push_str(uri),
            }
        }
        State::Execute { function } => {
            out.push_str("Execute ");
            out.push_str(function);
            out.push_str("()");
        }
        State::Begin => out.push_str("Begin"),
        State::Done(value) => {
            out.push_str("Done");
            if let Some(v) = value {
                out.push(' ');
                format_value(out, v);
            }
        }
        State::Skip => out.push_str("Skip"),
        State::Fail(value) => {
            out.push_str("Fail");
            if let Some(v) = value {
                out.push(' ');
                format_value(out, v);
            }
        }
    }
}

fn format_value(out: &mut String, value: &Value) {
    match value {
        Value::Unit => out.push_str("()"),
        Value::Literal(text) => {
            out.push('"');
            escape_literal(out, text);
            out.push('"');
        }
        Value::Tablet(text) => out.push_str(text),
    }
}

// Escape a literal so it occupies a single record line: backslash and quote
// are protected, and newlines/carriage returns become `\n` / `\r` so an
// embedded multi-line value (e.g. captured exec output) survives the
// line-oriented PFFTT format.
fn escape_literal(out: &mut String, text: &str) {
    for c in text.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            _ => out.push(c),
        }
    }
}

// Reverse `escape_literal`. An unknown escape (or a trailing backslash) is a
// malformed record.
fn unescape_literal(text: &str) -> Result<String, RecordError> {
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('\\') => out.push('\\'),
                Some('"') => out.push('"'),
                Some('n') => out.push('\n'),
                Some('r') => out.push('\r'),
                _ => return Err(RecordError::MalformedState),
            }
        } else {
            out.push(c);
        }
    }
    Ok(out)
}

// Parse a single PFFTT record line into a Record.
pub(crate) fn parse_record(line: &str) -> Result<Record, RecordError> {
    let line = line.trim_end_matches(['\r', '\n']);
    let mut parts = line.splitn(4, ' ');
    let recorded = parts
        .next()
        .ok_or(RecordError::MalformedRecord)?;
    let run_text = parts
        .next()
        .ok_or(RecordError::MalformedRecord)?;
    let path = parts
        .next()
        .ok_or(RecordError::MalformedRecord)?;
    let rest = parts
        .next()
        .ok_or(RecordError::MalformedRecord)?;
    if recorded.is_empty() || run_text.is_empty() || path.is_empty() || rest.is_empty() {
        return Err(RecordError::MalformedRecord);
    }
    let run_id = run_text
        .parse::<u32>()
        .map(RunId)
        .map_err(|_| RecordError::MalformedRecord)?;
    let state = parse_state(rest)?;
    Ok(Record {
        recorded: recorded.to_string(),
        run_id,
        path: path.to_string(),
        state,
    })
}

fn parse_state(text: &str) -> Result<State, RecordError> {
    let (keyword, rest) = match text.split_once(' ') {
        Some((k, r)) => (k, Some(r)),
        None => (text, None),
    };
    match keyword {
        "Start" => {
            let uri = rest.ok_or(RecordError::MalformedState)?;
            if uri.is_empty() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Start {
                uri: uri.to_string(),
            })
        }
        "Pause" => {
            if rest.is_some() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Pause)
        }
        "Resume" => {
            if rest.is_some() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Resume)
        }
        "Invoke" => {
            let payload = rest.ok_or(RecordError::MalformedState)?;
            if payload.is_empty() {
                return Err(RecordError::MalformedState);
            }
            if payload.starts_with("https://") || payload.starts_with("file:///") {
                Ok(State::Invoke(InvokeTarget::Uri(payload.to_string())))
            } else if let Some(name) = payload.strip_suffix(':') {
                if name.is_empty() {
                    return Err(RecordError::MalformedState);
                }
                Ok(State::Invoke(InvokeTarget::Procedure(name.to_string())))
            } else {
                Err(RecordError::MalformedState)
            }
        }
        "Execute" => {
            let payload = rest.ok_or(RecordError::MalformedState)?;
            let name = payload
                .strip_suffix("()")
                .ok_or(RecordError::MalformedState)?;
            if name.is_empty() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Execute {
                function: name.to_string(),
            })
        }
        "Begin" => {
            if rest.is_some() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Begin)
        }
        "Done" => Ok(State::Done(parse_optional_value(rest)?)),
        "Skip" => {
            if rest.is_some() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Skip)
        }
        "Fail" => Ok(State::Fail(parse_optional_value(rest)?)),
        other => Err(RecordError::UnknownState(other.to_string())),
    }
}

fn parse_optional_value(rest: Option<&str>) -> Result<Option<Value>, RecordError> {
    match rest {
        None => Ok(None),
        Some(text) => Ok(Some(parse_value(text)?)),
    }
}

fn parse_value(text: &str) -> Result<Value, RecordError> {
    if text == "()" {
        Ok(Value::Unit)
    } else if text.len() >= 2 && text.starts_with('"') && text.ends_with('"') {
        Ok(Value::Literal(unescape_literal(&text[1..text.len() - 1])?))
    } else if text.starts_with('[') && text.ends_with(']') {
        Ok(Value::Tablet(text.to_string()))
    } else {
        Err(RecordError::MalformedState)
    }
}

#[cfg(test)]
#[path = "checks/state.rs"]
mod check;
