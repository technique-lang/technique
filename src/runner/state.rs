//! On-disk state store and run identifiers.

use std::collections::HashSet;
use std::io;
use std::path::{Path, PathBuf};

use super::manifest::Manifest;
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
    MalformedTablet,
    MissingField(&'static str),
    UnknownOutcome(String),
}

/// One Result, recorded on disk in the form of a tablet.
#[allow(dead_code)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record {
    pub recorded: String,
    pub path: String,
    pub outcome: Outcome,
}

/// Outcome of executing a step.

/// The on-disk PFFTT format keeps `result` and `reason` as sibling fields of
/// `outcome` so these can be grepped for directly without needing to
/// destructure this type's constructors.
///
/// It also facilitates future combinations (e.g. partial result accompanying
/// a failure) that we currently don't need, but can accomodate in the future
/// without changing the on disk format.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Outcome {
    Done(Option<String>),
    Skipped,
    Failed(Option<String>),
}

impl Outcome {
    fn as_str(&self) -> &'static str {
        match self {
            Outcome::Done(_) => "Done",
            Outcome::Skipped => "Skipped",
            Outcome::Failed(_) => "Failed",
        }
    }
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

    /// Allocate a new run, write its manifest, and return the resulting run
    /// identifier and start state. The PFFTT file is named after the source
    /// document's basename (e.g. `NetworkProbe.pfftt`).
    pub fn create(
        &self,
        document: &Path,
        started: String,
    ) -> Result<(RunId, PathBuf, Manifest), RunnerError> {
        let (id, run_dir) = self.allocate()?;
        let manifest = Manifest {
            document: document.to_path_buf(),
            started,
        };
        let pfftt = construct_state_path(&run_dir, document);
        let text = format_manifest(&manifest);
        std::fs::write(&pfftt, text)
            .map_err(|error| RunnerError::StoreError { path: pfftt, error })?;
        Ok((id, run_dir, manifest))
    }

    /// Open an existing run. Parses the manifest and replays Result tablets
    /// into a set of completed step paths.
    pub fn open(&self, id: RunId) -> Result<(Manifest, HashSet<String>, PathBuf), RunnerError> {
        let run_dir = self
            .base
            .join(id.render());
        if !run_dir.is_dir() {
            return Err(RunnerError::NoSuchRun(id));
        }
        let pfftt = find_pfftt_file(&run_dir, id)?;
        let content = std::fs::read_to_string(&pfftt).map_err(|error| RunnerError::StoreError {
            path: pfftt.clone(),
            error,
        })?;

        let mut tablets = content
            .lines()
            .filter(|line| {
                !line
                    .trim()
                    .is_empty()
            });

        let manifest = match tablets.next() {
            Some(line) => parse_manifest(line)
                .map_err(|error| RunnerError::MalformedRecord { run: id, error })?,
            None => return Err(RunnerError::ManifestMissing(id)),
        };

        let mut completed = HashSet::new();
        for line in tablets {
            let record = parse_record(line)
                .map_err(|error| RunnerError::MalformedRecord { run: id, error })?;
            completed.insert(record.path);
        }
        Ok((manifest, completed, run_dir))
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

/// Append-only writer for a PFFTT file. This is used by the runner to record
/// a Result tablet for each completed Step.
#[allow(dead_code)]
pub struct Appender {
    file: std::fs::File,
    path: PathBuf,
}

#[allow(dead_code)]
impl Appender {
    /// Open the PFFTT file for append. The file must already exist (the
    /// runner writes the manifest first via `Store::create`).
    pub fn open(path: PathBuf) -> Result<Self, RunnerError> {
        let file = std::fs::OpenOptions::new()
            .append(true)
            .open(&path)
            .map_err(|error| RunnerError::StoreError {
                path: path.clone(),
                error,
            })?;
        Ok(Appender { file, path })
    }

    /// Append one Result tablet line. Flushes are left to the OS; on Quit
    /// the runner drops the Appender, which closes the file.
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
fn find_pfftt_file(run_dir: &Path, id: RunId) -> Result<PathBuf, RunnerError> {
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
    Err(RunnerError::ManifestMissing(id))
}

// Serialize a manifest as a single PFFTT tablet line. The trailing
// newline is part of the on-disk shape — every tablet occupies its own
// line.
pub(crate) fn format_manifest(m: &Manifest) -> String {
    format!(
        "[ document = file://{}, started = {} ]\n",
        m.document
            .display(),
        m.started,
    )
}

// Parse a manifest line into a Manifest. The `document` field is stored
// as a `file://` URL; the prefix is stripped on read.
pub(crate) fn parse_manifest(line: &str) -> Result<Manifest, RecordError> {
    let entries = parse_tablet(line)?;
    let document = entries
        .iter()
        .find(|(k, _)| *k == "document")
        .map(|(_, v)| *v)
        .ok_or(RecordError::MissingField("document"))?;
    let started = entries
        .iter()
        .find(|(k, _)| *k == "started")
        .map(|(_, v)| *v)
        .ok_or(RecordError::MissingField("started"))?;
    let document = document
        .strip_prefix("file://")
        .unwrap_or(document);
    Ok(Manifest {
        document: PathBuf::from(document),
        started: started.to_string(),
    })
}

// Serialize a Result tablet from a Record. The `outcome` field carries
// the bare discriminator; `result` and `reason` are sibling fields
// emitted only when the corresponding Outcome variant carries a payload.
#[allow(dead_code)]
pub(crate) fn format_record(record: &Record) -> String {
    let mut text = format!(
        "[ recorded = {}, path = {}, outcome = {}",
        record.recorded,
        record.path,
        record
            .outcome
            .as_str(),
    );
    match &record.outcome {
        Outcome::Done(Some(result)) => {
            text.push_str(", result = ");
            text.push_str(result);
        }
        Outcome::Failed(Some(reason)) => {
            text.push_str(", reason = ");
            text.push_str(reason);
        }
        _ => {}
    }
    text.push_str(" ]\n");
    text
}

// Parse a Result tablet line into a Record. Sibling fields `result` and
// `reason` are folded into the Outcome variant they belong to.
pub(crate) fn parse_record(line: &str) -> Result<Record, RecordError> {
    let entries = Entries(parse_tablet(line)?);
    let recorded = entries.required("recorded")?;
    let path = entries.required("path")?;
    let keyword = entries.required("outcome")?;
    let result = entries.optional("result");
    let reason = entries.optional("reason");
    let outcome = match keyword {
        "Done" => Outcome::Done(result.map(str::to_string)),
        "Skipped" => Outcome::Skipped,
        "Failed" => Outcome::Failed(reason.map(str::to_string)),
        other => return Err(RecordError::UnknownOutcome(other.to_string())),
    };
    Ok(Record {
        recorded: recorded.to_string(),
        path: path.to_string(),
        outcome,
    })
}

struct Entries<'a>(Vec<(&'a str, &'a str)>);

impl<'a> Entries<'a> {
    fn required(&self, name: &'static str) -> Result<&'a str, RecordError> {
        self.optional(name)
            .ok_or(RecordError::MissingField(name))
    }

    fn optional(&self, name: &'static str) -> Option<&'a str> {
        self.0
            .iter()
            .find(|(k, _)| *k == name)
            .map(|(_, v)| *v)
    }
}

// Split a `[ k = v, k = v, ... ]` line into (key, value) pairs. Permissive on
// whitespace; rejects anything without the bracketed envelope.
//
// TODO support nested tablets.
fn parse_tablet(line: &str) -> Result<Vec<(&str, &str)>, RecordError> {
    let trimmed = line.trim();
    let inner = trimmed
        .strip_prefix('[')
        .and_then(|s| s.strip_suffix(']'))
        .ok_or(RecordError::MalformedTablet)?
        .trim();
    let mut entries = Vec::new();
    for entry in inner.split(',') {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        let (k, v) = entry
            .split_once('=')
            .ok_or(RecordError::MalformedTablet)?;
        entries.push((k.trim(), v.trim()));
    }
    Ok(entries)
}

#[cfg(test)]
#[path = "checks/state.rs"]
mod check;
