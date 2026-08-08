//! The store of recorded runs: allocation of run identifiers, and the
//! append-only PFFTT file each run is written to.

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};

use crate::value;

use super::StoreError;
use super::record::{Record, RunId, State, Supplied, format_record, parse_record, parse_records};

/// On-disk store of runs, rooted at some base directory (conventionally
/// `.store/` relative to the user's current directory).
pub struct Store {
    base: PathBuf,
}

// Cap the number of times the allocator retries when another process has
// taken the identifier we just computed. The race window is small; a
// handful of retries is more than enough in practice.
const ALLOCATE_RETRIES: usize = 4;

impl Store {
    /// Build a handle to a store rooted at `base`. No I/O happens here; the
    /// directory is created on the first call to `allocate`.
    pub fn new(base: PathBuf) -> Self {
        Store { base }
    }

    /// Allocate a new run identifier and create its directory. Returns the
    /// identifier and the path of the new directory.
    pub fn allocate(&self) -> Result<(RunId, PathBuf), StoreError> {
        // Make sure the store root exists before scanning for siblings.
        if let Err(error) = std::fs::create_dir_all(&self.base) {
            return Err(StoreError::Io {
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
                Err(error) => return Err(StoreError::Io { path, error }),
            }
        }

        Err(StoreError::Io {
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
        libraries: &[String],
    ) -> Result<(RunId, PathBuf), StoreError> {
        let absolute = std::path::absolute(document).map_err(|error| StoreError::Io {
            path: document.to_path_buf(),
            error,
        })?;
        let (run_id, run_dir) = self.allocate()?;
        let pfftt = construct_state_path(&run_dir, &absolute);
        let mut uri = format!("file://{}", absolute.display());
        if !libraries.is_empty() {
            uri.push_str("?library=");
            uri.push_str(&libraries.join(","));
        }
        let record = Record {
            recorded: started,
            run_id,
            path: "/".to_string(),
            state: State::Start { uri },
        };
        std::fs::write(&pfftt, format_record(&record))
            .map_err(|error| StoreError::Io { path: pfftt, error })?;
        Ok((run_id, run_dir))
    }

    /// Read an existing run's trail back into memory, every record in the
    /// order it was written.
    pub fn read(&self, run_id: RunId) -> Result<Vec<Record>, StoreError> {
        let run_dir = self
            .base
            .join(run_id.render());
        if !run_dir.is_dir() {
            return Err(StoreError::NoSuchRun(run_id));
        }
        let pfftt = find_pfftt_file(&run_dir, run_id)?;
        let content = std::fs::read_to_string(&pfftt).map_err(|error| StoreError::Io {
            path: pfftt.clone(),
            error,
        })?;

        parse_records(&content).map_err(|error| StoreError::MalformedRecord { run_id, error })
    }

    /// Open an existing run. Parses the leading `Start` record to recover the
    /// source document and the libraries it was run with, then replays
    /// records into a map of completed step paths to the value each recorded
    /// (`Done` with its value, `Skip`/`Fail` mapping to `Unitus`), used to
    /// rehydrate bindings on resume. `Stop` and `Resume` records are passed
    /// over.
    pub fn open(
        &self,
        run_id: RunId,
    ) -> Result<
        (
            PathBuf,
            Vec<String>,
            HashMap<String, value::Value>,
            HashMap<String, Vec<Supplied>>,
            PathBuf,
        ),
        StoreError,
    > {
        let run_dir = self
            .base
            .join(run_id.render());
        if !run_dir.is_dir() {
            return Err(StoreError::NoSuchRun(run_id));
        }
        let pfftt = find_pfftt_file(&run_dir, run_id)?;
        let content = std::fs::read_to_string(&pfftt).map_err(|error| StoreError::Io {
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
            .ok_or(StoreError::StartMissing(run_id))?;
        let head =
            parse_record(first).map_err(|error| StoreError::MalformedRecord { run_id, error })?;
        let (document, libraries) = match head.state {
            State::Start { uri, .. } => parse_run_uri(&uri),
            _ => return Err(StoreError::StartMissing(run_id)),
        };

        let mut completed = HashMap::new();
        let mut inputs: HashMap<String, Vec<Supplied>> = HashMap::new();
        for line in lines {
            let record = parse_record(line)
                .map_err(|error| StoreError::MalformedRecord { run_id, error })?;
            match record.state {
                State::Done(value) => {
                    completed.insert(record.path, value.unwrap_or(value::Value::Unitus));
                }
                State::Skip | State::Fail(_) => {
                    completed.insert(record.path, value::Value::Unitus);
                }
                State::Input(supplied) => {
                    inputs.insert(record.path, supplied);
                }
                State::Start { .. }
                | State::Finish
                | State::Stop
                | State::Resume
                | State::Invoke(_)
                | State::Execute { .. }
                | State::Return(_)
                | State::Begin => {}
            }
        }
        Ok((document, libraries, completed, inputs, run_dir))
    }

    // Scan the store for the highest existing run identifier and return
    // one more. Entries whose names are not valid decimal integers are
    // ignored, which keeps the allocator robust against editor scratch
    // files left in `.store/`.
    fn next_identifier(&self) -> Result<RunId, StoreError> {
        let mut max: u32 = 0;
        let entries = std::fs::read_dir(&self.base).map_err(|error| StoreError::Io {
            path: self
                .base
                .clone(),
            error,
        })?;
        for entry in entries {
            let entry = entry.map_err(|error| StoreError::Io {
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

// Recover the source document's file path and the libraries that were
// selected from a Start URI of the form
//
// file://{path}?library=a,b

// written by `create` records. The query string parameters are optional.
pub(crate) fn parse_run_uri(uri: &str) -> (PathBuf, Vec<String>) {
    let (location, query) = match uri.split_once('?') {
        Some((location, query)) => (location, Some(query)),
        None => (uri, None),
    };
    let path = location
        .strip_prefix("file://")
        .unwrap_or(location);
    let libraries = query
        .and_then(|query| query.strip_prefix("library="))
        .map(|names| {
            names
                .split(',')
                .map(str::to_string)
                .collect()
        })
        .unwrap_or_default();
    (PathBuf::from(path), libraries)
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

/// Where an `Appender` sends its records, normally an append-only PFFTT file
/// in the store, or an in-memory sink for test runs that keep no persistent
/// state.
enum Target {
    File { file: std::fs::File, path: PathBuf },
    Memory(String),
    Discard,
}

/// Append-only writer for a PFFTT file. Used by the runner to append a
/// record for each step boundary and lifecycle event. Carries the
/// `RunId` so callers can stamp it onto records.
/// through every layer.
pub struct Appender {
    target: Target,
    run_id: RunId,
}

impl Appender {
    /// Open the PFFTT file for append. The file must already exist (the
    /// runner writes the opening `Start` record first via `Store::create`).
    pub fn open(path: PathBuf, run_id: RunId) -> Result<Self, StoreError> {
        let file = std::fs::OpenOptions::new()
            .append(true)
            .open(&path)
            .map_err(|error| StoreError::Io {
                path: path.clone(),
                error,
            })?;
        Ok(Appender {
            target: Target::File { file, path },
            run_id,
        })
    }

    /// An Appender that discards every record for use in tests.
    pub fn sink() -> Self {
        Appender {
            target: Target::Discard,
            run_id: RunId(0),
        }
    }

    /// An Appender that captures every record in memory for use in tests,
    /// readable afterwards with `contents()`. Touches no filesystem.
    pub fn memory() -> Self {
        Appender {
            target: Target::Memory(String::new()),
            run_id: RunId(0),
        }
    }

    /// The records captured by an in-memory Appender (`memory()`), as the raw
    /// PFFTT text; empty for a file or discarding Appender.
    pub fn contents(&self) -> &str {
        match &self.target {
            Target::Memory(buffer) => buffer,
            _ => "",
        }
    }

    /// The `RunId` this Appender is writing records for.
    pub fn run_id(&self) -> RunId {
        self.run_id
    }

    /// Append one record line. Flushes are left to the OS; on Quit the
    /// runner drops the Appender, which closes the file.
    pub fn append(&mut self, record: &Record) -> Result<(), StoreError> {
        use std::io::Write;
        let text = format_record(record);
        match &mut self.target {
            Target::File { file, path } => file
                .write_all(text.as_bytes())
                .map_err(|error| StoreError::Io {
                    path: path.clone(),
                    error,
                }),
            Target::Memory(buffer) => {
                buffer.push_str(&text);
                Ok(())
            }
            Target::Discard => Ok(()),
        }
    }
}

// Locate the single `*.pfftt` file in a run directory.
fn find_pfftt_file(run_dir: &Path, run_id: RunId) -> Result<PathBuf, StoreError> {
    let entries = std::fs::read_dir(run_dir).map_err(|error| StoreError::Io {
        path: run_dir.to_path_buf(),
        error,
    })?;
    for entry in entries {
        let entry = entry.map_err(|error| StoreError::Io {
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
    Err(StoreError::StartMissing(run_id))
}
