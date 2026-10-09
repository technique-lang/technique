//! The store of recorded runs: allocation of run identifiers, and the
//! append-only PFFTT file each run is written to.

use std::io;
use std::path::{Path, PathBuf};

use super::StoreError;
use super::record::{Record, RunId, Serial, State, format_record, parse_record};

/// On-disk store of runs, rooted at some base directory (conventionally
/// `.store/` relative to the user's current directory).
pub struct Store {
    base: PathBuf,
}

// Retries when another process takes the identifier just computed.
const MAX_RETRIES: usize = 4;

impl Store {
    /// Build a handle to a store rooted at `base`. No I/O happens here; the
    /// directory is created on the first call to `allocate`.
    pub fn new(base: PathBuf) -> Self {
        Store { base }
    }

    /// Allocate a new run identifier and create its directory.
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

        for _ in 0..MAX_RETRIES {
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

    /// Allocate a new run, copy the source document into it, and write its
    /// opening `Start` record. The PFFTT file is named after the source
    /// document's basename (e.g. `NetworkProbe.pfftt`).
    pub fn create(
        &self,
        document: &Path,
        source: &str,
        started: String,
        libraries: &[String],
    ) -> Result<(RunId, PathBuf), StoreError> {
        let absolute = std::path::absolute(document).map_err(|error| StoreError::Io {
            path: document.to_path_buf(),
            error,
        })?;
        let (run_id, run_dir) = self.allocate()?;
        let copy = construct_source_path(&run_dir, &absolute);
        std::fs::write(&copy, source).map_err(|error| StoreError::Io { path: copy, error })?;
        let pfftt = construct_state_path(&run_dir, &absolute);
        let mut uri = format!("file://{}", absolute.display());
        if !libraries.is_empty() {
            uri.push_str("?library=");
            uri.push_str(&libraries.join(","));
        }
        let record = Record {
            recorded: started,
            run_id,
            serial: Serial::ROOT,
            path: "/".to_string(),
            state: State::Start { uri },
        };
        std::fs::write(&pfftt, format_record(&record))
            .map_err(|error| StoreError::Io { path: pfftt, error })?;
        Ok((run_id, run_dir))
    }

    /// Read an existing run's journal back into memory, every record in the
    /// order it was written.
    pub fn read(&self, run_id: RunId) -> Result<Vec<Record>, StoreError> {
        let run_dir = self
            .base
            .join(run_id.render());
        if !run_dir.is_dir() {
            return Err(StoreError::NoSuchRun(run_id));
        }
        let pfftt = find_pfftt_file(&run_dir, run_id)?;
        let mut content = std::fs::read(&pfftt).map_err(|error| StoreError::Io {
            path: pfftt.clone(),
            error,
        })?;
        if let Some(start) = torn(&content) {
            content.truncate(start);
        }
        parse_journal(content, &pfftt, run_id)
    }

    /// Open an existing run. Parses the leading `Start` record to recover the
    /// source document and the libraries it was run with, and names the run's
    /// directory.
    pub fn open(&self, run_id: RunId) -> Result<(PathBuf, Vec<String>, PathBuf), StoreError> {
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
        if let Some(start) = torn(content.as_bytes()) {
            if content[..start]
                .trim()
                .is_empty()
            {
                return Err(StoreError::StartMissing(run_id));
            }
        }

        let (i, first) = content
            .lines()
            .enumerate()
            .find(|(_, line)| {
                !line
                    .trim()
                    .is_empty()
            })
            .ok_or(StoreError::StartMissing(run_id))?;
        let head = parse_record(first).map_err(|error| StoreError::MalformedRecord {
            run_id,
            line: i + 1,
            error,
        })?;
        let (document, libraries) = match head.state {
            State::Start { uri, .. } => parse_run_uri(&uri),
            _ => return Err(StoreError::StartMissing(run_id)),
        };
        Ok((document, libraries, run_dir))
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

// Recover the document path and selected libraries from the Start URI
// `file://{path}?library=a,b` that `create` records; the query is optional.
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

// Where a last line cut short mid-write begins: every record is written with
// its newline, so one lacking it is broken even if it parses.
fn torn(content: &[u8]) -> Option<usize> {
    if content.is_empty() || content.ends_with(b"\n") {
        return None;
    }
    let start = content
        .iter()
        .rposition(|b| *b == b'\n')
        .map_or(0, |i| i + 1);
    Some(start)
}

// Parse a journal's records, its torn last line already cut.
fn parse_journal(content: Vec<u8>, path: &Path, run_id: RunId) -> Result<Vec<Record>, StoreError> {
    let content = String::from_utf8(content).map_err(|error| StoreError::Io {
        path: path.to_path_buf(),
        error: std::io::Error::new(std::io::ErrorKind::InvalidData, error),
    })?;
    content
        .lines()
        .enumerate()
        .filter(|(_, line)| {
            !line
                .trim()
                .is_empty()
        })
        .map(|(i, line)| {
            parse_record(line).map_err(|error| StoreError::MalformedRecord {
                run_id,
                line: i + 1,
                error,
            })
        })
        .collect()
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

// Compute the path of the copy of the source document kept in a run's
// directory, named with the source document's basename.
pub(crate) fn construct_source_path(run_dir: &Path, document: &Path) -> PathBuf {
    let name = document
        .file_name()
        .unwrap_or_default();
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
pub struct Appender {
    target: Target,
    run_id: RunId,
}

impl Appender {
    /// Open an existing PFFTT file for append, and read back its records.
    /// Holds a lock on the file until dropped so if a second session is
    /// attempted from another process it won't be able to run.
    pub fn open(path: PathBuf, run_id: RunId) -> Result<(Self, Vec<Record>), StoreError> {
        use std::io::Read;
        let failed = |error| StoreError::Io {
            path: path.clone(),
            error,
        };
        let mut file = std::fs::OpenOptions::new()
            .read(true)
            .append(true)
            .open(&path)
            .map_err(failed)?;
        match file.try_lock() {
            Ok(()) => {}
            Err(std::fs::TryLockError::WouldBlock) => return Err(StoreError::InUse(run_id)),
            Err(std::fs::TryLockError::Error(error)) => return Err(failed(error)),
        }
        let mut content = Vec::new();
        file.read_to_end(&mut content)
            .map_err(failed)?;
        if let Some(start) = torn(&content) {
            file.set_len(start as u64)
                .map_err(failed)?;
            content.truncate(start);
        }
        let records = parse_journal(content, &path, run_id)?;
        Ok((
            Appender {
                target: Target::File { file, path },
                run_id,
            },
            records,
        ))
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
