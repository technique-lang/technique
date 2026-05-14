//! On-disk state store and run identifiers.

use std::io;
use std::path::PathBuf;

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

/// Errors arising from parsing or writing PFFTT files. Variants are added
/// as the parser and writer land.
#[derive(Debug)]
pub enum RecordError {}

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

#[cfg(test)]
#[path = "checks/state.rs"]
mod check;
