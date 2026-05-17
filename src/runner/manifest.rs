//! PFFTT manifest — the first tablet of a run's state file. Captures the
//! source document URL and the run's start time.

use std::path::PathBuf;

#[allow(dead_code)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Manifest {
    pub document: PathBuf,
    pub started: String,
}
