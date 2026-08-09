//! The PFFTT record format, and the local on-disk store these records are
//! written to.

use std::io;
use std::path::PathBuf;

mod record;
mod store;

/// Errors raised interacting with the store or PFFTT files therein.
#[derive(Debug)]
pub enum StoreError {
    NoSuchRun(RunId),
    StartMissing(RunId),
    InvalidRunId(String),
    MalformedRecord { run_id: RunId, error: RecordError },
    Io { path: PathBuf, error: io::Error },
}

pub use record::{
    InvokeTarget, Record, RecordError, RunId, State, Supplied, display_path, parse_records,
};
pub use store::{Appender, Store};

pub(crate) use record::{
    fail_reason, format_record, format_supplied, serialize_value, split_top_level, unescape_literal,
};
pub(crate) use store::{construct_state_path, parse_run_uri};

#[cfg(test)]
pub(crate) use record::parse_record;
