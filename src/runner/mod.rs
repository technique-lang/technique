//! Interactive runner that walks a translated Program step-by-step,
//! prompting the operator and recording each completed step to a state store
//! so a run can be resumed after interruption.

mod manifest;
mod path;
mod prompt;
mod runner;
mod state;
mod value;

pub use runner::RunnerError;
pub use state::{RecordError, RunId};
