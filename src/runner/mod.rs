//! Interactive runner that walks a translated Program step-by-step,
//! prompting the operator and recording each completed step to a state store
//! so a run can be resumed after interruption.

mod evaluator;
mod manifest;
mod path;
mod prompt;
mod runner;
mod state;

pub use runner::RunnerError;
pub use state::{RecordError, RunId};
