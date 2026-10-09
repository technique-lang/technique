//! Interactive runner that walks a translated Program step-by-step,
//! prompting the user and recording each completed step to a state store
//! so a run can be resumed after interruption.

pub(crate) mod context;
pub mod driver;
pub(crate) mod error;
pub(crate) mod evaluator;
pub(crate) mod library;
pub(crate) mod path;
mod session;
mod walker;

pub use context::Context;
pub use error::RunnerError;
pub use evaluator::Environment;
pub use library::{Builtin, Library, Native, library_for};

pub use driver::{Headless, Intent, Mode, intent};
pub use session::{Conclusion, Runner, bind_parameters, inspect, load, locate, resume, start};
pub use walker::Outcome;
