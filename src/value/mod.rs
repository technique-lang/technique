//! Runtime values. Owned mirror of the data that a translated Program
//! produces: the result of reducing value-bearing operations, the payload of
//! completed step Results in PFFTT files, and the eventual binding target for
//! CLI-supplied entry-procedure arguments.

mod types;

pub use types::*;
