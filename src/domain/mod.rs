//! Domain projections of Technique documents.
//!
//! A domain projection takes the parser's AST and transforms it into a
//! model suited to a particular kind of output. The **checklist** domain
//! flattens procedures into printable checklists; the **procedure** domain
//! preserves the full hierarchy; others are forthcoming.
//!
//! The **engine** module provides convenient accessors into the parser's AST
//! types so that adapters can extract content without needing to understand
//! the nuances of correclty matching directly on the internals types.

mod adapter;
pub mod checklist;
pub mod engine;
pub mod procedure;
pub mod source;
pub mod typst;

pub use adapter::Adapter;
