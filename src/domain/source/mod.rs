pub mod adapter;
pub mod types;
mod typst;

/// The source domain: renders Technique source with syntax highlighting.
pub struct Source;

impl crate::domain::Domain for Source {}
