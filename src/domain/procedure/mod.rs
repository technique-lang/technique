pub mod adapter;
pub mod types;
mod typst;

/// The procedure domain: preserves the full procedure hierarchy when rendering.
pub struct Procedure;

impl crate::domain::Domain for Procedure {}
