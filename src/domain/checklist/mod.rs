pub mod adapter;
pub mod types;
mod typst;

/// The checklist domain: flattens procedures into printable checklists.
pub struct Checklist;

impl crate::domain::Domain for Checklist {}
