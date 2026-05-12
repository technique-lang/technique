//! On-disk state store and run identifiers.

/// Monotonic identifier for a run. Conventionally rendered and stored as a
/// six-digit zero-padded string.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct RunId(pub u32);

/// Errors arising from parsing or writing PFFTT files. Variants are added
/// as the parser and writer land.
#[derive(Debug)]
pub enum RecordError {}
