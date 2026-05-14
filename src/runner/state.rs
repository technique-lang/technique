//! On-disk state store and run identifiers.

/// Monotonic identifier for a run. Conventionally rendered and stored as a
/// six-digit zero-padded string.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct RunId(pub u32);

impl RunId {
    /// Parse a run identifier. Both unpadded (`7`) and zero-padded
    /// (`000007`) decimal forms are accepted.
    pub fn parse(text: &str) -> Result<RunId, RunnerError> {
        text.parse::<u32>()
            .map(RunId)
            .map_err(|_| RunnerError::InvalidRunId(text.to_string()))
    }

    /// Render as a six-digit zero-padded decimal string.
    pub fn render(self) -> String {
        format!("{:06}", self.0)
    }
}

/// Errors arising from parsing or writing PFFTT files. Variants are added
/// as the parser and writer land.
#[derive(Debug)]
pub enum RecordError {}
