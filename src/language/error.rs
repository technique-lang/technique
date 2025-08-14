use std::{fmt, path::Path};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoadingError<'i> {
    pub problem: String,
    pub details: String,
    pub filename: &'i Path,
}

impl<'i> fmt::Display for LoadingError<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.problem, self.details)
    }
}
