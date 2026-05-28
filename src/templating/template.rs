//! Traits for the templating pipeline.

use crate::language;

/// A template transforms a Technique document into output.

pub trait Template {
    /// Render the document as Typst function-call markup.
    fn markup(&self, document: &language::Document) -> String;

    /// Return the Typst source for this template.
    fn typst(&self) -> &str;

    /// Return the domain name (used for the template filename on disk).
    fn domain(&self) -> &str;

    /// The page size, as (width, height) in millimetres, that this domain
    /// prefers when the user has not specified `--paper`. Return `None` to
    /// fall back to the global default.
    fn default_paper(&self) -> Option<(f64, f64)> {
        None
    }
}
