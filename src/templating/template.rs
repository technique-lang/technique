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
}
