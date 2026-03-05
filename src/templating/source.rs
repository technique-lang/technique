//! Render Technique source code with syntax highlighting into Typst. This
//! implements Template directly without the adapter/renderer split used in
//! normal renderers by instead delegating to the existing code formatting
//! pipeline underlying the `format` command.

use crate::language::Document;
use crate::highlighting::{render, Typst};

use super::Template;

pub struct Source {
    width: u8,
}

impl Source {
    pub fn new(width: u8) -> Self {
        Source { width }
    }
}

impl Template for Source {
    fn render(&self, document: &Document) -> String {
        render(&Typst, document, self.width)
    }
}
