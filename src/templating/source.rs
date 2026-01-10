//! Source template - syntax-highlighted source code rendering

use crate::language::Document;
use crate::rendering::{render, Typst};

use super::Template;

/// Template for rendering Technique source code with syntax highlighting
pub struct Source;

impl Template for Source {
    fn render(&self, document: &Document, width: u8) -> String {
        render(&Typst, document, width)
    }
}
