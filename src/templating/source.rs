//! Render Technique source code with syntax highlighting into Typst. This
//! implements Template directly without the adapter/renderer split used in
//! normal renderers by instead delegating to the existing code formatting
//! pipeline underlying the `format` command.

use crate::highlighting::{render, Typst};
use crate::language::Document;

use super::Template;

static PREAMBLE: &str = r#"
#show text: set text(font: "Inconsolata")
#show raw: set block(breakable: true)
"#;

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
        let mut out = String::from(PREAMBLE);
        out.push_str(&render(&Typst, document, self.width));
        out
    }
}
