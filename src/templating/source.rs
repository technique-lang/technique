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

const WIDTH: u8 = 70;

pub struct Source;

impl Template for Source {
    fn data(&self, document: &Document) -> String {
        let mut out = String::from(PREAMBLE);
        out.push_str(&render(&Typst, document, WIDTH));
        out
    }
}
