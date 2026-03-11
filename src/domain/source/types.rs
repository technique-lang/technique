//! Domain types for source code display.
//!
//! A source document is a flat sequence of syntax-tagged fragments,
//! produced by the code formatter. Each fragment carries a syntax tag
//! (e.g. "Declaration", "Keyword") and a content string.

use crate::domain::typst::{Data, Render};

pub struct Document {
    pub fragments: Vec<Fragment>,
}

pub struct Fragment {
    pub syntax: String,
    pub content: String,
}

impl Render for Document {
    fn render(&self, data: &mut Data) {
        data.open();
        data.list("fragments", &self.fragments);
        data.close();
    }
}

impl Render for Fragment {
    fn render(&self, data: &mut Data) {
        data.open();
        data.field("syntax", &self.syntax);
        data.field("content", &self.content);
        data.close();
    }
}
