//! Domain types for source code display.
//!
//! A source document is a flat sequence of syntax-tagged fragments,
//! produced by the code formatter. Each fragment carries a syntax tag
//! (e.g. "Declaration", "Keyword") and a content string.

pub struct Document {
    pub fragments: Vec<Fragment>,
}

pub struct Fragment {
    pub syntax: String,
    pub content: String,
}
