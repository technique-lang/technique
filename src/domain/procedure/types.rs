//! Domain types for a procedure.
//!
//! A procedure is a recursive tree of nodes mirroring the structure of the
//! source Technique document. Sections, procedures, steps, role groups —
//! whatever the author wrote, the domain model preserves.

pub use crate::domain::engine::{Inline, Prose};

/// A procedure document: title and description from the first procedure,
/// then a tree of nodes representing the body.
pub struct Document {
    pub source: Option<String>,
    pub name: Option<String>,
    pub title: Option<String>,
    pub description: Vec<Prose>,
    pub body: Vec<Node>,
}

impl Document {
    pub fn new() -> Self {
        Document {
            source: None,
            name: None,
            title: None,
            description: Vec::new(),
            body: Vec::new(),
        }
    }
}

/// A node in the procedure tree.
pub enum Node {
    Section {
        ordinal: String,
        heading: Option<String>,
        children: Vec<Node>,
    },
    Procedure {
        name: String,
        title: Option<String>,
        description: Vec<Prose>,
        children: Vec<Node>,
    },
    Sequential {
        ordinal: String,
        title: Option<String>,
        body: Vec<Prose>,
        invocations: Vec<String>,
        responses: Vec<Response>,
        children: Vec<Node>,
    },
    Parallel {
        title: Option<String>,
        body: Vec<Prose>,
        invocations: Vec<String>,
        responses: Vec<Response>,
        children: Vec<Node>,
    },
    Attribute {
        name: String,
        children: Vec<Node>,
    },
    CodeBlock {
        expression: String,
        body: Vec<String>,
        responses: Vec<Response>,
        children: Vec<Node>,
    },
}

/// A response option with an optional condition.
pub struct Response {
    pub value: String,
    pub condition: Option<String>,
}
