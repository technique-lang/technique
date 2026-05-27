//! Domain types for checklists
//!
//! A checklist is a sequence of items, where an item is a step, a named
//! procedure with optional description and its own steps, or a section
//! grouping further items under an ordinal heading.

pub use crate::domain::engine::{Inline, Prose};

/// A checklist: a sequence of top-level items.
pub struct Document {
    pub items: Vec<Item>,
}

impl Document {
    pub fn new() -> Self {
        Document { items: Vec::new() }
    }
}

/// A top-level entry in a checklist.
pub enum Item {
    Step(Step),
    Procedure(Procedure),
    Section(Section),
}

/// A named subroutine: name, optional title, optional description, and steps.
pub struct Procedure {
    pub name: String,
    pub title: Option<String>,
    pub description: Vec<Prose>,
    pub steps: Vec<Step>,
}

/// A grouping with an ordinal heading; contains procedures or steps.
pub struct Section {
    pub ordinal: Option<String>,
    pub heading: Option<String>,
    pub items: Vec<Item>,
}

/// A checkbox action.
pub struct Step {
    pub ordinal: Option<String>,
    pub title: Option<String>,
    pub body: Vec<Prose>,
    pub role: Option<String>,
    pub responses: Vec<Response>,
    pub children: Vec<Step>,
}

/// A response option with an optional condition.
pub struct Response {
    pub value: String,
    pub condition: Option<String>,
}
