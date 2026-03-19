//! Domain types for checklists
//!
//! A checklist is moderately structured and relatively flat: sections with
//! headings, steps with checkboxes, response options, and limited nesting.

/// A checklist is a document of sections containing steps.
pub struct Document {
    pub name: Option<String>,
    pub title: Option<String>,
    pub sections: Vec<Section>,
}

impl Document {
    pub fn new() -> Self {
        Document {
            name: None,
            title: None,
            sections: Vec::new(),
        }
    }
}

/// A section within a checklist.
pub struct Section {
    pub ordinal: Option<String>,
    pub heading: Option<String>,
    pub steps: Vec<Step>,
}

/// A step within a checklist section.
pub struct Step {
    pub name: Option<String>,
    pub ordinal: Option<String>,
    pub title: Option<String>,
    pub body: Vec<String>,
    pub role: Option<String>,
    pub responses: Vec<Response>,
    pub children: Vec<Step>,
}

/// A response option with an optional condition.
pub struct Response {
    pub value: String,
    pub condition: Option<String>,
}
