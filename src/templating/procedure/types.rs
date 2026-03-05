//! Domain types for a procedure.
//!
//! A procedure preserves the full hierarchy of the source Technique document:
//! sections containing procedures, procedures containing steps, with role
//! groups and responses, substeps, etc. This is basically a full fidelity
//! renderer of the input Technique structure.

/// A procedure document is sections containing items.
pub struct Document {
    pub title: Option<String>,
    pub description: Vec<String>,
    pub sections: Vec<Section>,
}

impl Document {
    pub fn new() -> Self {
        Document {
            title: None,
            description: Vec::new(),
            sections: Vec::new(),
        }
    }
}

/// A section within a procedure document.
pub struct Section {
    pub ordinal: Option<String>,
    pub heading: Option<String>,
    pub description: Vec<String>,
    pub items: Vec<Item>,
}

/// An item within a section: either a step or a role group. This
/// distinction matters because `@beaker` with lettered tasks is a
/// structural container, not a step annotation — the role group
/// owns its children rather than decorating them.
pub enum Item {
    Step(Step),
    RoleGroup(RoleGroup),
}

/// A step within a procedure.
pub struct Step {
    pub kind: StepKind,
    pub ordinal: Option<String>,
    pub title: Option<String>,
    pub body: Vec<String>,
    pub responses: Vec<Response>,
    pub children: Vec<Item>,
}

/// Whether a step is dependent (numbered) or parallel (bulleted).
pub enum StepKind {
    Dependent,
    Parallel,
}

/// A role group: a named container for items assigned to a role.
pub struct RoleGroup {
    pub name: String,
    pub items: Vec<Item>,
}

/// A response option with an optional condition.
pub struct Response {
    pub value: String,
    pub condition: Option<String>,
}
