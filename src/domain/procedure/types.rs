//! Domain types for a procedure.
//!
//! A procedure is a recursive tree of nodes mirroring the structure of the
//! source Technique document. Sections, procedures, steps, role groups —
//! whatever the author wrote, the domain model preserves.

use crate::domain::typst::{Data, Render};

/// A procedure document: title and description from the first procedure,
/// then a tree of nodes representing the body.
pub struct Document {
    pub title: Option<String>,
    pub description: Vec<String>,
    pub body: Vec<Node>,
}

impl Document {
    pub fn new() -> Self {
        Document {
            title: None,
            description: Vec::new(),
            body: Vec::new(),
        }
    }
}

impl Render for Document {
    fn render(&self, data: &mut Data) {
        data.open();
        data.field("title", &self.title);
        data.list("description", &self.description);
        data.list("body", &self.body);
        data.close();
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
        description: Vec<String>,
        children: Vec<Node>,
    },
    Sequential {
        ordinal: String,
        title: Option<String>,
        body: Vec<String>,
        invocations: Vec<String>,
        responses: Vec<Response>,
        children: Vec<Node>,
    },
    Parallel {
        title: Option<String>,
        body: Vec<String>,
        invocations: Vec<String>,
        responses: Vec<Response>,
        children: Vec<Node>,
    },
    Attribute {
        name: String,
        children: Vec<Node>,
    },
}

impl Render for Node {
    fn render(&self, data: &mut Data) {
        match self {
            Node::Section { ordinal, heading, children } => {
                data.open();
                data.tag("section");
                data.field("ordinal", ordinal);
                data.field("heading", heading);
                data.list("children", children);
                data.close();
            }
            Node::Procedure { name, title, description, children } => {
                data.open();
                data.tag("procedure");
                data.field("name", name);
                data.field("title", title);
                data.list("description", description);
                data.list("children", children);
                data.close();
            }
            Node::Sequential { ordinal, title, body, invocations, responses, children } => {
                data.open();
                data.tag("sequential");
                data.field("ordinal", ordinal);
                data.field("title", title);
                data.list("body", body);
                data.list("invocations", invocations);
                data.list("responses", responses);
                data.list("children", children);
                data.close();
            }
            Node::Parallel { title, body, invocations, responses, children } => {
                data.open();
                data.tag("parallel");
                data.field("title", title);
                data.list("body", body);
                data.list("invocations", invocations);
                data.list("responses", responses);
                data.list("children", children);
                data.close();
            }
            Node::Attribute { name, children } => {
                data.open();
                data.tag("attribute");
                data.field("name", name);
                data.list("children", children);
                data.close();
            }
        }
    }
}

/// A response option with an optional condition.
pub struct Response {
    pub value: String,
    pub condition: Option<String>,
}

impl Render for Response {
    fn render(&self, data: &mut Data) {
        data.open();
        data.field("value", &self.value);
        data.field("condition", &self.condition);
        data.close();
    }
}
