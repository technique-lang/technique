//! Projects the parser's AST into a domain model suitable for procedures.
//!
//! This is a recursive walk of the AST producing a tree of Nodes. The first
//! procedure provides document-level title and description; its steps (and
//! any SectionChunks within) become the body. Remaining top-level procedures
//! are appended as Procedure nodes.

use crate::domain::Adapter;
use crate::language;

use super::types::{Document, Node, Response};

pub struct ProcedureAdapter;

impl Adapter for ProcedureAdapter {
    type Model = Document;

    fn extract(&self, document: &language::Document) -> Document {
        extract(document)
    }
}

fn extract(document: &language::Document) -> Document {
    let mut doc = Document::new();

    let mut procedures = document.procedures();

    if let Some(first) = procedures.next() {
        doc.title = first
            .title()
            .map(String::from);
        doc.description = first
            .description()
            .map(|p| p.content())
            .collect();

        for scope in first.steps() {
            doc.body
                .extend(nodes_from_scope(scope));
        }

        for procedure in procedures {
            doc.body
                .push(node_from_procedure(procedure));
        }
    }

    // Handle bare top-level steps (no procedures)
    if doc
        .body
        .is_empty()
    {
        for scope in document.steps() {
            doc.body
                .extend(nodes_from_scope(scope));
        }
    }

    doc
}

fn node_from_procedure(procedure: &language::Procedure) -> Node {
    let mut children = Vec::new();
    for scope in procedure.steps() {
        children.extend(nodes_from_scope(scope));
    }

    Node::Procedure {
        name: procedure
            .name()
            .to_string(),
        title: procedure
            .title()
            .map(String::from),
        description: procedure
            .description()
            .map(|p| p.content())
            .collect(),
        children,
    }
}

/// Extract nodes from a scope, handling different scope types.
fn nodes_from_scope(scope: &language::Scope) -> Vec<Node> {
    if scope.is_step() {
        return vec![node_from_step(scope)];
    }

    // AttributeBlock — role group with children
    let roles: Vec<_> = scope
        .roles()
        .collect();
    if !roles.is_empty() {
        let name = roles.join(" + ");
        let mut children = Vec::new();
        for child in scope.children() {
            children.extend(nodes_from_scope(child));
        }
        return vec![Node::Attribute { name, children }];
    }

    // SectionChunk
    if let Some((numeral, title)) = scope.section_info() {
        let heading = title.map(|para| para.text());
        let mut children = Vec::new();

        if let Some(body) = scope.body() {
            for procedure in body.procedures() {
                children.push(node_from_procedure(procedure));
            }
            for step in body.steps() {
                children.extend(nodes_from_scope(step));
            }
        }

        return vec![Node::Section {
            ordinal: numeral.to_string(),
            heading,
            children,
        }];
    }

    Vec::new()
}

/// Convert a step-like scope into a Sequential or Parallel node.
fn node_from_step(scope: &language::Scope) -> Node {
    let mut responses = Vec::new();
    let mut children = Vec::new();

    for subscope in scope.children() {
        for response in subscope.responses() {
            responses.push(Response {
                value: response
                    .value()
                    .to_string(),
                condition: response
                    .condition()
                    .map(String::from),
            });
        }
        children.extend(nodes_from_scope(subscope));
    }

    let paras: Vec<_> = scope
        .description()
        .collect();

    let invocations: Vec<String> = paras
        .first()
        .map(|p| {
            p.invocations()
                .into_iter()
                .map(String::from)
                .collect()
        })
        .unwrap_or_default();

    let paragraphs: Vec<String> = paras
        .iter()
        .map(|p| p.content())
        .collect();
    let (title, body) = match paragraphs.split_first() {
        Some((first, rest)) => {
            let mut t = first.clone();
            for inv in &invocations {
                t = t.replace(inv, "");
            }
            let t = t.trim().to_string();
            (if t.is_empty() { None } else { Some(t) }, rest.to_vec())
        }
        None => (None, Vec::new()),
    };

    match scope {
        language::Scope::DependentBlock { .. } => Node::Sequential {
            ordinal: scope
                .ordinal()
                .map(String::from)
                .unwrap_or_default(),
            title,
            body,
            invocations,
            responses,
            children,
        },
        language::Scope::ParallelBlock { .. } => Node::Parallel {
            title,
            body,
            invocations,
            responses,
            children,
        },
        _ => panic!("node_from_step called with non-step scope"),
    }
}

#[cfg(test)]
mod check {
    use std::path::Path;

    use crate::domain::Adapter;
    use crate::parsing;

    use super::super::types::Node;
    use super::ProcedureAdapter;

    fn trim(s: &str) -> &str {
        s.strip_prefix('\n')
            .unwrap_or(s)
    }

    fn extract(source: &str) -> super::Document {
        let path = Path::new("test.tq");
        let doc = parsing::parse(path, source).unwrap();
        ProcedureAdapter.extract(&doc)
    }

    #[test]
    fn procedure_title_becomes_document_title() {
        let doc = extract(trim(
            r#"
emergency :

# Don't Panic

    1. Stay calm
            "#,
        ));
        assert_eq!(doc.title, Some("Don't Panic".into()));
    }

    #[test]
    fn role_preserved_as_group() {
        let doc = extract(trim(
            r#"
build :

    1. Define Interfaces
        @programmers
            a. <define_interfaces>
            "#,
        ));
        if let Node::Sequential { children, .. } = &doc.body[0] {
            assert_eq!(children.len(), 1);
            if let Node::Attribute { name, children } = &children[0] {
                assert_eq!(name, "programmers");
                assert_eq!(children.len(), 1);
            } else {
                panic!("expected RoleGroup");
            }
        } else {
            panic!("expected Sequential");
        }
    }

    #[test]
    fn dependent_step_has_ordinal() {
        let doc = extract(trim(
            r#"
checks :

    1. First step
    2. Second step
            "#,
        ));
        assert_eq!(
            doc.body
                .len(),
            2
        );
        if let Node::Sequential { ordinal, .. } = &doc.body[0] {
            assert_eq!(ordinal, "1");
        } else {
            panic!("expected Sequential");
        }
    }

    #[test]
    fn parallel_step() {
        let doc = extract(trim(
            r#"
checks :

    - First item
    - Second item
            "#,
        ));
        assert_eq!(
            doc.body
                .len(),
            2
        );
        if let Node::Parallel { .. } = &doc.body[0] {
            // ok
        } else {
            panic!("expected Parallel");
        }
    }

    #[test]
    fn invocation_only_step_has_content() {
        let doc = extract(trim(
            r#"
main :

    1. <ensure_safety>

ensure_safety :

# Safety First

    - Check exits
            "#,
        ));
        if let Node::Sequential {
            title,
            invocations,
            ..
        } = &doc.body[0]
        {
            assert_eq!(*title, None);
            assert_eq!(invocations, &["ensure_safety"]);
        } else {
            panic!("expected Sequential");
        }
    }

    #[test]
    fn sections_contain_their_procedures() {
        let doc = extract(trim(
            r#"
main :

# Upgrade

    I. Preparation <preparation>

preparation :

    1. Check systems
    2. Notify staff

    II. Execution <execution>

execution :

    3. Run scripts
    4. Verify
            "#,
        ));
        assert_eq!(
            doc.body
                .len(),
            2
        );

        if let Node::Section {
            ordinal,
            heading,
            children,
        } = &doc.body[0]
        {
            assert_eq!(ordinal, "I");
            assert_eq!(*heading, Some("Preparation".into()));
            // Section contains a Procedure node with 2 steps
            assert_eq!(children.len(), 1);
            if let Node::Procedure { children, .. } = &children[0] {
                assert_eq!(children.len(), 2);
            } else {
                panic!("expected Procedure in section");
            }
        } else {
            panic!("expected Section");
        }

        if let Node::Section {
            ordinal,
            heading,
            children,
        } = &doc.body[1]
        {
            assert_eq!(ordinal, "II");
            assert_eq!(*heading, Some("Execution".into()));
            assert_eq!(children.len(), 1);
            if let Node::Procedure { children, .. } = &children[0] {
                assert_eq!(children.len(), 2);
            } else {
                panic!("expected Procedure in section");
            }
        } else {
            panic!("expected Section");
        }
    }
}
