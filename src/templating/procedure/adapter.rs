//! Projects the parser's AST into a domain model suitable for procedures.
//!
//! This model preserves hierarchy. The first procedure may be a container
//! whose steps are SectionChunks (each becoming a Section here). Remaining
//! procedures become additional Sections. AttributeBlocks become RoleGroups
//! (structural containers, not step annotations). ResponseBlocks attach to
//! their parent step.

use crate::language;
use crate::templating::template::Adapter;

use super::types::{Document, Item, Response, RoleGroup, Section, Step, StepKind};

pub struct ProcedureAdapter;

impl Adapter for ProcedureAdapter {
    type Model = Document;

    fn extract(&self, document: &language::Document) -> Document {
        extract(document)
    }
}

fn extract(document: &language::Document) -> Document {
    let mut doc = Document::new();

    // Try procedures first. The first procedure may be a container whose
    // steps are SectionChunks; remaining procedures are leaf procedures
    // referenced from within sections.
    let mut procedures = document.procedures();

    if let Some(first) = procedures.next() {
        doc.title = first
            .title()
            .map(String::from);
        doc.description = first
            .description()
            .map(|p| p.content())
            .collect();

        let has_sections = first
            .steps()
            .any(|s| {
                s.section_info()
                    .is_some()
            });

        if has_sections {
            // First procedure is a container with sections
            for scope in first.steps() {
                if let Some(section) = section_from_scope(scope) {
                    doc.sections
                        .push(section);
                }
            }
        } else {
            // First procedure has direct steps
            doc.sections
                .push(section_from_procedure(first));
        }

        // Remaining procedures become additional sections
        for procedure in procedures {
            doc.sections
                .push(section_from_procedure(procedure));
        }
    }

    // Handle bare top-level steps (no procedures)
    if doc
        .sections
        .is_empty()
    {
        let items: Vec<Item> = document
            .steps()
            .flat_map(|s| items_from_scope(s))
            .collect();

        if !items.is_empty() {
            doc.sections
                .push(Section {
                    ordinal: None,
                    heading: None,
                    description: Vec::new(),
                    items,
                });
        }
    }

    doc
}

fn section_from_scope(scope: &language::Scope) -> Option<Section> {
    let (numeral, title) = scope.section_info()?;
    let heading = title.map(|para| para.text());

    let mut description = Vec::new();
    let mut items = Vec::new();

    if let Some(body) = scope.body() {
        for procedure in body.procedures() {
            description.extend(procedure.description().map(|p| p.content()));
            items.extend(items_from_procedure(procedure));
        }
        for step in body.steps() {
            items.extend(items_from_scope(step));
        }
    }

    Some(Section {
        ordinal: Some(numeral.to_string()),
        heading,
        description,
        items,
    })
}

fn section_from_procedure(procedure: &language::Procedure) -> Section {
    let items = items_from_procedure(procedure);
    let description: Vec<String> = procedure
        .description()
        .map(|p| p.content())
        .collect();

    Section {
        ordinal: None,
        heading: procedure
            .title()
            .map(String::from),
        description,
        items,
    }
}

fn items_from_procedure(procedure: &language::Procedure) -> Vec<Item> {
    procedure
        .steps()
        .flat_map(|s| items_from_scope(s))
        .collect()
}

/// Extract items from a scope, handling different scope types.
fn items_from_scope(scope: &language::Scope) -> Vec<Item> {
    if scope.is_step() {
        return vec![Item::Step(step_from_scope(scope))];
    }

    // Handle AttributeBlock — extract role and process children as a RoleGroup
    let roles: Vec<_> = scope
        .roles()
        .collect();
    if !roles.is_empty() {
        let name = roles.join(" + ");
        let items: Vec<Item> = scope
            .children()
            .flat_map(|s| items_from_scope(s))
            .collect();
        return vec![Item::RoleGroup(RoleGroup { name, items })];
    }

    // Handle SectionChunk nested within procedures
    if scope
        .section_info()
        .is_some()
    {
        // Sections within procedures become sub-items
        if let Some(body) = scope.body() {
            return body
                .steps()
                .flat_map(|s| items_from_scope(s))
                .collect();
        }
    }

    Vec::new()
}

/// Convert a step-like scope into a Step.
fn step_from_scope(scope: &language::Scope) -> Step {
    let kind = match scope {
        language::Scope::DependentBlock { .. } => StepKind::Dependent,
        _ => StepKind::Parallel,
    };

    let mut responses = Vec::new();
    let mut children = Vec::new();

    for subscope in scope.children() {
        // Collect responses
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

        // Collect child items (steps, role groups, etc.)
        children.extend(items_from_scope(subscope));
    }

    let paragraphs: Vec<String> = scope
        .description()
        .map(|p| p.content())
        .collect();
    let (title, body) = match paragraphs.split_first() {
        Some((first, rest)) => (Some(first.clone()), rest.to_vec()),
        None => (None, Vec::new()),
    };

    Step {
        kind,
        ordinal: scope
            .ordinal()
            .map(String::from),
        title,
        body,
        responses,
        children,
    }
}

#[cfg(test)]
mod check {
    use std::path::Path;

    use crate::parsing;
    use crate::templating::template::Adapter;

    use super::ProcedureAdapter;
    use super::super::types::{Item, StepKind};

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
        assert_eq!(doc.title.as_deref(), Some("Don't Panic"));
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
        let items = &doc.sections[0].items;
        assert_eq!(items.len(), 1);
        if let Item::Step(step) = &items[0] {
            assert_eq!(step.children.len(), 1);
            if let Item::RoleGroup(group) = &step.children[0] {
                assert_eq!(group.name, "programmers");
                assert_eq!(group.items.len(), 1);
            } else {
                panic!("expected RoleGroup");
            }
        } else {
            panic!("expected Step");
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
        let items = &doc.sections[0].items;
        assert_eq!(items.len(), 2);
        if let Item::Step(s) = &items[0] {
            assert!(matches!(s.kind, StepKind::Dependent));
            assert_eq!(s.ordinal.as_deref(), Some("1"));
        } else {
            panic!("expected Step");
        }
    }

    #[test]
    fn parallel_step_no_ordinal() {
        let doc = extract(trim(
            r#"
checks :

    - First item
    - Second item
            "#,
        ));
        let items = &doc.sections[0].items;
        assert_eq!(items.len(), 2);
        if let Item::Step(s) = &items[0] {
            assert!(matches!(s.kind, StepKind::Parallel));
            assert_eq!(s.ordinal, None);
        } else {
            panic!("expected Step");
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
        let items = &doc.sections[0].items;
        if let Item::Step(step) = &items[0] {
            assert_eq!(step.title.as_deref(), Some("ensure_safety"));
        } else {
            panic!("expected Step");
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
        assert_eq!(doc.sections.len(), 2);

        assert_eq!(doc.sections[0].ordinal.as_deref(), Some("I"));
        assert_eq!(doc.sections[0].heading.as_deref(), Some("Preparation"));
        assert_eq!(doc.sections[0].items.len(), 2);

        assert_eq!(doc.sections[1].ordinal.as_deref(), Some("II"));
        assert_eq!(doc.sections[1].heading.as_deref(), Some("Execution"));
        assert_eq!(doc.sections[1].items.len(), 2);
    }
}
