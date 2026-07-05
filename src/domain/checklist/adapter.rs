//! Projects the AST into the checklist domain model.
//!
//! Each top-level procedure becomes an `Item::Procedure` carrying its own
//! direct steps. Section chunks found within a procedure's body float up as
//! sibling `Item::Section`s alongside the procedure. Sections contain either
//! sub-procedures or steps. If the document has no procedures at all, its
//! top-level scopes (sections or bare steps) become items directly.

use crate::domain::Adapter;
use crate::language;

use super::types::{Document, Item, Procedure, Prose, Response, Section, Step};

pub struct ChecklistAdapter;

impl Adapter for ChecklistAdapter {
    type Model = Document;

    fn extract(&self, document: &language::Document) -> Document {
        extract(document)
    }
}

fn extract(document: &language::Document) -> Document {
    let mut items = Vec::new();
    let mut had_procedures = false;

    for procedure in document.procedures() {
        had_procedures = true;
        let (steps, sections) = split_procedure_body(procedure);
        items.push(Item::Procedure(Procedure {
            name: procedure
                .name()
                .to_string(),
            title: procedure
                .title()
                .map(String::from),
            description: procedure
                .description()
                .map(|p| Prose::parse(&p.content()))
                .collect(),
            steps,
        }));
        for section in sections {
            items.push(Item::Section(section));
        }
    }

    if !had_procedures {
        for scope in document.steps() {
            if scope
                .section_info()
                .is_some()
            {
                items.push(Item::Section(section_from_scope(scope)));
            } else {
                items.extend(
                    steps_from_scope(scope, None)
                        .into_iter()
                        .map(Item::Step),
                );
            }
        }
    }

    Document { items }
}

fn split_procedure_body(procedure: &language::Procedure) -> (Vec<Step>, Vec<Section>) {
    let mut steps = Vec::new();
    let mut sections = Vec::new();
    for scope in procedure.steps() {
        if scope
            .section_info()
            .is_some()
        {
            sections.push(section_from_scope(scope));
        } else {
            steps.extend(steps_from_scope(scope, None));
        }
    }
    (steps, sections)
}

fn section_from_scope(scope: &language::Scope) -> Section {
    let (numeral, title) = scope
        .section_info()
        .expect("scope is a section");
    let mut items = Vec::new();
    if let Some(body) = scope.body() {
        for p in body.procedures() {
            items.push(Item::Procedure(extract_subprocedure(p)));
        }
        for s in body.steps() {
            items.extend(
                steps_from_scope(s, None)
                    .into_iter()
                    .map(Item::Step),
            );
        }
    }
    Section {
        ordinal: Some(numeral.to_string()),
        heading: title.map(|para| para.text()),
        items,
    }
}

fn extract_subprocedure(procedure: &language::Procedure) -> Procedure {
    let mut steps = Vec::new();
    for scope in procedure.steps() {
        steps.extend(steps_from_scope(scope, None));
    }
    Procedure {
        name: procedure
            .name()
            .to_string(),
        title: procedure
            .title()
            .map(String::from),
        description: procedure
            .description()
            .map(|p| Prose::parse(&p.content()))
            .collect(),
        steps,
    }
}

fn steps_from_scope(scope: &language::Scope, inherited_role: Option<&str>) -> Vec<Step> {
    if scope.is_step() {
        return vec![step_from_scope(scope, inherited_role)];
    }

    let roles: Vec<_> = scope
        .roles()
        .collect();
    if !roles.is_empty() {
        let role = roles
            .first()
            .copied();
        return scope
            .children()
            .flat_map(|s| steps_from_scope(s, role))
            .collect();
    }

    Vec::new()
}

fn step_from_scope(scope: &language::Scope, inherited_role: Option<&str>) -> Step {
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
        children.extend(steps_from_scope(subscope, inherited_role));
    }

    let paragraphs: Vec<String> = scope
        .description()
        .map(|p| p.content())
        .collect();
    let (title, body) = match paragraphs.split_first() {
        Some((first, rest)) => (
            Some(first.clone()),
            rest.iter()
                .map(|s| Prose::parse(s))
                .collect(),
        ),
        None => (None, Vec::new()),
    };

    Step {
        ordinal: scope
            .ordinal()
            .map(String::from),
        title,
        body,
        role: inherited_role.map(String::from),
        responses,
        children,
    }
}

#[cfg(test)]
mod check {
    use std::path::Path;

    use crate::domain::Adapter;
    use crate::domain::checklist::types::Item;
    use crate::parsing;

    use super::ChecklistAdapter;

    fn trim(s: &str) -> &str {
        s.strip_prefix('\n')
            .unwrap_or(s)
    }

    fn extract(source: &str) -> super::Document {
        let path = Path::new("Test.tq");
        let doc = parsing::parse(path, source).unwrap();
        ChecklistAdapter.extract(&doc)
    }

    fn as_procedure(item: &Item) -> &super::Procedure {
        match item {
            Item::Procedure(p) => p,
            _ => panic!("expected procedure"),
        }
    }

    fn as_section(item: &Item) -> &super::Section {
        match item {
            Item::Section(s) => s,
            _ => panic!("expected section"),
        }
    }

    fn as_step(item: &Item) -> &super::Step {
        match item {
            Item::Step(s) => s,
            _ => panic!("expected step"),
        }
    }

    #[test]
    fn single_procedure_with_steps() {
        let doc = extract(trim(
            r#"
preflight :

# Pre-flight Checks

    1. Fasten seatbelt
            "#,
        ));
        assert_eq!(
            doc.items
                .len(),
            1
        );
        let proc = as_procedure(&doc.items[0]);
        assert_eq!(proc.name, "preflight");
        assert_eq!(proc.title, Some("Pre-flight Checks".into()));
        assert_eq!(
            proc.steps
                .len(),
            1
        );
    }

    #[test]
    fn role_flattened_onto_steps() {
        let doc = extract(trim(
            r#"
checks :

    @surgeon
        1. Confirm identity
        2. Mark surgical site
            "#,
        ));
        let proc = as_procedure(&doc.items[0]);
        let steps = &proc.steps;
        assert_eq!(steps.len(), 2);
        assert_eq!(steps[0].role, Some("surgeon".into()));
        assert_eq!(steps[1].role, Some("surgeon".into()));
    }

    #[test]
    fn responses_with_conditions() {
        let doc = extract(trim(
            r#"
checks :

    1. Is the patient ready?
            'Yes' | 'No' if complications
            "#,
        ));
        let proc = as_procedure(&doc.items[0]);
        let step = &proc.steps[0];
        assert_eq!(
            step.responses
                .len(),
            2
        );
        assert_eq!(step.responses[0].value, "Yes");
        assert_eq!(step.responses[0].condition, None);
        assert_eq!(step.responses[1].value, "No");
        assert_eq!(step.responses[1].condition, Some("if complications".into()));
    }

    #[test]
    fn sibling_procedures_are_peers() {
        let doc = extract(trim(
            r#"
main :

    1. <ensure_safety>

ensure_safety :

# Safety First

    - Check exits
            "#,
        ));
        assert_eq!(
            doc.items
                .len(),
            2
        );
        let first = as_procedure(&doc.items[0]);
        assert_eq!(first.name, "main");
        let second = as_procedure(&doc.items[1]);
        assert_eq!(second.name, "ensure_safety");
        assert_eq!(second.title, Some("Safety First".into()));
        assert_eq!(
            second
                .steps
                .len(),
            1
        );
    }

    #[test]
    fn no_procedure_sections_of_steps() {
        let doc = extract(trim(
            r#"
% technique v1
& checklist

I. Morning

    1. Eat breakfast
    2. Brush teeth

II. Evening

    1. Dinner
    2. Sleep
            "#,
        ));
        assert_eq!(
            doc.items
                .len(),
            2
        );
        let s1 = as_section(&doc.items[0]);
        assert_eq!(s1.ordinal, Some("I".into()));
        assert_eq!(s1.heading, Some("Morning".into()));
        assert_eq!(
            s1.items
                .len(),
            2
        );
        let _ = as_step(&s1.items[0]);
    }

    #[test]
    fn procedure_with_sections_of_subprocedures() {
        let doc = extract(trim(
            r#"
outer :

I. Setup

setup_machine :
# Setup Machine
    1. Plug in
            "#,
        ));
        assert_eq!(
            doc.items
                .len(),
            2
        );
        let outer = as_procedure(&doc.items[0]);
        assert_eq!(outer.name, "outer");
        assert_eq!(
            outer
                .steps
                .len(),
            0
        );
        let section = as_section(&doc.items[1]);
        assert_eq!(section.ordinal, Some("I".into()));
        let sub = as_procedure(&section.items[0]);
        assert_eq!(sub.name, "setup_machine");
        assert_eq!(sub.title, Some("Setup Machine".into()));
    }
}
