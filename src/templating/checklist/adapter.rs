//! Projects the AST into the checklist domain model.
//!
//! This flattens the parser type hierarchy. Each procedure becomes a section,
//! role assignments are inherited by sub steps, and SectionChunks are
//! rendered as headings with their sub-procedures' steps as children.

use crate::language;
use crate::templating::template::Adapter;

use super::types::{Document, Response, Section, Step};

pub struct ChecklistAdapter;

impl Adapter for ChecklistAdapter {
    type Model = Document;

    fn extract(&self, document: &language::Document) -> Document {
        extract(document)
    }
}

/// Transform the parsed AST into a checklist Document.
fn extract(document: &language::Document) -> Document {
    let mut extracted = Document::new();

    for procedure in document.procedures() {
        extract_procedure(&mut extracted, procedure);
    }

    // Handle top-level steps (if no procedures)
    if extracted
        .sections
        .is_empty()
    {
        let steps: Vec<Step> = document
            .steps()
            .filter(|s| s.is_step())
            .map(|s| step_from_scope(s, None))
            .collect();

        if !steps.is_empty() {
            extracted
                .sections
                .push(Section {
                    ordinal: None,
                    heading: None,
                    steps,
                });
        }
    }

    extracted
}

fn extract_procedure(content: &mut Document, procedure: &language::Procedure) {
    let steps: Vec<Step> = procedure
        .steps()
        .flat_map(|s| steps_from_scope(s, None))
        .collect();

    if !steps.is_empty() {
        content
            .sections
            .push(Section {
                ordinal: None,
                heading: procedure
                    .title()
                    .map(String::from),
                steps,
            });
    }
}

/// Extract steps from a scope, handling different scope types.
fn steps_from_scope(scope: &language::Scope, inherited_role: Option<&str>) -> Vec<Step> {
    if scope.is_step() {
        return vec![step_from_scope(scope, inherited_role)];
    }

    // Handle AttributeBlock — extract role and process children
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

    // Handle SectionChunk
    if let Some((numeral, title)) = scope.section_info() {
        let heading = title.map(|para| para.text());

        let mut steps = vec![Step {
            name: None,
            ordinal: Some(numeral.to_string()),
            title: heading,
            body: Vec::new(),
            role: None,
            responses: Vec::new(),
            children: Vec::new(),
        }];

        // Handle nested procedures in section body
        if let Some(body) = scope.body() {
            for procedure in body.procedures() {
                if let Some(title) = procedure.title() {
                    let children: Vec<Step> = procedure
                        .steps()
                        .flat_map(|s| steps_from_scope(s, None))
                        .collect();

                    steps.push(Step {
                        name: Some(
                            procedure
                                .name()
                                .to_string(),
                        ),
                        ordinal: None,
                        title: Some(title.to_string()),
                        body: Vec::new(),
                        role: None,
                        responses: Vec::new(),
                        children,
                    });
                }
            }
        }

        return steps;
    }

    Vec::new()
}

/// Convert a step-like scope into a Step.
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

    // First paragraph becomes title, rest becomes body
    let paragraphs: Vec<String> = scope
        .description()
        .map(|p| p.text())
        .collect();
    let (title, body) = match paragraphs.split_first() {
        Some((first, rest)) => (Some(first.clone()), rest.to_vec()),
        None => (None, Vec::new()),
    };

    Step {
        name: None,
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
