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

fn extract(document: &language::Document) -> Document {
    let mut extracted = Document::new();

    for procedure in document.procedures() {
        extract_procedure(&mut extracted, procedure);
    }

    if extracted
        .sections
        .is_empty()
    {
        // Handle top-level SectionChunks (no procedures)
        for scope in document.steps() {
            if let Some((numeral, title)) = scope.section_info() {
                let heading = title.map(|para| para.text());
                let steps: Vec<Step> = match scope.body() {
                    Some(body) => body
                        .steps()
                        .filter(|s| s.is_step())
                        .map(|s| step_from_scope(s, None))
                        .collect(),
                    None => Vec::new(),
                };

                if !steps.is_empty() {
                    extracted
                        .sections
                        .push(Section {
                            ordinal: Some(numeral.to_string()),
                            heading,
                            steps,
                        });
                }
            }
        }

        // Handle bare top-level steps (no sections, no procedures)
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

fn steps_from_scope(scope: &language::Scope, inherited_role: Option<&str>) -> Vec<Step> {
    if scope.is_step() {
        return vec![step_from_scope(scope, inherited_role)];
    }

    // AttributeBlock — extract role and process children
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

    // SectionChunk
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

    let paragraphs: Vec<String> = scope
        .description()
        .map(|p| p.content())
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

#[cfg(test)]
mod check {
    use std::path::Path;

    use crate::parsing;
    use crate::templating::template::Adapter;

    use super::ChecklistAdapter;

    fn trim(s: &str) -> &str {
        s.strip_prefix('\n')
            .unwrap_or(s)
    }

    fn extract(source: &str) -> super::Document {
        let path = Path::new("test.tq");
        let doc = parsing::parse(path, source).unwrap();
        ChecklistAdapter.extract(&doc)
    }

    #[test]
    fn procedure_title_becomes_section_heading() {
        let doc = extract(trim(
            r#"
preflight :

# Pre-flight Checks

    1. Fasten seatbelt
            "#,
        ));
        assert_eq!(
            doc.sections
                .len(),
            1
        );
        assert_eq!(doc.sections[0].heading, Some("Pre-flight Checks".into()));
    }

    #[test]
    fn role_flattened_onto_children() {
        let doc = extract(trim(
            r#"
checks :

    @surgeon
        1. Confirm identity
        2. Mark surgical site
            "#,
        ));
        let steps = &doc.sections[0].steps;
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
        let step = &doc.sections[0].steps[0];
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
        let steps = &doc.sections[0].steps;
        assert_eq!(steps[0].title, Some("ensure_safety".into()));
    }
}
