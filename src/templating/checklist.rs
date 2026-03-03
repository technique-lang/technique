//! Checklist template - renders procedures as printable checklists
//!
//! A checklist is moderately structured and relatively flat: sections with
//! headings, steps with checkboxes, response options, and limited nesting.

use crate::language;

use super::template::Template;

// ============================================================================
// Checklist domain types
// ============================================================================

/// A checklist document: sections containing steps.
struct Document {
    sections: Vec<Section>,
}

impl Document {
    fn new() -> Self {
        Document {
            sections: Vec::new(),
        }
    }
}

/// A section within a checklist.
struct Section {
    #[allow(dead_code)]
    ordinal: Option<String>,
    heading: Option<String>,
    steps: Vec<Step>,
}

/// A step within a checklist section.
struct Step {
    #[allow(dead_code)]
    name: Option<String>,
    ordinal: Option<String>,
    title: Option<String>,
    body: Vec<String>,
    role: Option<String>,
    responses: Vec<String>,
    children: Vec<Step>,
}

// ============================================================================
// Template implementation
// ============================================================================

pub struct Checklist;

impl Template for Checklist {
    fn render(&self, document: &language::Document, _width: u8) -> String {
        let extracted = extract(document);
        render(&extracted)
    }
}

/// Transform the parsed AST Document into template Document
fn extract(document: &language::Document) -> Document {
    let mut extracted = Document::new();

    // Handle procedures
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
    // Extract steps into a section
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

    // Handle AttributeBlock - extract role and process children
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
        let heading = match title {
            Some(para) => format!("{}. {}", numeral, para.text()),
            None => format!("{}.", numeral),
        };

        let mut steps = vec![Step {
            name: None,
            ordinal: None,
            title: Some(heading),
            body: Vec::new(),
            role: None,
            responses: Vec::new(),
            children: Vec::new(),
        }];

        // Handle nested procedures in section body
        if let language::Scope::SectionChunk { body, .. } = scope {
            if let language::Technique::Procedures(procedures) = body {
                for procedure in procedures {
                    if let Some(title) = procedure.title() {
                        let children: Vec<Step> = procedure
                            .steps()
                            .flat_map(|s| steps_from_scope(s, None))
                            .collect();

                        steps.push(Step {
                            name: Some(
                                procedure
                                    .name
                                    .0
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
        // Collect responses
        for response in subscope.responses() {
            responses.push(
                response
                    .value
                    .to_string(),
            );
        }

        // Collect children (substeps)
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

fn render(document: &Document) -> String {
    let mut output = String::new();

    // Typst preamble
    output.push_str("#set page(margin: 1.5cm)\n");
    output.push_str("#set text(size: 10pt)\n\n");

    // Sections
    for section in &document.sections {
        render_section(&mut output, section);
    }

    output
}

fn render_section(output: &mut String, section: &Section) {
    if let Some(heading) = &section.heading {
        output.push_str(&format!("== {}\n\n", escape(heading)));
    }

    for step in &section.steps {
        render_step(output, step, 0);
    }

    output.push('\n');
}

fn render_step(output: &mut String, step: &Step, depth: usize) {
    let indent = "  ".repeat(depth);

    // Role header if present
    if let Some(role) = &step.role {
        output.push_str(&format!("{}#text(weight: \"bold\")[{}]\n\n", indent, role));
    }

    // Checkbox with ordinal and title
    output.push_str(&format!(
        "{}#box(stroke: 0.5pt, width: 0.8em, height: 0.8em) ",
        indent
    ));

    if let Some(ordinal) = &step.ordinal {
        output.push_str(&format!("*{}.*  ", ordinal));
    }

    if let Some(title) = &step.title {
        output.push_str(&escape(title));
    }
    output.push_str("\n\n");

    // Body paragraphs
    for para in &step.body {
        output.push_str(&format!("{}    {}\n\n", indent, escape(para)));
    }

    // Response options
    if !step
        .responses
        .is_empty()
    {
        output.push_str(&format!("{}    ", indent));
        for (i, response) in step
            .responses
            .iter()
            .enumerate()
        {
            if i > 0 {
                output.push_str(" | ");
            }
            output.push_str(&format!(
                "#box(stroke: 0.5pt, width: 0.6em, height: 0.6em) _{}_",
                response
            ));
        }
        output.push_str("\n\n");
    }

    // Children
    for child in &step.children {
        render_step(output, child, depth + 1);
    }
}

fn escape(text: &str) -> String {
    text.replace('\\', "\\\\")
        .replace('#', "\\#")
        .replace('$', "\\$")
        .replace('*', "\\*")
        .replace('_', "\\_")
        .replace('@', "\\@")
        .replace('<', "\\<")
        .replace('>', "\\>")
}
