//! Formats procedure domain types into Typst.

use crate::templating::template::Renderer;
use crate::templating::typst;

use super::types::{Document, Item, Section, Step, StepKind};

pub struct ProcedureRenderer;

impl Renderer for ProcedureRenderer {
    type Model = Document;

    fn render(&self, model: &Document) -> String {
        render(model)
    }
}

fn render(document: &Document) -> String {
    let mut output = typst::preamble();

    if let Some(title) = &document.title {
        output.push_str(&typst::heading(1, title));
    }

    for para in &document.description {
        output.push_str(&typst::description(para));
    }

    for section in &document.sections {
        render_section(&mut output, section);
    }

    output
}

fn render_section(output: &mut String, section: &Section) {
    match (&section.ordinal, &section.heading) {
        (Some(ord), Some(heading)) => {
            output.push_str(&typst::heading(2, &format!("{}. {}", ord, heading)));
        }
        (Some(ord), None) => {
            output.push_str(&typst::heading(2, &format!("{}.", ord)));
        }
        (None, Some(heading)) => {
            output.push_str(&typst::heading(2, heading));
        }
        (None, None) => {}
    }

    for para in &section.description {
        output.push_str(&typst::description(para));
    }

    for item in &section.items {
        render_item(output, item);
    }
}

fn render_item(output: &mut String, item: &Item) {
    match item {
        Item::Step(step) => render_step(output, step),
        Item::RoleGroup(group) => {
            output.push_str(&typst::role(&group.name));
            for child in &group.items {
                render_item(output, child);
            }
        }
    }
}

fn render_step(output: &mut String, step: &Step) {
    let ordinal = match step.kind {
        StepKind::Dependent => step
            .ordinal
            .as_deref(),
        StepKind::Parallel => None,
    };

    output.push_str(&typst::step(
        ordinal,
        step.title
            .as_deref(),
    ));

    for para in &step.body {
        output.push_str(&typst::description(para));
    }

    let display: Vec<String> = step
        .responses
        .iter()
        .map(|r| match &r.condition {
            Some(cond) => format!("{} {}", r.value, cond),
            None => r
                .value
                .clone(),
        })
        .collect();
    output.push_str(&typst::responses(&display));

    for child in &step.children {
        render_item(output, child);
    }
}
