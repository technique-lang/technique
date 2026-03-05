//! Format checklist domain types into Typst.

use crate::templating::template::Renderer;
use crate::templating::typst;

use super::types::{Document, Section, Step};

pub struct ChecklistRenderer;

impl Renderer for ChecklistRenderer {
    type Model = Document;

    fn render(&self, model: &Document) -> String {
        render(model)
    }
}

fn render(document: &Document) -> String {
    let mut output = typst::preamble();

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

    for step in &section.steps {
        render_step(output, step);
    }

    output.push('\n');
}

fn render_step(output: &mut String, step: &Step) {
    if let Some(r) = &step.role {
        output.push_str(&typst::role(r));
    }

    output.push_str(&typst::step(
        step.ordinal
            .as_deref(),
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
        render_step(output, child);
    }
}
