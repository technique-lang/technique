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

/// Render a single step and its children into the output buffer.
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

#[cfg(test)]
mod check {
    use crate::templating::template::Renderer;

    use super::ChecklistRenderer;
    use super::super::types::{Document, Response, Section, Step};

    fn step(ordinal: Option<&str>, title: Option<&str>) -> Step {
        Step {
            name: None,
            ordinal: ordinal.map(String::from),
            title: title.map(String::from),
            body: Vec::new(),
            role: None,
            responses: Vec::new(),
            children: Vec::new(),
        }
    }

    #[test]
    fn section_heading_with_ordinal() {
        let doc = Document {
            sections: vec![Section {
                ordinal: Some("I".into()),
                heading: Some("Before anaesthesia".into()),
                steps: vec![step(Some("1"), Some("Check pulse"))],
            }],
        };
        let out = ChecklistRenderer.render(&doc);
        assert!(out.contains("== I. Before anaesthesia"));
    }

    #[test]
    fn step_with_ordinal_and_title() {
        let doc = Document {
            sections: vec![Section {
                ordinal: None,
                heading: None,
                steps: vec![step(Some("3"), Some("Verify identity"))],
            }],
        };
        let out = ChecklistRenderer.render(&doc);
        assert!(out.contains("*3.*"));
        assert!(out.contains("Verify identity"));
    }

    #[test]
    fn role_rendered_before_step() {
        let mut s = step(Some("1"), Some("Confirm site"));
        s.role = Some("surgeon".into());
        let doc = Document {
            sections: vec![Section {
                ordinal: None,
                heading: None,
                steps: vec![s],
            }],
        };
        let out = ChecklistRenderer.render(&doc);
        let role_pos = out.find("surgeon").unwrap();
        let step_pos = out.find("Confirm site").unwrap();
        assert!(role_pos < step_pos);
    }

    #[test]
    fn responses_rendered() {
        let mut s = step(Some("1"), Some("Ready?"));
        s.responses = vec![
            Response { value: "Yes".into(), condition: None },
            Response { value: "No".into(), condition: Some("if complications".into()) },
        ];
        let doc = Document {
            sections: vec![Section {
                ordinal: None,
                heading: None,
                steps: vec![s],
            }],
        };
        let out = ChecklistRenderer.render(&doc);
        assert!(out.contains("Yes"));
        assert!(out.contains("No if complications"));
    }
}
