//! Formats procedure domain types into Typst.
//!
//! Produces output styled after operational procedures: sans-serif font,
//! title block with overview, blue "Procedure" bar, numbered steps with
//! bold titles, roles as indented bold names, and lettered substeps.

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
    let mut out = String::new();

    out.push_str("#set page(margin: 1.5cm)\n");
    out.push_str("#set par(justify: false)\n");
    out.push_str("#show text: set text(size: 9pt, font: \"TeX Gyre Heros\")\n\n");

    // Outer block wraps entire procedure
    out.push_str("#block(width: 100%, stroke: 0.1pt, inset: 10pt)[\n");

    if let Some(title) = &document.title {
        out.push_str(&format!(
            "#text(size: 15pt)[*{}*]\n\n",
            typst::escape(title)
        ));
    }
    if !document.description.is_empty() {
        out.push_str("_Overview_\n\n");
        for para in &document.description {
            out.push_str(&typst::escape(para));
            out.push('\n');
        }
    }

    // Procedure bar
    out.push_str("#block(width: 100%, fill: rgb(\"#006699\"), inset: 5pt)[#text(fill: white)[*Procedure*]]\n\n");

    // Sections
    let total = document.sections.len();
    for (i, section) in document.sections.iter().enumerate() {
        render_section(&mut out, section);
        if i + 1 < total {
            out.push_str("#line(length: 100%, stroke: (thickness: 0.5pt, paint: rgb(\"#003366\"), dash: (\"dot\", 2pt, 4pt, 2pt)))\n\n");
        }
    }

    out.push_str("]\n");
    out
}

fn render_section(out: &mut String, section: &Section) {
    // Section heading
    match (&section.ordinal, &section.heading) {
        (Some(ord), Some(heading)) => {
            out.push_str(&format!(
                "#text(size: 14pt)[*{}.* #h(8pt) *{}*]\n\n",
                ord,
                typst::escape(heading)
            ));
        }
        (Some(ord), None) => {
            out.push_str(&format!("#text(size: 14pt)[*{}.*]\n\n", ord));
        }
        (None, Some(heading)) => {
            out.push_str(&format!(
                "#text(size: 14pt)[*{}*]\n\n",
                typst::escape(heading)
            ));
        }
        (None, None) => {}
    }

    for para in &section.description {
        out.push_str(&typst::escape(para));
        out.push_str("\n\n");
    }

    // Steps indented slightly from section heading
    if !section.items.is_empty() {
        out.push_str("#pad(left: 8pt)[\n");
        for item in &section.items {
            render_item(out, item);
        }
        out.push_str("]\n");
    }
}

fn render_item(out: &mut String, item: &Item) {
    match item {
        Item::Step(step) => render_step(out, step),
        Item::RoleGroup(group) => {
            render_role(out, group);
        }
    }
}

fn render_role(out: &mut String, group: &super::types::RoleGroup) {
    out.push_str(&format!("- *{}*\n", typst::escape(&group.name)));
    if !group.items.is_empty() {
        let start = ordinal_start(&group.items);
        out.push_str(&format!(
            "#pad(left: 20pt)[\n#set par(leading: 0.5em)\n#set enum(numbering: \"a.\", start: {}, spacing: 0.8em)\n",
            start
        ));
        for child in &group.items {
            render_child(out, child);
        }
        out.push_str("]\n");
    }
}

/// Convert the first child's letter ordinal to a numeric start value.
fn ordinal_start(items: &[Item]) -> u32 {
    if let Some(Item::Step(step)) = items.first() {
        if let Some(ord) = &step.ordinal {
            if let Some(c) = ord.chars().next() {
                if c.is_ascii_lowercase() {
                    return (c as u32) - ('a' as u32) + 1;
                }
            }
        }
    }
    1
}

/// Render items nested under a role group (substeps).
fn render_child(out: &mut String, item: &Item) {
    match item {
        Item::Step(step) => {
            match step.title.as_deref() {
                Some(t) => {
                    out.push_str(&format!("+ {}\n", typst::escape(t)));
                }
                None => {}
            }
        }
        Item::RoleGroup(group) => {
            render_role(out, group);
        }
    }
}

fn render_step(out: &mut String, step: &Step) {
    let ord = match step.kind {
        StepKind::Dependent => step.ordinal.as_deref(),
        StepKind::Parallel => None,
    };

    // Step heading
    match (ord, step.title.as_deref()) {
        (Some(o), Some(t)) => {
            out.push_str(&format!(
                "*{}.* #h(4pt) *{}*\n\n",
                o,
                typst::escape(t)
            ));
        }
        (Some(o), None) => {
            out.push_str(&format!("*{}.*\n\n", o));
        }
        (None, Some(t)) => {
            out.push_str(&format!("*{}*\n\n", typst::escape(t)));
        }
        (None, None) => {}
    }

    for para in &step.body {
        out.push_str(&typst::escape(para));
        out.push_str("\n\n");
    }

    if !step.responses.is_empty() {
        for r in &step.responses {
            match &r.condition {
                Some(cond) => out.push_str(&format!(
                    "- _{} {}_\n",
                    typst::escape(&r.value),
                    typst::escape(cond)
                )),
                None => out.push_str(&format!("- _{}_\n", typst::escape(&r.value))),
            }
        }
        out.push('\n');
    }

    if !step.children.is_empty() {
        out.push_str("#pad(left: 16pt)[\n");
        for child in &step.children {
            render_item(out, child);
        }
        out.push_str("]\n\n");
    }
}

#[cfg(test)]
mod check {
    use crate::templating::template::Renderer;

    use super::ProcedureRenderer;
    use super::super::types::{Document, Item, RoleGroup, Section, Step, StepKind};

    fn dep(ordinal: &str, title: &str) -> Step {
        Step {
            kind: StepKind::Dependent,
            ordinal: Some(ordinal.into()),
            title: Some(title.into()),
            body: Vec::new(),
            responses: Vec::new(),
            children: Vec::new(),
        }
    }

    fn par(title: &str) -> Step {
        Step {
            kind: StepKind::Parallel,
            ordinal: None,
            title: Some(title.into()),
            body: Vec::new(),
            responses: Vec::new(),
            children: Vec::new(),
        }
    }

    #[test]
    fn document_title_in_block() {
        let doc = Document {
            title: Some("Emergency Procedure".into()),
            description: Vec::new(),
            sections: Vec::new(),
        };
        let out = ProcedureRenderer.render(&doc);
        assert!(out.contains("*Emergency Procedure*"));
    }

    #[test]
    fn dependent_step_shows_ordinal() {
        let doc = Document {
            title: None,
            description: Vec::new(),
            sections: vec![Section {
                ordinal: None,
                heading: None,
                description: Vec::new(),
                items: vec![Item::Step(dep("4", "Engineering Design"))],
            }],
        };
        let out = ProcedureRenderer.render(&doc);
        assert!(out.contains("*4.*"));
        assert!(out.contains("*Engineering Design*"));
    }

    #[test]
    fn parallel_step_has_title() {
        let doc = Document {
            title: None,
            description: Vec::new(),
            sections: vec![Section {
                ordinal: None,
                heading: None,
                description: Vec::new(),
                items: vec![Item::Step(par("Check exits"))],
            }],
        };
        let out = ProcedureRenderer.render(&doc);
        assert!(out.contains("Check exits"));
    }

    #[test]
    fn role_group_wraps_children() {
        let doc = Document {
            title: None,
            description: Vec::new(),
            sections: vec![Section {
                ordinal: None,
                heading: None,
                description: Vec::new(),
                items: vec![Item::RoleGroup(RoleGroup {
                    name: "programmers".into(),
                    items: vec![Item::Step(dep("a", "define_interfaces"))],
                })],
            }],
        };
        let out = ProcedureRenderer.render(&doc);
        let role_pos = out.find("programmers").unwrap();
        let step_pos = out.find("define\\_interfaces").unwrap();
        assert!(role_pos < step_pos);
    }

    #[test]
    fn section_heading_with_ordinal() {
        let doc = Document {
            title: None,
            description: Vec::new(),
            sections: vec![Section {
                ordinal: Some("III".into()),
                heading: Some("Implementation".into()),
                description: Vec::new(),
                items: Vec::new(),
            }],
        };
        let out = ProcedureRenderer.render(&doc);
        assert!(out.contains("*III.*"));
        assert!(out.contains("*Implementation*"));
    }
}
