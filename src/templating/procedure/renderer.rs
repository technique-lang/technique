//! Formats procedure domain types into Typst.
//!
//! Produces output styled after operational procedures: sans-serif font,
//! title block with overview, blue "Procedure" bar, numbered steps with
//! bold titles, roles as indented bold names, and lettered substeps.

use crate::templating::template::Renderer;
use crate::templating::typst;

use super::types::{Document, Node, Response, StepKind};

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

    if !document.description.is_empty() || has_sections(&document.body) {
        out.push_str("_Overview_\n\n");
        for para in &document.description {
            out.push_str(&typst::escape(para));
            out.push('\n');
        }
        if has_sections(&document.body) {
            out.push_str("\n#grid(columns: (auto, 1fr), column-gutter: 6pt, row-gutter: 0.3em,\n");
            for node in &document.body {
                render_outline_entry(&mut out, node);
            }
            out.push_str(")\n");
        }
    }

    // Procedure bar
    out.push_str("#block(width: 100%, fill: rgb(\"#006699\"), inset: 5pt)[#text(fill: white)[*Procedure*]]\n\n");

    // Body
    let total = document.body.len();
    for (i, node) in document.body.iter().enumerate() {
        render_node(&mut out, node);
        // Section dividers
        if i + 1 < total {
            if let Node::Section { .. } = node {
                out.push_str("#line(length: 100%, stroke: (thickness: 0.5pt, paint: rgb(\"#003366\"), dash: (\"dot\", 2pt, 4pt, 2pt)))\n\n");
            }
        }
    }

    out.push_str("]\n");
    out
}

/// True if any top-level node is a Section.
fn has_sections(body: &[Node]) -> bool {
    body.iter()
        .any(|n| matches!(n, Node::Section { .. }))
}

fn render_outline_entry(out: &mut String, node: &Node) {
    if let Node::Section { ordinal, heading, .. } = node {
        match heading {
            Some(heading) => {
                out.push_str(&format!(
                    "[{}.], [{}],\n",
                    ordinal,
                    typst::escape(heading)
                ));
            }
            None => {
                out.push_str(&format!("[{}.], [],\n", ordinal));
            }
        }
    }
}

fn render_node(out: &mut String, node: &Node) {
    match node {
        Node::Section { ordinal, heading, children } => {
            render_section(out, ordinal, heading.as_deref(), children);
        }
        Node::Procedure { name, title, description, children } => {
            render_procedure(out, name, title.as_deref(), description, children);
        }
        Node::Step { .. } => {
            render_step(out, node);
        }
        Node::Attribute { name, children } => {
            render_role(out, name, children);
        }
    }
}

fn render_section(out: &mut String, ordinal: &str, heading: Option<&str>, children: &[Node]) {
    match heading {
        Some(heading) => {
            out.push_str(&format!(
                "#text(size: 14pt)[*{}.* #h(8pt) *{}*]\n\n",
                ordinal,
                typst::escape(heading)
            ));
        }
        None => {
            out.push_str(&format!("#text(size: 14pt)[*{}.*]\n\n", ordinal));
        }
    }

    for child in children {
        render_node(out, child);
    }
}

fn render_procedure(out: &mut String, name: &str, title: Option<&str>, description: &[String], children: &[Node]) {
    out.push_str(&format!(
        "#text(size: 7pt)[`{}`]\\\n",
        name
    ));
    if let Some(title) = title {
        out.push_str(&format!(
            "#text(size: 11pt)[*{}*]\n\n",
            typst::escape(title)
        ));
    } else {
        out.push('\n');
    }

    for para in description {
        out.push_str(&typst::escape(para));
        out.push_str("\n\n");
    }

    if !children.is_empty() {
        out.push_str("#pad(left: 8pt)[\n");
        for child in children {
            render_node(out, child);
        }
        out.push_str("]\n");
    }
}

fn render_role(out: &mut String, name: &str, children: &[Node]) {
    out.push_str(&format!("- *{}*\n", typst::escape(name)));
    if !children.is_empty() {
        let start = ordinal_start(children);
        out.push_str(&format!(
            "#pad(left: 20pt)[\n#set par(leading: 0.5em)\n#set enum(numbering: \"a.\", start: {}, spacing: 0.8em)\n",
            start
        ));
        for child in children {
            render_child(out, child);
        }
        out.push_str("]\n");
    }
}

/// Convert the first child's letter ordinal to a numeric start value.
fn ordinal_start(children: &[Node]) -> u32 {
    if let Some(Node::Step { ordinal, .. }) = children.first() {
        if let Some(ord) = ordinal {
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
fn render_child(out: &mut String, node: &Node) {
    match node {
        Node::Step { title, .. } => {
            if let Some(t) = title {
                out.push_str(&format!("+ {}\n", typst::escape(t)));
            }
        }
        Node::Attribute { name, children } => {
            render_role(out, name, children);
        }
        _ => {}
    }
}

fn render_step(out: &mut String, node: &Node) {
    let Node::Step { kind, ordinal, title, body, invocations, responses, children } = node else {
        return;
    };

    let ord = match kind {
        StepKind::Dependent => ordinal.as_deref(),
        StepKind::Parallel => None,
    };

    // Invocations on the line before the step heading
    render_invocations(out, invocations);

    // Step heading
    match (ord, title.as_deref()) {
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

    for para in body {
        out.push_str(&typst::escape(para));
        out.push_str("\n\n");
    }

    if !responses.is_empty() {
        render_responses(out, responses);
    }

    if !children.is_empty() {
        out.push_str("#pad(left: 16pt)[\n");
        for child in children {
            render_node(out, child);
        }
        out.push_str("]\n\n");
    }
}

fn render_invocations(out: &mut String, invocations: &[String]) {
    if !invocations.is_empty() {
        let names = invocations.join(", ");
        out.push_str(&format!(
            "#text(size: 7pt)[`{}`]\\\n",
            names
        ));
    }
}

fn render_responses(out: &mut String, responses: &[Response]) {
    for r in responses {
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

#[cfg(test)]
mod check {
    use crate::templating::template::Renderer;

    use super::ProcedureRenderer;
    use super::super::types::{Document, Node, StepKind};

    fn dep(ordinal: &str, title: &str) -> Node {
        Node::Step {
            kind: StepKind::Dependent,
            ordinal: Some(ordinal.into()),
            title: Some(title.into()),
            body: Vec::new(),
            invocations: Vec::new(),
            responses: Vec::new(),
            children: Vec::new(),
        }
    }

    fn par(title: &str) -> Node {
        Node::Step {
            kind: StepKind::Parallel,
            ordinal: None,
            title: Some(title.into()),
            body: Vec::new(),
            invocations: Vec::new(),
            responses: Vec::new(),
            children: Vec::new(),
        }
    }

    #[test]
    fn document_title_in_block() {
        let doc = Document {
            title: Some("Emergency Procedure".into()),
            description: Vec::new(),
            body: Vec::new(),
        };
        let out = ProcedureRenderer.render(&doc);
        assert!(out.contains("*Emergency Procedure*"));
    }

    #[test]
    fn dependent_step_shows_ordinal() {
        let doc = Document {
            title: None,
            description: Vec::new(),
            body: vec![dep("4", "Engineering Design")],
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
            body: vec![par("Check exits")],
        };
        let out = ProcedureRenderer.render(&doc);
        assert!(out.contains("Check exits"));
    }

    #[test]
    fn role_group_wraps_children() {
        let doc = Document {
            title: None,
            description: Vec::new(),
            body: vec![Node::Attribute {
                name: "programmers".into(),
                children: vec![dep("a", "define_interfaces")],
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
            body: vec![Node::Section {
                ordinal: "III".into(),
                heading: Some("Implementation".into()),
                children: Vec::new(),
            }],
        };
        let out = ProcedureRenderer.render(&doc);
        assert!(out.contains("*III.*"));
        assert!(out.contains("*Implementation*"));
    }
}
