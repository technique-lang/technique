//! Typst serialization for procedure domain types.

use crate::domain::serialize::{escape_string, Markup, Render};

use super::types::{Document, Node, Response};

impl Render for Document {
    fn render(&self, out: &mut Markup) {
        out.call("render-document");
        out.param_opt("source", &self.source);
        out.param_opt("name", &self.name);
        out.param_opt("title", &self.title);
        out.param_list("description", &self.description);
        out.content_open("children");

        let has_sections = self
            .body
            .iter()
            .any(|n| {
                if let Node::Section { .. } = n {
                    true
                } else {
                    false
                }
            });

        if has_sections {
            render_outline(out, &self.body);
        }

        for (i, node) in self
            .body
            .iter()
            .enumerate()
        {
            if i > 0 {
                if let Node::Section { .. } = node {
                    out.call("section-divider");
                    out.close();
                }
            }
            node.render(out);
        }

        out.content_close();
        out.close();
    }
}

fn render_outline(out: &mut Markup, body: &[Node]) {
    let sections: Vec<_> = body
        .iter()
        .filter_map(|n| {
            if let Node::Section {
                ordinal, heading, ..
            } = n
            {
                Some((ordinal.as_str(), heading.as_deref()))
            } else {
                None
            }
        })
        .collect();

    out.call("render-outline");
    out.raw("sections: (");
    for (ordinal, heading) in &sections {
        out.raw(&format!(
            "(ordinal: \"{}\", heading: {}), ",
            escape_string(ordinal),
            match heading {
                Some(h) => format!("\"{}\"", escape_string(h)),
                None => "none".to_string(),
            }
        ));
    }
    out.raw("), ");
    out.close();
}

impl Render for Node {
    fn render(&self, out: &mut Markup) {
        match self {
            Node::Section {
                ordinal,
                heading,
                children,
            } => {
                out.call("render-section");
                out.param("ordinal", ordinal);
                out.param_opt("heading", heading);
                if !children.is_empty() {
                    out.content_open("children");
                    for child in children {
                        child.render(out);
                    }
                    out.content_close();
                }
                out.close();
            }
            Node::Procedure {
                name,
                title,
                description,
                children,
            } => {
                out.call("render-procedure");
                out.param("name", name);
                out.param_opt("title", title);
                out.param_list("description", description);
                if !children.is_empty() {
                    out.content_open("children");
                    for child in children {
                        child.render(out);
                    }
                    out.content_close();
                }
                out.close();
            }
            Node::Sequential {
                ordinal,
                title,
                body,
                invocations,
                responses,
                children,
            } => {
                out.call("render-step");
                out.param("ordinal", ordinal);
                out.param_opt("title", title);
                out.param_list("body", body);
                out.param_list("invocations", invocations);
                if !responses.is_empty() {
                    out.content_open("responses");
                    for r in responses {
                        r.render(out);
                    }
                    out.content_close();
                }
                if !children.is_empty() {
                    out.content_open("children");
                    for child in children {
                        child.render(out);
                    }
                    out.content_close();
                }
                out.close();
            }
            Node::Parallel {
                title,
                body,
                invocations,
                responses,
                children,
            } => {
                out.call("render-step");
                out.param_opt("title", title);
                out.param_list("body", body);
                out.param_list("invocations", invocations);
                if !responses.is_empty() {
                    out.content_open("responses");
                    for r in responses {
                        r.render(out);
                    }
                    out.content_close();
                }
                if !children.is_empty() {
                    out.content_open("children");
                    for child in children {
                        child.render(out);
                    }
                    out.content_close();
                }
                out.close();
            }
            Node::Attribute { name, children } => {
                out.call("render-attribute");
                out.param("name", name);
                if !children.is_empty() {
                    out.content_open("children");
                    for child in children {
                        child.render(out);
                    }
                    out.content_close();
                }
                out.close();
            }
            Node::CodeBlock {
                expression,
                responses,
                children,
            } => {
                out.call("render-code-block");
                out.param("expression", expression);
                if !responses.is_empty() {
                    out.content_open("responses");
                    for r in responses {
                        r.render(out);
                    }
                    out.content_close();
                }
                if !children.is_empty() {
                    out.content_open("children");
                    for child in children {
                        child.render(out);
                    }
                    out.content_close();
                }
                out.close();
            }
        }
    }
}

impl Render for Response {
    fn render(&self, out: &mut Markup) {
        out.call("render-response");
        out.param("value", &self.value);
        out.param_opt("condition", &self.condition);
        out.close();
    }
}
