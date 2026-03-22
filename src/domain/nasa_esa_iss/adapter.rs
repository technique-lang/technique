//! Projects the parser's AST into the NASA/ESA ISS Crew Procedure domain
//! model.
//!
//! This isn't really a serious example (because the procedure template used
//! by NASA and ESA for ISS operations is a bit ridiculous), but it shows
//! Technique is usable in many procedural domains.
//!
//! Delegates to the procedure adapter for initial extraction, then
//! post-processes the result: invocation-only steps are replaced by
//! their target procedures (with ordinals transferred), producing
//! the flat numbered-procedure layout used in ISS flight documents.
//! Domain-specific builtins like `cmd()` are recognized and their
//! expressions reformatted for template rendering.

use std::collections::HashMap;

use crate::domain::procedure::adapter::ProcedureAdapter;
use crate::domain::procedure::types::{Document, Node};
use crate::domain::Adapter;
use crate::language;

pub struct NasaEsaIssAdapter;

impl Adapter for NasaEsaIssAdapter {
    type Model = Document;

    fn extract(&self, document: &language::Document) -> Document {
        let mut doc = ProcedureAdapter.extract(document);
        inline_procedures(&mut doc);
        rewrite_builtins(&mut doc.body);
        doc
    }
}

// -- Procedure inlining ------------------------------------------------------

/// Replace invocation-only steps with their target Procedure nodes,
/// transferring the step's ordinal into the procedure's name field
/// (which the template renders as the step number).
fn inline_procedures(doc: &mut Document) {
    let ordinals: HashMap<String, String> = collect_ordinals(&doc.body)
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();

    for node in &mut doc.body {
        if let Node::Procedure { name, .. } = node {
            if let Some(ord) = ordinals.get(name.as_str()) {
                *name = ord.clone();
            }
        }
    }

    doc.body.retain(|node| {
        !matches!(node, Node::Sequential { title: None, invocations, .. } if !invocations.is_empty())
    });
}

/// Collect ordinals from invocation-only steps: procedure_name -> ordinal.
fn collect_ordinals(nodes: &[Node]) -> HashMap<&str, String> {
    let mut map = HashMap::new();
    for node in nodes {
        if let Node::Sequential {
            ordinal,
            title: None,
            invocations,
            ..
        } = node
        {
            if invocations.len() == 1 {
                map.insert(invocations[0].as_str(), ordinal.clone());
            }
        }
    }
    map
}

// -- Builtin functions -------------------------------------------------------

/// Rewrite domain-specific builtin expressions in CodeBlock nodes.
/// `cmd(Inhibit)` becomes `cmd Inhibit` for template styling.
fn rewrite_builtins(nodes: &mut [Node]) {
    for node in nodes.iter_mut() {
        match node {
            Node::CodeBlock {
                expression,
                children,
                ..
            } => {
                *expression = rewrite_expression(expression);
                rewrite_builtins(children);
            }
            Node::Sequential { children, .. }
            | Node::Parallel { children, .. }
            | Node::Section { children, .. }
            | Node::Procedure { children, .. }
            | Node::Attribute { children, .. } => {
                rewrite_builtins(children);
            }
        }
    }
}

/// Rewrite a single expression string if it matches a builtin pattern.
fn rewrite_expression(expr: &str) -> String {
    if let Some(arg) = expr
        .strip_prefix("cmd(")
        .and_then(|s| s.strip_suffix(')'))
    {
        return format!("cmd {}", arg);
    }
    expr.to_string()
}
