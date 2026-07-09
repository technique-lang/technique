//! Engine: accessor helpers over the parser's AST types.
//!
//! The Technique language parser deals with considerable complexity and
//! ambiguity in the surface language, and as a result the parser's AST is
//! somewhat tailored to the form of that surface language. This is fine for
//! compiling and code formatting, but contains too much internal detail for
//! someone writing an output renderer to deal with.
//!
//! This module thus provides convenient iteration methods on AST types so
//! that adapters can extract content without having to match on parser
//! internals directly. The types returned are still the parser's own types
//! (Scope, Paragraph, Response, etc.) — the "adapters" are responsible for
//! projecting these into domain-specific models.

use crate::language::{
    Attribute, Descriptive, Document, Element, Expression, Numeric, Pair, Paragraph, Piece,
    Procedure, Response, Scope, Target, Technique,
};

impl<'i> Document<'i> {
    /// Get all the procedures in the document as an iterator.
    pub fn procedures(&self) -> impl Iterator<Item = &Procedure<'i>> {
        let slice: &[Procedure<'i>] = match &self.body {
            Some(Technique::Procedures(procedures)) => procedures,
            _ => &[],
        };
        slice.iter()
    }

    /// Get all the document's top-level steps as an iterator.
    pub fn steps(&self) -> impl Iterator<Item = &Scope<'i>> {
        let slice: &[Scope<'i>] = match &self.body {
            Some(Technique::Steps(steps)) => steps,
            _ => &[],
        };
        slice.iter()
    }
}

impl<'i> Procedure<'i> {
    // a title() method already exists in language/types.rs

    /// Returns an iterator over the procedure's top-level steps.
    pub fn steps(&self) -> impl Iterator<Item = &Scope<'i>> {
        self.elements
            .iter()
            .flat_map(|element| match element {
                Element::Steps(steps, _) => steps.iter(),
                _ => [].iter(),
            })
    }

    /// Returns an iterator over the procedure's descriptive paragraphs.
    pub fn description(&self) -> impl Iterator<Item = &Paragraph<'i>> {
        self.elements
            .iter()
            .flat_map(|element| match element {
                Element::Description(paragraphs, _) => paragraphs.iter(),
                _ => [].iter(),
            })
    }
}

impl<'i> Scope<'i> {
    /// Returns an iterator over all children.
    pub fn children(&self) -> impl Iterator<Item = &Scope<'i>> {
        let slice: &[Scope<'i>] = match self {
            Scope::DependentBlock { subscopes, .. } => subscopes,
            Scope::ParallelBlock { subscopes, .. } => subscopes,
            Scope::AttributeBlock { subscopes, .. } => subscopes,
            Scope::CodeBlock { subscopes, .. } => subscopes,
            Scope::ResponseBlock { .. } => &[],
            Scope::SectionChunk { .. } => &[],
        };
        slice.iter()
    }

    /// Returns an iterator over child steps only (DependentBlock, ParallelBlock).
    /// Filters out ResponseBlock, CodeBlock, AttributeBlock, etc.
    pub fn substeps(&self) -> impl Iterator<Item = &Scope<'i>> {
        self.children()
            .filter(|s| s.is_step())
    }

    /// Returns the text content of this step (first paragraph).
    pub fn text(&self) -> Option<String> {
        self.description()
            .next()
            .map(|p| p.text())
    }

    /// Returns an iterator over description paragraphs (for step-like scopes).
    pub fn description(&self) -> impl Iterator<Item = &Paragraph<'i>> {
        let slice: &[Paragraph<'i>] = match self {
            Scope::DependentBlock { description, .. } => description,
            Scope::ParallelBlock { description, .. } => description,
            _ => &[],
        };
        slice.iter()
    }

    /// Returns the ordinal if this is a DependentBlock (numbered step).
    pub fn ordinal(&self) -> Option<&'i str> {
        match self {
            Scope::DependentBlock { ordinal, .. } => Some(ordinal),
            _ => None,
        }
    }

    /// Returns an iterator over responses if this is a ResponseBlock.
    pub fn responses(&self) -> impl Iterator<Item = &Response<'i>> {
        let slice: &[Response<'i>] = match self {
            Scope::ResponseBlock { responses, .. } => responses,
            _ => &[],
        };
        slice.iter()
    }

    /// Returns an iterator over role names if this is an AttributeBlock.
    pub fn roles(&self) -> impl Iterator<Item = &'i str> {
        match self {
            Scope::AttributeBlock { attributes, .. } => attributes
                .iter()
                .filter_map(|attr| match attr {
                    Attribute::Role(id, _) => Some(id.value),
                    _ => None,
                })
                .collect::<Vec<_>>()
                .into_iter(),
            _ => Vec::new().into_iter(),
        }
    }

    /// Returns an iterator over place names if this is an AttributeBlock.
    pub fn places(&self) -> impl Iterator<Item = &'i str> {
        match self {
            Scope::AttributeBlock { attributes, .. } => attributes
                .iter()
                .filter_map(|attr| match attr {
                    Attribute::Place(id, _) => Some(id.value),
                    _ => None,
                })
                .collect::<Vec<_>>()
                .into_iter(),
            _ => Vec::new().into_iter(),
        }
    }

    /// Returns the tablet pairs if this is a CodeBlock containing a single
    /// list whose elements are all labelled values.
    pub fn tablet(&self) -> Option<Vec<&Pair<'i>>> {
        match self {
            Scope::CodeBlock { expressions, .. } => {
                if expressions.len() == 1 {
                    if let Expression::List(elements, _) = &expressions[0] {
                        let pairs: Vec<&Pair<'i>> = elements
                            .iter()
                            .filter_map(|element| {
                                if let Expression::Pair(pair, _) = element {
                                    Some(pair.as_ref())
                                } else {
                                    None
                                }
                            })
                            .collect();
                        if !pairs.is_empty() && pairs.len() == elements.len() {
                            return Some(pairs);
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Returns tablet pairs from an inline code block in a step's description,
    /// the folded-inline counterpart of `tablet()`.
    pub fn inline_tablet(&self) -> Option<Vec<&Pair<'i>>> {
        self.description()
            .find_map(|paragraph| paragraph.tablet())
    }

    /// Returns true if this scope represents a step (dependent or parallel).
    pub fn is_step(&self) -> bool {
        match self {
            Scope::DependentBlock { .. } | Scope::ParallelBlock { .. } => true,
            _ => false,
        }
    }

    /// Returns section info (numeral, title) if this is a SectionChunk.
    pub fn section_info(&self) -> Option<(&'i str, Option<&Paragraph<'i>>)> {
        match self {
            Scope::SectionChunk { numeral, title, .. } => Some((numeral, title.as_ref())),
            _ => None,
        }
    }

    /// Returns the body of a SectionChunk.
    pub fn body(&self) -> Option<&Technique<'i>> {
        match self {
            Scope::SectionChunk { body, .. } => Some(body),
            _ => None,
        }
    }

    /// Returns the expression of a CodeBlock as readable text.
    pub fn expression_text(&self) -> Option<String> {
        match self {
            Scope::CodeBlock { expressions, .. } => {
                if expressions.is_empty() {
                    return None;
                }
                let texts: Vec<String> = expressions
                    .iter()
                    .map(render_expression)
                    .collect();
                Some(texts.join("\n"))
            }
            _ => None,
        }
    }
}

impl<'i> Technique<'i> {
    /// Returns an iterator over procedures if this is a Procedures variant.
    pub fn procedures(&self) -> impl Iterator<Item = &Procedure<'i>> {
        let slice: &[Procedure<'i>] = match self {
            Technique::Procedures(procedures) => procedures,
            _ => &[],
        };
        slice.iter()
    }

    /// Returns an iterator over steps if this is a Steps variant.
    pub fn steps(&self) -> impl Iterator<Item = &Scope<'i>> {
        let slice: &[Scope<'i>] = match self {
            Technique::Steps(steps) => steps,
            _ => &[],
        };
        slice.iter()
    }
}

impl<'i> Procedure<'i> {
    /// Returns the procedure name.
    pub fn name(&self) -> &'i str {
        self.name
            .value
    }
}

impl<'i> Response<'i> {
    /// Returns the response value.
    pub fn value(&self) -> &'i str {
        self.value
    }

    /// Returns the optional condition.
    pub fn condition(&self) -> Option<&'i str> {
        self.condition
    }
}

/// Render an Expression as human-readable text.
/// Returns (expression_text, body_lines) where body_lines captures multiline
/// content separately for distinct styling.
fn render_expression_parts(expr: &Expression) -> (String, Vec<String>) {
    if let Expression::Execution(func, _) = expr {
        let mut body = Vec::new();
        for param in &func.parameters {
            if let Expression::Multiline(_, lines, _) = param {
                body.extend(
                    lines
                        .iter()
                        .map(|s| s.to_string()),
                );
            }
        }
        if !body.is_empty() {
            return (
                format!(
                    "{}(",
                    func.target
                        .value
                ),
                body,
            );
        }
    }
    (render_expression(expr), Vec::new())
}

fn render_expression(expr: &Expression) -> String {
    match expr {
        Expression::Repeat(inner, _) => {
            format!("repeat {}", render_expression(inner))
        }
        Expression::Within(inner, _) => {
            format!("within {}", render_expression(inner))
        }
        Expression::Cost(inner, _) => {
            format!("$({})", render_expression(inner))
        }
        Expression::Foreach(ids, inner, _) => {
            let vars = if ids.len() == 1 {
                ids[0]
                    .value
                    .to_string()
            } else {
                format!(
                    "({})",
                    ids.iter()
                        .map(|id| id.value)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            format!("foreach {} in {}", vars, render_expression(inner))
        }
        Expression::Application(inv, _) => {
            let name = match &inv.target {
                Target::Local(id) => id.value,
                Target::Remote(ext) => ext.value,
            };
            if let Some(params) = &inv.parameters {
                let args: Vec<_> = params
                    .iter()
                    .map(render_expression)
                    .collect();
                format!("<{}>({})", name, args.join(", "))
            } else {
                format!("<{}>", name)
            }
        }
        Expression::Execution(func, _) => {
            let args: Vec<_> = func
                .parameters
                .iter()
                .map(render_expression)
                .collect();
            format!(
                "{}({})",
                func.target
                    .value,
                args.join(", ")
            )
        }
        Expression::Multiline(_, lines, _) => lines.join("\n"),
        Expression::Variable(id, _) => id
            .value
            .to_string(),
        Expression::Binding(inner, _, _) => render_expression(inner),
        Expression::String(pieces, _) => {
            let mut result = String::new();
            for piece in pieces {
                match piece {
                    Piece::Text(t) => result.push_str(t),
                    Piece::Interpolation(e) => result.push_str(&render_expression(e)),
                }
            }
            result
        }
        Expression::Response(value, _) => format!("'{}'", value),
        Expression::Number(Numeric::Scientific(q), _) => q.to_string(),
        Expression::Number(Numeric::Integral(n), _) => n.to_string(),
        Expression::Pair(pair, _) => {
            format!("\"{}\" = {}", pair.label, render_expression(&pair.value))
        }
        Expression::List(elements, _) => {
            let items: Vec<_> = elements
                .iter()
                .map(render_expression)
                .collect();
            format!("[{}]", items.join(", "))
        }
        Expression::Tuple(elements, _) => {
            let items: Vec<_> = elements
                .iter()
                .map(render_expression)
                .collect();
            format!("({})", items.join(", "))
        }
        Expression::Hole(_) => "?".to_string(),
        Expression::Unit(_) => "()".to_string(),
        Expression::Separator => String::new(),
    }
}

impl<'i> Paragraph<'i> {
    /// Returns only the text content of this paragraph.
    pub fn text(&self) -> String {
        let mut result = String::new();
        for d in &self.0 {
            Self::append_text(&mut result, d);
        }
        result
    }

    /// Returns invocation target names from this paragraph.
    pub fn invocations(&self) -> Vec<&'i str> {
        let mut targets = Vec::new();
        for d in &self.0 {
            Self::extract_invocations(&mut targets, d);
        }
        targets
    }

    /// Returns tablet pairs if a `CodeInline` in this paragraph is a single
    /// list whose elements are all labelled values.
    pub fn tablet(&self) -> Option<Vec<&Pair<'i>>> {
        for d in &self.0 {
            if let Descriptive::CodeInline(exprs) = d {
                let [Expression::List(elements, _)] = exprs.as_slice() else {
                    continue;
                };
                let pairs: Vec<&Pair<'i>> = elements
                    .iter()
                    .filter_map(|element| {
                        if let Expression::Pair(pair, _) = element {
                            Some(pair.as_ref())
                        } else {
                            None
                        }
                    })
                    .collect();
                if !pairs.is_empty() && pairs.len() == elements.len() {
                    return Some(pairs);
                }
            }
        }
        None
    }

    /// Returns rendered code inline expressions from this paragraph.
    /// Each entry is (expression, body_lines) where body_lines captures
    /// multiline content for separate styling.
    pub fn code_inlines(&self) -> Vec<(String, Vec<String>)> {
        let mut results = Vec::new();
        for d in &self.0 {
            if let Descriptive::CodeInline(exprs) = d {
                for expr in exprs {
                    let (text, body) = render_expression_parts(expr);
                    if !text.is_empty() {
                        results.push((text, body));
                    }
                }
            }
        }
        results
    }

    /// Returns text of the step body if present, otherwise (for the scenarion
    /// where the step is a bare invocation or code expression) a readable
    /// rendering of the first non-text element.
    pub fn content(&self) -> String {
        let text = self.text();
        if !text.is_empty() {
            return text;
        }
        for descriptive in &self.0 {
            let result = Self::descriptive_content(descriptive);
            if !result.is_empty() {
                return result;
            }
        }
        String::new()
    }

    fn descriptive_content(descriptive: &Descriptive<'i>) -> String {
        match descriptive {
            Descriptive::Application(inv) => Self::invocation_name(inv).to_string(),
            Descriptive::CodeInline(exprs) => exprs
                .iter()
                .map(Self::expression_content)
                .find(|content| !content.is_empty())
                .unwrap_or_default(),
            Descriptive::Binding(inner, _) => Self::descriptive_content(inner),
            _ => String::new(),
        }
    }

    fn expression_content(expr: &crate::language::Expression<'i>) -> String {
        match expr {
            crate::language::Expression::Application(invocation, _) => {
                Self::invocation_name(invocation).to_string()
            }
            crate::language::Expression::Repeat(inner, _) => {
                format!("repeat {}", Self::expression_content(inner))
            }
            crate::language::Expression::Foreach(_, inner, _) => {
                format!("foreach {}", Self::expression_content(inner))
            }
            crate::language::Expression::Binding(inner, _, _) => Self::expression_content(inner),
            _ => String::new(),
        }
    }

    fn append_text(result: &mut String, descriptive: &Descriptive<'i>) {
        match descriptive {
            Descriptive::Text(text) => {
                if !result.is_empty() && !result.ends_with(' ') {
                    result.push(' ');
                }
                result.push_str(text);
            }
            Descriptive::Binding(inner, _) => Self::append_text(result, inner),
            _ => {}
        }
    }

    fn extract_invocations(targets: &mut Vec<&'i str>, descriptive: &Descriptive<'i>) {
        match descriptive {
            Descriptive::Application(inv) => {
                targets.push(Self::invocation_name(inv));
            }
            Descriptive::CodeInline(exprs) => {
                for expr in exprs {
                    Self::extract_expression_invocations(targets, expr);
                }
            }
            Descriptive::Binding(inner, _) => {
                Self::extract_invocations(targets, inner);
            }
            _ => {}
        }
    }

    fn extract_expression_invocations(
        targets: &mut Vec<&'i str>,
        expr: &crate::language::Expression<'i>,
    ) {
        match expr {
            crate::language::Expression::Application(inv, _) => {
                targets.push(Self::invocation_name(inv));
            }
            crate::language::Expression::Repeat(inner, _) => {
                Self::extract_expression_invocations(targets, inner);
            }
            crate::language::Expression::Foreach(_, inner, _) => {
                Self::extract_expression_invocations(targets, inner);
            }
            crate::language::Expression::Binding(inner, _, _) => {
                Self::extract_expression_invocations(targets, inner);
            }
            _ => {}
        }
    }

    fn invocation_name(inv: &crate::language::Invocation<'i>) -> &'i str {
        match &inv.target {
            crate::language::Target::Local(id) => id.value,
            crate::language::Target::Remote(ext) => ext.value,
        }
    }
}

/// A paragraph of prose with inline markup.
#[derive(Debug, PartialEq)]
pub struct Prose(pub Vec<Inline>);

/// An inline fragment within prose text.
#[derive(Debug, PartialEq)]
pub enum Inline {
    Text(String),
    Emphasis(String),
    Strong(String),
    Code(String),
}

impl Prose {
    /// Parse a plain string, converting _text_ to emphasis, *text* to strong,
    /// and `text` to code.
    pub fn parse(s: &str) -> Prose {
        let mut fragments = Vec::new();
        let mut rest = s;

        while !rest.is_empty() {
            let next = rest.find(|c: char| c == '_' || c == '*' || c == '`');

            match next {
                None => {
                    fragments.push(Inline::Text(rest.to_string()));
                    break;
                }
                Some(i) => {
                    let delim = rest.as_bytes()[i] as char;
                    let after = &rest[i + 1..];

                    match after.find(delim) {
                        Some(end) if end > 0 => {
                            if i > 0 {
                                fragments.push(Inline::Text(rest[..i].to_string()));
                            }
                            let content = after[..end].to_string();
                            fragments.push(match delim {
                                '_' => Inline::Emphasis(content),
                                '*' => Inline::Strong(content),
                                '`' => Inline::Code(content),
                                _ => unreachable!(),
                            });
                            rest = &after[end + 1..];
                        }
                        _ => {
                            fragments.push(Inline::Text(rest[..i + 1].to_string()));
                            rest = after;
                        }
                    }
                }
            }
        }

        Prose(fragments)
    }
}

#[cfg(test)]
mod check {
    use crate::language::{
        Descriptive, Expression, Identifier, Invocation, Paragraph, Span, Target,
    };

    fn local<'a>(name: &'a str) -> Invocation<'a> {
        Invocation {
            target: Target::Local(Identifier::new(name)),
            parameters: None,
        }
    }

    // Pure text: "Ensure physical and digital safety"
    #[test]
    fn text_only_paragraph() {
        let p = Paragraph::new(vec![Descriptive::Text(
            "Ensure physical and digital safety",
        )]);
        assert_eq!(p.text(), "Ensure physical and digital safety");
        assert!(
            p.invocations()
                .is_empty()
        );
        assert_eq!(p.content(), "Ensure physical and digital safety");
    }

    // Bare invocation: <ensure_safety>
    #[test]
    fn invocation_only_paragraph() {
        let p = Paragraph::new(vec![Descriptive::Application(local("ensure_safety"))]);
        assert_eq!(p.text(), "");
        assert_eq!(p.invocations(), vec!["ensure_safety"]);
        assert_eq!(p.content(), "ensure_safety");
    }

    // Mixed: Define Requirements <define_requirements>(concept)
    // Text is present so content() returns just the text.
    #[test]
    fn mixed_text_and_invocation() {
        let p = Paragraph::new(vec![
            Descriptive::Text("Define Requirements"),
            Descriptive::Application(local("define_requirements")),
        ]);
        assert_eq!(p.text(), "Define Requirements");
        assert_eq!(p.invocations(), vec!["define_requirements"]);
        assert_eq!(p.content(), "Define Requirements");
    }

    // CodeInline with repeat: { repeat <incident_action_cycle> }
    #[test]
    fn repeat_expression() {
        let p = Paragraph::new(vec![Descriptive::CodeInline(vec![Expression::Repeat(
            Box::new(Expression::Application(
                local("incident_action_cycle"),
                Span::default(),
            )),
            Span::default(),
        )])]);
        assert_eq!(p.text(), "");
        assert_eq!(p.invocations(), vec!["incident_action_cycle"]);
        assert_eq!(p.content(), "repeat incident_action_cycle");
    }

    // Binding wrapping an invocation: <observe>(s) ~ e
    #[test]
    fn binding_with_invocation() {
        let p = Paragraph::new(vec![Descriptive::Binding(
            Box::new(Descriptive::Application(local("observe"))),
            vec![Identifier::new("e")],
        )]);
        assert_eq!(p.text(), "");
        assert_eq!(p.invocations(), vec!["observe"]);
        assert_eq!(p.content(), "observe");
    }

    // CodeInline with foreach: { foreach design in designs }
    #[test]
    fn foreach_expression() {
        let p = Paragraph::new(vec![Descriptive::CodeInline(vec![Expression::Foreach(
            vec![Identifier::new("design")],
            Box::new(Expression::Application(local("implement"), Span::default())),
            Span::default(),
        )])]);
        assert_eq!(p.text(), "");
        assert_eq!(p.invocations(), vec!["implement"]);
        assert_eq!(p.content(), "foreach implement");
    }

    // -- Prose inline markup --

    use super::{Inline, Prose};

    #[test]
    fn prose_plain_text() {
        let p = Prose::parse("hello world");
        assert_eq!(p.0, vec![Inline::Text("hello world".into())]);
    }

    #[test]
    fn prose_emphasis() {
        let p = Prose::parse("the _idea_ is good");
        assert_eq!(
            p.0,
            vec![
                Inline::Text("the ".into()),
                Inline::Emphasis("idea".into()),
                Inline::Text(" is good".into()),
            ]
        );
    }

    #[test]
    fn prose_strong() {
        let p = Prose::parse("a *bold* move");
        assert_eq!(
            p.0,
            vec![
                Inline::Text("a ".into()),
                Inline::Strong("bold".into()),
                Inline::Text(" move".into()),
            ]
        );
    }

    #[test]
    fn prose_code() {
        let p = Prose::parse("run `cmd` now");
        assert_eq!(
            p.0,
            vec![
                Inline::Text("run ".into()),
                Inline::Code("cmd".into()),
                Inline::Text(" now".into()),
            ]
        );
    }

    #[test]
    fn prose_mixed() {
        let p = Prose::parse("the _idea_ and *design* with `code`");
        assert_eq!(
            p.0,
            vec![
                Inline::Text("the ".into()),
                Inline::Emphasis("idea".into()),
                Inline::Text(" and ".into()),
                Inline::Strong("design".into()),
                Inline::Text(" with ".into()),
                Inline::Code("code".into()),
            ]
        );
    }

    #[test]
    fn prose_unclosed_delimiter() {
        let p = Prose::parse("a_b has no pair");
        assert_eq!(
            p.0,
            vec![
                Inline::Text("a_".into()),
                Inline::Text("b has no pair".into()),
            ]
        );
    }

    #[test]
    fn prose_empty_string() {
        let p = Prose::parse("");
        assert_eq!(p.0, vec![]);
    }
}
