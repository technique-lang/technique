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
    Attribute, Descriptive, Document, Element, Paragraph, Procedure, Response, Scope, Technique,
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
                Element::Steps(steps) => steps.iter(),
                _ => [].iter(),
            })
    }

    /// Returns an iterator over the procedure's descriptive paragraphs.
    pub fn description(&self) -> impl Iterator<Item = &Paragraph<'i>> {
        self.elements
            .iter()
            .flat_map(|element| match element {
                Element::Description(paragraphs) => paragraphs.iter(),
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
            .filter(|s| {
                matches!(
                    s,
                    Scope::DependentBlock { .. } | Scope::ParallelBlock { .. }
                )
            })
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
            Scope::ResponseBlock { responses } => responses,
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
                    Attribute::Role(id) => Some(id.0),
                    _ => None,
                })
                .collect::<Vec<_>>()
                .into_iter(),
            _ => Vec::new().into_iter(),
        }
    }

    /// Returns true if this scope represents a step (dependent or parallel).
    pub fn is_step(&self) -> bool {
        matches!(
            self,
            Scope::DependentBlock { .. } | Scope::ParallelBlock { .. }
        )
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
            .0
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

    /// Returns displayable content: text if present, otherwise the
    /// first invocation target name.
    pub fn content(&self) -> String {
        let text = self.text();
        if !text.is_empty() {
            return text;
        }
        self.invocations()
            .first()
            .unwrap_or(&"")
            .to_string()
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
            Descriptive::CodeInline(expr) => {
                Self::extract_expression_invocations(targets, expr);
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
            crate::language::Expression::Application(inv) => {
                targets.push(Self::invocation_name(inv));
            }
            crate::language::Expression::Repeat(inner) => {
                Self::extract_expression_invocations(targets, inner);
            }
            crate::language::Expression::Foreach(_, inner) => {
                Self::extract_expression_invocations(targets, inner);
            }
            crate::language::Expression::Binding(inner, _) => {
                Self::extract_expression_invocations(targets, inner);
            }
            _ => {}
        }
    }

    fn invocation_name(inv: &crate::language::Invocation<'i>) -> &'i str {
        match &inv.target {
            crate::language::Target::Local(id) => id.0,
            crate::language::Target::Remote(ext) => ext.0,
        }
    }
}
