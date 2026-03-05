//! Templating engine: AST accessor methods.
//!
//! This module provides accessor methods on AST types for convenient
//! iteration when extracting content for templates. This allows us to hide
//! parser artifacts like `Scope` and iterate over the constructs we recognize
//! as sections, steps, and tasks.

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
    /// Returns the text content of this paragraph as a single String.
    /// When text is present, invocations are treated as cross-references
    /// and omitted. When the only content is invocations (and bindings),
    /// the invocation target names are included as fallback text.
    pub fn text(&self) -> String {
        let has_text = self
            .0
            .iter()
            .any(|d| Self::has_text(d));

        let mut result = String::new();
        for descriptive in &self.0 {
            Self::append_descriptive(&mut result, descriptive, !has_text);
        }
        result
    }

    fn has_text(descriptive: &Descriptive<'i>) -> bool {
        match descriptive {
            Descriptive::Text(_) => true,
            Descriptive::Binding(inner, _) => Self::has_text(inner),
            _ => false,
        }
    }

    fn append_descriptive(
        result: &mut String,
        descriptive: &Descriptive<'i>,
        include_invocations: bool,
    ) {
        match descriptive {
            Descriptive::Text(text) => {
                if !result.is_empty() && !result.ends_with(' ') {
                    result.push(' ');
                }
                result.push_str(text);
            }
            Descriptive::Application(invocation) if include_invocations => {
                Self::append_invocation_name(result, invocation);
            }
            Descriptive::CodeInline(expr) if include_invocations => {
                Self::append_expression_name(result, expr);
            }
            Descriptive::Binding(inner, _) => {
                Self::append_descriptive(result, inner, include_invocations);
            }
            _ => {}
        }
    }

    fn append_invocation_name(result: &mut String, invocation: &crate::language::Invocation<'i>) {
        if !result.is_empty() && !result.ends_with(' ') {
            result.push(' ');
        }
        let name = match &invocation.target {
            crate::language::Target::Local(id) => id.0,
            crate::language::Target::Remote(ext) => ext.0,
        };
        result.push_str(name);
    }

    fn append_expression_name(result: &mut String, expr: &crate::language::Expression<'i>) {
        match expr {
            crate::language::Expression::Application(invocation) => {
                Self::append_invocation_name(result, invocation);
            }
            crate::language::Expression::Repeat(inner) => {
                Self::append_expression_name(result, inner);
            }
            crate::language::Expression::Foreach(_, inner) => {
                Self::append_expression_name(result, inner);
            }
            crate::language::Expression::Binding(inner, _) => {
                Self::append_expression_name(result, inner);
            }
            _ => {}
        }
    }

    /// Returns an iterator over the descriptive elements.
    pub fn elements(&self) -> impl Iterator<Item = &Descriptive<'i>> {
        self.0
            .iter()
    }
}
