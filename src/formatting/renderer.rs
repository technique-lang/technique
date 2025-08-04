//! Renderers for colourizing Technique language

use owo_colors::OwoColorize;

/// Types of content that can be rendered with different styles
#[derive(Debug, Clone, Copy)]
pub enum Syntax {
    Header,
    Declaration,
    Description,
    Genus,
    StepItem,
    CodeBlock,
    Variable,
    Section,
    String,
    Numeric,
    Syntax,
}

/// Trait for different rendering backends (Identity, ANSI, Typst)
pub trait Render {
    /// Render content with the specified type/style
    fn render(&self, content_type: Syntax, content: &str) -> String;
}

/// Identity renderer - returns content unchanged (no markup)
pub struct Identity;

impl Render for Identity {
    fn render(&self, _syntax: Syntax, content: &str) -> String {
        content.to_string()
    }
}

pub struct Terminal;

impl Render for Terminal {
    fn render(&self, syntax: Syntax, content: &str) -> String {
        match syntax {
            Syntax::Header => content.to_string(),
            Syntax::Declaration => content
                .blue()
                .to_string(),
            Syntax::Genus => content
                .black()
                .to_string(),
            Syntax::Description => content.to_string(),
            Syntax::StepItem => content
                .bold()
                .to_string(),
            Syntax::CodeBlock => content.to_string(),
            Syntax::Variable => content.to_string(),
            Syntax::Section => content.to_string(),
            Syntax::String => content
                .green()
                .to_string(),
            Syntax::Numeric => content.to_string(),
            Syntax::Syntax => content.to_string(),
        }
    }
}

pub struct Typst;

impl Render for Typst {
    fn render(&self, syntax: Syntax, content: &str) -> String {
        match syntax {
            Syntax::Header => todo!(),
            Syntax::Declaration => todo!(),
            Syntax::Description => todo!(),
            Syntax::Genus => todo!(),
            Syntax::StepItem => todo!(),
            Syntax::CodeBlock => todo!(),
            Syntax::Variable => todo!(),
            Syntax::Section => todo!(),
            Syntax::String => todo!(),
            Syntax::Numeric => todo!(),
            Syntax::Syntax => todo!(),
        }
    }
}
