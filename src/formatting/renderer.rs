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

/// We do the code formatting in two passes. First we convert from our
/// Abstract Syntax Tree types into a Vec of "fragments" (Syntax tag, String
/// pairs). Then second we apply the specified renderer to each pair to result
/// in an embellished/highlighted/marked-up String.
pub fn render(renderer: &impl Render, technique: &Document, width: u8) -> String {
    // Pass 1: Format AST to tagged fragments
    let fragments = format_to_fragments(technique, width);

    // Pass 2: Render tagged fragments to final output
    let result = render_to_string(renderer, fragments);

    result
}

/// Pass 1: Convert AST to semantic fragments.
fn format_to_fragments(technique: &Document, width: u8) -> Vec<(Syntax, String)> {
    // Use the existing formatter.rs logic but collect fragments instead
    crate::formatting::formatter::format_with_renderer(technique, width)
}

/// Pass 2: apply markup to fragments via render() and combine.
fn render_to_string(renderer: &impl Render, fragments: Vec<(Syntax, String)>) -> String {
    let mut output = String::new();

    for (syntax, content) in fragments {
        let rendered = renderer.render(syntax, &content);
        output.push_str(&rendered);
    }

    if !output.is_empty() && !output.ends_with('\n') {
        output.push('\n');
    }

    output
}
