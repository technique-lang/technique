//! Renderers for colourizing Technique language

use crate::language::*;
use owo_colors::OwoColorize;

/// Types of content that can be rendered with different styles
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Syntax {
    Neutral, // default
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
    Response,
    Invocation,
    Title,
    Function,
    Multiline,
    Label,
    Operator,
    Punctuation,
    Syntax,
}

/// Trait for different rendering backends (the no-op no-markup one, ANSI
/// escapes for terminal colouring, Typst markup for documents)
pub trait Render {
    /// Render content with the specified type/style
    fn render(&self, content_type: Syntax, content: &str) -> String;
}

/// Returns content unchanged, with no markup applied
pub struct Identity;

impl Render for Identity {
    fn render(&self, _syntax: Syntax, content: &str) -> String {
        content.to_string()
    }
}

/// Embellish fragments with ANSI escapes to create syntax highlighting in
/// terminal output.
pub struct Terminal;

impl Render for Terminal {
    fn render(&self, syntax: Syntax, content: &str) -> String {
        match syntax {
            Syntax::Neutral => content.to_string(),
            Syntax::Header => content.to_string(),
            Syntax::Declaration => content // entity.name.function - #3465a4 (blue) bold
                .color(owo_colors::Rgb(52, 101, 164))
                .bold()
                .to_string(),
            Syntax::Genus => content // entity.name.type.technique - #8f5902 (brown) bold
                .color(owo_colors::Rgb(143, 89, 2))
                .bold()
                .to_string(),
            Syntax::Description => content.to_string(),
            Syntax::StepItem => content // markup.list.numbered/unnumbered - #000000 bold
                .bright_white()
                .bold()
                .to_string(),
            Syntax::CodeBlock => content // punctuation.section.braces - #999999 bold
                .color(owo_colors::Rgb(153, 153, 153))
                .bold()
                .to_string(),
            Syntax::Variable => content // variable.parameter.technique - #729fcf (light blue) bold
                .color(owo_colors::Rgb(114, 159, 207))
                .bold()
                .to_string(),
            Syntax::Section => content // markup.heading.technique - #000000 bold
                .black()
                .bold()
                .to_string(),
            Syntax::String => content // string - #4e9a06 (green) bold
                .color(owo_colors::Rgb(78, 154, 6))
                .bold()
                .to_string(),
            Syntax::Numeric => content // constant.numeric - #ad7fa8 (purple) bold
                .color(owo_colors::Rgb(173, 127, 168))
                .bold()
                .to_string(),
            Syntax::Response => content // string.quoted.single.technique - #4e9a06 (green) bold
                .color(owo_colors::Rgb(78, 154, 6))
                .bold()
                .to_string(),
            Syntax::Invocation => content // meta.function-call.technique - #3465a4 (blue) bold
                .color(owo_colors::Rgb(52, 101, 164))
                .bold()
                .to_string(),
            Syntax::Title => content // markup.heading.technique - #000000 bold
                .black()
                .bold()
                .to_string(),
            Syntax::Function => content // entity.name.function.technique - #3465a4 (blue) bold
                .color(owo_colors::Rgb(52, 101, 164))
                .bold()
                .to_string(),
            Syntax::Multiline => content // string.multiline.technique - #4e9a06 (green)
                .color(owo_colors::Rgb(78, 154, 6))
                .to_string(),
            Syntax::Label => content // entity.name.tag.technique - #ad7fa8 (purple) bold
                .color(owo_colors::Rgb(173, 127, 168))
                .bold()
                .to_string(),
            Syntax::Operator => content // keyword.operator.technique - #cc0000 (red) bold
                .color(owo_colors::Rgb(204, 0, 0))
                .bold()
                .to_string(),
            Syntax::Punctuation => content // punctuation.technique - #999999 (grey)
                .color(owo_colors::Rgb(153, 153, 153))
                .to_string(),
            Syntax::Syntax => content.to_string(),
        }
    }
}

/// Add markup around syntactic elements for use when including
/// Technique source in Typst documents.
pub struct Typst;

impl Render for Typst {
    fn render(&self, syntax: Syntax, content: &str) -> String {
        match syntax {
            Syntax::Neutral => content.to_string(),
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
            Syntax::Response => todo!(),
            Syntax::Invocation => todo!(),
            Syntax::Title => todo!(),
            Syntax::Function => todo!(),
            Syntax::Multiline => todo!(),
            Syntax::Label => todo!(),
            Syntax::Operator => todo!(),
            Syntax::Punctuation => todo!(),
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
