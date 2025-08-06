//! Renderers for colourizing Technique language

use crate::language::*;
use owo_colors::OwoColorize;
use std::borrow::Cow;

/// Types of content that can be rendered with different styles
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Syntax {
    Neutral, // default
    Header,
    Declaration,
    Description,
    Forma,
    StepItem,
    CodeBlock,
    Variable,
    Section,
    String,
    Numeric,
    Response,
    Invocation,
    Title,
    Keyword,
    Function,
    Multiline,
    Label,
    Operator,
    Quote,
    Language,
    Attribute,
    Structure,
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
            Syntax::Header => content
                .color(owo_colors::Rgb(0x75, 0x50, 0x7b))
                .to_string(),
            Syntax::Declaration => content // entity.name.function - #3465a4 (blue) bold
                .color(owo_colors::Rgb(0x34, 0x65, 0xa4))
                .bold()
                .to_string(),
            Syntax::Forma => content // entity.name.type.technique - #8f5902 (brown) bold
                .color(owo_colors::Rgb(0x8f, 0x59, 0x02))
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
                .color(owo_colors::Rgb(0x72, 0x9f, 0xcf))
                .bold()
                .to_string(),
            Syntax::Section => content // markup.heading.technique
                .to_string(),
            Syntax::String => content // string - #4e9a06 (green) bold
                .color(owo_colors::Rgb(0x4e, 0x9a, 0x06))
                .bold()
                .to_string(),
            Syntax::Numeric => content // constant.numeric - #ad7fa8 (purple) bold
                .color(owo_colors::Rgb(0xad, 0x7f, 0xa8))
                .bold()
                .to_string(),
            Syntax::Response => content // string.quoted.single.technique
                .color(owo_colors::Rgb(0xf5, 0x79, 0x00))
                .bold()
                .to_string(),
            Syntax::Invocation => content // meta.function-call.technique
                .color(owo_colors::Rgb(0x3b, 0x5d, 0x7d))
                .bold()
                .to_string(),
            Syntax::Title => content // markup.heading.technique - #000000 bold
                .bright_white()
                .bold()
                .to_string(),
            Syntax::Keyword => content // keyword.control.technique
                .color(owo_colors::Rgb(0x75, 0x50, 0x7b))
                .bold()
                .to_string(),
            Syntax::Function => content // entity.name.function.technique - #3465a4 (blue) bold
                .color(owo_colors::Rgb(52, 101, 164))
                .bold()
                .to_string(),
            Syntax::Multiline => content // string.multiline.technique - #4e9a06 (green)
                .color(owo_colors::Rgb(0x4e, 0x9a, 0x06))
                .bold()
                .to_string(),
            Syntax::Label => content // variable.other.tablet
                .color(owo_colors::Rgb(0x60, 0x98, 0x9a))
                .bold()
                .to_string(),
            Syntax::Operator => content // keyword.operator.technique - #cc0000 (red) bold
                .color(owo_colors::Rgb(204, 0, 0))
                .bold()
                .to_string(),
            Syntax::Quote => content // punctuation.technique - #999999 (grey)
                .color(owo_colors::Rgb(0x99, 0x99, 0x99))
                .bold()
                .to_string(),
            Syntax::Language => content // storage.type.embedded
                .color(owo_colors::Rgb(0xc4, 0xa0, 0x00))
                .bold()
                .to_string(),
            Syntax::Attribute => content // entity.name.tag.attribute
                .bright_white()
                .bold()
                .to_string(),
            Syntax::Structure => content
                .color(owo_colors::Rgb(153, 153, 153))
                .bold()
                .to_string(),
        }
    }
}

/// Add markup around syntactic elements for use when including
/// Technique source in Typst documents.
pub struct Typst;

impl Render for Typst {
    fn render(&self, syntax: Syntax, content: &str) -> String {
        let content = escape_typst(content);
        match syntax {
            Syntax::Neutral => markup("", &content),
            Syntax::Header => markup("fill: rgb(0x75, 0x50, 0x7b)", &content),
            Syntax::Declaration => {
                markup("fill: rgb(0x34, 0x65, 0xa4), weight: \"bold\"", &content)
            }
            Syntax::Description => markup("", &content),
            Syntax::Forma => markup("fill: rgb(0x8f, 0x59, 0x02), weight: \"bold\"", &content),
            Syntax::StepItem => markup("weight: \"bold\"", &content),
            Syntax::CodeBlock => markup("fill: rgb(0x99, 0x99, 0x99), weight: \"bold\"", &content),
            Syntax::Variable => markup("fill: rgb(0x72, 0x9f, 0xcf), weight: \"bold\"", &content),
            Syntax::Section => markup("", &content),
            Syntax::String => markup("fill: rgb(0x4e, 0x9a, 0x06), weight: \"bold\"", &content),
            Syntax::Numeric => markup("fill: rgb(0xad, 0x7f, 0xa8), weight: \"bold\"", &content),
            Syntax::Response => markup("fill: rgb(0xf5, 0x79, 0x00), weight: \"bold\"", &content),
            Syntax::Invocation => markup("fill: rgb(0x3b, 0x5d, 0x7d), weight: \"bold\"", &content),
            Syntax::Title => markup("weight: \"bold\"", &content),
            Syntax::Keyword => markup("fill: rgb(0x75, 0x50, 0x7b), weight: \"bold\"", &content),
            Syntax::Function => markup("fill: rgb(0x34, 0x65, 0xa4), weight: \"bold\"", &content),
            Syntax::Multiline => markup("fill: rgb(0x4e, 0x9a, 0x06), weight: \"bold\"", &content),
            Syntax::Label => markup("fill: rgb(0x60, 0x98, 0x9a), weight: \"bold\"", &content),
            Syntax::Operator => markup("fill: red", &content),
            Syntax::Quote => markup("fill: rgb(0x99, 0x99, 0x99), weight: \"bold\"", &content),
            Syntax::Language => markup("fill: rgb(0xc4, 0xa0, 0x00), weight: \"bold\"", &content),
            Syntax::Attribute => markup("weight: \"bold\"", &content),
            Syntax::Structure => markup("fill: rgb(0x99, 0x99, 0x99), weight: \"bold\"", &content),
        }
    }
}

fn escape_typst(content: &str) -> Cow<str> {
    if content.contains('"') {
        Cow::Owned(content.replace("\"", "\\\""))
    } else {
        Cow::Borrowed(content)
    }
}

fn markup(prefix: &str, content: &Cow<str>) -> String {
    let mut result = String::with_capacity(6 + prefix.len() + 2 + 5 + content.len() + 3);
    result.push_str("#text(");
    if prefix.len() > 0 {
        result.push_str(prefix);
        result.push_str(", ");
    }
    result.push_str("raw(\"");
    result.push_str(content);
    result.push_str("\"))");
    result
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escape_typst_no_allocation_when_no_quotes() {
        let input = "hello world";
        let result = escape_typst(input);

        // Should return borrowed reference when no quotes to escape
        assert!(matches!(result, Cow::Borrowed(_)));
        assert_eq!(result, "hello world");
    }

    #[test]
    fn escape_typst_allocates_when_quotes_present() {
        let input = "hello \"world\"";
        let result = escape_typst(input);

        // Should return owned string when quotes need escaping
        assert!(matches!(result, Cow::Owned(_)));
        assert_eq!(result, "hello \\\"world\\\"");
    }

    #[test]
    fn build_typst_markup_efficiently() {
        // Test that build_typst_markup works correctly
        let content = Cow::Borrowed("test content");
        let result = markup("color: red", &content);
        assert_eq!(result, "#text(color: red, raw(\"test content\"))");

        // Test with escaped content
        let content = Cow::Owned("escaped \"content\"".to_string());
        let result = markup("", &content);
        assert_eq!(result, "#text(raw(\"escaped \"content\"\"))");
    }
}
