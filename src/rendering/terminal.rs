//! Renderers for colourizing Technique language

use owo_colors::OwoColorize;
use crate::formatting::*;

/// Embellish fragments with ANSI escapes to create syntax highlighting in
/// terminal output.
pub struct Terminal;

impl Render for Terminal {
    fn style(&self, syntax: Syntax, content: &str) -> String {
        match syntax {
            Syntax::Neutral => content.to_string(),
            Syntax::Indent => content.to_string(),
            Syntax::Newline => "\n".to_string(),
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

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn basic_handling() {
        let result = Render::style(&Terminal, Syntax::Neutral, "hello world");
        assert_eq!(result, "hello world");
    }
}
