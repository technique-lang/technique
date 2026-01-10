//! Renderers for colourizing Technique language

use crate::language::*;
use crate::formatting::*;

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
fn format_to_fragments<'i>(
    technique: &'i Document,
    width: u8,
) -> Vec<(Syntax, std::borrow::Cow<'i, str>)> {
    // Use the existing formatter.rs logic but collect fragments instead
    crate::formatting::formatter::format_with_renderer(technique, width)
}

/// Pass 2: apply markup to fragments via style() and combine.
fn render_to_string<'i>(
    renderer: &impl Render,
    fragments: Vec<(Syntax, std::borrow::Cow<'i, str>)>,
) -> String {
    let mut output = String::new();

    for (syntax, content) in fragments {
        let rendered = renderer.style(syntax, &content);
        output.push_str(&rendered);
    }

    if !output.is_empty() && !output.ends_with('\n') {
        output.push('\n');
    }

    output
}
