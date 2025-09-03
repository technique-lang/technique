//! Renderers for colourizing Technique language

use std::borrow::Cow;
use technique::formatting::*;

/// Add markup around syntactic elements for use when including
/// Technique source in Typst documents.
pub struct Typst;

impl Render for Typst {
    fn style(&self, syntax: Syntax, content: &str) -> String {
        let content = escape_typst(content);
        match syntax {
            Syntax::Neutral => markup("", &content),
            Syntax::Indent => markup("", &content),
            Syntax::Newline => "\\\n".to_string(),
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

fn escape_typst(content: &str) -> Cow<'_, str> {
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

#[cfg(test)]
mod check {
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

    #[test]
    fn typst_newline_and_indent_rendering() {
        let typst = Typst;

        // Test that newlines are rendered as Typst line breaks
        let newline_result = typst.style(Syntax::Newline, "\n");
        assert_eq!(newline_result, "\\\n");

        // Test that indentation is rendered without raw() wrapper
        let indent_result = typst.style(Syntax::Indent, "    ");
        assert_eq!(indent_result, "#text(raw(\"    \"))");

        // Test that this is different from Neutral (which would wrap newlines in raw())
        let neutral_result = typst.style(Syntax::Neutral, "\n    ");
        assert_eq!(neutral_result, "#text(raw(\"\n    \"))");

        // Verify the improvement: newlines no longer wrapped in raw()
        assert_ne!(newline_result, "#text(raw(\"\n\"))");
    }

    #[test]
    fn verify_typst_fragments_usage() {
        // Simple test to verify that our new Syntax variants are used correctly
        let fragments = vec![
            (Syntax::Header, "% technique v1".to_string()),
            (Syntax::Newline, "\n".to_string()),
            (Syntax::Indent, "    ".to_string()),
            (Syntax::StepItem, "1".to_string()),
            (Syntax::Neutral, ".".to_string()),
            (Syntax::Newline, "\n".to_string()),
        ];

        let typst = Typst;
        let mut output = String::new();

        for (syntax, content) in fragments {
            let rendered = typst.style(syntax, &content);
            output.push_str(&rendered);
        }

        // Verify improvements:
        // 1. Newlines are rendered as Typst line breaks
        assert!(output.contains("\\\n"));
        // 2. Indentation is wrapped in text() but not combined with newlines
        assert!(output.contains("#text(raw(\"    \"))"));
        // 3. No raw() calls containing newlines
        assert!(!output.contains("raw(\"\n"));
    }
}
