//! Renderers for colourizing Technique language

/// Types of content that can be rendered with different styles
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Syntax {
    Neutral, // default
    Indent,
    Newline,
    Header,
    Declaration,
    Description,
    Forma,
    StepItem,
    CodeBlock,
    Variable,
    Hole,
    Unit,
    Section,
    String,
    Numeric,
    Cost,
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
    Marker,
    Done,
    Skip,
    Fail,
    BlockBegin,
    BlockEnd,
}

/// Trait for different rendering backends (the no-op no-markup one, ANSI
/// escapes for terminal colouring, Typst markup for documents)
pub trait Render {
    /// Apply styling to content with the specified syntax type
    fn style(&self, content_type: Syntax, content: &str) -> String;
}

/// Returns content unchanged, with no markup applied
pub struct Identity;

impl Render for Identity {
    fn style(&self, syntax: Syntax, content: &str) -> String {
        match syntax {
            Syntax::BlockBegin | Syntax::BlockEnd => String::new(),
            _ => content.to_string(),
        }
    }
}
