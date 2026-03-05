//! Shared Typst markup primitives available for use by all template
//! renderers.
//!
//! This provides building blocks (headings, steps, roles, responses, etc)
//! that renderers can compose into complete output documents.
//!
//! Note that this is distinct from `rendering::typst` which renders Technique
//! in its original surface language syntax form; this module operates over
//! constructs made in any particular domain.

/// Escape special Typst characters in text content.
pub fn escape(text: &str) -> String {
    text.replace('\\', "\\\\")
        .replace('#', "\\#")
        .replace('$', "\\$")
        .replace('*', "\\*")
        .replace('_', "\\_")
        .replace('@', "\\@")
        .replace('<', "\\<")
        .replace('>', "\\>")
}

/// Standard page and text setup preamble.
pub fn preamble() -> String {
    "#set page(margin: 1.5cm)\n#set text(size: 10pt)\n\n".to_string()
}

/// Section heading.
pub fn heading(level: u8, text: &str) -> String {
    let markers = "=".repeat(level as usize);
    format!("{} {}\n\n", markers, escape(text))
}

/// Descriptive text paragraph.
pub fn description(indent: &str, text: &str) -> String {
    format!("{}{}\n\n", indent, escape(text))
}

/// Step with checkbox, optional ordinal, and text.
pub fn step(indent: &str, ordinal: Option<&str>, text: Option<&str>) -> String {
    let mut out = format!(
        "{}#box(stroke: 0.5pt, width: 0.8em, height: 0.8em) ",
        indent
    );
    if let Some(ord) = ordinal {
        out.push_str(&format!("*{}.*  ", ord));
    }
    if let Some(t) = text {
        out.push_str(&escape(t));
    }
    out.push_str("\n\n");
    out
}

/// Role attribution header.
pub fn role(indent: &str, name: &str) -> String {
    format!("{}#text(weight: \"bold\")[{}]\n\n", indent, name)
}

/// Response options with small checkboxes.
pub fn responses(indent: &str, options: &[String]) -> String {
    if options.is_empty() {
        return String::new();
    }
    let mut out = format!("{}", indent);
    for (i, option) in options
        .iter()
        .enumerate()
    {
        if i > 0 {
            out.push_str(" | ");
        }
        out.push_str(&format!(
            "#box(stroke: 0.5pt, width: 0.6em, height: 0.6em) _{}_",
            option
        ));
    }
    out.push_str("\n\n");
    out
}
