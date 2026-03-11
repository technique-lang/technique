//! Typst rendering primitives for use by template renderers.
//!
//! These building blocks (headings, steps, roles, responses, etc) are
//! composed by renderers into complete Typst markup for PDF output.
//!
//! Note that this is distinct from `highlighting::typst` which renders
//! Technique in its original surface language syntax form; this module
//! operates over constructs made in any particular domain.

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
pub fn description(text: &str) -> String {
    format!("{}\n\n", escape(text))
}

/// Step with checkbox, optional ordinal, and text.
pub fn step(ordinal: Option<&str>, text: Option<&str>) -> String {
    let mut out = "#box(stroke: 0.5pt, width: 0.8em, height: 0.8em) ".to_string();
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
pub fn role(name: &str) -> String {
    format!("#text(weight: \"bold\")[{}]\n\n", name)
}

/// Response options with small checkboxes.
pub fn responses(options: &[String]) -> String {
    if options.is_empty() {
        return String::new();
    }
    let mut out = String::new();
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
