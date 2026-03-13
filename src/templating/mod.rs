//! Render Technique documents into formatted output.
//!
//! The **Template** trait provides `markup()` for Typst function-call
//! output. Each domain template composes an adapter from `crate::domain`
//! internally.

mod checklist;
mod procedure;
mod source;
mod template;

pub use checklist::Checklist;
pub use procedure::Procedure;
pub use source::Source;
pub use template::Template;

/// Assemble a complete, compilable Typst document from domain template
/// imports, optional user template, and rendered markup.
/// 
/// This second import line is the critical aspect of the ability of a user to
/// customize the output template. Because the import is * any functions that
/// the user redefines in their template will overrides the names from the
/// default.
pub fn assemble(domain: &str, markup: &str, custom: Option<&str>) -> String {
    let mut doc = format!("#import \".{}.typ\": *\n", domain);
    if let Some(path) = custom {
        doc.push_str(&format!("#import \"/{}\": *\n", path));
    }
    doc.push_str("\n#show: template\n\n");
    doc.push_str(markup);
    doc
}
