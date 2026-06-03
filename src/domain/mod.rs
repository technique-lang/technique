//! Domain projections of Technique documents.
//!
//! A domain projection takes the parser's AST and transforms it into a
//! model suited to a particular kind of output. The **checklist** domain
//! flattens procedures into printable checklists; the **procedure** domain
//! preserves the full hierarchy; others are forthcoming.
//!
//! The **engine** module provides convenient accessors into the parser's AST
//! types so that adapters can extract content without needing to understand
//! the nuances of correclty matching directly on the internals types.

mod adapter;
pub mod checklist;
pub mod engine;
pub mod nasa_esa_iss;
pub mod procedure;
pub mod recipe;
pub(crate) mod serialize;
pub mod source;

pub use adapter::Adapter;
pub use checklist::Checklist;
pub use nasa_esa_iss::NasaEsaIss;
pub use procedure::Procedure;
pub use recipe::Recipe;
pub use source::Source;

use crate::runner::Builtin;

/// The runtime facet of a domain: the domain-specific host functions it
/// contributes to the interpreter's function table, on top of `Library::core`
/// and the `Library::system` layer. Orthogonal to a domain's rendering
/// projection (the `Template` trait).
pub trait Domain {
    fn functions(&self) -> Vec<Builtin> {
        Vec::new()
    }
}

/// Select a domain by name, for both the renderer and the runtime.
/// `None` if the name matches no known domain — the caller reports that as an
/// error rather than silently substituting a default.
pub fn domain_for(name: &str) -> Option<&'static dyn Domain> {
    match name {
        "checklist" => Some(&Checklist),
        "nasa-esa-iss" => Some(&NasaEsaIss),
        "procedure" => Some(&Procedure),
        "recipe" => Some(&Recipe),
        "source" => Some(&Source),
        _ => None,
    }
}
