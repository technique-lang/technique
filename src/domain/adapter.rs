//! Adapter trait for domain projections.

use crate::language;

/// Adapters project the AST into a domain-specific model. Each domain
/// defines its own model types (e.g. checklist::Document,
/// procedure::Document) reflecting how that domain thinks about the elements
/// of procedures as encoded in Technique.
pub trait Adapter {
    type Model;
    fn extract(&self, document: &language::Document) -> Self::Model;
}
