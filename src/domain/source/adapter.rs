//! Projects the AST into the source domain model.
//!
//! This runs the code formatter to produce syntax-tagged fragments,
//! then wraps them as domain types for serialization.

use crate::domain::Adapter;
use crate::formatting::formatter::format_with_renderer;
use crate::language;

use super::types::{Document, Fragment};

const WIDTH: u8 = 70;

pub struct SourceAdapter;

impl Adapter for SourceAdapter {
    type Model = Document;

    fn extract(&self, document: &language::Document) -> Document {
        let fragments = format_with_renderer(document, WIDTH);

        let fragments: Vec<Fragment> = fragments
            .into_iter()
            .map(|(syntax, content)| Fragment {
                syntax: format!("{:?}", syntax),
                content: content.into_owned(),
            })
            .collect();

        Document {
            fragments: coalesce(fragments),
        }
    }
}

/// Merge adjacent fragments to reduce verbosity in serialized output.
/// Same-syntax fragments are concatenated. Whitespace-only fragments
/// tagged Neutral or Description are absorbed into the preceding
/// fragment, allowing subsequent same-syntax merges to collapse runs
/// of words into single strings.
fn coalesce(fragments: Vec<Fragment>) -> Vec<Fragment> {
    let mut result: Vec<Fragment> = Vec::with_capacity(fragments.len());

    for frag in fragments {
        if let Some(last) = result.last_mut() {
            if last.syntax == frag.syntax
                && frag.syntax != "Newline"
                && frag.syntax != "BlockBegin"
                && frag.syntax != "BlockEnd"
            {
                last.content.push_str(&frag.content);
                continue;
            }
            if is_text_whitespace(&frag) {
                last.content.push_str(&frag.content);
                continue;
            }
        }
        result.push(frag);
    }

    result
}

fn is_text_whitespace(frag: &Fragment) -> bool {
    (frag.syntax == "Neutral" || frag.syntax == "Description")
        && !frag.content.is_empty()
        && frag
            .content
            .bytes()
            .all(|b| b == b' ')
}

