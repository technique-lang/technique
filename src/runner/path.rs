//! Fully-qualified path rendering for steps as the walker descends.

use crate::language;

/// One position in the structural path to a step. The runner pushes one
/// of these as it enters each containing scope, pops on exit. Strings
/// are borrowed from the source via the IR; the renderer materialises
/// an owned String only on demand.
#[allow(dead_code)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PathSegment<'i> {
    Section(&'i str),
    DependentStep(&'i str),
    ParallelStep(usize),
    Attributes(&'i [language::Attribute<'i>]),
    Procedure(&'i str),
}

/// Absolute path from the document root to the walker's current
/// position. The runner pushes a segment on scope entry, pops on exit;
/// `render()` produces the document-rooted form (the tail of the
/// fully-qualified identifier — URL, file path, and run id are added by
/// outer layers).
#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct QualifiedPath<'i> {
    segments: Vec<PathSegment<'i>>,
}

#[allow(dead_code)]
impl<'i> QualifiedPath<'i> {
    pub fn new() -> Self {
        QualifiedPath {
            segments: Vec::new(),
        }
    }

    pub fn push(&mut self, segment: PathSegment<'i>) {
        self.segments
            .push(segment);
    }

    pub fn pop(&mut self) -> Option<PathSegment<'i>> {
        self.segments
            .pop()
    }

    /// Render the current path as a PFFTT absolute path string,
    /// always rooted at `/`. A `Procedure` segment opens a fresh scope,
    /// so only segments after the last `Procedure` are shown; the
    /// procedure's name (with trailing `:`) joins immediately to the
    /// root. Other segments are `/`-joined after the procedure prefix.
    /// An attribute frame containing the `@*` reset role contributes
    /// nothing.
    pub fn render(&self) -> String {
        let mut prefix: Option<&str> = None;
        let mut start: usize = 0;
        for (i, segment) in self
            .segments
            .iter()
            .enumerate()
        {
            if let PathSegment::Procedure(name) = segment {
                prefix = Some(*name);
                start = i + 1;
            }
        }

        let pieces: Vec<String> = self.segments[start..]
            .iter()
            .filter_map(render_segment)
            .collect();

        let mut text = String::from("/");
        if let Some(name) = prefix {
            text.push_str(name);
            text.push(':');
        }
        text.push_str(&pieces.join("/"));
        text
    }
}

fn render_segment(segment: &PathSegment) -> Option<String> {
    match segment {
        PathSegment::Section(numeral) => Some(numeral.to_string()),
        PathSegment::DependentStep(ordinal) => Some(ordinal.to_string()),
        PathSegment::ParallelStep(idx) => Some(format!("-{}", idx)),
        PathSegment::Attributes(frame) => render_attributes(frame),
        PathSegment::Procedure(_) => None,
    }
}

fn render_attributes(frame: &[language::Attribute]) -> Option<String> {
    let mut text = String::new();
    for attr in frame {
        match attr {
            // `@*` resets the inherited role; it contributes no segment of
            // its own. A sibling Place attribute in the same frame is
            // unaffected and still renders.
            language::Attribute::Role(id, _) if id.value == "*" => {}
            language::Attribute::Role(id, _) => {
                text.push('@');
                text.push_str(id.value);
            }
            language::Attribute::Place(id, _) => {
                text.push('^');
                text.push_str(id.value);
            }
        }
    }
    if text.is_empty() {
        return None;
    }
    Some(text)
}

#[cfg(test)]
#[path = "checks/path.rs"]
mod check;
