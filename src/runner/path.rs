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
    Prologue,
    DependentStep(&'i str),
    ParallelStep(usize),
    Iteration(usize),
    Attributes(&'i [language::Attribute<'i>]),
    Procedure(&'i str),
    External(&'i str),
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

    /// Swap in a fresh set of segments, returning the displaced ones.
    pub fn replace(&mut self, segments: Vec<PathSegment<'i>>) -> Vec<PathSegment<'i>> {
        std::mem::replace(&mut self.segments, segments)
    }

    /// Document indentation level of the current step: the count of nested
    /// step scopes on the path — dependent and parallel steps and rendered
    /// attribute frames. Iteration, section, procedure, and external segments
    /// carry no visual indent, matching the formatter's nesting.
    pub fn depth(&self) -> usize {
        self.segments
            .iter()
            .filter(|segment| match segment {
                PathSegment::DependentStep(_) | PathSegment::ParallelStep(_) => true,
                PathSegment::Attributes(frame) => render_attributes(frame).is_some(),
                _ => false,
            })
            .count()
    }

    /// Render the current path as a PFFTT absolute path string, always
    /// rooted at `/`. The full enclosing hierarchy is shown: every scope
    /// level is its own `/`-delimited component, with a `Procedure`
    /// segment suffixed by `:`. An attribute frame containing the `@*`
    /// reset role contributes nothing.
    ///
    /// A `foreach` or `repeat` keyword creates a scope as well. When
    /// iterating it is recorded using a path segment of the form `[n]`,
    /// one-origin. Thus `/5/[2]/a` would be the first substep within the
    /// second iteration of a scope within the 5th step of a Technique.
    pub fn render(&self) -> String {
        render_path(&self.segments)
    }
}

/// Trim a rendered PFFTT path to the live-prompt form: drop the leading `/`,
/// and the entry procedure's `name:` head when a section immediately follows.
pub fn display_path(qualified: &str) -> String {
    let body = qualified
        .strip_prefix('/')
        .unwrap_or(qualified);
    let mut parts: Vec<&str> = body
        .split('/')
        .collect();
    if parts.len() >= 2 && parts[0].ends_with(':') && is_section_component(parts[1]) {
        parts.remove(0);
    }
    parts.join("/")
}

/// Leading token, not the whole component: an acquire label can glue an
/// ` <invocation>` annotation after the numeral.
fn is_section_component(part: &str) -> bool {
    let token = part
        .split_whitespace()
        .next()
        .unwrap_or("");
    !token.is_empty()
        && token
            .bytes()
            .all(|b| b"IVXLCDM".contains(&b))
}

/// Render a segment slice as a PFFTT absolute path, always rooted at `/`.
pub fn render_path(segments: &[PathSegment]) -> String {
    let pieces: Vec<String> = segments
        .iter()
        .filter_map(render_segment)
        .collect();

    let mut text = String::from("/");
    text.push_str(&pieces.join("/"));
    text
}

fn render_segment(segment: &PathSegment) -> Option<String> {
    match segment {
        PathSegment::Section(numeral) => Some(numeral.to_string()),
        PathSegment::Prologue => Some("0".to_string()),
        PathSegment::DependentStep(ordinal) => Some(ordinal.to_string()),
        PathSegment::ParallelStep(index) => Some(format!("-{}", index)),
        PathSegment::Iteration(number) => Some(format!("[{}]", number)), // you can't "index" into it!
        PathSegment::Attributes(frame) => render_attributes(frame),
        PathSegment::Procedure(name) => Some(format!("{}:", name)),
        // An external invocation target, addressed by its source `<uri>` form.
        PathSegment::External(uri) => Some(format!("<{}>", uri)),
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
