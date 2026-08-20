//! The content of a ``` fence

use std::borrow::Cow;

#[derive(Debug, Eq, PartialEq)]
pub struct Multiline<'i> {
    pub language: Option<&'i str>,
    pub lines: Vec<&'i str>,
}

impl<'i> Multiline<'i> {
    /// The lines as they were present in the input source but with the leading block
    /// indentation removed.
    pub fn lines(&self) -> impl Iterator<Item = Cow<'i, str>> + '_ {
        let common = self
            .lines
            .iter()
            .filter(|line| {
                !line
                    .trim_ascii()
                    .is_empty()
            })
            .map(|line| indent(line))
            .min()
            .unwrap_or(0);

        self.lines
            .iter()
            .map(move |line| strip(expand(line), common))
    }

    /// The lines, joined, suitable for use by builtin functions.
    pub fn content(&self) -> String {
        self.lines()
            .collect::<Vec<Cow<'i, str>>>()
            .join("\n")
    }
}

fn indent(line: &str) -> usize {
    let mut column = 0;

    for c in line.chars() {
        match c {
            ' ' => column += 1,
            '\t' => column = (column / 4 + 1) * 4,
            _ => break,
        }
    }

    column
}

/// Expand any tabs present into 4 spaces. If the author needs an actual tab
/// character in a sting literal they can use the `\t` escape.
fn expand(line: &str) -> Cow<'_, str> {
    if !line.contains('\t') {
        return Cow::Borrowed(line);
    }

    let mut result = String::with_capacity(line.len() + 8);
    let mut column = 0;

    for c in line.chars() {
        if c == '\t' {
            let stop = (column / 4 + 1) * 4;
            result.push_str(&" ".repeat(stop - column));
            column = stop;
        } else {
            result.push(c);
            column += 1;
        }
    }

    result.into()
}

/// Strip leading block indendation from a line.
fn strip(line: Cow<'_, str>, common: usize) -> Cow<'_, str> {
    match line {
        Cow::Borrowed(text) => Cow::Borrowed(&text[common.min(text.len())..]),
        Cow::Owned(mut text) => {
            text.drain(..common.min(text.len()));
            Cow::Owned(text)
        }
    }
}
