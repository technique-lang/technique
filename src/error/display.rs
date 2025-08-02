use std::{fmt, path::Path};

use owo_colors::OwoColorize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TechniqueError<'i> {
    pub problem: String,
    pub details: String,
    pub filename: &'i Path,
    pub source: &'i str,
    pub offset: usize,
    pub width: Option<usize>,
}

// Verbose detailed explanation
impl<'i> TechniqueError<'i> {
    pub fn full_details(&self) -> String {
        let i = calculate_line_number(self.source, self.offset);
        let j = calculate_column_number(self.source, self.offset);

        let code = self
            .source
            .lines()
            .nth(i)
            .unwrap_or("?");

        let line = i + 1;
        let column = j + 1;

        let width = line
            .to_string()
            .len();
        let width = 3.max(width);

        format!(
            r#"
{}: {}
{}:{}:{}

{:width$} {}
{:width$} {} {}
{:width$} {} {:>j$}

{}
            "#,
            "error".bright_red(),
            self.problem
                .bold(),
            self.filename
                .to_string_lossy(),
            line,
            column,
            ' ',
            '|'.bright_blue(),
            line.bright_blue(),
            '|'.bright_blue(),
            code,
            ' ',
            '|'.bright_blue(),
            '^'.bright_red(),
            self.details
        )
        .trim_ascii()
        .to_string()
    }
}

// Concise version for internal use
impl<'i> fmt::Display for TechniqueError<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let i = calculate_line_number(self.source, self.offset);
        let j = calculate_column_number(self.source, self.offset);

        let line = i + 1;
        let column = j + 1;

        write!(
            f,
            "error: {}:{}:{} {}",
            self.filename
                .to_string_lossy(),
            line,
            column,
            self.problem
        )
    }
}

// This returns a zero-origin result so that it can subsequently be used for
// splitting; for display to humans you'll have to add 1.
fn calculate_line_number(content: &str, offset: usize) -> usize {
    content[..offset]
        .bytes()
        .filter(|&b| b == b'\n')
        .count()
}

// Calculate the column number, also zero-origin for consistency.
fn calculate_column_number(content: &str, offset: usize) -> usize {
    let before = &content[..offset];
    match before.rfind('\n') {
        Some(start) => offset - start,
        None => offset,
    }
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn counting_lines() {
        let content = "This is a test";

        let n = calculate_line_number(content, 5);
        assert_eq!(n + 1, 1);

        let content = r#"
This
is
a
test
            "#
        .trim_ascii();

        let n = calculate_line_number(content, 10);
        assert_eq!(n + 1, 4);

        let after = content
            .lines()
            .nth(n)
            .unwrap();
        assert_eq!(after, "test");
    }
}
