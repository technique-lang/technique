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
        let column = j;

        let width = line
            .to_string()
            .len();
        let width = 3.max(width);

        format!(
            r#"
{}: {}:{}:{} {}

{:width$} {}
{:width$} {} {}
{:width$} {} {:>j$}

{}
            "#,
            "error".bright_red(),
            self.filename
                .to_string_lossy(),
            line,
            column,
            self.problem
                .bold(),
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

    pub fn concise_error(&self) -> String {
        let i = calculate_line_number(self.source, self.offset);
        let j = calculate_column_number(self.source, self.offset);

        let line = i + 1;
        let column = j;

        format!(
            "{}: {}:{}:{} {}",
            "error".bright_red(),
            self.filename
                .to_string_lossy(),
            line,
            column,
            self.problem
                .bold(),
        )
    }
}

// Concise version for internal use
impl<'i> fmt::Display for TechniqueError<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.concise_error())
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
        Some(start) => {
            // Count Unicode characters from the start of the line to the offset
            content[start + 1..offset]
                .chars()
                .count()
        }
        None => {
            // No newline found, count characters from the beginning
            before
                .chars()
                .count()
        }
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

    #[test]
    fn counting_columns_ascii() {
        let content = "This is a test";

        let col = calculate_column_number(content, 5);
        assert_eq!(col, 5); // After "This "
    }

    #[test]
    fn counting_columns_unicode() {
        // Test with Unicode characters like those in Three.t: "3.0 × 10⁸ m_s"
        let content = "3.0 × 10⁸ m_s";

        // At the underscore (× and ⁸ are multi-byte Unicode chars)
        let offset = "3.0 × 10⁸ m".len();
        let col = calculate_column_number(content, offset);
        assert_eq!(col, 11); // Should count 11 characters, not bytes
    }

    #[test]
    fn counting_columns_multiline() {
        let content = "First line\nSecond × line";

        // After "Second " on second line (× is multi-byte)
        let offset = "First line\n".len();
        let col = calculate_column_number(content, offset + 7);
        assert_eq!(col, 7);
    }
}

#[derive(Debug)]
pub struct LoadingError<'i> {
    pub problem: String,
    pub details: String,
    pub filename: &'i Path,
}

impl<'i> LoadingError<'i> {
    pub fn concise_details(&self) -> String {
        format!(
            "{}: {} {}{} {}",
            "error".bright_red(),
            self.filename
                .to_string_lossy(),
            self.problem
                .bright_white()
                .bold(),
            if self
                .details
                .is_empty()
            {
                ""
            } else {
                ":"
            },
            self.details
        )
        .to_string()
    }
}
