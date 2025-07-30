#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TechniqueError<'i> {
    pub problem: String,
    pub details: String,
    pub source: &'i str,
    pub offset: usize,
    pub width: Option<usize>,
}

use std::fmt;

impl<'i> TechniqueError<'i> {
    pub fn full_details(&self) -> String {
        let n = calculate_line_number(self.source, self.offset);

        let line = self
            .source
            .lines()
            .nth(n)
            .unwrap_or("<NOT FOUND>");

        format!("{}\n{}: {}\n\n{}", self.problem, n + 1, line, self.details)
    }
}

impl<'i> fmt::Display for TechniqueError<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let n = calculate_line_number(self.source, self.offset);

        let line = self
            .source
            .lines()
            .nth(n)
            .unwrap_or("<NOT FOUND>");

        write!(f, "{}\n{}: {}", self.problem, n + 1, line)
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
