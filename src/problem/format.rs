use super::messages::generate_error_message;
use owo_colors::OwoColorize;
use std::path::Path;
use technique::{formatting::Render, language::LoadingError, parsing::ParsingError};

/// Format a parsing error with full details including source code context
pub fn full_parsing_error<'i>(
    error: &ParsingError,
    filename: &'i Path,
    source: &'i str,
    renderer: &impl Render,
) -> String {
    let (problem, details) = generate_error_message(error, renderer);
    let offset = error.offset();

    let i = calculate_line_number(source, offset);
    let j = calculate_column_number(source, offset);

    let code = source
        .lines()
        .nth(i)
        .unwrap_or("?");
    let line = i + 1;
    let column = j + 1;
    let width = 3.max(
        line.to_string()
            .len(),
    );

    format!(
        r#"
{}: {}:{}:{} {}

{:width$} {}
{:width$} {} {}
{:width$} {} {:>column$}

{}
        "#,
        "error".bright_red(),
        filename.to_string_lossy(),
        line,
        column,
        problem.bold(),
        ' ',
        '|'.bright_blue(),
        line.bright_blue(),
        '|'.bright_blue(),
        code,
        ' ',
        '|'.bright_blue(),
        '^'.bright_red(),
        details
    )
    .trim_ascii()
    .to_string()
}

/// Format a parsing error with concise single-line output
pub fn concise_parsing_error<'i>(
    error: &ParsingError,
    filename: &'i Path,
    source: &'i str,
    renderer: &impl Render,
) -> String {
    let (problem, _) = generate_error_message(error, renderer);
    let offset = error.offset();
    let i = calculate_line_number(source, offset);
    let j = calculate_column_number(source, offset);
    let line = i + 1;
    let column = j + 1;

    format!(
        "{}: {}:{}:{} {}",
        "error".bright_red(),
        filename.to_string_lossy(),
        line,
        column,
        problem.bold(),
    )
}

/// Format a LoadingError with concise single-line output
pub fn concise_loading_error<'i>(error: &LoadingError<'i>) -> String {
    format!(
        "{}: {}:{}",
        "error".bright_red(),
        error
            .filename
            .display(),
        error
            .problem
            .bold()
    )
}

// Helper functions for line/column calculation
fn calculate_line_number(content: &str, offset: usize) -> usize {
    content[..offset]
        .bytes()
        .filter(|&b| b == b'\n')
        .count()
}

fn calculate_column_number(content: &str, offset: usize) -> usize {
    let before = &content[..offset];
    match before.rfind('\n') {
        Some(start) => content[start + 1..offset]
            .chars()
            .count(),
        None => before
            .chars()
            .count(),
    }
}
