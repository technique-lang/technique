use super::messages::{
    generate_error_message, generate_linking_error, generate_runner_error,
    generate_translation_error,
};
use owo_colors::OwoColorize;
use std::path::Path;
use technique::{
    formatting::Render, language::LoadingError, linking::LinkingError, parsing::ParsingError,
    runner::RunnerError, translation::TranslationError,
};

/// Render an error with full source context: a header line, the offending
/// source line, a caret underline of the given width, and the detail text.
fn full_error(
    problem: String,
    details: String,
    filename: &Path,
    source: &str,
    offset: usize,
    width: usize,
) -> String {
    let input = generate_filename(filename);

    let i = calculate_line_number(source, offset);
    let j = calculate_column_number(source, offset);

    let code = source
        .lines()
        .nth(i)
        .unwrap_or("?");
    let line = i + 1;
    let column = j + 1;
    let indent = 3.max(
        line.to_string()
            .len(),
    );

    // Create underline string based on error width
    let spacer = " ".repeat(j);
    let width = if width > 0 { width } else { 1 };
    let underline = "^".repeat(width);

    format!(
        r#"
{}: {}:{}:{} {}

{:indent$} {}
{:indent$} {} {}
{:indent$} {} {}{}

{}
        "#,
        "error".bright_red(),
        input,
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
        spacer,
        underline.bright_red(),
        details
    )
    .trim_ascii()
    .to_string()
}

/// Format a parsing error with full details including source code context
pub fn full_parsing_error<'i>(
    error: &ParsingError,
    filename: &'i Path,
    source: &'i str,
    renderer: &impl Render,
) -> String {
    let (problem, details) = generate_error_message(error, renderer);
    full_error(
        problem,
        details,
        filename,
        source,
        error.offset(),
        error.width(),
    )
}

/// Format a translation error with full details including source code context
pub fn full_translation_error<'i>(
    error: &TranslationError<'i>,
    filename: &'i Path,
    source: &'i str,
    renderer: &impl Render,
) -> String {
    let (problem, details) = generate_translation_error(error, renderer);
    let span = error.span();
    full_error(problem, details, filename, source, span.offset, span.length)
}

/// Format a linking error with full details including source code context
pub fn full_linking_error<'i>(
    error: &LinkingError<'i>,
    filename: &'i Path,
    source: &'i str,
    renderer: &impl Render,
) -> String {
    let (problem, details) = generate_linking_error(error, renderer);
    let span = error.span();
    full_error(problem, details, filename, source, span.offset, span.length)
}

/// Format a parsing error with concise single-line output
pub fn concise_parsing_error<'i>(
    error: &ParsingError,
    filename: &'i Path,
    source: &'i str,
    renderer: &impl Render,
) -> String {
    let (problem, _) = generate_error_message(error, renderer);
    let input = generate_filename(filename);
    let offset = error.offset();
    let i = calculate_line_number(source, offset);
    let j = calculate_column_number(source, offset);
    let line = i + 1;
    let column = j + 1;

    format!(
        "{}: {}:{}:{} {}",
        "error".bright_red(),
        input,
        line,
        column,
        problem.bold(),
    )
}

/// Format a translation error with concise single-line output.
pub fn concise_translation_error<'i>(
    error: &TranslationError<'i>,
    filename: &'i Path,
    source: &'i str,
    renderer: &impl Render,
) -> String {
    let (problem, _) = generate_translation_error(error, renderer);
    let input = generate_filename(filename);
    let offset = error
        .span()
        .offset;
    let i = calculate_line_number(source, offset);
    let j = calculate_column_number(source, offset);
    let line = i + 1;
    let column = j + 1;

    format!(
        "{}: {}:{}:{} {}",
        "error".bright_red(),
        input,
        line,
        column,
        problem.bold(),
    )
}

/// Format a linking error with concise single-line output.
pub fn concise_linking_error<'i>(
    error: &LinkingError<'i>,
    filename: &'i Path,
    source: &'i str,
    renderer: &impl Render,
) -> String {
    let (problem, _) = generate_linking_error(error, renderer);
    let input = generate_filename(filename);
    let offset = error
        .span()
        .offset;
    let i = calculate_line_number(source, offset);
    let j = calculate_column_number(source, offset);
    let line = i + 1;
    let column = j + 1;

    format!(
        "{}: {}:{}:{} {}",
        "error".bright_red(),
        input,
        line,
        column,
        problem.bold(),
    )
}

/// Format a runner error with concise single-line output.
pub fn concise_runner_error(error: &RunnerError, renderer: &impl Render) -> String {
    let (problem, _) = generate_runner_error(error, renderer);
    format!("{}: {}", "error".bright_red(), problem.bold())
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

fn generate_filename(filename: &Path) -> String {
    if filename.to_str() == Some("-") {
        "<stdin>".to_string()
    } else {
        filename
            .display()
            .to_string()
    }
}

// Helper functions for line/column calculation
pub fn calculate_line_number(content: &str, offset: usize) -> usize {
    content[..offset]
        .bytes()
        .filter(|&b| b == b'\n')
        .count()
}

pub fn calculate_column_number(content: &str, offset: usize) -> usize {
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
