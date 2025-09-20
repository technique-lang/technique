use super::*;
use std::path::Path;

/// Helper function to check if parsing produces the expected error
fn expect_error(content: &str, expected: ParsingError) {
    let result = parse_with_recovery(Path::new("test.tq"), content);
    match result {
        Ok(_) => panic!(
            "Expected parsing to fail, but it succeeded for input: {}",
            content
        ),
        Err(errors) => {
            // Check if any error exactly matches the expected error
            let found_expected = errors.contains(&expected);

            if !found_expected {
                panic!(
                    "Expected error {:?} but got: {:?} for input '{}'",
                    expected, errors, content
                );
            }
        }
    }
}

#[test]
fn invalid_identifier_uppercase_start() {
    expect_error(
        r#"
Making_Coffee : Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidIdentifier(0, 13, "Making_Coffee".to_string()),
    );
}

#[test]
fn invalid_identifier_mixed_case() {
    expect_error(
        r#"
makeCoffee : Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidIdentifier(0, 10, "makeCoffee".to_string()),
    );
}

#[test]
fn invalid_identifier_with_dashes() {
    expect_error(
        r#"
make-coffee : Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidIdentifier(0, 11, "make-coffee".to_string()),
    );
}

#[test]
fn invalid_identifier_with_spaces() {
    expect_error(
        r#"
make coffee : Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidParameters(5, 6),
    );
}

#[test]
fn invalid_signature_wrong_arrow() {
    expect_error(
        r#"
making_coffee : Ingredients => Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidSignature(28, 0),
    );
}

#[test]
fn invalid_genus_lowercase_forma() {
    expect_error(
        r#"
making_coffee : ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidGenus(16, 11),
    );
}

#[test]
fn invalid_genus_both_lowercase() {
    expect_error(
        r#"
making_coffee : ingredients -> coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidGenus(16, 11),
    );
}

#[test]
fn invalid_signature_missing_arrow() {
    expect_error(
        r#"
making_coffee : Ingredients Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidSignature(28, 0),
    );
}

#[test]
fn invalid_declaration_missing_colon() {
    expect_error(
        r#"
making_coffee Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::Unrecognized(0, 0),
    );
}

#[test]
fn invalid_identifier_in_parameters() {
    expect_error(
        r#"
making_coffee(BadParam) : Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidIdentifier(0, 8, "BadParam".to_string()),
    );
}

#[test]
fn invalid_identifier_empty() {
    expect_error(
        r#"
 : Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidDeclaration(0, 0),
    );
}

#[test]
fn invalid_step_format() {
    expect_error(
        r#"
making_coffee :

    A. First step (should be lowercase 'a.')
            "#
        .trim_ascii(),
        ParsingError::InvalidStep(21, 0),
    );
}

#[test]
fn invalid_response_wrong_quotes() {
    expect_error(
        r#"
making_coffee :

    1. Do you want coffee?
        "Yes" | "No"
            "#
        .trim_ascii(),
        ParsingError::InvalidResponse(52, 0),
    );
}

#[test]
fn invalid_multiline_missing_closing() {
    expect_error(
        r#"
making_coffee :

    1. Do something with ```
       This is missing closing backticks
            "#
        .trim_ascii(),
        ParsingError::InvalidMultiline(24, 0),
    );
}

#[test]
fn invalid_code_block_missing_closing_brace() {
    expect_error(
        r#"
making_coffee :

    1. Do something { exec("command"
            "#
        .trim_ascii(),
        ParsingError::ExpectedMatchingChar(37, 0, "a code block", '{', '}'),
    );
}

#[test]
fn invalid_step_wrong_ordinal() {
    expect_error(
        r#"
making_coffee :

    i. Wrong case section
            "#
        .trim_ascii(),
        ParsingError::InvalidStep(21, 0),
    );
}

#[test]
fn invalid_invocation_malformed() {
    expect_error(
        r#"
making_coffee :

    1. Do <something_without_closing
            "#
        .trim_ascii(),
        ParsingError::ExpectedMatchingChar(27, 0, "an invocation", '<', '>'),
    );
}

#[test]
fn invalid_execution_malformed() {
    expect_error(
        r#"
making_coffee :

    1. Do something { exec("command" }
            "#
        .trim_ascii(),
        ParsingError::ExpectedMatchingChar(43, 0, "parameters for a function", '(', ')'),
    );
}

#[test]
fn invalid_function_with_space_in_name() {
    expect_error(
        r#"
making_coffee :

    1. Do something { re peat() }
            "#
        .trim_ascii(),
        ParsingError::InvalidCodeBlock(39, 10),
    );
}

#[test]
fn invalid_function_with_space_and_invocation() {
    expect_error(
        r#"
making_coffee :

    1. Do something { re peat <thing>() }
            "#
        .trim_ascii(),
        ParsingError::InvalidCodeBlock(39, 18),
    );
}

#[test]
fn invalid_invocation_in_repeat() {
    expect_error(
        r#"
making_coffee :

    1. { repeat <making_coffee }
            "#
        .trim_ascii(),
        ParsingError::ExpectedMatchingChar(33, 0, "an invocation", '<', '>'),
    );
}

#[test]
fn invalid_substep_uppercase() {
    expect_error(
        r#"
making_coffee :

    1. First step
        A. This should be lowercase
            "#
        .trim_ascii(),
        ParsingError::InvalidSubstep(43, 0),
    );
}

#[test]
fn invalid_code_block_with_leftover_content() {
    expect_error(
        r#"
robot :

Your plastic pal who's fun to be with! { re peat <jingle> }
        "#
        .trim_ascii(),
        ParsingError::InvalidCodeBlock(50, 7),
    );
}
