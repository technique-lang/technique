use super::*;
use std::path::Path;

/// Helper function to check if parsing produces the expected error
fn expect_error(content: &str, expected: ParsingError) {
    let result = parse_with_recovery(Path::new("Test.tq"), content);
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
        ParsingError::InvalidIdentifier(Span::new(0, 13), "Making_Coffee".to_string()),
    );
}

#[test]
fn invalid_identifier_mixed_case() {
    expect_error(
        r#"
makeCoffee : Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidIdentifier(Span::new(0, 10), "makeCoffee".to_string()),
    );
}

#[test]
fn invalid_identifier_with_dashes() {
    expect_error(
        r#"
make-coffee : Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidIdentifier(Span::new(0, 11), "make-coffee".to_string()),
    );
}

#[test]
fn invalid_identifier_with_spaces() {
    expect_error(
        r#"
make coffee : Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidParameters(Span::new(5, 6)),
    );
}

#[test]
fn invalid_signature_wrong_arrow() {
    expect_error(
        r#"
making_coffee : Ingredients => Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidSignature(Span::new(28, 0)),
    );
}

#[test]
fn invalid_genus_lowercase_forma() {
    expect_error(
        r#"
making_coffee : ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidGenus(Span::new(16, 11)),
    );
}

#[test]
fn invalid_genus_both_lowercase() {
    expect_error(
        r#"
making_coffee : ingredients -> coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidGenus(Span::new(16, 11)),
    );
}

#[test]
fn invalid_signature_missing_arrow() {
    expect_error(
        r#"
making_coffee : Ingredients Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidSignature(Span::new(28, 0)),
    );
}

#[test]
fn invalid_declaration_missing_colon() {
    expect_error(
        r#"
making_coffee Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::Unrecognized(Span::new(0, 0)),
    );
}

// A paragraph of nothing but a name and a colon is a declaration whose colon
// was not set apart from the name, not prose
#[test]
fn declaration_without_space_before_colon() {
    expect_error(
        r#"
making_coffee :

    1.  Boil the water

beta:

    2.  Pour it out
            "#
        .trim_ascii(),
        ParsingError::InvalidDeclaration(Span::new(41, 5)),
    );
}

// A signature after the colon settles it; no sentence is written that way
#[test]
fn declaration_without_space_but_with_signature() {
    expect_error(
        r#"
making_coffee :

    1.  Boil the water

beta: Ingredients -> Coffee

    2.  Pour it out
            "#
        .trim_ascii(),
        ParsingError::InvalidDeclaration(Span::new(41, 27)),
    );
}

// ... whereas the last line of a wrapped sentence is prose, and the blank
// line that would have set it apart as its own paragraph is absent
#[test]
fn colon_ending_wrapped_prose_is_not_a_declaration() {
    let source = r#"
making_coffee :

The one you want is
this one:

    1.  Boil the water
            "#
    .trim_ascii();

    let result = parse_with_recovery(Path::new("Test.tq"), source);
    assert!(
        result.is_ok(),
        "Prose ending in a colon should parse, got: {:?}",
        result.err()
    );
}

// Content a section cannot hold is reported, not quietly dropped on the
// floor as it makes its way past
#[test]
fn unrecognized_content_in_section() {
    expect_error(
        r#"
making_coffee :

    1.  Boil the water

I. Second Section

    # Overview notes

    1.  Pour it out
            "#
        .trim_ascii(),
        ParsingError::Unrecognized(Span::new(64, 0)),
    );
}

// A malformed declaration must end the procedure before it, rather than
// being taken as description and swallowing the procedure that follows
#[test]
fn invalid_identifier_in_following_declaration() {
    expect_error(
        r#"
making_coffee :

    1.  Boil the water

Prepare Gin :

    1.  Pour it out
            "#
        .trim_ascii(),
        ParsingError::InvalidIdentifier(Span::new(41, 7), "Prepare".to_string()),
    );
}

#[test]
fn invalid_identifier_in_parameters() {
    expect_error(
        r#"
making_coffee(BadParam) : Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidIdentifier(Span::new(14, 8), "BadParam".to_string()),
    );
}

#[test]
fn invalid_identifier_empty() {
    expect_error(
        r#"
 : Ingredients -> Coffee
            "#
        .trim_ascii(),
        ParsingError::InvalidDeclaration(Span::new(0, 0)),
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
        ParsingError::InvalidStep(Span::new(21, 0)),
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
        ParsingError::InvalidResponse(Span::new(52, 0)),
    );
}

#[test]
fn invalid_text_after_responses() {
    expect_error(
        r#"
making_coffee :

    1. Do you want coffee?
        'Yes' | 'No'
        Tell the barista.
            "#
        .trim_ascii(),
        ParsingError::MixedStepContent(Span::new(73, 0)),
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
        ParsingError::InvalidMultiline(Span::new(24, 0)),
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
        ParsingError::ExpectedMatchingChar(Span::new(37, 0), "a code block", '{', '}'),
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
        ParsingError::InvalidStep(Span::new(21, 0)),
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
        ParsingError::ExpectedMatchingChar(Span::new(27, 0), "an invocation", '<', '>'),
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
        ParsingError::ExpectedMatchingChar(Span::new(43, 0), "parameters for a function", '(', ')'),
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
        ParsingError::InvalidCodeBlock(Span::new(39, 10)),
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
        ParsingError::InvalidCodeBlock(Span::new(39, 18)),
    );
}

#[test]
fn invalid_tuple_whitespace_parens() {
    expect_error(
        r#"
making_coffee :

    1. Do something { ( ) }
            "#
        .trim_ascii(),
        ParsingError::InvalidTuple(Span::new(39, 3)),
    );
}

#[test]
fn invalid_tuple_parenthesised_expression() {
    expect_error(
        r#"
making_coffee :

    1. Do something { (x) }
            "#
        .trim_ascii(),
        ParsingError::InvalidTuple(Span::new(39, 3)),
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
        ParsingError::ExpectedMatchingChar(Span::new(33, 0), "an invocation", '<', '>'),
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
        ParsingError::InvalidSubstep(Span::new(43, 0)),
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
        ParsingError::InvalidCodeBlock(Span::new(50, 3)),
    );
}
