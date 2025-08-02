#[cfg(test)]
mod syntax {
    use technique::parsing::parser::{Parser, ParsingError};

    /// Helper function to check if parsing produces the expected error type
    fn expect_error(content: &str, expected: ParsingError) {
        let mut input = Parser::new();
        input.initialize(content);

        let result = input.read_procedure();
        match result {
            Ok(_) => panic!(
                "Expected parsing to fail, but it succeeded for input: {}",
                content
            ),
            Err(error) => {
                // Compare error types by discriminant
                if std::mem::discriminant(&error) != std::mem::discriminant(&expected) {
                    panic!(
                        "Expected error type like {:?} but got: {:?} for input '{}'",
                        expected, error, content
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
            ParsingError::InvalidIdentifier(0, ""),
        );
    }

    #[test]
    fn invalid_identifier_mixed_case() {
        expect_error(
            r#"
makeCoffee : Ingredients -> Coffee
            "#
            .trim_ascii(),
            ParsingError::InvalidIdentifier(0, ""),
        );
    }

    #[test]
    fn invalid_identifier_with_dashes() {
        expect_error(
            r#"
make-coffee : Ingredients -> Coffee
            "#
            .trim_ascii(),
            ParsingError::InvalidIdentifier(0, ""),
        );
    }

    #[test]
    fn invalid_identifier_with_spaces() {
        expect_error(
            r#"
make coffee : Ingredients -> Coffee
            "#
            .trim_ascii(),
            ParsingError::InvalidIdentifier(0, ""),
        );
    }

    #[test]
    fn invalid_signature_wrong_arrow() {
        expect_error(
            r#"
making_coffee : Ingredients => Coffee
            "#
            .trim_ascii(),
            ParsingError::InvalidSignature(0),
        );
    }

    #[test]
    fn invalid_genus_lowercase_forma() {
        expect_error(
            r#"
making_coffee : ingredients -> Coffee
            "#
            .trim_ascii(),
            ParsingError::InvalidGenus(16),
        );
    }

    #[test]
    fn invalid_genus_both_lowercase() {
        expect_error(
            r#"
making_coffee : ingredients -> coffee
            "#
            .trim_ascii(),
            ParsingError::InvalidGenus(16),
        );
    }

    #[test]
    fn invalid_signature_missing_arrow() {
        expect_error(
            r#"
making_coffee : Ingredients Coffee
            "#
            .trim_ascii(),
            ParsingError::InvalidSignature(16),
        );
    }

    #[test]
    fn invalid_declaration_missing_colon() {
        expect_error(
            r#"
making_coffee Ingredients -> Coffee
            "#
            .trim_ascii(),
            ParsingError::InvalidDeclaration(0),
        );
    }

    #[test]
    fn invalid_identifier_in_parameters() {
        expect_error(
            r#"
making_coffee(BadParam) : Ingredients -> Coffee
            "#
            .trim_ascii(),
            ParsingError::InvalidIdentifier(14, ""),
        );
    }

    #[test]
    fn invalid_identifier_empty() {
        expect_error(
            r#"
 : Ingredients -> Coffee
            "#
            .trim_ascii(),
            ParsingError::InvalidDeclaration(0),
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
            ParsingError::InvalidStep(21),
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
            ParsingError::InvalidResponse(52),
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
            ParsingError::InvalidMultiline(41),
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
            ParsingError::ExpectedMatchingChar(38, "a code block", '{', '}'),
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
            ParsingError::InvalidStep(21),
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
            ParsingError::ExpectedMatchingChar(27, "an invocation", '<', '>'),
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
            ParsingError::ExpectedMatchingChar(43, "a function call", '(', ')'),
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
            ParsingError::ExpectedMatchingChar(29, "an invocation", '<', '>'),
        );
    }
}
