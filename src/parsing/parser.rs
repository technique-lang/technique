use lalrpop_util::lalrpop_mod;
use lalrpop_util::ParseError;
use technique::language::*;

lalrpop_mod!(pub grammar);

pub fn parse_via_lalrpop(_content: &str) {
    std::process::exit(0);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValidationError {
    ZeroLengthToken,
    InvalidIdentifier,
}

/// An adapter trait to wrap our custom errors into a `ParseError`.
pub trait WrapError<T, E, L, R> {
    fn wrap(self) -> Result<T, ParseError<L, R, E>>;
}

impl<T, E, L, R> WrapError<T, E, L, R> for Result<T, E> {
    fn wrap(self) -> Result<T, ParseError<L, R, E>> {
        self.map_err(|e| ParseError::User { error: e })
    }
}

/// Validates if the input string is a valid identifier corresponding
/// to `[a-z][a-zA-Z0-9_]*` as an identifier.
fn validate_identifier(input: &str) -> Result<String, ValidationError> {
    if input.len() == 0 {
        return Err(ValidationError::ZeroLengthToken);
    }

    let mut cs = input.chars();

    if !cs
        .next()
        .unwrap()
        .is_ascii_lowercase()
    {
        return Err(ValidationError::InvalidIdentifier);
    }

    for c in cs {
        if !(c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_') {
            return Err(ValidationError::InvalidIdentifier);
        }
    }

    Ok(input.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_identifier_rules() {
        let p = grammar::identifierParser::new();

        assert_eq!(p.parse("a"), Ok("a".to_string()));
        assert_eq!(p.parse("ab"), Ok("ab".to_string()));
        assert_eq!(p.parse("johnny5"), Ok("johnny5".to_string()));
        assert!(p
            .parse("Pizza")
            .is_err(),);
        assert!(p
            .parse("pizZa")
            .is_err());
        assert!(p
            .parse("0trust")
            .is_err());
        assert_eq!(p.parse("make_dinner"), Ok("make_dinner".to_string()));
        assert!(p
            .parse("MakeDinner")
            .is_err());
        assert!(p
            .parse("make-dinner")
            .is_err());
    }

    #[test]
    fn check_magic_line() {
        let p = grammar::magic_lineParser::new();
        assert_eq!(p.parse("% technique v1"), Ok(1));
        assert_eq!(p.parse("%technique v1"), Ok(1));
        // this is rejected because the technique keyword isn't present.
        assert!(p
            .parse("%techniquev1")
            .is_err());
    }

    #[test]
    fn check_header_spdx() {
        let l = grammar::licenseParser::new();
        let c = grammar::copyrightParser::new();
        let p = grammar::spdx_lineParser::new();

        assert_eq!(l.parse("MIT"), Ok("MIT".to_string()));
        assert_eq!(l.parse("Public Domain"), Ok("Public Domain".to_string()));
        assert_eq!(
            l.parse("CC BY-SA 3.0 IGO"),
            Ok("CC BY-SA 3.0 IGO".to_string())
        );

        assert_eq!(c.parse("ACME"), Ok("ACME".to_string()));
        assert_eq!(c.parse("lower"), Ok("lower".to_string()));
        assert_eq!(c.parse("ACME, Inc."), Ok("ACME, Inc.".to_string()));

        assert_eq!(
            c.parse("2024 ACME, Inc."),
            Ok("2024 ACME, Inc.".to_string())
        );

        assert_eq!(p.parse("! PD"), Ok(("PD".to_string(), "".to_string())));
        assert_eq!(
            p.parse("! MIT; (c) ACME, Inc."),
            Ok(("MIT".to_string(), "ACME, Inc.".to_string()))
        );
        assert_eq!(
            p.parse("! MIT; (C) ACME, Inc."),
            Ok(("MIT".to_string(), "ACME, Inc.".to_string()))
        );
        assert_eq!(
            p.parse("! MIT; © ACME, Inc."),
            Ok(("MIT".to_string(), "ACME, Inc.".to_string()))
        );
        assert_eq!(
            p.parse("! MIT; (c) 2024 ACME, Inc."),
            Ok(("MIT".to_string(), "2024 ACME, Inc.".to_string()))
        );
        assert_eq!(
            p.parse("! CC BY-SA 3.0 [IGO]; (c) 2024 ACME, Inc."),
            Ok((
                "CC BY-SA 3.0 [IGO]".to_string(),
                "2024 ACME, Inc.".to_string()
            ))
        );
    }

    #[test]
    fn check_header_template() {
        let t = grammar::templateParser::new();
        let p = grammar::template_lineParser::new();

        assert_eq!(t.parse("checklist"), Ok("checklist".to_string()));
        assert_eq!(t.parse("checklist,v1"), Ok("checklist,v1".to_string()));
        assert_eq!(t.parse("checklist-v1.0"), Ok("checklist-v1.0".to_string()));

        assert_eq!(p.parse("& nasa"), Ok("nasa".to_string()));
        assert_eq!(p.parse("& nasa-flight-procedure,v9"), Ok("nasa-flight-procedure,v9".to_string()));
    }
}
/*
    /*
        #[test]
        fn check_procedure_declaration_explicit() {
            let input = "making_coffee : Beans, Milk -> Coffee";

            // let declaration = TechniqueParser::parse(Rule::declaration, &input)
            //     .expect("Unsuccessful Parse")
            //     .next()
            //     .unwrap();

            assert_eq!(
                input, // FIXME
                "making_coffee : Beans, Milk -> Coffee"
            );

            // assert_eq!(identifier.as_str(), "making_coffee");
            // assert_eq!(identifier.as_rule(), Rule::identifier);

            // assert_eq!(signature.as_str(), "Beans, Milk -> Coffee");
            // assert_eq!(signature.as_rule(), Rule::signature);

            // assert_eq!(domain1.as_str(), "Beans");
            // assert_eq!(domain1.as_rule(), Rule::forma);

            // assert_eq!(domain2.as_str(), "Milk");
            // assert_eq!(domain2.as_rule(), Rule::forma);

            // assert_eq!(range.as_str(), "Coffee");
            // assert_eq!(range.as_rule(), Rule::forma);
        }
    */
    /*
            #[test]
            fn check_procedure_declaration_macro() {
                parses_to! {
                    parser: TechniqueParser,
                    input: "making_coffee : Beans, Milk -> Coffee",
                    rule: Rule::declaration,
                    tokens: [
                        declaration(0, 37, [
                            identifier(0, 13),
                            signature(16, 37, [
                                forma(16, 21),
                                forma(23, 27),
                                forma(31, 37)
                            ])
                        ])
                    ]
                };
            }
    */

    /*
        #[test]
        fn check_header_template() {
            parses_to! {
                parser: TechniqueParser,
                input: "& checklist",
                rule: Rule::template_line,
                tokens: [
                    template_line(0, 11, [
                        template(2, 11)
                    ])
                ]
            };
            parses_to! {
                parser: TechniqueParser,
                input: "& nasa-flight-plan-v4.0",
                rule: Rule::template_line,
                tokens: [
                    template_line(0, 23, [
                        template(2, 23)
                    ])
                ]
            };
            fails_with! {
                parser: TechniqueParser,
                input: "&",
                rule: Rule::template_line,
                positives: [Rule::template],
                negatives: [],
                pos: 1
            };
        }

    #[test]
    fn check_declaration_syntax() {
        parses_to! {
            parser: TechniqueParser,
            input: "p :",
            rule: Rule::declaration,
            tokens: [
                declaration(0, 3, [
                    identifier(0, 1)
                ])
            ]
        };
        parses_to! {
            parser: TechniqueParser,
            input: "p : A -> B",
            rule: Rule::declaration,
            tokens: [
                declaration(0, 10, [
                    identifier(0, 1),
                    signature(4, 10, [
                        forma(4, 5),
                        forma(9, 10)
                    ])
                ])
            ]
        };
        fails_with! {
            parser: TechniqueParser,
            input: "cook-pizza :",
            rule: Rule::declaration,
            positives: [Rule::declaration],
            negatives: [],
            pos: 0
        };
    }
    */
}
*/
