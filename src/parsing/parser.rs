use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub technique);

pub fn parse_via_lalrpop(_content: &str) {
    std::process::exit(0);
}

/*
// takes a single lower case character then any lower case character, digit,
// or unerscore. Based on the parser code in chumsky::text::ident().

fn parse_identifier() -> impl Parser<char, Identifier, Error = Simple<char>> {
    filter(|c: &char| c.is_ascii_lowercase())
        .map(Some)
        .chain::<char, Vec<_>, _>(
            filter(|c: &char| c.is_ascii_lowercase() || c.is_ascii_digit() || *c == '_').repeated(),
        )
        .collect()
    // .validate(|s : String, span : Range, emit| if s.len() != span.end() - span.start() { emit(Simple::custom(span, "Wrong length")) })
}

fn parse_magic_line() -> impl Parser<char, u8, Error = Simple<char>> {
    just('%')
        .ignore_then(just("technique").padded())
        .ignore_then(just("v1").to(1u8))
}

fn parse_spdx_line() -> impl Parser<char, (Option<String>, Option<String>), Error = Simple<char>> {
    just('!')
        .ignore_then(
            parse_license()
                .padded()
                .or_not(),
        )
        .then(
            just(';')
                .ignore_then(
                    just("(c)")
                        .or(just("(C)"))
                        .or(just("©"))
                        .padded(),
                )
                .ignore_then(parse_copyright().padded())
                .or_not(),
        )
}

fn parse_license() -> impl Parser<char, String, Error = Simple<char>> {
    filter(|c: &char| {
        *c != ';'
            && (c.is_ascii_uppercase()
                || c.is_ascii_lowercase()
                || c.is_ascii_digit()
                || c.is_ascii_punctuation()
                || *c == ' ')
    })
    .repeated()
    .at_least(1)
    .collect()
}

fn parse_copyright() -> impl Parser<char, String, Error = Simple<char>> {
    filter(|c: &char| {
        c.is_ascii_uppercase()
            || c.is_ascii_lowercase()
            || c.is_ascii_digit()
            || c.is_ascii_punctuation()
            || *c == ' '
    })
    .repeated()
    .at_least(1)
    .collect()
}

fn parse_template_line() -> impl Parser<char, Option<String>, Error = Simple<char>> {
    just('&').ignore_then(
        parse_template()
            .padded()
            .or_not(),
    )
}

fn parse_template() -> impl Parser<char, String, Error = Simple<char>> {
    filter(|c: &char| {
        c.is_ascii_uppercase()
            || c.is_ascii_lowercase()
            || c.is_ascii_digit()
            || *c == '.'
            || *c == ','
            || *c == '-'
    })
    .repeated()
    .at_least(1)
    .collect()
}
*/

#[cfg(test)]
mod tests {
    use super::*;
    use lalrpop_util::{lexer::Token, ParseError};

    #[test]
    fn check_identifier_rules() {
        let p = technique::identifierParser::new();

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
        let p = technique::magic_lineParser::new();
        assert_eq!(p.parse("% technique v1"), Ok(1));
        assert_eq!(p.parse("%technique v1"), Ok(1));
        // this is rejected because the technique keyword isn't present. I'm
        // not convinced there is great value to having an error of this degree
        // of detail hard-coded in the test case; change to .is_err() if it
        // ever becomes a problem.
        assert!(p
            .parse("%techniquev1")
            .is_err());
    }
}
/*
    #[test]
    fn check_header_spdx() {
        assert_eq!(parse_license().parse("MIT"), Ok("MIT".to_string()));
        assert_eq!(
            parse_license().parse("Public Domain"),
            Ok("Public Domain".to_string())
        );
        assert_eq!(
            parse_license().parse("CC BY-SA 3.0 IGO"),
            Ok("CC BY-SA 3.0 IGO".to_string())
        );

        assert_eq!(parse_copyright().parse("ACME"), Ok("ACME".to_string()));
        assert_eq!(
            parse_copyright().parse("ACME, Inc."),
            Ok("ACME, Inc.".to_string())
        );

        assert_eq!(
            parse_copyright().parse("2024 ACME, Inc."),
            Ok("2024 ACME, Inc.".to_string())
        );

        assert_eq!(
            parse_spdx_line().parse("! PD"),
            Ok((Some("PD".to_string()), None))
        );
        assert_eq!(
            parse_spdx_line().parse("! MIT; (c) ACME, Inc.".to_string()),
            Ok((Some("MIT".to_string()), Some("ACME, Inc.".to_string())))
        );
        assert_eq!(
            parse_spdx_line().parse("! MIT; (C) ACME, Inc.".to_string()),
            Ok((Some("MIT".to_string()), Some("ACME, Inc.".to_string())))
        );
        assert_eq!(
            parse_spdx_line().parse("! MIT; © ACME, Inc.".to_string()),
            Ok((Some("MIT".to_string()), Some("ACME, Inc.".to_string())))
        );
        assert_eq!(
            parse_spdx_line().parse("! MIT; (c) 2024 ACME, Inc."),
            Ok((Some("MIT".to_string()), Some("2024 ACME, Inc.".to_string())))
        );
        assert_eq!(
            parse_spdx_line().parse("! CC BY-SA 3.0 [IGO]; (c) 2024 ACME, Inc."),
            Ok((
                Some("CC BY-SA 3.0 [IGO]".to_string()),
                Some("2024 ACME, Inc.".to_string())
            ))
        );
    }

    #[test]
    fn check_header_template() {
        assert_eq!(
            parse_template().parse("checklist"),
            Ok("checklist".to_string())
        );
        assert_eq!(
            parse_template().parse("checklist,v1"),
            Ok("checklist,v1".to_string())
        );
        assert_eq!(
            parse_template().parse("checklist-v1.0"),
            Ok("checklist-v1.0".to_string())
        );
        assert_eq!(
            parse_template_line().parse("& checklist-v1"),
            Ok(Some("checklist-v1".to_string()))
        );
    }
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
