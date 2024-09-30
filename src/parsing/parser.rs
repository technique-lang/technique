// parsing machinery

// struct TechniqueParser;

use chumsky::prelude::*;

pub fn parse_via_chumsky(content: &str) {
    let result = parse_identifier().parse(content);
    println!("{:?}", result);
    std::process::exit(0);
}

type Identifier = String;

// takes a single lower case character then any lower case character, digit,
// or unerscore. Based on the parser code in chumsky::text::ident().

fn parse_identifier() -> impl Parser<char, Identifier, Error = Simple<char>> {
    filter(|c: &char| c.is_ascii_lowercase())
        .map(Some)
        .chain::<char, Vec<_>, _>(
            filter(|c: &char| c.is_ascii_lowercase() || c.is_ascii_digit() || *c == '_').repeated(),
        )
        .collect()
}

fn parse_magic_line() -> impl Parser<char, u8, Error = Simple<char>> {
    just('%')
        .ignore_then(just("technique").padded())
        .ignore_then(just("v1").to(1u8))
}

fn parse_spdx_line() -> impl Parser<char, (String, String), Error = Simple<char>>
{
    just('!')
        .ignore_then(parse_license())
        .then_ignore(just(';'))
        .then(parse_copyright())
}

fn parse_license() -> impl Parser<char, String, Error = Simple<char>> {
    filter(|c: &char| {
        c.is_ascii_uppercase()
            || c.is_ascii_lowercase()
            || c.is_ascii_digit()
            || *c != ';' // symbol which separates license and copyright probably shouldn't ever encounter it
            || c.is_ascii_punctuation()
            || *c == ' '
    })
    .repeated()
    .at_least(1)
    .collect()
}

// change to a semantic Copyright type
fn parse_copyright() -> impl Parser<char, String, Error = Simple<char>> {
    let p = parse_copyright_year()
        .padded()
        .then(parse_copyright_owner());

    p.map(|((y1, y2), o)| {
        let mut r = String::new();
        r.push_str(&y1);
        r.push_str(&y2);
        r.push_str(&o);
        r
    })
}

fn year() -> impl Parser<char, String, Error = Simple<char>> {
    filter(|c: &char| c.is_ascii_digit())
        .repeated()
        .at_least(4)
        .at_most(4)
        .collect()
}

fn parse_copyright_year() -> impl Parser<char, (String, String), Error = Simple<char>> {
    year()
        .then_ignore(just('-'))
        .then(year())
        .or(year()
            .then_ignore(just('-'))
            .map(|yyyy| (yyyy, "".to_string())))
        .or(year().map(|yyyy| (yyyy, "".to_string())))
}

fn parse_copyright_owner() -> impl Parser<char, String, Error = Simple<char>> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_identifier_rules() {
        let input = "make_dinner";

        let result = parse_identifier().parse(input);

        assert_eq!(result, Ok("make_dinner".to_string()));

        let input = "";

        let result = parse_identifier().parse(input);

        assert!(result.is_err());

        let input = "MakeDinner";

        let result = parse_identifier().parse(input);

        assert!(result.is_err());
    }

    #[test]
    fn check_magic_line() {
        assert_eq!(parse_magic_line().parse("% technique v1"), Ok(1));
        assert_eq!(parse_magic_line().parse("%technique v1"), Ok(1));
        // this isn't really ideal, but there's no absolutely vital reason it
        // has to be rejected.
        assert_eq!(parse_magic_line().parse("%techniquev1"), Ok(1));
    }

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

        assert_eq!(
            parse_copyright_year().parse("2024"),
            Ok(("2024".to_string(), "".to_string()))
        );
        assert_eq!(
            parse_copyright_year().parse("2024-"),
            Ok(("2024".to_string(), "".to_string()))
        );
        assert_eq!(
            parse_copyright_year().parse("2002-2024"),
            Ok(("2002".to_string(), "2024".to_string()))
        );

        assert!(parse_copyright_year()
            .parse("24")
            .is_err());
        assert!(parse_copyright_year()
            .parse("02-24")
            .is_err());

        assert_eq!(
            parse_copyright_owner().parse("ACME"),
            Ok("ACME".to_string())
        );
        assert_eq!(
            parse_copyright_owner().parse("ACME, Inc."),
            Ok("ACME, Inc.".to_string())
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
