// parsing machinery

// struct TechniqueParser;

use winnow::combinator::empty;
use winnow::stream::AsChar;
use winnow::token::{one_of, take_while};
use winnow::{PResult, Parser};

pub fn parse_via_winnow(_content: &str) {
    // let technique = TechniqueParser::parse(Rule::technique, &content);
    // println!("{:?}", technique);
}

// a winnow parser that takes an alpha and then any character
fn parse_identifier<'s>(input: &mut &'s str) -> PResult<&'s str> {
    (
        one_of('a'..='z'),
        take_while(0.., (('0'..='9'), ('a'..='z'), ('_'))),
    )
        .take()
        .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_identifier_rules() {
        let mut input = "p";

        let result = parse_identifier
            .parse_next(&mut input)
            .unwrap();

        assert_eq!(result, "p");

        let mut input = "pizza";
        let result = parse_identifier
            .parse_next(&mut input)
            .unwrap();
        assert_eq!(result, "pizza");

        let mut input = "cook_pizza";
        let result = parse_identifier
            .parse_next(&mut input)
            .unwrap();
        assert_eq!(result, "cook_pizza");

        assert!(parse_identifier(&mut "0trust").is_err());
        assert!(parse_identifier(&mut "Pizza").is_err());
    }

    // Import all parent module items
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

        #[test]
        fn check_header_spdx() {
            parses_to! {
                parser: TechniqueParser,
                input: "! MIT; (c) ACME, Inc.",
                rule: Rule::spdx_line,
                tokens: [
                    spdx_line(0, 21, [
                        license(2, 5),
                        copyright(7, 21, [
                            owner(11, 21)
                        ])
                    ])
                ]
            };
            parses_to! {
                parser: TechniqueParser,
                input: "! MIT; (c) 2024 ACME, Inc.",
                rule: Rule::spdx_line,
                tokens: [
                    spdx_line(0, 26, [
                        license(2, 5),
                        copyright(7, 26, [
                            year(11, 15),
                            owner(16, 26)
                        ])
                    ])
                ]
            };
            parses_to! {
                parser: TechniqueParser,
                input: "! PD",
                rule: Rule::spdx_line,
                tokens: [
                    spdx_line(0, 4, [
                        license(2, 4)
                    ])
                ]
            };

            parses_to! {
                parser: TechniqueParser,
                input: "MIT",
                rule: Rule::license,
                tokens: [
                    license(0, 3),
                ]
            };
            parses_to! {
                parser: TechniqueParser,
                input: "Public Domain",
                rule: Rule::license,
                tokens: [
                    license(0, 13),
                ]
            };
            parses_to! {
                parser: TechniqueParser,
                input: "CC BY-SA 3.0 IGO",
                rule: Rule::license,
                tokens: [
                    license(0, 16),
                ]
            };

            parses_to! {
                parser: TechniqueParser,
                input: "2024",
                rule: Rule::year,
                tokens: [
                    year(0, 4),
                ]
            };
            parses_to! {
                parser: TechniqueParser,
                input: "2024-",
                rule: Rule::year,
                tokens: [
                    year(0, 5),
                ]
            };
            parses_to! {
                parser: TechniqueParser,
                input: "2002-2024",
                rule: Rule::year,
                tokens: [
                    year(0, 9),
                ]
            };
            fails_with! {
                parser: TechniqueParser,
                input: "02",
                rule: Rule::year,
                positives: [Rule::year],
                negatives: [],
                pos: 0
            };
            fails_with! {
                parser: TechniqueParser,
                input: "02-24",
                rule: Rule::year,
                positives: [Rule::year],
                negatives: [],
                pos: 0
            };
        }

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
