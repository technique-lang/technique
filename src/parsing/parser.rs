// parsing machinery

use pest::{consumes_to, parses_to, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "../technique.pest"]
struct TechniqueParser;

pub fn parse_via_pest(content: &str) {
    let technique = TechniqueParser::parse(Rule::technique, &content);
    println!("{:?}", technique);
}

#[cfg(test)]
mod tests {
    use super::*; // Import all parent module items

    #[test]
    fn check_procedure_declaration_explicit() {
        let input = "making_coffee : Beans, Milk -> Coffee";

        let declaration = TechniqueParser::parse(Rule::declaration, &input)
            .expect("Unsuccessful Parse")
            .next()
            .unwrap();

        assert_eq!(
            declaration.as_str(),
            "making_coffee : Beans, Milk -> Coffee"
        );
        assert_eq!(declaration.as_rule(), Rule::declaration);

        let mut pairs = declaration.into_inner();

        let identifier = pairs
            .next()
            .unwrap();

        assert_eq!(identifier.as_str(), "making_coffee");
        assert_eq!(identifier.as_rule(), Rule::identifier);

        let signature = pairs
            .next()
            .unwrap();

        assert_eq!(signature.as_str(), "Beans, Milk -> Coffee");
        assert_eq!(signature.as_rule(), Rule::signature);

        let mut pairs = signature.into_inner();

        let domain1 = pairs
            .next()
            .unwrap();

        assert_eq!(domain1.as_str(), "Beans");
        assert_eq!(domain1.as_rule(), Rule::forma);

        let domain2 = pairs
            .next()
            .unwrap();

        assert_eq!(domain2.as_str(), "Milk");
        assert_eq!(domain2.as_rule(), Rule::forma);

        let range = pairs
            .next()
            .unwrap();

        assert_eq!(range.as_str(), "Coffee");
        assert_eq!(range.as_rule(), Rule::forma);
    }

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
                        year(11,15),
                        owner(16, 26)
                    ])
                ])
            ]
        };

        parses_to! {
            parser: TechniqueParser,
            input: "2024",
            rule: Rule::year,
            tokens: [
                year(0,4),
            ]
        };
    }
}
