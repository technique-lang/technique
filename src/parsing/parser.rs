#![allow(unused_variables)]
#![allow(dead_code)]

use regex::Regex;
use technique::language::*;

pub fn parse_via_string(content: &str) {
    // let result = p.parse(content);
    // println!("{:?}", result);
    std::process::exit(0);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValidationError {
    ZeroLengthToken,
    Unrecognized, // improve this
    InvalidIdentifier,
    InvalidForma,
}

fn validate_forma(input: &str) -> Result<Forma, ValidationError> {
    if input.len() == 0 {
        return Err(ValidationError::ZeroLengthToken);
    }

    let mut cs = input.chars();

    if !cs
        .next()
        .unwrap()
        .is_ascii_uppercase()
    {
        return Err(ValidationError::InvalidForma);
    }

    for c in cs {
        if !(c.is_ascii_uppercase() || c.is_ascii_lowercase() || c.is_ascii_digit()) {
            return Err(ValidationError::InvalidForma);
        }
    }

    Ok(Forma {
        name: input.to_owned(),
    })
}

fn parse_magic_line(input: &str) -> Result<u8, ValidationError> {
    let re = Regex::new(r"%\s*technique\s+v1").unwrap();

    let i = re
        .find(input)
        .map(|_| 1)
        .ok_or(ValidationError::Unrecognized);
    i
}

fn parse_identifier(input: &str) -> Result<&str, ValidationError> {
    if input.len() == 0 {
        return Err(ValidationError::ZeroLengthToken);
    }

    let re = Regex::new(r"^[a-z][a-z0-9_]*$").unwrap();
    let i = re
        .find(input)
        .map(|v| v.as_str())
        .ok_or(ValidationError::InvalidIdentifier);
    i
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_identifier_rules() {
        assert_eq!(parse_identifier("a"), Ok("a"));
        assert_eq!(parse_identifier("ab"), Ok("ab"));
        assert_eq!(parse_identifier("johnny5"), Ok("johnny5"));
        assert_eq!(
            parse_identifier("Pizza"),
            Err(ValidationError::InvalidIdentifier)
        );
        assert_eq!(
            parse_identifier("pizZa"),
            Err(ValidationError::InvalidIdentifier)
        );
        assert!(parse_identifier("0trust").is_err());
        assert_eq!(parse_identifier("make_dinner"), Ok("make_dinner"));
        assert!(parse_identifier("MakeDinner").is_err());
        assert!(parse_identifier("make-dinner").is_err());
    }

    #[test]
    fn check_magic_line() {
        assert_eq!(parse_magic_line("% technique v1"), Ok(1));
        assert_eq!(parse_magic_line("%technique v1"), Ok(1));
        // this is rejected because the technique keyword isn't present.
        assert_eq!(
            parse_magic_line("%techniquev1"),
            Err(ValidationError::Unrecognized)
        );
    }
}
/*
    #[test]
    fn check_header_spdx() {
        let l = grammar::licenseParser::new();
        let c = grammar::copyrightParser::new();
        let p = grammar::spdx_lineParser::new();

        assert_eq!(l.parse("MIT"), Ok("MIT".to_owned()));
        assert_eq!(l.parse("Public Domain"), Ok("Public Domain".to_owned()));
        assert_eq!(
            l.parse("CC BY-SA 3.0 IGO"),
            Ok("CC BY-SA 3.0 IGO".to_owned())
        );

        assert_eq!(c.parse("ACME"), Ok("ACME".to_owned()));
        assert_eq!(c.parse("lower"), Ok("lower".to_owned()));
        assert_eq!(c.parse("ACME, Inc."), Ok("ACME, Inc.".to_owned()));

        assert_eq!(c.parse("2024 ACME, Inc."), Ok("2024 ACME, Inc.".to_owned()));

        assert_eq!(p.parse("! PD"), Ok((Some("PD".to_owned()), None)));
        assert_eq!(
            p.parse("! MIT; (c) ACME, Inc."),
            Ok((Some("MIT".to_owned()), Some("ACME, Inc.".to_owned())))
        );
        assert_eq!(
            p.parse("! MIT; (C) ACME, Inc."),
            Ok((Some("MIT".to_owned()), Some("ACME, Inc.".to_owned())))
        );
        assert_eq!(
            p.parse("! MIT; © ACME, Inc."),
            Ok((Some("MIT".to_owned()), Some("ACME, Inc.".to_owned())))
        );
        assert_eq!(
            p.parse("! MIT; (c) 2024 ACME, Inc."),
            Ok((Some("MIT".to_owned()), Some("2024 ACME, Inc.".to_owned())))
        );
        assert_eq!(
            p.parse("! CC BY-SA 3.0 [IGO]; (c) 2024 ACME, Inc."),
            Ok((
                Some("CC BY-SA 3.0 [IGO]".to_owned()),
                Some("2024 ACME, Inc.".to_owned())
            ))
        );
    }

    #[test]
    fn check_header_template() {
        let t = grammar::templateParser::new();
        let p = grammar::template_lineParser::new();

        assert_eq!(t.parse("checklist"), Ok("checklist".to_owned()));
        assert_eq!(t.parse("checklist,v1"), Ok("checklist,v1".to_owned()));
        assert_eq!(t.parse("checklist-v1.0"), Ok("checklist-v1.0".to_owned()));

        assert_eq!(p.parse("& nasa"), Ok(Some("nasa".to_owned())));
        assert_eq!(
            p.parse("& nasa-flight-plan,v4.0"),
            Ok(Some("nasa-flight-plan,v4.0".to_owned()))
        );
    }

    #[test]
    fn check_type_definitions() {
        let f = grammar::formaParser::new();
        let g = grammar::genusParser::new();

        assert_eq!(
            f.parse("A"),
            Ok(Forma {
                name: "A".to_owned()
            })
        );

        assert_eq!(
            g.parse("A"),
            Ok(Genus::Single(Forma {
                name: "A".to_owned()
            }))
        );
    }

    #[test]
    fn check_procedure_signature() {
        let p = grammar::signatureParser::new();

        assert_eq!(
            p.parse("A -> B"),
            Ok(Signature {
                domain: Genus::Single(Forma {
                    name: "A".to_owned()
                }),
                range: Genus::Single(Forma {
                    name: "B".to_owned()
                })
            })
        );
        assert!(p
            .parse("A ->")
            .is_err());
        assert!(p
            .parse("A")
            .is_err());
    }

    #[test]
    fn check_procedure_declaration() {
        let d = grammar::declarationParser::new();

        assert_eq!(d.parse("making_coffee :"), Ok("making_coffee".to_owned()));

        let p = grammar::declaration_lineParser::new();

        assert_eq!(
            p.parse("f :"),
            Ok(Procedure {
                name: "f".to_owned(),
                signature: None
            })
        );

        assert!(p
            .parse("cook-pizza :B")
            .is_err());

        assert_eq!(
            p.parse("f : A -> B"),
            Ok(Procedure {
                name: "f".to_owned(),
                signature: Some(Signature {
                    domain: Genus::Single(Forma {
                        name: "A".to_owned()
                    }),
                    range: Genus::Single(Forma {
                        name: "B".to_owned()
                    })
                })
            })
        );
    }

    #[test]
    fn check_attribute_role() {
        let a = grammar::attributeParser::new();

        assert_eq!(
            a.parse("@chef"),
            Ok(Attribute {
                name: "chef".to_owned()
            })
        );

        let p = grammar::attribute_lineParser::new();

        assert_eq!(
            p.parse("@chef"),
            Ok(vec![Attribute {
                name: "chef".to_owned()
            }])
        );
        assert_eq!(
            p.parse("@chef + @sous"),
            Ok(vec![
                Attribute {
                    name: "chef".to_owned()
                },
                Attribute {
                    name: "sous".to_owned()
                }
            ])
        );
    }

    // the verify_*() functions are where we do verificaton of larger composite
    // structures built up from the smaller pieces check_*()'d above.

    #[test]
    fn verify_technique_header() {
        let p = grammar::technique_fileParser::new();

        assert_eq!(
            p.parse("% technique v1"),
            Ok(Technique {
                version: 1,
                license: None,
                copyright: None,
                template: None
            })
        );

        assert_eq!(
            p.parse(
                r#"
% technique v1
! MIT; (c) ACME, Inc
& checklist
            "#
            ),
            Ok(Technique {
                version: 1,
                license: Some("MIT".to_owned()),
                copyright: Some("ACME, Inc".to_owned()),
                template: Some("checklist".to_owned())
            })
        );
    }
*/

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
*/
