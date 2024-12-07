#![allow(unused_variables)]
#![allow(dead_code)]

use regex::Regex;
use technique::language::*;

pub fn parse_via_string(content: &str) {
    let mut input = Parser::new();
    input.initialize(content);
    // println!("{:?}", result);
    std::process::exit(0);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValidationError {
    ZeroLengthToken,
    Unrecognized, // improve this
    InvalidHeader,
    InvalidIdentifier,
    InvalidForma,
}

struct Parser<'i> {
    source: &'i str,
    offset: usize,
}

impl<'i> Parser<'i> {
    fn new() -> Parser<'i> {
        Parser {
            source: "",
            offset: 0,
        }
    }

    fn initialize(&mut self, content: &'i str) {
        self.source = content;
        self.offset = 0;
    }

    // hardwire the version for now. If we ever grow to supporting multiple
    // major versions then this will become a lot more complicated.
    fn parse_magic_line(&mut self) -> Result<u8, ValidationError> {
        let re = Regex::new(r"%\s*technique\s+v1").unwrap();

        let m = re
            .find(self.source)
            .ok_or(ValidationError::Unrecognized)?;

        let l = m.end();

        self.source = &self.source[l..];

        Ok(1)
    }

    // This one is awkward because if a SPDX line is present, then it really needs
    // to have a license, whereas the copyright part is optional.
    fn parse_spdx_line(&mut self) -> Result<(Option<&str>, Option<&str>), ValidationError> {
        let re = Regex::new(r"!\s*([^;]+)(?:;\s*\(c\)\s*(.+))?").unwrap();

        let cap = re
            .captures(self.source)
            .ok_or(ValidationError::Unrecognized)?;

        let l = cap
            .get(0)
            .ok_or(ValidationError::Unrecognized)?
            .len();

        let one = cap
            .get(1)
            .map(|v| v.as_str())
            .ok_or(ValidationError::InvalidHeader)?;

        let one = validate_license(one)?;

        let one = Some(one);

        let two = cap
            .get(2)
            .map(|v| v.as_str());

        let two = match two {
            Some(text) => Some(validate_copyright(text)?),
            None => None,
        };

        self.source = &self.source[l..];

        Ok((one, two))
    }

    fn parse_template_line(&mut self) -> Result<Option<&str>, ValidationError> {
        let re = Regex::new(r"&\s*(.+)$").unwrap();

        let cap = re
            .captures(self.source)
            .ok_or(ValidationError::Unrecognized)?;

        let l = cap
            .get(0)
            .unwrap()
            .len();

        let one = cap
            .get(1)
            .map(|v| v.as_str())
            .ok_or(ValidationError::InvalidHeader)?;

        let one = validate_template(one)?;

        let one = Some(one);

        self.source = &self.source[l..];
        Ok(one)
    }
}

fn parse_technique_header<'i>(input: &'i str) -> Result<Technique<'i>, ValidationError> {
    let version = parse_magic_line(input)?;

    let (license, copyright) = parse_spdx_line(input)?;

    let template = parse_template_line(input)?;

    Ok(Technique {
        version: version,
        license: license,
        copyright: copyright,
        template: template,
    })
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

    Ok(Forma { name: input })
}

// the validate functions all need to have start and end anchors, which seems
// like it should be abstracted away.

fn validate_license(input: &str) -> Result<&str, ValidationError> {
    let re = Regex::new(r"^[A-Za-z0-9.,\-_ \(\)\[\]]+$").unwrap();

    if re.is_match(input) {
        Ok(input)
    } else {
        Err(ValidationError::InvalidHeader)
    }
}

fn validate_copyright(input: &str) -> Result<&str, ValidationError> {
    let re = Regex::new(r"^[A-Za-z0-9.,\-_ \(\)\[\]]+$").unwrap();

    if re.is_match(input) {
        Ok(input)
    } else {
        Err(ValidationError::InvalidHeader)
    }
}

fn validate_template(input: &str) -> Result<&str, ValidationError> {
    let re = Regex::new(r"^[A-Za-z0-9.,\-]+$").unwrap();

    if re.is_match(input) {
        Ok(input)
    } else {
        Err(ValidationError::InvalidHeader)
    }
}

fn validate_identifier(input: &str) -> Result<&str, ValidationError> {
    if input.len() == 0 {
        return Err(ValidationError::ZeroLengthToken);
    }

    let re = Regex::new(r"^[a-z][a-z0-9_]*$").unwrap();
    if re.is_match(input) {
        Ok(input)
    } else {
        Err(ValidationError::InvalidIdentifier)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_identifier_rules() {
        assert_eq!(validate_identifier("a"), Ok("a"));
        assert_eq!(validate_identifier("ab"), Ok("ab"));
        assert_eq!(validate_identifier("johnny5"), Ok("johnny5"));
        assert_eq!(
            validate_identifier("Pizza"),
            Err(ValidationError::InvalidIdentifier)
        );
        assert_eq!(
            validate_identifier("pizZa"),
            Err(ValidationError::InvalidIdentifier)
        );
        assert!(validate_identifier("0trust").is_err());
        assert_eq!(validate_identifier("make_dinner"), Ok("make_dinner"));
        assert!(validate_identifier("MakeDinner").is_err());
        assert!(validate_identifier("make-dinner").is_err());
    }

    #[test]
    fn check_magic_line() {
        let mut input = Parser::new();

        input.initialize("% technique v1");
        assert_eq!(input.parse_magic_line(), Ok(1));

        input.initialize("%technique v1");
        assert_eq!(input.parse_magic_line(), Ok(1));

        // this is rejected because the technique keyword isn't present.
        input.initialize("%techniquev1");
        assert_eq!(input.parse_magic_line(), Err(ValidationError::Unrecognized));
    }

    #[test]
    fn check_header_spdx() {
        let mut input = Parser::new();

        assert_eq!(validate_license("MIT"), Ok("MIT"));
        assert_eq!(validate_license("Public Domain"), Ok("Public Domain"));
        assert_eq!(validate_license("CC BY-SA 3.0 IGO"), Ok("CC BY-SA 3.0 IGO"));

        assert_eq!(validate_copyright("ACME"), Ok("ACME"));
        assert_eq!(validate_copyright("lower"), Ok("lower"));
        assert_eq!(validate_copyright("ACME, Inc"), Ok("ACME, Inc"));
        assert_eq!(validate_copyright("2024 ACME, Inc."), Ok("2024 ACME, Inc."));

        input.initialize("! PD");
        assert_eq!(input.parse_spdx_line(), Ok((Some("PD"), None)));

        input.initialize("! MIT; (c) ACME, Inc.");
        assert_eq!(
            input.parse_spdx_line(),
            Ok((Some("MIT"), Some("ACME, Inc.")))
        );

        input.initialize("! MIT; (c) 2024 ACME, Inc.");
        assert_eq!(
            input.parse_spdx_line(),
            Ok((Some("MIT"), Some("2024 ACME, Inc.")))
        );

        input.initialize("! CC BY-SA 3.0 [IGO]; (c) 2024 ACME, Inc.");
        assert_eq!(
            input.parse_spdx_line(),
            Ok((Some("CC BY-SA 3.0 [IGO]"), Some("2024 ACME, Inc.")))
        );
    }

    #[test]
    fn check_header_template() {
        assert_eq!(validate_template("checklist"), Ok("checklist"));
        assert_eq!(validate_template("checklist,v1"), Ok("checklist,v1"));
        assert_eq!(validate_template("checklist-v1.0"), Ok("checklist-v1.0"));


        let mut input = Parser::new();
        input.initialize("& nasa");
        assert_eq!(input.parse_template_line(), Ok(Some("nasa")));

        input.initialize("& nasa-flight-plan,v4.0");
        assert_eq!(
            input.parse_template_line(),
            Ok(Some("nasa-flight-plan,v4.0"))
        );
    }

    #[test]
    fn verify_technique_header() {
        assert_eq!(
            parse_technique_header("% technique v1"),
            Ok(Technique {
                version: 1,
                license: None,
                copyright: None,
                template: None
            })
        );

        assert_eq!(
            parse_technique_header(
                r#"
% technique v1
! MIT; (c) ACME, Inc
& checklist
            "#
            ),
            Ok(Technique {
                version: 1,
                license: Some("MIT"),
                copyright: Some("ACME, Inc"),
                template: Some("checklist")
            })
        );
    }
}

/*

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
