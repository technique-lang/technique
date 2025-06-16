#![allow(unused_variables)]
#![allow(dead_code)]

use regex::Regex;
use technique::language::*;

use super::scope::*;

pub fn parse_via_scopes(content: &str) {
    let mut input = Parser::new();
    input.initialize(content);

    let result = input.parse_technique_header();
    println!("{:?}", result);
    println!("{:?}", input);

    std::process::exit(0);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ParsingError {
    IllegalParserState,
    ZeroLengthToken,
    Unrecognized, // improve this
    InvalidHeader,
    ValidationFailure(ValidationError),
    InvalidCharacter(char),
    UnexpectedEndOfInput,
    InvalidForma,
    InvalidGenus,
}

impl From<ValidationError> for ParsingError {
    fn from(error: ValidationError) -> Self {
        ParsingError::ValidationFailure(error)
    }
}

#[derive(Debug)]
struct Parser<'i> {
    scope: Scope,
    source: &'i str,
    offset: usize,
    count: usize,
}

impl<'i> Parser<'i> {
    fn new() -> Parser<'i> {
        Parser {
            scope: Scope::new(),
            source: "",
            offset: 0,
            count: 0,
        }
    }

    fn initialize(&mut self, content: &'i str) {
        self.scope = Scope::new();
        self.source = content;
        self.count = 0;
        self.offset = 0;
    }

    fn parse_from_start(&mut self) -> Result<(), ParsingError> {
        let layer = self
            .scope
            .current();

        match layer {
            Layer::Technique => (), // this is where we should be
            _ => return Err(ParsingError::IllegalParserState),
        }

        let header = self.parse_technique_header()?;
        Ok(()) // FIXME
    }

    fn parse_newline(&mut self) -> Result<(), ParsingError> {
        for (i, c) in self
            .source
            .char_indices()
        {
            let l = i + 1;

            if c == '\n' {
                self.source = &self.source[l..];
                self.count += 1;
                self.offset += l;
                return Ok(());
            } else if c.is_ascii_whitespace() {
                continue;
            } else {
                return Err(ParsingError::InvalidCharacter(c));
            }
        }

        // We don't actually require a newline to end the file.

        self.source = "";
        self.offset += self
            .source
            .len();
        Ok(())
        // Err(ParsingError::UnexpectedEndOfInput)
    }

    // hard wire the version for now. If we ever grow to supporting multiple
    // major versions then this will become a lot more complicated.
    fn parse_magic_line(&mut self) -> Result<u8, ParsingError> {
        let re = Regex::new(r"%\s*technique\s+v1").unwrap();

        let line = self
            .source
            .lines()
            .next()
            .unwrap();

        let m = re
            .find(self.source)
            .ok_or(ParsingError::Unrecognized)?;

        let l = m.end();

        self.source = &self.source[l..];
        self.offset += l;

        Ok(1)
    }

    // This one is awkward because if a SPDX line is present, then it really needs
    // to have a license, whereas the copyright part is optional.
    fn parse_spdx_line(&mut self) -> Result<(Option<&'i str>, Option<&'i str>), ParsingError> {
        // First establish we have a valid line.

        if self
            .source
            .len()
            == 0
        {
            return Ok((None, None));
        }

        let x = self
            .source
            .chars()
            .next()
            .unwrap();

        if x != '!' {
            return Err(ParsingError::InvalidHeader);
        }

        let mut lines = self
            .source
            .lines();
        let line = lines
            .next()
            .unwrap();

        let re = Regex::new(r"!\s*([^;]+)(?:;\s*(?:\(c\)|\(C\)|©)\s*(.+))?").unwrap();

        let cap = re
            .captures(line)
            .ok_or(ParsingError::Unrecognized)?;

        // Get the length of the match as a whole so we can advance the parser
        // state later.

        let l = cap
            .get(0)
            .ok_or(ParsingError::Unrecognized)?
            .end();

        // Now to extracting the values we need. We get the license code from
        // the first capture. It must be present otherwise we don't have a
        // valid SPDX line (and we declared that we're on an SPDX line by the
        // presence of the '!' character at the beginning of the line).

        let one = cap
            .get(1)
            .map(|v| v.as_str())
            .ok_or(ParsingError::InvalidHeader)?;

        let one = validate_license(one)?;
        let one = Some(one);

        // Now dig out the copyright, if present:

        let two = cap
            .get(2)
            .map(|v| v.as_str());

        let two = match two {
            Some(text) => Some(validate_copyright(text)?),
            None => None,
        };

        // Advance the parser state, and return.

        self.source = &self.source[l..];

        Ok((one, two))
    }

    fn parse_template_line(&mut self) -> Result<Option<&'i str>, ParsingError> {
        let re = Regex::new(r"&\s*(.+)").unwrap();

        let cap = match re.captures(self.source) {
            Some(c) => c,
            None => return Ok(None),
        };

        let l = cap
            .get(0)
            .unwrap()
            .end();

        let one = cap
            .get(1)
            .map(|v| v.as_str())
            .ok_or(ParsingError::InvalidHeader)?;

        let one = validate_template(one)?;

        let one = Some(one);

        self.source = &self.source[l..];
        Ok(one)
    }

    fn parse_technique_header(&mut self) -> Result<Metadata<'i>, ParsingError> {
        let version = self.parse_magic_line()?;
        self.parse_newline()?;

        let (license, copyright) = self.parse_spdx_line()?;
        self.parse_newline()?;

        let template = self.parse_template_line()?;
        self.parse_newline()?;

        Ok(Metadata {
            version,
            license,
            copyright,
            template,
        })
    }

    fn parse_identifier(&mut self) -> Result<&'i str, ParsingError> {
        Ok("")
    }

    fn parse_forma(&mut self) -> Result<Forma<'i>, ParsingError> {
        let re = Regex::new(r"\s*(.+)").unwrap();

        let cap = match re.captures(self.source) {
            Some(c) => c,
            None => return Err(ParsingError::ZeroLengthToken),
        };

        let l = cap
            .get(0)
            .unwrap()
            .end();

        let one = cap
            .get(1)
            .map(|v| v.as_str())
            .ok_or(ParsingError::InvalidForma)?;

        let one = validate_forma(one)?;

        self.source = &self.source[l..];
        self.offset += l;
        Ok(one)
    }

    fn ensure_nonempty(&mut self) -> Result<(), ParsingError> {
        if self
            .source
            .len()
            == 0
        {
            return Err(ParsingError::UnexpectedEndOfInput);
        }
        Ok(())
    }

    fn trim_whitespace(&mut self) -> Result<(), ParsingError> {
        let mut l = 0;

        for (i, c) in self
            .source
            .char_indices()
        {
            if c == '\n' {
                break;
            } else if c.is_ascii_whitespace() {
                l = i + 1;
                continue;
            } else {
                break;
            }
        }

        self.source = &self.source[l..];
        self.offset += l;

        Ok(())
    }

    fn parse_genus(&mut self) -> Result<Genus<'i>, ParsingError> {
        self.trim_whitespace()?;
        self.ensure_nonempty()?;

        let first = self
            .source
            .chars()
            .next()
            .unwrap();

        match first {
            '[' => {
                // consume up to closing bracket
                let re = Regex::new(r"\[\s*(.+)\s*\]").unwrap();

                let cap = match re.captures(self.source) {
                    Some(c) => c,
                    None => return Err(ParsingError::ZeroLengthToken),
                };

                let l = cap
                    .get(0)
                    .unwrap()
                    .end();

                let one = cap
                    .get(1)
                    .map(|v| v.as_str())
                    .ok_or(ParsingError::InvalidGenus)?;

                let forma = validate_forma(one)?;

                self.source = &self.source[l..];
                self.offset += l;

                Ok(Genus::List(forma))
            }
            '(' => {
                // first trim off the parenthesis and whitespace
                let re = Regex::new(r"\(\s*(.*)\s*\)").unwrap();

                let cap = match re.captures(self.source) {
                    Some(c) => c,
                    None => return Err(ParsingError::ZeroLengthToken),
                };

                let l = cap
                    .get(0)
                    .unwrap()
                    .end();

                let one = cap
                    .get(1)
                    .map(|v| v.as_str())
                    .ok_or(ParsingError::InvalidGenus)?;

                if one.len() == 0 {
                    self.source = &self.source[l..];
                    self.offset += l;
                    return Ok(Genus::Unit);
                }

                // now split on , characters, and gather

                let mut formas: Vec<Forma<'i>> = Vec::new();

                for text in one.split(",") {
                    let text = text.trim();
                    let forma = validate_forma(text)?;
                    formas.push(forma);
                }

                self.source = &self.source[l..];
                self.offset += l;

                Ok(Genus::Tuple(formas))
            }
            _ => {
                let re = Regex::new(r"(.+)\s*").unwrap();

                let cap = match re.captures(self.source) {
                    Some(c) => c,
                    None => return Err(ParsingError::ZeroLengthToken),
                };

                let l = cap
                    .get(0)
                    .unwrap()
                    .end();

                let one = cap
                    .get(1)
                    .map(|v| v.as_str())
                    .ok_or(ParsingError::InvalidGenus)?;

                let forma = validate_forma(one)?;

                self.source = &self.source[l..];
                self.offset += l;

                Ok(Genus::Single(forma))
            }
        }
    }

    fn parse_procedure_declaration(
        &mut self,
    ) -> Result<(&'i str, Option<Signature<'i>>), ParsingError> {
        let name = self.parse_identifier()?;

        Ok((name, None))
    }

    fn parse_procedure(&mut self) -> Result<Procedure<'i>, ParsingError> {
        let (name, _) = self.parse_procedure_declaration()?;

        // let body = self.parse_body()?;
        self.parse_newline()?;

        Ok(Procedure {
            name,
            signature: None, // description: None
                             // body: None
        })
    }
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn magic_line() {
        let mut input = Parser::new();

        input.initialize("% technique v1");
        assert_eq!(input.parse_magic_line(), Ok(1));

        input.initialize("%technique v1");
        assert_eq!(input.parse_magic_line(), Ok(1));

        // this is rejected because the technique keyword isn't present.
        input.initialize("%techniquev1");
        assert_eq!(input.parse_magic_line(), Err(ParsingError::Unrecognized));
    }

    #[test]
    fn header_spdx() {
        let mut input = Parser::new();

        input.initialize("! PD");
        assert_eq!(input.parse_spdx_line(), Ok((Some("PD"), None)));

        input.initialize("! MIT; (c) ACME, Inc.");
        assert_eq!(
            input.parse_spdx_line(),
            Ok((Some("MIT"), Some("ACME, Inc.")))
        );

        input.initialize("! MIT; (C) 2024 ACME, Inc.");
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
    fn header_template() {
        let mut input = Parser::new();
        input.initialize("& checklist");
        assert_eq!(input.parse_template_line(), Ok(Some("checklist")));

        input.initialize("& nasa-flight-plan,v4.0");
        assert_eq!(
            input.parse_template_line(),
            Ok(Some("nasa-flight-plan,v4.0"))
        );
    }

    // now we test incremental parsing

    #[test]
    fn check_not_eof() {
        let mut input = Parser::new();
        input.initialize("Hello World");
        assert_eq!(input.ensure_nonempty(), Ok(()));

        input.initialize("");
        assert_eq!(
            input.ensure_nonempty(),
            Err(ParsingError::UnexpectedEndOfInput)
        );
    }

    #[test]
    fn consume_whitespace() {
        let mut input = Parser::new();
        input.initialize("  hello");
        assert_eq!(input.trim_whitespace(), Ok(()));
        assert_eq!(input.source, "hello");
    }

    #[test]
    fn single_genus_definitions() {
        let mut input = Parser::new();
        input.initialize("A");
        assert_eq!(input.parse_forma(), Ok(Forma { name: "A" }));

        input.initialize("A");
        assert_eq!(input.parse_genus(), Ok(Genus::Single(Forma { name: "A" })));
        assert_eq!(input.source, "");
    }

    #[test]
    fn list_genus_definitions() {
        let mut input = Parser::new();
        input.initialize("[A]");
        assert_eq!(input.parse_genus(), Ok(Genus::List(Forma { name: "A" })));
        assert_eq!(input.source, "");
    }

    #[test]
    fn tuple_genus_definitions() {
        let mut input = Parser::new();

        input.initialize("(A, B)");
        assert_eq!(
            input.parse_genus(),
            Ok(Genus::Tuple(vec![Forma { name: "A" }, Forma { name: "B" }]))
        );
        assert_eq!(input.source, "");

        // not actually sure whether we should be normalizing this? Probably
        // not, because formatting and linting is a separate concern.

        input.initialize("(A)");
        assert_eq!(
            input.parse_genus(),
            Ok(Genus::Tuple(vec![Forma { name: "A" }]))
        );
        assert_eq!(input.source, "");
    }

    #[test]
    fn unit_genus_definitions() {
        let mut input = Parser::new();

        // and now the special case of the unit type

        input.initialize("()");
        assert_eq!(input.parse_genus(), Ok(Genus::Unit));
        assert_eq!(input.source, "")
    }
}

#[cfg(test)]
mod verify {
    use super::*;

    #[test]
    fn technique_header() {
        let mut input = Parser::new();
        input.initialize("% technique v1");

        assert_eq!(
            input.parse_technique_header(),
            Ok(Metadata {
                version: 1,
                license: None,
                copyright: None,
                template: None
            })
        );

        input.initialize(
            r#"
% technique v1
! MIT; (c) ACME, Inc
& checklist
            "#,
        );
        assert_eq!(
            input.parse_technique_header(),
            Ok(Metadata {
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

        }
    */
*/
