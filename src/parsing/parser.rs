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
    Unimplemented,
    ZeroLengthToken,
    Unrecognized, // improve this
    InvalidHeader,
    ValidationFailure(ValidationError),
    InvalidCharacter(char),
    UnexpectedEndOfInput,
    InvalidIdentifier,
    InvalidForma,
    InvalidGenus,
    InvalidSignature,
    InvalidDeclaration,
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

        let _header = self.parse_technique_header()?;
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
        self.offset += l;

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
        self.offset += l;

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

    fn parse_identifier(&mut self) -> Result<Identifier<'i>, ParsingError> {
        Err(ParsingError::Unimplemented)
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

        let re = match first {
            '[' => {
                // consume up to closing bracket
                Regex::new(r"\[.+?\]").unwrap()
            }
            '(' => {
                // consume up to closing parenthesis
                Regex::new(r"\(.*?\)").unwrap()
            }
            _ => Regex::new(r".+?").unwrap(),
        };

        let cap = match re.captures(self.source) {
            Some(c) => c,
            None => return Err(ParsingError::InvalidGenus),
        };

        let l = cap
            .get(0)
            .unwrap()
            .end();

        let zero = cap
            .get(0)
            .map(|v| v.as_str())
            .ok_or(ParsingError::ZeroLengthToken)?;

        let genus = validate_genus(zero)?;

        self.source = &self.source[l..];
        self.offset += l;

        Ok(genus)
    }

    fn parse_signature(&mut self) -> Result<Signature<'i>, ParsingError> {
        let re = Regex::new(r"\s*(.+?)\s*->\s*(.+?)\s*$").unwrap();

        let cap = match re.captures(self.source) {
            Some(c) => c,
            None => return Err(ParsingError::InvalidSignature),
        };

        let l = cap
            .get(0)
            .unwrap()
            .end();

        let one = cap
            .get(1)
            .map(|v| v.as_str())
            .ok_or(ParsingError::ZeroLengthToken)?;

        let two = cap
            .get(2)
            .map(|v| v.as_str())
            .ok_or(ParsingError::ZeroLengthToken)?;

        let domain = validate_genus(one)?;
        let range = validate_genus(two)?;

        Ok(Signature { domain, range })
    }

    /// declarations are of the form
    ///
    ///     identifier : signature
    ///
    /// where the optional signature is
    ///
    ///     genus -> genus
    ///
    fn parse_procedure_declaration(
        &mut self,
    ) -> Result<(Identifier<'i>, Option<Signature<'i>>), ParsingError> {
        // These capture groups use .+? to make "match more than one, but
        // lazily" so that the subsequent grabs of whitespace and the all
        // important ':' character are not absorbed.
        let re = Regex::new(r"^\s*(.+?)\s*:\s*(.+?)?\s*$").unwrap();

        let cap = match re.captures(self.source) {
            Some(c) => c,
            None => return Err(ParsingError::InvalidDeclaration),
        };

        let l = cap
            .get(0)
            .unwrap()
            .end();

        let one = cap
            .get(1)
            .map(|v| v.as_str())
            .ok_or(ParsingError::ZeroLengthToken)?;

        let name = self.parse_identifier()?;

        let two = cap
            .get(2)
            .map(|v| v.as_str())
            .ok_or(ParsingError::ZeroLengthToken)?;

        self.parse_signature()?;

        self.source = &self.source[l..];
        self.offset += l;

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
        assert_eq!(input.parse_forma(), Ok(Forma("A")));

        input.initialize("A");
        assert_eq!(input.parse_genus(), Ok(Genus::Single(Forma("A"))));
        assert_eq!(input.source, "");
    }

    #[test]
    fn list_genus_definitions() {
        let mut input = Parser::new();
        input.initialize("[A]");
        assert_eq!(input.parse_genus(), Ok(Genus::List(Forma("A"))));
        assert_eq!(input.source, "");
    }

    #[test]
    fn tuple_genus_definitions() {
        let mut input = Parser::new();

        input.initialize("(A, B)");
        assert_eq!(
            input.parse_genus(),
            Ok(Genus::Tuple(vec![Forma("A"), Forma("B")]))
        );
        assert_eq!(input.source, "");

        // not actually sure whether we should be normalizing this? Probably
        // not, because formatting and linting is a separate concern.

        input.initialize("(A)");
        assert_eq!(input.parse_genus(), Ok(Genus::Tuple(vec![Forma("A")])));
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

    #[test]
    fn signatures() {
        let mut input = Parser::new();

        input.initialize("A -> B");
        assert_eq!(
            input.parse_signature(),
            Ok(Signature {
                domain: Genus::Single(Forma("A")),
                range: Genus::Single(Forma("B"))
            })
        );

        input.initialize("Beans -> Coffee");
        assert_eq!(
            input.parse_signature(),
            Ok(Signature {
                domain: Genus::Single(Forma("Beans")),
                range: Genus::Single(Forma("Coffee"))
            })
        );

        input.initialize("[Bits] -> Bob");
        assert_eq!(
            input.parse_signature(),
            Ok(Signature {
                domain: Genus::List(Forma("Bits")),
                range: Genus::Single(Forma("Bob"))
            })
        );

        input.initialize("Complex -> (Real, Imaginary)");
        assert_eq!(
            input.parse_signature(),
            Ok(Signature {
                domain: Genus::Single(Forma("Complex")),
                range: Genus::Tuple(vec![Forma("Real"), Forma("Imaginary")])
            })
        );
    }

    #[test]
    fn declarations_simple() {
        let mut input = Parser::new();

        input.initialize("making_coffee :");
        assert_eq!(
            input.parse_procedure_declaration(),
            Ok((Identifier("making_coffee"), None))
        );
    }

    #[test]
    fn declarations_full() {
        let mut input = Parser::new();

        input.initialize("f : A -> B");
        assert_eq!(
            input.parse_procedure_declaration(),
            Ok((
                Identifier("f"),
                Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                })
            ))
        );

        input.initialize("making_coffee : Beans -> Coffee");
        assert_eq!(
            input.parse_procedure_declaration(),
            Ok((
                Identifier("making_coffee"),
                Some(Signature {
                    domain: Genus::Single(Forma("Beans")),
                    range: Genus::Single(Forma("Coffee"))
                })
            ))
        );

        input.initialize("making_coffee : (Beans, Milk) -> Coffee");
        assert_eq!(
            input.parse_procedure_declaration(),
            Ok((
                Identifier("making_coffee"),
                Some(Signature {
                    domain: Genus::Tuple(vec![Forma("Beans"), Forma("Milk")]),
                    range: Genus::Single(Forma("Coffee"))
                })
            ))
        );
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
    fn check_procedure_signature() {
        let p = grammar::signatureParser::new();

        assert_eq!(
            p.parse(""),
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
