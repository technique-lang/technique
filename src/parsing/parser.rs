#![allow(dead_code)]

use regex::Regex;
use technique::language::*;

use super::scope::*;

pub fn parse_via_scopes(content: &str) {
    let mut input = Parser::new();
    input.initialize(content);

    let result = input.read_technique_header();
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
    Expected(&'static str),
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

/// Wrap parse results with the width consumed.
#[derive(Debug, PartialEq)]
struct Parsed<'i>(&'i str);

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

    fn advance(&mut self, parsed: Parsed) {
        let width = parsed
            .0
            .len();

        // advance the parser position
        self.source = &self.source[width..];
        self.offset += width;
    }

    fn parse_from_start(&mut self) -> Result<(), ParsingError> {
        let layer = self
            .scope
            .current();

        match layer {
            Layer::Technique => (), // this is where we should be
            _ => return Err(ParsingError::IllegalParserState),
        }

        let _header = self.read_technique_header()?;
        Ok(()) // FIXME
    }

    /// consume up to but not including newline (or end)
    fn take_line<A, F>(&mut self, f: F) -> Result<A, ParsingError>
    where
        F: Fn(&'i str) -> Result<A, ParsingError>,
    {
        match self
            .source
            .split_once('\n')
        {
            Some((before, after)) => {
                let result = f(before)?;

                self.source = after;
                self.offset += before.len() + 1;
                self.count += 1;
                Ok(result)
            }
            None => {
                let before = self.source;
                let result = f(before)?;

                self.source = "";
                self.offset += before.len() + 1;
                Ok(result)
            }
        }
    }

    fn entire(&self) -> &'i str {
        self.source
    }

    fn take_until<A, F, P>(&mut self, predicate: P, function: F) -> Result<A, ParsingError>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError>,
        P: Fn(&str) -> bool,
    {
        let mut i = 0;

        for line in self
            .source
            .lines()
        {
            if predicate(line) {
                // Found the predicate, include this line and stop
                break;
            }
            i += line.len() + 1; // plus newline
        }

        // Extract the substring from start to the found position
        let block = &self.source[..i];

        let mut parser = self.subparser(block);

        // Pass to closure for processing
        let result = function(&mut parser)?;

        // Advance parser state
        self.source = &self.source[i..];
        self.offset += i;

        Ok(result)
    }

    /// Given a string, fork a copy of the parser state and run a nested
    /// parser on that string. Does NOT advance the parent's parser state;
    /// the caller needs to do that via one of the take_*() methods.
    fn subparser(&self, content: &'i str) -> Parser<'i> {
        let parser = Parser {
            scope: self
                .scope
                .clone(),
            source: content,
            count: self.count,
            offset: self.offset,
        };

        // and return
        parser
    }

    fn read_newline(&mut self) -> Result<(), ParsingError> {
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
    }

    fn read_technique_header(&mut self) -> Result<Metadata<'i>, ParsingError> {
        // Process magic line
        let version = self.take_line(|content| {
            if is_magic_line(content) {
                Ok(parse_magic_line(content)?)
            } else {
                Err(ParsingError::Expected("The % symbol"))
            }
        })?;

        // Process SPDX line
        let (license, copyright) = self.take_line(|content| {
            if is_spdx_line(content) {
                Ok(parse_spdx_line(content)?)
            } else {
                Ok((None, None))
            }
        })?;

        // Process template line
        let template = self.take_line(|content| {
            if is_template_line(content) {
                Ok(parse_template_line(content)?)
            } else {
                Ok(None)
            }
        })?;

        Ok(Metadata {
            version,
            license,
            copyright,
            template,
        })
    }

    fn read_procedure(&mut self) -> Result<Procedure<'i>, ParsingError> {
        let declaration = self.take_until(is_procedure_declaration, |parser| {
            parser.take_line(|subcontent| {
                if is_procedure_declaration(subcontent) {
                    Ok(parse_procedure_declaration(subcontent)?)
                } else {
                    Err(ParsingError::Expected(
                        "Not sure what we expected, actually",
                    ))
                }
            })
        })?;

        let name = declaration.0;
        let signature = declaration.1;

        Ok(Procedure { name, signature })
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
}

fn parse_identifier(content: &str) -> Result<Identifier, ParsingError> {
    let result = validate_identifier(content)?;
    Ok(result)
}

fn parse_forma(content: &str) -> Result<Forma, ParsingError> {
    let result = validate_forma(content)?;
    Ok(result)
}

fn parse_genus(content: &str) -> Result<Genus, ParsingError> {
    let result = validate_genus(content)?;
    Ok(result)
}

/// A signature is of the form
///
/// genus -> genus
///
/// terminated by an end of line.

fn is_signature(content: &str) -> bool {
    let re = Regex::new(r"\s*.+?\s*->\s*.+?\s*$").unwrap();

    re.is_match(content)
}

fn parse_signature(content: &str) -> Result<Signature, ParsingError> {
    let re = Regex::new(r"\s*(.+?)\s*->\s*(.+?)\s*$").unwrap();

    let cap = match re.captures(content) {
        Some(c) => c,
        None => return Err(ParsingError::InvalidSignature),
    };

    let one = cap
        .get(1)
        .ok_or(ParsingError::Expected("a Genus for the domain"))?;

    let two = cap
        .get(2)
        .ok_or(ParsingError::Expected("a Genus for the range"))?;

    let domain = validate_genus(one.as_str())?;
    let range = validate_genus(two.as_str())?;

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
/// as above.

fn is_procedure_declaration(content: &str) -> bool {
    let re = Regex::new(r"^\s*(.+?)\s*:\s*(.+?)?\s*$").unwrap();

    re.is_match(content)
}

fn parse_procedure_declaration(
    content: &str,
) -> Result<(Identifier, Option<Signature>), ParsingError> {
    // These capture groups use .+? to make "match more than one, but
    // lazily" so that the subsequent grabs of whitespace and the all
    // important ':' character are not absorbed.
    let re = Regex::new(r"^\s*(.+?)\s*:\s*(.+?)?\s*$").unwrap();

    let cap = re
        .captures(content)
        .ok_or(ParsingError::InvalidDeclaration)?;

    let one = cap
        .get(1)
        .ok_or(ParsingError::Expected(
            "an Identifier for the procedure declaration",
        ))?;

    let name = validate_identifier(one.as_str())?;

    let signature = match cap.get(2) {
        Some(two) => {
            let result = parse_signature(two.as_str())?;
            Some(result)
        }
        None => None,
    };

    Ok((name, signature))
}

fn is_magic_line(content: &str) -> bool {
    let re = Regex::new(r"%\s*technique").unwrap();

    re.is_match(content)
}

// hard wire the version for now. If we ever grow to supporting multiple major
// versions then this will be a lot more complicated than just dealing with a
// different natural number here.
fn parse_magic_line(content: &str) -> Result<u8, ParsingError> {
    let re = Regex::new(r"%\s*technique\s+v1").unwrap();

    if re.is_match(content) {
        Ok(1)
    } else {
        Err(ParsingError::InvalidHeader)
    }
}

fn is_spdx_line(content: &str) -> bool {
    let re = Regex::new(r"!\s*[^;]+(?:;\s*.+)?").unwrap();

    re.is_match(content)
}

// This one is awkward because if a SPDX line is present, then it really needs
// to have a license, whereas the copyright part is optional.
fn parse_spdx_line(content: &str) -> Result<(Option<&str>, Option<&str>), ParsingError> {
    let re = Regex::new(r"^!\s*([^;]+)(?:;\s*(?:\(c\)|\(C\)|©)\s*(.+))?$").unwrap();

    let cap = re
        .captures(content)
        .ok_or(ParsingError::InvalidHeader)?;

    // Now to extracting the values we need. We get the license code from
    // the first capture. It must be present otherwise we don't have a
    // valid SPDX line (and we declared that we're on an SPDX line by the
    // presence of the '!' character at the beginning of the line).

    let one = cap
        .get(1)
        .ok_or(ParsingError::Expected("the license name"))?;

    let result = validate_license(one.as_str())?;
    let license = Some(result);

    // Now dig out the copyright, if present:

    let copyright = match cap.get(2) {
        Some(two) => {
            let result = validate_copyright(two.as_str())?;
            Some(result)
        }
        None => None,
    };

    Ok((license, copyright))
}

fn is_template_line(content: &str) -> bool {
    let re = Regex::new(r"&\s*.+").unwrap();

    re.is_match(content)
}

fn parse_template_line(content: &str) -> Result<Option<&str>, ParsingError> {
    let re = Regex::new(r"^&\s*(.+)$").unwrap();

    let cap = re
        .captures(content)
        .ok_or(ParsingError::InvalidHeader)?;

    let one = cap
        .get(1)
        .ok_or(ParsingError::Expected("a template name"))?;

    let result = validate_template(one.as_str())?;

    Ok(Some(result))
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn magic_line() {
        let content = "% technique v1";
        assert!(is_magic_line(content));

        let result = parse_magic_line(content);
        assert_eq!(result, Ok(1));

        let content = "%technique v1";
        assert!(is_magic_line(content));

        let result = parse_magic_line(content);
        assert_eq!(result, Ok(1));

        let content = "%techniquev1";
        assert!(is_magic_line(content));

        // this is rejected because the technique keyword isn't present.
        let result = parse_magic_line(content);
        assert!(result.is_err());
    }

    #[test]
    fn header_spdx() {
        let content = "! PD";
        assert!(is_spdx_line(content));

        let result = parse_spdx_line(content);
        assert_eq!(result, Ok((Some("PD"), None)));

        let content = "! MIT; (c) ACME, Inc.";
        assert!(is_spdx_line(content));

        let result = parse_spdx_line(content);
        assert_eq!(result, Ok((Some("MIT"), Some("ACME, Inc."))));

        let content = "! MIT; (C) 2024 ACME, Inc.";
        assert!(is_spdx_line(content));

        let result = parse_spdx_line(content);
        assert_eq!(result, Ok((Some("MIT"), Some("2024 ACME, Inc."))));

        let content = "! CC BY-SA 3.0 [IGO]; (c) 2024 ACME, Inc.";
        assert!(is_spdx_line(content));

        let result = parse_spdx_line(content);
        assert_eq!(
            result,
            Ok((Some("CC BY-SA 3.0 [IGO]"), Some("2024 ACME, Inc.")))
        );
    }

    #[test]
    fn header_template() {
        let content = "& checklist";
        assert!(is_template_line(content));

        let result = parse_template_line(content);
        assert_eq!(result, Ok(Some("checklist")));

        let content = "& nasa-flight-plan,v4.0";
        assert!(is_template_line(content));

        let result = parse_template_line(content);
        assert_eq!(result, Ok(Some("nasa-flight-plan,v4.0")));
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

    // It is not clear that we will ever actually need parse_identifier(),
    // parse_forma(), parse_genus(), or parse_signature() as they are not
    // called directly, but even though they are not used in composition of
    // the parse_procedure_declaration() parser, it is highly likely that
    // someday we will need to be able to parse them individually, perhaps for
    // a future language server or code highlighter. So we test them properly
    // here; in any event it exercises the underlying validate_*() codepaths.

    #[test]
    fn identifier_rules() {
        let input = "p";
        let result = parse_identifier(input);
        assert_eq!(result, Ok(Identifier("p")));

        let input = "pizza";
        let result = parse_identifier(input);
        assert_eq!(result, Ok(Identifier("pizza")));

        let input = "pizza0";
        let result = parse_identifier(input);
        assert_eq!(result, Ok(Identifier("pizza0")));

        let input = "0pizza";
        let result = parse_forma(input);
        assert!(result.is_err());

        let input = "cook_pizza";
        let result = parse_identifier(input);
        assert_eq!(result, Ok(Identifier("cook_pizza")));

        let input = "cook-pizza";
        let result = parse_forma(input);
        assert!(result.is_err());
    }

    #[test]
    fn forma_rules() {
        let input = "A";
        let result = parse_forma(input);
        assert_eq!(result, Ok(Forma("A")));

        let input = "Apple";
        let result = parse_forma(input);
        assert_eq!(result, Ok(Forma("Apple")));

        let input = "apple";
        let result = parse_forma(input);
        assert_eq!(
            result,
            Err(ParsingError::ValidationFailure(
                ValidationError::InvalidForma
            ))
        );
    }

    #[test]
    fn single_genus_definitions() {
        let input = "A";
        let result = parse_genus(input);
        assert_eq!(result, Ok(Genus::Single(Forma("A"))));

        let input = "Apple";
        let result = parse_genus(input);
        assert_eq!(result, Ok(Genus::Single(Forma("Apple"))));
    }

    #[test]
    fn list_genus_definitions() {
        let input = "[A]";
        let result = parse_genus(input);
        assert_eq!(result, Ok(Genus::List(Forma("A"))))
    }

    #[test]
    fn tuple_genus_definitions() {
        let input = "(A, B)";
        let result = parse_genus(input);
        assert_eq!(result, Ok(Genus::Tuple(vec![Forma("A"), Forma("B")])));

        // not actually sure whether we should be normalizing this? Probably
        // not, because formatting and linting is a separate concern.
        let input = "(A)";
        let result = parse_genus(input);
        assert_eq!(result, Ok(Genus::Tuple(vec![Forma("A")])));
    }

    #[test]
    fn unit_genus_definitions() {
        // and now the special case of the unit type
        let input = "()";
        let result = parse_genus(input);
        assert_eq!(result, Ok(Genus::Unit));
    }

    #[test]
    fn signatures() {
        let input = "A -> B";
        let result = parse_signature(input);
        assert_eq!(
            result,
            Ok(Signature {
                domain: Genus::Single(Forma("A")),
                range: Genus::Single(Forma("B"))
            })
        );

        let input = "Beans -> Coffee";
        let result = parse_signature(input);
        assert_eq!(
            result,
            Ok(Signature {
                domain: Genus::Single(Forma("Beans")),
                range: Genus::Single(Forma("Coffee"))
            })
        );

        let input = "[Bits] -> Bob";
        let result = parse_signature(input);
        assert_eq!(
            result,
            Ok(Signature {
                domain: Genus::List(Forma("Bits")),
                range: Genus::Single(Forma("Bob"))
            })
        );

        let input = "Complex -> (Real, Imaginary)";
        let result = parse_signature(input);
        assert_eq!(
            result,
            Ok(Signature {
                domain: Genus::Single(Forma("Complex")),
                range: Genus::Tuple(vec![Forma("Real"), Forma("Imaginary")])
            })
        );
    }

    #[test]
    fn declaration_simple() {
        let content = "making_coffee :";

        assert!(is_procedure_declaration(content));

        let result = parse_procedure_declaration(content);
        assert_eq!(result, Ok((Identifier("making_coffee"), None)));
    }

    #[test]
    fn declaration_full() {
        let content = "f : A -> B";
        assert!(is_procedure_declaration(content));

        let result = parse_procedure_declaration(content);
        assert_eq!(
            result,
            Ok((
                Identifier("f"),
                Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                })
            ))
        );

        let content = "making_coffee : (Beans, Milk) -> [Coffee]";
        assert!(is_procedure_declaration(content));

        let result = parse_procedure_declaration(content);
        assert_eq!(
            result,
            Ok((
                Identifier("making_coffee"),
                Some(Signature {
                    domain: Genus::Tuple(vec![Forma("Beans"), Forma("Milk")]),
                    range: Genus::List(Forma("Coffee"))
                })
            ))
        );

        let content = "f : B";
        // it should detect a procedure is being declared
        assert!(is_procedure_declaration(content));

        // but it is invalid
        let result = parse_procedure_declaration(content);
        assert!(result.is_err());
    }
}

#[cfg(test)]
mod verify {
    use super::*;

    fn trim(s: &str) -> &str {
        s.strip_prefix('\n')
            .unwrap_or(s)
    }

    #[test]
    fn detect_declarations() {
        let content = trim(
            r#"
making_coffee :
            "#,
        );

        assert!(is_procedure_declaration(content));
    }

    #[test]
    fn technique_header() {
        let mut input = Parser::new();
        input.initialize("% technique v1");

        let metadata = input.read_technique_header();
        assert_eq!(
            metadata,
            Ok(Metadata {
                version: 1,
                license: None,
                copyright: None,
                template: None
            })
        );

        input.initialize(trim(
            r#"
% technique v1
! MIT; (c) ACME, Inc
& checklist
            "#,
        ));

        let metadata = input.read_technique_header();
        assert_eq!(
            metadata,
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
*/
