// Abstract Syntax Trees for the Technique language

use regex::Regex;

#[derive(Eq, Debug, PartialEq)]
pub struct Technique<'i> {
    pub header: Option<Metadata<'i>>,
    pub body: Option<Vec<Procedure<'i>>>,
}

#[derive(Eq, Debug, PartialEq)]
pub struct Metadata<'i> {
    pub version: u8,
    pub license: Option<&'i str>,
    pub copyright: Option<&'i str>,
    pub template: Option<&'i str>,
}

impl Default for Metadata<'_> {
    fn default() -> Self {
        Metadata {
            version: 1,
            license: None,
            copyright: None,
            template: None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValidationError {
    ZeroLengthToken,
    InvalidLicense,
    InvalidCopyright,
    InvalidTemplate,
    InvalidIdentifier,
    InvalidForma,
    InvalidGenus,
    InvalidInvocation,
}

#[derive(Eq, Debug, PartialEq)]
pub struct Procedure<'i> {
    pub name: Identifier<'i>,
    pub signature: Option<Signature<'i>>,
    pub title: Option<&'i str>,
    pub description: Option<&'i str>,
}

#[derive(Eq, Debug, PartialEq)]
pub struct Identifier<'i>(pub &'i str);

#[derive(Eq, Debug, PartialEq)]
pub struct Forma<'i>(pub &'i str);

#[derive(Eq, Debug, PartialEq)]
pub enum Genus<'i> {
    Unit,
    Single(Forma<'i>),
    Tuple(Vec<Forma<'i>>),
    List(Forma<'i>),
}

#[derive(Eq, Debug, PartialEq)]
pub struct Signature<'i> {
    pub domain: Genus<'i>,
    pub range: Genus<'i>,
}

// now types for procedure bodies

#[derive(Eq, Debug, PartialEq)]
pub struct Invocation<'i> {
    pub target: Identifier<'i>,
    pub parameters: Option<Vec<Identifier<'i>>>,
}

#[derive(Eq, Debug, PartialEq)]
pub struct Attribute<'i>(pub &'i str);

pub fn validate_identifier(input: &str) -> Result<Identifier, ValidationError> {
    if input.len() == 0 {
        return Err(ValidationError::ZeroLengthToken);
    }

    let re = Regex::new(r"^[a-z][a-z0-9_]*$").unwrap();
    if re.is_match(input) {
        Ok(Identifier(input))
    } else {
        Err(ValidationError::InvalidIdentifier)
    }
}

pub fn validate_forma(input: &str) -> Result<Forma, ValidationError> {
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

    Ok(Forma(input))
}

/// This one copes with (and discards) any internal whitespace encountered.
pub fn validate_genus(input: &str) -> Result<Genus, ValidationError> {
    let first = input
        .chars()
        .next()
        .unwrap();

    match first {
        '[' => {
            // consume up to closing bracket
            let re = Regex::new(r"\[\s*(.+)\s*\]").unwrap();

            let cap = match re.captures(input) {
                Some(c) => c,
                None => return Err(ValidationError::ZeroLengthToken),
            };

            let one = cap
                .get(1)
                .ok_or(ValidationError::InvalidGenus)?;

            let forma = validate_forma(one.as_str())?;

            Ok(Genus::List(forma))
        }
        '(' => {
            // first trim off the parenthesis and whitespace
            let re = Regex::new(r"\(\s*(.*)\s*\)").unwrap();

            let cap = match re.captures(input) {
                Some(c) => c,
                None => return Err(ValidationError::ZeroLengthToken),
            };

            let one = cap
                .get(1)
                .ok_or(ValidationError::InvalidGenus)?;

            if one.len() == 0 {
                return Ok(Genus::Unit);
            }

            // now split on , characters, and gather

            let mut formas: Vec<Forma> = Vec::new();

            for text in one
                .as_str()
                .split(",")
            {
                let text = text.trim();
                let forma = validate_forma(text)?;
                formas.push(forma);
            }

            Ok(Genus::Tuple(formas))
        }
        _ => {
            if input.len() == 0 {
                return Err(ValidationError::ZeroLengthToken);
            };

            let forma = validate_forma(input)?;

            Ok(Genus::Single(forma))
        }
    }
}

// the validate functions all need to have start and end anchors, which seems
// like it should be abstracted away.

pub fn validate_license(input: &str) -> Result<&str, ValidationError> {
    let re = Regex::new(r"^[A-Za-z0-9.,\-_ \(\)\[\]]+$").unwrap();

    if re.is_match(input) {
        Ok(input)
    } else {
        Err(ValidationError::InvalidLicense)
    }
}

pub fn validate_copyright(input: &str) -> Result<&str, ValidationError> {
    let re = Regex::new(r"^[A-Za-z0-9.,\-_ \(\)\[\]]+$").unwrap();

    if re.is_match(input) {
        Ok(input)
    } else {
        Err(ValidationError::InvalidCopyright)
    }
}

pub fn validate_template(input: &str) -> Result<&str, ValidationError> {
    let re = Regex::new(r"^[A-Za-z0-9.,\-]+$").unwrap();

    if re.is_match(input) {
        Ok(input)
    } else {
        Err(ValidationError::InvalidTemplate)
    }
}

pub fn validate_invocation(input: &str) -> Result<Invocation, ValidationError> {
    if input.len() == 0 {
        return Err(ValidationError::ZeroLengthToken);
    }

    let re = Regex::new(r"^<(.+?)>\s*(?:(\(.*?\)))?$").unwrap();

    let cap = match re.captures(input) {
        Some(c) => c,
        None => return Err(ValidationError::InvalidInvocation),
    };

    let one = cap
        .get(1)
        .ok_or(ValidationError::InvalidInvocation)?;

    let target = validate_identifier(one.as_str())?;

    let parameters = match cap.get(2) {
        Some(two) => {
            let mut parameters: Vec<Identifier> = Vec::new();

            // trim leading ( and trailing ) off
            let body = two.as_str();
            let texts = &body[1..body.len() - 1].trim();

            if !texts.is_empty() {
                for text in texts.split(",") {
                    let text = text.trim();
                    let parameter = validate_identifier(text)?;
                    parameters.push(parameter);
                }
            }

            Some(parameters)
        }
        None => None,
    };

    Ok(Invocation { target, parameters })
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn identifier_rules() {
        assert_eq!(validate_identifier("a"), Ok(Identifier("a")));
        assert_eq!(validate_identifier("ab"), Ok(Identifier("ab")));
        assert_eq!(validate_identifier("johnny5"), Ok(Identifier("johnny5")));
        assert_eq!(
            validate_identifier("Pizza"),
            Err(ValidationError::InvalidIdentifier)
        );
        assert_eq!(
            validate_identifier("pizZa"),
            Err(ValidationError::InvalidIdentifier)
        );
        assert!(validate_identifier("0trust").is_err());
        assert_eq!(
            validate_identifier("make_dinner"),
            Ok(Identifier("make_dinner"))
        );
        assert!(validate_identifier("MakeDinner").is_err());
        assert!(validate_identifier("make-dinner").is_err());
    }

    #[test]
    fn forma_rules() {
        assert_eq!(validate_forma("A"), Ok(Forma("A")));
        assert_eq!(validate_forma("Beans"), Ok(Forma("Beans")));
        assert_eq!(validate_forma("lower"), Err(ValidationError::InvalidForma));
        assert_eq!(
            validate_forma("0Degrees"),
            Err(ValidationError::InvalidForma)
        );
    }

    #[test]
    fn genus_rules_single() {
        assert_eq!(validate_genus("A"), Ok(Genus::Single(Forma("A"))));
    }

    #[test]
    fn genus_rules_list() {
        assert_eq!(validate_genus("[A]"), Ok(Genus::List(Forma("A"))));
    }

    #[test]
    fn genus_rules_tuple() {
        assert_eq!(
            validate_genus("(A, B)"),
            Ok(Genus::Tuple(vec![Forma("A"), Forma("B")]))
        );

        assert_eq!(
            validate_genus("(Coffee, Tea)"),
            Ok(Genus::Tuple(vec![Forma("Coffee"), Forma("Tea")]))
        );

        // not actually sure whether we should be normalizing this? Probably
        // not, because formatting and linting is a separate concern.

        assert_eq!(validate_genus("(A)"), Ok(Genus::Tuple(vec![Forma("A")])));
    }

    #[test]
    fn genus_rules_unit() {
        assert_eq!(validate_genus("()"), Ok(Genus::Unit));
    }

    #[test]
    fn license_rules() {
        assert_eq!(validate_license("MIT"), Ok("MIT"));
        assert_eq!(validate_license("Public Domain"), Ok("Public Domain"));
        assert_eq!(validate_license("CC BY-SA 3.0 IGO"), Ok("CC BY-SA 3.0 IGO"));
    }

    #[test]
    fn copyright_rules() {
        assert_eq!(validate_copyright("ACME"), Ok("ACME"));
        assert_eq!(validate_copyright("lower"), Ok("lower"));
        assert_eq!(validate_copyright("ACME, Inc"), Ok("ACME, Inc"));
        assert_eq!(validate_copyright("2024 ACME, Inc."), Ok("2024 ACME, Inc."));
    }

    #[test]
    fn template_rules() {
        assert_eq!(validate_template("checklist"), Ok("checklist"));
        assert_eq!(validate_template("checklist,v1"), Ok("checklist,v1"));
        assert_eq!(validate_template("checklist-v1.0"), Ok("checklist-v1.0"));
    }

    #[test]
    fn invocation_rules() {
        assert_eq!(
            validate_invocation("<hello>"),
            Ok(Invocation {
                target: Identifier("hello"),
                parameters: None
            })
        );

        assert_eq!(
            validate_invocation("<hello_world>"),
            Ok(Invocation {
                target: Identifier("hello_world"),
                parameters: None
            })
        );

        assert_eq!(
            validate_invocation("<hello_world>()"),
            Ok(Invocation {
                target: Identifier("hello_world"),
                parameters: Some(vec![])
            })
        );

        assert_eq!(
            validate_invocation("<greet>(name)"),
            Ok(Invocation {
                target: Identifier("greet"),
                parameters: Some(vec![Identifier("name")])
            })
        );

        assert_eq!(
            validate_invocation("<start_finish>(alpha, omega)"),
            Ok(Invocation {
                target: Identifier("start_finish"),
                parameters: Some(vec![Identifier("alpha"), Identifier("omega")])
            })
        );
    }

    fn maker<'i>() -> Metadata<'i> {
        let t1 = Metadata {
            version: 1,
            license: None,
            copyright: None,
            template: None,
        };

        t1
    }

    #[test]
    fn ast_construction() {
        let t1 = Metadata {
            version: 1,
            license: None,
            copyright: None,
            template: None,
        };

        assert_eq!(Metadata::default(), t1);

        let t2 = Metadata {
            version: 1,
            license: Some("MIT"),
            copyright: Some("ACME, Inc"),
            template: Some("checklist"),
        };

        let t3 = maker();

        assert_eq!(t3, t1);

        let t4 = Metadata {
            license: Some("MIT"),
            copyright: Some("ACME, Inc"),
            template: Some("checklist"),
            ..t3
        };

        assert_eq!(t4, t2);
    }
}
