// Abstract Syntax Trees for the Technique language

use regex::Regex;

#[derive(Eq, Debug, PartialEq)]
pub struct Technique<'i> {
    pub version: u8,
    pub license: Option<&'i str>,
    pub copyright: Option<&'i str>,
    pub template: Option<&'i str>,
}

impl Default for Technique<'_> {
    fn default() -> Self {
        Technique {
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
    InvalidIdentifier,
    InvalidForma,
}

#[derive(Eq, Debug, PartialEq)]
pub struct Procedure<'i> {
    pub name: &'i str,
    pub signature: Option<Signature<'i>>,
}

#[derive(Eq, Debug, PartialEq)]
pub struct Forma<'i> {
    pub name: &'i str,
}

#[derive(Eq, Debug, PartialEq)]
pub enum Genus<'i> {
    Single(Forma<'i>),
    Tuple(Vec<Forma<'i>>),
    List(Forma<'i>),
}

#[derive(Eq, Debug, PartialEq)]
pub struct Signature<'i> {
    pub domain: Genus<'i>,
    pub range: Genus<'i>,
}

#[derive(Eq, Debug, PartialEq)]
pub struct Attribute<'i> {
    pub name: &'i str,
}

// HERE ALSO ... validating identifiers should be here!!!

pub fn validate_identifier(input: &str) -> Result<&str, ValidationError> {
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

    fn maker<'i>() -> Technique<'i> {
        let t1 = Technique {
            version: 1,
            license: None,
            copyright: None,
            template: None,
        };

        t1
    }

    #[test]
    fn check_ast_construction() {
        let t1 = Technique {
            version: 1,
            license: None,
            copyright: None,
            template: None,
        };

        assert_eq!(Technique::default(), t1);

        let t2 = Technique {
            version: 1,
            license: Some("MIT"),
            copyright: Some("ACME, Inc"),
            template: Some("checklist"),
        };

        let t3 = maker();

        assert_eq!(t3, t1);

        let t4 = Technique {
            license: Some("MIT"),
            copyright: Some("ACME, Inc"),
            template: Some("checklist"),
            ..t3
        };

        assert_eq!(t4, t2);
    }
}
