// Abstract Syntax Trees for the Technique language

use regex::Regex;

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

    Ok(Forma { name: input })
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

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn identifier_rules() {
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
    fn forma_rules() {
        assert_eq!(validate_forma("A"), Ok(Forma { name: "A" }));
        assert_eq!(validate_forma("Beans"), Ok(Forma { name: "Beans" }));
        assert_eq!(validate_forma("lower"), Err(ValidationError::InvalidForma));
        assert_eq!(
            validate_forma("0Degrees"),
            Err(ValidationError::InvalidForma)
        );
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
