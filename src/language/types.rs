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

#[derive(Eq, Debug, PartialEq)]
pub struct Procedure<'i> {
    pub name: Identifier<'i>,
    pub parameters: Option<Vec<Identifier<'i>>>,
    pub signature: Option<Signature<'i>>,
    pub title: Option<&'i str>,
    pub description: Vec<Descriptive<'i>>,
    pub attribute: Vec<Attribute<'i>>,
    pub steps: Vec<Step<'i>>,
}

#[derive(Eq, Debug, PartialEq)]
pub struct Identifier<'i>(pub &'i str);

#[derive(Eq, Debug, PartialEq)]
pub struct External<'i>(pub &'i str);

#[derive(Eq, Debug, PartialEq)]
pub enum Target<'i> {
    Local(Identifier<'i>),
    Remote(External<'i>),
}

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
    pub target: Target<'i>,
    pub parameters: Option<Vec<Expression<'i>>>,
}

// types for descriptive content

#[derive(Eq, Debug, PartialEq)]
pub enum Descriptive<'i> {
    Text(&'i str),
    CodeBlock(Expression<'i>),
    Application(Invocation<'i>),
    Binding(Box<Descriptive<'i>>, Identifier<'i>),
}

// types for Steps within procedures

#[derive(Eq, Debug, PartialEq)]
pub struct Scope<'i> {
    pub roles: Vec<Attribute<'i>>, // empty for non-role scopes
    pub substeps: Vec<Step<'i>>,
}

#[derive(Eq, Debug, PartialEq)]
pub enum Step<'i> {
    Dependent {
        ordinal: &'i str,
        content: Vec<Descriptive<'i>>,
        responses: Vec<Response<'i>>,
        scopes: Vec<Scope<'i>>,
    },
    Parallel {
        content: Vec<Descriptive<'i>>,
        responses: Vec<Response<'i>>,
        scopes: Vec<Scope<'i>>,
    },
}

// enum responses like 'Yes' | 'No'

#[derive(Eq, Debug, PartialEq)]
pub struct Response<'i> {
    pub value: &'i str,
    pub condition: Option<&'i str>,
}

// attributes like @chef

#[derive(Eq, Debug, PartialEq)]
pub enum Attribute<'i> {
    Role(Identifier<'i>),
    Place(Identifier<'i>),
}

// now types used within code blocks

#[derive(Eq, Debug, PartialEq)]
pub struct Function<'i> {
    pub target: Identifier<'i>,
    pub parameters: Vec<Expression<'i>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression<'i> {
    Value(Identifier<'i>),
    String(&'i str),
    Multiline(&'i str),
    Repeat(Box<Expression<'i>>),
    Foreach(Identifier<'i>, Box<Expression<'i>>),
    Application(Invocation<'i>),
    Execution(Function<'i>),
    Binding(Box<Expression<'i>>, Identifier<'i>),
}

// the validate functions all need to have start and end anchors, which seems
// like it should be abstracted away.

pub fn validate_license(input: &str) -> Option<&str> {
    let re = Regex::new(r"^[A-Za-z0-9.,\-_ \(\)\[\]]+$").unwrap();

    if re.is_match(input) {
        Some(input)
    } else {
        None
    }
}

pub fn validate_copyright(input: &str) -> Option<&str> {
    let re = Regex::new(r"^[A-Za-z0-9.,\-_ \(\)\[\]]+$").unwrap();

    if re.is_match(input) {
        Some(input)
    } else {
        None
    }
}

pub fn validate_template(input: &str) -> Option<&str> {
    let re = Regex::new(r"^[A-Za-z0-9.,\-]+$").unwrap();

    if re.is_match(input) {
        Some(input)
    } else {
        None
    }
}

pub fn validate_identifier(input: &str) -> Option<Identifier> {
    if input.len() == 0 {
        return None;
    }

    let re = Regex::new(r"^[a-z][a-z0-9_]*$").unwrap();
    if re.is_match(input) {
        Some(Identifier(input))
    } else {
        None
    }
}

pub fn validate_forma(input: &str) -> Option<Forma> {
    if input.len() == 0 {
        return None;
    }

    let mut cs = input.chars();

    if !cs
        .next()
        .unwrap()
        .is_ascii_uppercase()
    {
        return None;
    }

    for c in cs {
        if !(c.is_ascii_uppercase() || c.is_ascii_lowercase() || c.is_ascii_digit()) {
            return None;
        }
    }

    Some(Forma(input))
}

fn parse_tuple(input: &str) -> Option<Genus> {
    let mut formas: Vec<Forma> = Vec::new();

    for text in input.split(",") {
        let text = text.trim();
        let forma = validate_forma(text)?;
        formas.push(forma);
    }

    Some(Genus::Tuple(formas))
}

/// This one copes with (and discards) any internal whitespace encountered.
pub fn validate_genus(input: &str) -> Option<Genus> {
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
                None => return None,
            };

            let one = cap.get(1)?;

            let forma = validate_forma(one.as_str())?;

            Some(Genus::List(forma))
        }
        '(' => {
            // first trim off the parenthesis and whitespace
            let re = Regex::new(r"\(\s*(.*)\s*\)").unwrap();

            let cap = match re.captures(input) {
                Some(c) => c,
                None => return None,
            };

            let one = cap.get(1)?;

            if one.len() == 0 {
                return Some(Genus::Unit);
            }

            // now split on , characters, and gather

            let mut formas: Vec<Forma> = Vec::new();

            if content.is_empty() {
                return Some(Genus::Unit);
            }

            parse_tuple(content)
        }
        _ => {
            if input.len() == 0 {
                return None;
            };

            // Check if this is a bare tuple (comma-separated but non-parenthesized)
            if input.contains(',') {
                parse_tuple(input)
            } else {
                let forma = validate_forma(input)?;

                Some(Genus::Single(forma))
            }
        }
    }
}

pub fn validate_response(input: &str) -> Option<Response> {
    if input.len() == 0 {
        return None;
    }

    // Handle conditions like 'Yes and equipment available'
    let re = Regex::new(r"^'(.*?)'(?:\s+(.+))?$").unwrap();
    let cap = re.captures(input)?;

    let value = cap
        .get(1)
        .unwrap()
        .as_str();

    let condition = match cap.get(2) {
        Some(two) => Some(two.as_str()),
        None => None,
    };

    Some(Response { value, condition })
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn identifier_rules() {
        assert_eq!(validate_identifier("a"), Some(Identifier("a")));
        assert_eq!(validate_identifier("ab"), Some(Identifier("ab")));
        assert_eq!(validate_identifier("johnny5"), Some(Identifier("johnny5")));
        assert_eq!(validate_identifier("Pizza"), None);
        assert_eq!(validate_identifier("pizZa"), None);
        assert!(validate_identifier("0trust").is_none());
        assert_eq!(
            validate_identifier("make_dinner"),
            Some(Identifier("make_dinner"))
        );
        assert!(validate_identifier("MakeDinner").is_none());
        assert!(validate_identifier("make-dinner").is_none());
    }

    #[test]
    fn forma_rules() {
        assert_eq!(validate_forma("A"), Some(Forma("A")));
        assert_eq!(validate_forma("Beans"), Some(Forma("Beans")));
        assert_eq!(validate_forma("lower"), None);
    }

    #[test]
    fn genus_rules_single() {
        assert_eq!(validate_genus("A"), Some(Genus::Single(Forma("A"))));
    }

    #[test]
    fn genus_rules_list() {
        assert_eq!(validate_genus("[A]"), Some(Genus::List(Forma("A"))));
    }

    #[test]
    fn genus_rules_tuple_parens() {
        assert_eq!(
            validate_genus("(A, B)"),
            Some(Genus::Tuple(vec![Forma("A"), Forma("B")]))
        );

        assert_eq!(
            validate_genus("(Coffee, Tea)"),
            Some(Genus::Tuple(vec![Forma("Coffee"), Forma("Tea")]))
        );

        // not actually sure whether we should be normalizing this? Probably
        // not, because formatting and linting is a separate concern.

        assert_eq!(validate_genus("(A)"), Some(Genus::Tuple(vec![Forma("A")])));
    }

    #[test]
    fn genus_rules_tuple_bare() {
        assert_eq!(
            validate_genus("A, B"),
            Some(Genus::Tuple(vec![Forma("A"), Forma("B")]))
        );

        assert_eq!(
            validate_genus("Coffee, Tea"),
            Some(Genus::Tuple(vec![Forma("Coffee"), Forma("Tea")]))
        );

        assert_eq!(
            validate_genus("Input, Data, Config"),
            Some(Genus::Tuple(vec![
                Forma("Input"),
                Forma("Data"),
                Forma("Config")
            ]))
        );

        assert_eq!(
            validate_genus("A,B"),
            Some(Genus::Tuple(vec![Forma("A"), Forma("B")]))
        );

        assert_eq!(
            validate_genus("A , B"),
            Some(Genus::Tuple(vec![Forma("A"), Forma("B")]))
        );

        // Test edge cases with whitespace
        assert_eq!(
            validate_genus("  A  ,  B  "),
            Some(Genus::Tuple(vec![Forma("A"), Forma("B")]))
        );

        assert_eq!(
            validate_genus("\tA\t,\tB\t"),
            Some(Genus::Tuple(vec![Forma("A"), Forma("B")]))
        );
    }

    #[test]
    fn genus_rules_unit() {
        assert_eq!(validate_genus("()"), Some(Genus::Unit));
    }

    #[test]
    fn license_rules() {
        assert_eq!(validate_license("MIT"), Some("MIT"));
        assert_eq!(validate_license("Public Domain"), Some("Public Domain"));
        assert_eq!(
            validate_license("CC BY-SA 3.0 IGO"),
            Some("CC BY-SA 3.0 IGO")
        );
    }

    #[test]
    fn copyright_rules() {
        assert_eq!(validate_copyright("ACME"), Some("ACME"));
        assert_eq!(validate_copyright("lower"), Some("lower"));
        assert_eq!(validate_copyright("ACME, Inc"), Some("ACME, Inc"));
        assert_eq!(
            validate_copyright("2024 ACME, Inc."),
            Some("2024 ACME, Inc.")
        );
    }

    #[test]
    fn template_rules() {
        assert_eq!(validate_template("checklist"), Some("checklist"));
        assert_eq!(validate_template("checklist,v1"), Some("checklist,v1"));
        assert_eq!(validate_template("checklist-v1.0"), Some("checklist-v1.0"));
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
