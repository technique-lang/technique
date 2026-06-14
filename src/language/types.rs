//! Types representing an Abstract Syntax Tree for the Technique language

use crate::regex::*;

/// Byte range within the original source. `length` excludes trailing whitespace.
#[derive(Copy, Clone, Default, Eq, Debug, PartialEq, PartialOrd, Ord)]
pub struct Span {
    pub offset: usize,
    pub length: usize,
}

impl Span {
    pub const fn new(offset: usize, length: usize) -> Self {
        Span { offset, length }
    }
}

#[derive(Eq, Debug, PartialEq)]
pub struct Document<'i> {
    pub source: Option<&'i str>,
    pub header: Option<Metadata<'i>>,
    pub body: Option<Technique<'i>>,
}

#[derive(Eq, Debug)]
pub struct Metadata<'i> {
    pub version: u8,
    pub license: Option<&'i str>,
    pub copyright: Option<&'i str>,
    pub domain: Option<&'i str>,
    pub span: Span,
}

impl PartialEq for Metadata<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.version == other.version
            && self.license == other.license
            && self.copyright == other.copyright
            && self.domain == other.domain
    }
}

impl Default for Metadata<'_> {
    fn default() -> Self {
        Metadata {
            version: 1,
            license: None,
            copyright: None,
            domain: None,
            span: Span::default(),
        }
    }
}

#[derive(Eq, Debug, PartialEq)]
pub enum Technique<'i> {
    Steps(Vec<Scope<'i>>),
    Procedures(Vec<Procedure<'i>>),
    Empty,
}

#[derive(Eq, Debug)]
pub enum Element<'i> {
    Title(&'i str, Span),
    Description(Vec<Paragraph<'i>>, Span),
    Steps(Vec<Scope<'i>>, Span),
    CodeBlock(Vec<Expression<'i>>, Vec<Scope<'i>>, Span),
}

impl PartialEq for Element<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Element::Title(a, _), Element::Title(b, _)) => a == b,
            (Element::Description(a, _), Element::Description(b, _)) => a == b,
            (Element::Steps(a, _), Element::Steps(b, _)) => a == b,
            (Element::CodeBlock(a, sa, _), Element::CodeBlock(b, sb, _)) => a == b && sa == sb,
            _ => false,
        }
    }
}

#[derive(Eq, Debug)]
pub struct Procedure<'i> {
    pub name: Identifier<'i>,
    pub parameters: Option<Vec<Identifier<'i>>>,
    pub signature: Option<Signature<'i>>,
    pub elements: Vec<Element<'i>>,
    pub span: Span,
}

impl PartialEq for Procedure<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.parameters == other.parameters
            && self.signature == other.signature
            && self.elements == other.elements
    }
}

impl<'i> Procedure<'i> {
    pub fn title(&self) -> Option<&'i str> {
        self.elements
            .iter()
            .find_map(|element| match element {
                Element::Title(title, _) => return Some(*title),
                _ => None,
            })
    }
}

#[derive(Clone, Copy, Eq, Debug)]
pub struct Identifier<'i> {
    pub value: &'i str,
    pub span: Span,
}

impl PartialEq for Identifier<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<'i> Identifier<'i> {
    /// Test helper: builds an `Identifier` with a default span. See also the
    /// `PartialEq` instance.
    pub const fn new(value: &'i str) -> Self {
        Identifier {
            value,
            span: Span::new(0, 0),
        }
    }
}

#[derive(Clone, Copy, Eq, Debug)]
pub struct External<'i> {
    pub value: &'i str,
    pub span: Span,
}

impl PartialEq for External<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<'i> External<'i> {
    pub const fn new(value: &'i str) -> Self {
        External {
            value,
            span: Span::new(0, 0),
        }
    }
}

#[derive(Eq, Debug, PartialEq)]
pub enum Target<'i> {
    Local(Identifier<'i>),
    Remote(External<'i>),
}

#[derive(Eq, Debug)]
pub struct Forma<'i> {
    pub value: &'i str,
    pub span: Span,
}

impl PartialEq for Forma<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<'i> Forma<'i> {
    pub const fn new(value: &'i str) -> Self {
        Forma {
            value,
            span: Span::new(0, 0),
        }
    }
}

#[derive(Eq, Debug, PartialEq)]
pub enum Genus<'i> {
    Unit,
    Single(Forma<'i>),
    Tuple(Vec<Forma<'i>>),
    Naked(Vec<Forma<'i>>),
    List(Forma<'i>),
}

#[derive(Eq, Debug, PartialEq)]
pub struct Signature<'i> {
    pub requires: Genus<'i>,
    pub provides: Genus<'i>,
}

// now types for procedure bodies

#[derive(Eq, Debug, PartialEq)]
pub struct Invocation<'i> {
    pub target: Target<'i>,
    pub parameters: Option<Vec<Expression<'i>>>,
}

// types for descriptive content

#[derive(Eq, Debug)]
pub struct Paragraph<'i>(pub Vec<Descriptive<'i>>, pub Span);

impl PartialEq for Paragraph<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<'i> Paragraph<'i> {
    pub fn new(descriptives: Vec<Descriptive<'i>>) -> Self {
        Paragraph(descriptives, Span::default())
    }
}

#[derive(Eq, Debug, PartialEq)]
pub enum Descriptive<'i> {
    Text(&'i str),
    CodeInline(Expression<'i>),
    Application(Invocation<'i>),
    Binding(Box<Descriptive<'i>>, Vec<Identifier<'i>>),
}

// types for Steps within procedures

#[derive(Eq, Debug)]
pub enum Scope<'i> {
    DependentBlock {
        ordinal: &'i str,
        description: Vec<Paragraph<'i>>,
        subscopes: Vec<Scope<'i>>,
        span: Span,
    },

    ParallelBlock {
        bullet: char,
        description: Vec<Paragraph<'i>>,
        subscopes: Vec<Scope<'i>>,
        span: Span,
    },

    // Attribute scope: @role (or other attributes) with substeps
    AttributeBlock {
        attributes: Vec<Attribute<'i>>,
        subscopes: Vec<Scope<'i>>,
        span: Span,
    },

    // Code block scope: { foreach ... } with substeps
    CodeBlock {
        expressions: Vec<Expression<'i>>,
        subscopes: Vec<Scope<'i>>,
        span: Span,
    },

    // Response block scope: 'Yes' | 'No' responses
    ResponseBlock {
        responses: Vec<Response<'i>>,
        span: Span,
    },

    // Section chunk scope: organizational container with technique content
    SectionChunk {
        numeral: &'i str,
        title: Option<Paragraph<'i>>,
        body: Technique<'i>,
        span: Span,
    },
}

impl PartialEq for Scope<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Scope::DependentBlock {
                    ordinal: a1,
                    description: a2,
                    subscopes: a3,
                    ..
                },
                Scope::DependentBlock {
                    ordinal: b1,
                    description: b2,
                    subscopes: b3,
                    ..
                },
            ) => a1 == b1 && a2 == b2 && a3 == b3,
            (
                Scope::ParallelBlock {
                    bullet: a1,
                    description: a2,
                    subscopes: a3,
                    ..
                },
                Scope::ParallelBlock {
                    bullet: b1,
                    description: b2,
                    subscopes: b3,
                    ..
                },
            ) => a1 == b1 && a2 == b2 && a3 == b3,
            (
                Scope::AttributeBlock {
                    attributes: a1,
                    subscopes: a2,
                    ..
                },
                Scope::AttributeBlock {
                    attributes: b1,
                    subscopes: b2,
                    ..
                },
            ) => a1 == b1 && a2 == b2,
            (
                Scope::CodeBlock {
                    expressions: a1,
                    subscopes: a2,
                    ..
                },
                Scope::CodeBlock {
                    expressions: b1,
                    subscopes: b2,
                    ..
                },
            ) => a1 == b1 && a2 == b2,
            (
                Scope::ResponseBlock { responses: a, .. },
                Scope::ResponseBlock { responses: b, .. },
            ) => a == b,
            (
                Scope::SectionChunk {
                    numeral: a1,
                    title: a2,
                    body: a3,
                    ..
                },
                Scope::SectionChunk {
                    numeral: b1,
                    title: b2,
                    body: b3,
                    ..
                },
            ) => a1 == b1 && a2 == b2 && a3 == b3,
            _ => false,
        }
    }
}

// enum responses like 'Yes' | 'No'

#[derive(Eq, Debug)]
pub struct Response<'i> {
    pub value: &'i str,
    pub condition: Option<&'i str>,
    pub span: Span,
}

impl PartialEq for Response<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.condition == other.condition
    }
}

// attributes like @chef

#[derive(Eq, Debug)]
pub enum Attribute<'i> {
    Role(Identifier<'i>, Span),
    Place(Identifier<'i>, Span),
}

impl PartialEq for Attribute<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Attribute::Role(a, _), Attribute::Role(b, _)) => a == b,
            (Attribute::Place(a, _), Attribute::Place(b, _)) => a == b,
            _ => false,
        }
    }
}

// now types used within code blocks

#[derive(Eq, Debug, PartialEq)]
pub struct Function<'i> {
    pub target: Identifier<'i>,
    pub parameters: Vec<Expression<'i>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Pair<'i> {
    pub label: &'i str,
    pub value: Expression<'i>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Piece<'i> {
    Text(&'i str),
    Interpolation(Expression<'i>),
}

#[derive(Debug, Eq)]
pub enum Expression<'i> {
    Variable(Identifier<'i>, Span),
    String(Vec<Piece<'i>>, Span),
    Number(Numeric<'i>, Span),
    Multiline(Option<&'i str>, Vec<&'i str>, Span),
    Repeat(Box<Expression<'i>>, Span),
    Foreach(Vec<Identifier<'i>>, Box<Expression<'i>>, Span),
    Application(Invocation<'i>, Span),
    Execution(Function<'i>, Span),
    Binding(Box<Expression<'i>>, Vec<Identifier<'i>>, Span),
    Pair(Box<Pair<'i>>, Span),
    List(Vec<Expression<'i>>, Span),
    Separator,
}

impl PartialEq for Expression<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expression::Variable(a, _), Expression::Variable(b, _)) => a == b,
            (Expression::String(a, _), Expression::String(b, _)) => a == b,
            (Expression::Number(a, _), Expression::Number(b, _)) => a == b,
            (Expression::Multiline(a1, a2, _), Expression::Multiline(b1, b2, _)) => {
                a1 == b1 && a2 == b2
            }
            (Expression::Repeat(a, _), Expression::Repeat(b, _)) => a == b,
            (Expression::Foreach(a1, a2, _), Expression::Foreach(b1, b2, _)) => {
                a1 == b1 && a2 == b2
            }
            (Expression::Application(a, _), Expression::Application(b, _)) => a == b,
            (Expression::Execution(a, _), Expression::Execution(b, _)) => a == b,
            (Expression::Binding(a1, a2, _), Expression::Binding(b1, b2, _)) => {
                a1 == b1 && a2 == b2
            }
            (Expression::Pair(a, _), Expression::Pair(b, _)) => a == b,
            (Expression::List(a, _), Expression::List(b, _)) => a == b,
            (Expression::Separator, Expression::Separator) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Numeric<'i> {
    Integral(i64),
    Scientific(Quantity<'i>),
}

pub use crate::language::quantity::Quantity;

// the validate functions all need to have start and end anchors, which seems
// like it should be abstracted away.

pub(crate) fn validate_license(input: &str) -> Option<&str> {
    let re = regex!(r"^[A-Za-z0-9.,\-_ \(\)\[\]]+$");

    if re.is_match(input) {
        Some(input)
    } else {
        None
    }
}

pub(crate) fn validate_copyright(input: &str) -> Option<&str> {
    let re = regex!(r"^[A-Za-z0-9.,\-_ \(\)\[\]]+$");

    if re.is_match(input) {
        Some(input)
    } else {
        None
    }
}

pub(crate) fn validate_domain(input: &str) -> Option<&str> {
    let re = regex!(r"^[A-Za-z0-9.,\-]+$");

    if re.is_match(input) {
        Some(input)
    } else {
        None
    }
}

pub(crate) fn validate_identifier(input: &str, span: Span) -> Option<Identifier<'_>> {
    if input.len() == 0 {
        return None;
    }

    let re = regex!(r"^[a-z][a-z0-9_]*$");
    if re.is_match(input) {
        Some(Identifier { value: input, span })
    } else {
        None
    }
}

pub(crate) fn validate_forma(input: &str, span: Span) -> Option<Forma<'_>> {
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

    Some(Forma { value: input, span })
}

/// `child` must be a sub-slice of `parent`.
fn sub_span(parent: &str, child: &str, parent_span: Span) -> Span {
    let inner = (child.as_ptr() as usize) - (parent.as_ptr() as usize);
    Span::new(parent_span.offset + inner, child.len())
}

fn parse_tuple(input: &str, span: Span) -> Option<Vec<Forma<'_>>> {
    let mut formas: Vec<Forma> = Vec::new();

    for text in input.split(",") {
        let text = text.trim_ascii();
        let forma = validate_forma(text, sub_span(input, text, span))?;
        formas.push(forma);
    }

    Some(formas)
}

/// This one copes with (and discards) any internal whitespace encountered.
pub(crate) fn validate_genus(input: &str, span: Span) -> Option<Genus<'_>> {
    let first = input
        .chars()
        .next()
        .unwrap();

    match first {
        '[' => {
            // consume up to closing bracket
            if !input.ends_with(']') {
                return None;
            }

            let content = &input[1..input.len() - 1].trim_ascii();

            if content.is_empty() {
                return None;
            }

            let forma = validate_forma(content, sub_span(input, content, span))?;

            Some(Genus::List(forma))
        }
        '(' => {
            // first trim off the parenthesis and whitespace
            if !input.ends_with(')') {
                return None;
            }

            let content = &input[1..input.len() - 1].trim_ascii();

            if content.is_empty() {
                return Some(Genus::Unit);
            }

            let formas = parse_tuple(content, sub_span(input, content, span))?;
            Some(Genus::Tuple(formas))
        }
        _ => {
            if input.len() == 0 {
                return None;
            };

            // Check if this is a bare tuple (comma-separated but non-parenthesized)
            if input.contains(',') {
                let formas = parse_tuple(input, span)?;
                Some(Genus::Naked(formas))
            } else {
                let forma = validate_forma(input, span)?;
                Some(Genus::Single(forma))
            }
        }
    }
}

pub fn validate_response(input: &str) -> Option<Response<'_>> {
    if input.len() == 0 {
        return None;
    }

    // Handle conditions like 'Yes and equipment available'
    let re = regex!(r"^'(.*?)'(?:\s+(.+))?$");
    let cap = re.captures(input)?;

    let value = cap
        .get(1)
        .unwrap()
        .as_str();

    let condition = match cap.get(2) {
        Some(two) => Some(two.as_str()),
        None => None,
    };

    Some(Response {
        value,
        condition,
        span: Span::default(),
    })
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn identifier_rules() {
        let s = Span::default();
        assert_eq!(validate_identifier("a", s), Some(Identifier::new("a")));
        assert_eq!(validate_identifier("ab", s), Some(Identifier::new("ab")));
        assert_eq!(
            validate_identifier("johnny5", s),
            Some(Identifier::new("johnny5"))
        );
        assert_eq!(validate_identifier("Pizza", s), None);
        assert_eq!(validate_identifier("pizZa", s), None);
        assert!(validate_identifier("0trust", s).is_none());
        assert_eq!(
            validate_identifier("make_dinner", s),
            Some(Identifier::new("make_dinner"))
        );
        assert!(validate_identifier("MakeDinner", s).is_none());
        assert!(validate_identifier("make-dinner", s).is_none());
    }

    #[test]
    fn forma_rules() {
        assert_eq!(validate_forma("A", Span::default()), Some(Forma::new("A")));
        assert_eq!(
            validate_forma("Beans", Span::default()),
            Some(Forma::new("Beans"))
        );
        assert_eq!(validate_forma("lower", Span::default()), None);
    }

    #[test]
    fn genus_rules_single() {
        assert_eq!(
            validate_genus("A", Span::default()),
            Some(Genus::Single(Forma::new("A")))
        );
    }

    #[test]
    fn genus_rules_list() {
        assert_eq!(
            validate_genus("[A]", Span::default()),
            Some(Genus::List(Forma::new("A")))
        );

        // Test list with whitespace
        assert_eq!(
            validate_genus("[ Input ]", Span::default()),
            Some(Genus::List(Forma::new("Input")))
        );

        assert_eq!(
            validate_genus("[\tOutput\t]", Span::default()),
            Some(Genus::List(Forma::new("Output")))
        );

        // Test malformed lists
        assert_eq!(validate_genus("[Input", Span::default()), None);
        assert_eq!(validate_genus("Input]", Span::default()), None);
    }

    #[test]
    fn genus_rules_tuple_parens() {
        assert_eq!(
            validate_genus("(A, B)", Span::default()),
            Some(Genus::Tuple(vec![Forma::new("A"), Forma::new("B")]))
        );

        assert_eq!(
            validate_genus("(Coffee, Tea)", Span::default()),
            Some(Genus::Tuple(vec![Forma::new("Coffee"), Forma::new("Tea")]))
        );

        // not actually sure whether we should be normalizing this? Probably
        // not, because formatting and linting is a separate concern.

        assert_eq!(
            validate_genus("(A)", Span::default()),
            Some(Genus::Tuple(vec![Forma::new("A")]))
        );

        // Test parenthesized tuples with whitespace
        assert_eq!(
            validate_genus("( A , B )", Span::default()),
            Some(Genus::Tuple(vec![Forma::new("A"), Forma::new("B")]))
        );

        assert_eq!(
            validate_genus("(\tA\t,\tB\t)", Span::default()),
            Some(Genus::Tuple(vec![Forma::new("A"), Forma::new("B")]))
        );

        // Test malformed tuples
        assert_eq!(validate_genus("(Input", Span::default()), None);
        assert_eq!(validate_genus("Input)", Span::default()), None);
    }

    #[test]
    fn genus_rules_tuple_bare() {
        assert_eq!(
            validate_genus("A, B", Span::default()),
            Some(Genus::Naked(vec![Forma::new("A"), Forma::new("B")]))
        );

        assert_eq!(
            validate_genus("Coffee, Tea", Span::default()),
            Some(Genus::Naked(vec![Forma::new("Coffee"), Forma::new("Tea")]))
        );

        assert_eq!(
            validate_genus("Input, Data, Config", Span::default()),
            Some(Genus::Naked(vec![
                Forma::new("Input"),
                Forma::new("Data"),
                Forma::new("Config")
            ]))
        );

        assert_eq!(
            validate_genus("A,B", Span::default()),
            Some(Genus::Naked(vec![Forma::new("A"), Forma::new("B")]))
        );

        assert_eq!(
            validate_genus("A , B", Span::default()),
            Some(Genus::Naked(vec![Forma::new("A"), Forma::new("B")]))
        );

        // Test edge cases with whitespace
        assert_eq!(
            validate_genus("  A  ,  B  ", Span::default()),
            Some(Genus::Naked(vec![Forma::new("A"), Forma::new("B")]))
        );

        assert_eq!(
            validate_genus("\tA\t,\tB\t", Span::default()),
            Some(Genus::Naked(vec![Forma::new("A"), Forma::new("B")]))
        );
    }

    #[test]
    fn genus_rules_unit() {
        assert_eq!(validate_genus("()", Span::default()), Some(Genus::Unit));

        // Test unit with whitespace
        assert_eq!(validate_genus("(   )", Span::default()), Some(Genus::Unit));
        assert_eq!(validate_genus("(\t)", Span::default()), Some(Genus::Unit));
    }

    #[test]
    fn genus_rules_malformed() {
        // Test malformed brackets/parens
        assert_eq!(validate_genus("[Input", Span::default()), None);
        assert_eq!(validate_genus("Input]", Span::default()), None);
        assert_eq!(validate_genus("(Input", Span::default()), None);
        assert_eq!(validate_genus("Input)", Span::default()), None);
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
    fn domain_rules() {
        assert_eq!(validate_domain("checklist"), Some("checklist"));
        assert_eq!(validate_domain("checklist,v1"), Some("checklist,v1"));
        assert_eq!(validate_domain("checklist-v1.0"), Some("checklist-v1.0"));
    }

    fn maker<'i>() -> Metadata<'i> {
        let t1 = Metadata {
            version: 1,
            license: None,
            copyright: None,
            domain: None,
            span: Span::default(),
        };

        t1
    }

    #[test]
    fn ast_construction() {
        let t1 = Metadata {
            version: 1,
            license: None,
            copyright: None,
            domain: None,
            span: Span::default(),
        };

        assert_eq!(Metadata::default(), t1);

        let t2 = Metadata {
            version: 1,
            license: Some("MIT"),
            copyright: Some("ACME, Inc"),
            domain: Some("checklist"),
            span: Span::default(),
        };

        let t3 = maker();

        assert_eq!(t3, t1);

        let t4 = Metadata {
            license: Some("MIT"),
            copyright: Some("ACME, Inc"),
            domain: Some("checklist"),
            ..t3
        };

        assert_eq!(t4, t2);
    }
}
