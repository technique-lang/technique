//! Domain types for a procedure.
//!
//! A procedure is a recursive tree of nodes mirroring the structure of the
//! source Technique document. Sections, procedures, steps, role groups —
//! whatever the author wrote, the domain model preserves.

/// A procedure document: title and description from the first procedure,
/// then a tree of nodes representing the body.
pub struct Document {
    pub source: Option<String>,
    pub name: Option<String>,
    pub title: Option<String>,
    pub description: Vec<Prose>,
    pub body: Vec<Node>,
}

impl Document {
    pub fn new() -> Self {
        Document {
            source: None,
            name: None,
            title: None,
            description: Vec::new(),
            body: Vec::new(),
        }
    }
}

/// A node in the procedure tree.
pub enum Node {
    Section {
        ordinal: String,
        heading: Option<String>,
        children: Vec<Node>,
    },
    Procedure {
        name: String,
        title: Option<String>,
        description: Vec<Prose>,
        children: Vec<Node>,
    },
    Sequential {
        ordinal: String,
        title: Option<String>,
        body: Vec<Prose>,
        invocations: Vec<String>,
        responses: Vec<Response>,
        children: Vec<Node>,
    },
    Parallel {
        title: Option<String>,
        body: Vec<Prose>,
        invocations: Vec<String>,
        responses: Vec<Response>,
        children: Vec<Node>,
    },
    Attribute {
        name: String,
        children: Vec<Node>,
    },
    CodeBlock {
        expression: String,
        body: Vec<String>,
        responses: Vec<Response>,
        children: Vec<Node>,
    },
}

/// A response option with an optional condition.
pub struct Response {
    pub value: String,
    pub condition: Option<String>,
}

/// A paragraph of prose with inline markup.
#[derive(Debug, PartialEq)]
pub struct Prose(pub Vec<Inline>);

/// An inline fragment within prose text.
#[derive(Debug, PartialEq)]
pub enum Inline {
    Text(String),
    Emphasis(String),
    Strong(String),
    Code(String),
}

impl Prose {
    /// Parse a plain string, converting _text_ to emphasis, *text* to strong,
    /// and `text` to code.
    pub fn parse(s: &str) -> Prose {
        let mut fragments = Vec::new();
        let mut rest = s;

        while !rest.is_empty() {
            let next = rest.find(|c: char| c == '_' || c == '*' || c == '`');

            match next {
                None => {
                    fragments.push(Inline::Text(rest.to_string()));
                    break;
                }
                Some(i) => {
                    let delim = rest.as_bytes()[i] as char;
                    let after = &rest[i + 1..];

                    match after.find(delim) {
                        Some(end) if end > 0 => {
                            if i > 0 {
                                fragments.push(Inline::Text(rest[..i].to_string()));
                            }
                            let content = after[..end].to_string();
                            fragments.push(match delim {
                                '_' => Inline::Emphasis(content),
                                '*' => Inline::Strong(content),
                                '`' => Inline::Code(content),
                                _ => unreachable!(),
                            });
                            rest = &after[end + 1..];
                        }
                        _ => {
                            fragments.push(Inline::Text(rest[..i + 1].to_string()));
                            rest = after;
                        }
                    }
                }
            }
        }

        Prose(fragments)
    }
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn plain_text() {
        let p = Prose::parse("hello world");
        assert_eq!(p.0, vec![Inline::Text("hello world".into())]);
    }

    #[test]
    fn emphasis() {
        let p = Prose::parse("the _idea_ is good");
        assert_eq!(
            p.0,
            vec![
                Inline::Text("the ".into()),
                Inline::Emphasis("idea".into()),
                Inline::Text(" is good".into()),
            ]
        );
    }

    #[test]
    fn strong() {
        let p = Prose::parse("a *bold* move");
        assert_eq!(
            p.0,
            vec![
                Inline::Text("a ".into()),
                Inline::Strong("bold".into()),
                Inline::Text(" move".into()),
            ]
        );
    }

    #[test]
    fn code() {
        let p = Prose::parse("run `cmd` now");
        assert_eq!(
            p.0,
            vec![
                Inline::Text("run ".into()),
                Inline::Code("cmd".into()),
                Inline::Text(" now".into()),
            ]
        );
    }

    #[test]
    fn mixed() {
        let p = Prose::parse("the _idea_ and *design* with `code`");
        assert_eq!(
            p.0,
            vec![
                Inline::Text("the ".into()),
                Inline::Emphasis("idea".into()),
                Inline::Text(" and ".into()),
                Inline::Strong("design".into()),
                Inline::Text(" with ".into()),
                Inline::Code("code".into()),
            ]
        );
    }

    #[test]
    fn unclosed_delimiter() {
        let p = Prose::parse("a_b has no pair");
        assert_eq!(
            p.0,
            vec![
                Inline::Text("a_".into()),
                Inline::Text("b has no pair".into()),
            ]
        );
    }

    #[test]
    fn empty_string() {
        let p = Prose::parse("");
        assert_eq!(p.0, vec![]);
    }
}
