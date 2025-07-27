#[cfg(test)]
mod verify {
    use technique::formatting::*;
    use technique::language::*;

    fn trim(text: &str) -> &str {
        let head = text.trim_start_matches('\n');
        let tail = head.trim_end_matches(' ');
        tail
    }

    #[test]
    fn header_and_body() {
        let technique = Technique {
            header: Some(Metadata {
                version: 1,
                license: Some("MIT"),
                copyright: None,
                template: Some("checklist"),
            }),
            body: None,
        };

        let result = format(&technique);
        assert_eq!(
            result,
            trim(
                r#"
% technique v1
! MIT
& checklist
                "#
            )
        );

        let technique = Technique {
            header: None,
            body: Some(vec![Procedure {
                name: Identifier("first"),
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B")),
                }),
                elements: vec![],
            }]),
        };

        let result = format(&technique);
        assert_eq!(
            result,
            trim(
                r#"
first : A -> B
                "#
            )
        );

        let technique = Technique {
            header: Some(Metadata {
                version: 1,
                license: Some("PD"),
                copyright: Some("2025 The First Procedure Society, Inc"),
                template: None,
            }),
            body: Some(vec![
                Procedure {
                    name: Identifier("first"),
                    parameters: None,
                    signature: Some(Signature {
                        domain: Genus::Single(Forma("A")),
                        range: Genus::Single(Forma("B")),
                    }),
                    elements: vec![],
                },
                Procedure {
                    name: Identifier("second"),
                    parameters: None,
                    signature: Some(Signature {
                        domain: Genus::List(Forma("Thing")),
                        range: Genus::Tuple(vec![Forma("Who"), Forma("Where"), Forma("Why")]),
                    }),
                    elements: vec![],
                },
            ]),
        };

        let result = format(&technique);
        assert_eq!(
            result,
            trim(
                r#"
% technique v1
! PD; © 2025 The First Procedure Society, Inc

first : A -> B

second : [Thing] -> (Who, Where, Why)
                "#
            )
        );
    }

    #[test]
    fn steps_and_substeps() {
        let technique = Technique {
            header: None,
            body: Some(vec![Procedure {
                name: Identifier("win_le_tour"),
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("Bicycle")),
                    range: Genus::Single(Forma("YellowJersey")),
                }),
                elements: vec![Element::Steps(vec![
                    Scope::DependentBlock {
                        ordinal: "1",
                        description: vec![Paragraph(vec![Descriptive::Text(
                            "Eat breakfast.",
                        )])],
                        responses: vec![],
                        subscopes: vec![],
                    },
                    Scope::DependentBlock {
                        ordinal: "2",
                        description: vec![Paragraph(vec![Descriptive::Text(
                            "Win a stage:",
                        )])],
                        responses: vec![],
                        subscopes: vec![Scope::DependentBlock {
                                    ordinal: "a",
                                    description: vec![Paragraph(vec![Descriptive::Text(
                                        "Ride really fast, then",
                                    )])],
                                    responses: vec![],
                                    subscopes: vec![],
                                },
                                Scope::DependentBlock {
                                    ordinal: "b",
                                    description: vec![Paragraph(vec![Descriptive::Text(
                                        "Win the sprint.",
                                    )])],
                                    responses: vec![],
                                    subscopes: vec![],
                                },
                            ],
                    },
                    Scope::DependentBlock {
                        ordinal: "3",
                        description: vec![Paragraph(vec![Descriptive::Text(
                            "Eat dinner.",
                        )])],
                        responses: vec![],
                        subscopes: vec![],
                    },
                ])]
            }])
        };

        let result = format(&technique);
        assert_eq!(
            result,
            trim(
                r#"
win_le_tour : Bicycle -> YellowJersey

    1.  Eat breakfast.
    2.  Win a stage:
        a.  Ride really fast, then
        b.  Win the sprint.
    3.  Eat dinner.
                "#
            )
        );
    }

    #[test]
    fn code_blocks() {
        let technique = Technique {
            header: None,
            body: Some(vec![Procedure {
                name: Identifier("vibe_coding"),
                parameters: None,
                signature: None,
                elements: vec![Element::CodeBlock(Expression::Execution(Function {
                    target: Identifier("exec"),
                    parameters: vec![Expression::Multiline(Some("bash"), vec!["rm -rf /"])],
                }))],
            }]),
        };

        let result = format(&technique);
        assert_eq!(
            result,
            trim(
                r#"
vibe_coding :
{
    exec(
    ```bash
        rm -rf /
    ```
    )
}
                "#
            )
        );
    }
}
