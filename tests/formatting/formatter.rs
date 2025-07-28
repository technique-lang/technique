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

        let result = format(&technique, 78);
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

        let result = format(&technique, 78);
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

        let result = format(&technique, 78);
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
                        description: vec![Paragraph(vec![Descriptive::Text("Eat breakfast.")])],
                        responses: vec![],
                        subscopes: vec![],
                    },
                    Scope::DependentBlock {
                        ordinal: "2",
                        description: vec![Paragraph(vec![Descriptive::Text("Win a stage:")])],
                        responses: vec![],
                        subscopes: vec![
                            Scope::DependentBlock {
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
                        description: vec![Paragraph(vec![Descriptive::Text("Eat dinner.")])],
                        responses: vec![],
                        subscopes: vec![],
                    },
                ])],
            }]),
        };

        let result = format(&technique, 78);
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

        let result = format(&technique, 78);
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

    #[test]
    fn multiline_in_code_inline() {
        let technique = Technique {
            header: None,
            body: Some(vec![Procedure {
                name: Identifier("action"),
                parameters: None,
                signature: None,
                elements: vec![
                    Element::Description(vec![Paragraph(vec![Descriptive::Text(
                        "We must take action!",
                    )])]),
                    Element::Steps(vec![Scope::DependentBlock {
                        ordinal: "1",
                        description: vec![Paragraph(vec![
                            Descriptive::Text("To take the action, we must:"),
                            Descriptive::CodeInline(Expression::Execution(Function {
                                target: Identifier("exec"),
                                parameters: vec![Expression::Multiline(
                                    Some("bash"),
                                    vec!["rm -rf /"],
                                )],
                            })),
                        ])],
                        responses: vec![],
                        subscopes: vec![],
                    }]),
                ],
            }]),
        };

        let result = format(&technique, 78);
        assert_eq!(
            result,
            trim(
                r#"
action :

We must take action!

    1.  To take the action, we must: { exec(
        ```bash
            rm -rf /
        ```
        ) }
                "#
            )
        );
    }

    #[test]
    fn code_block_under_attribute() {
        let technique = Technique {
            header: None,
            body: Some(vec![Procedure {
                name: Identifier("journal"),
                parameters: None,
                signature: None,
                elements: vec![
                    Element::Description(vec![Paragraph(vec![Descriptive::Text(
                        "Record everything, with timestamps.",
                    )])]),
                    Element::Steps(vec![Scope::ParallelBlock {
                        bullet: '-',
                        description: vec![Paragraph(vec![Descriptive::Text(
                            "Record event as it happens",
                        )])],
                        responses: vec![],
                        subscopes: vec![Scope::AttributeBlock {
                            attributes: vec![Attribute::Role(Identifier("journalist"))],
                            subscopes: vec![Scope::CodeBlock {
                                expression: Expression::Tablet(vec![
                                    Pair {
                                        label: "timestamp",
                                        value: Expression::Execution(Function {
                                            target: Identifier("now"),
                                            parameters: vec![],
                                        }),
                                    },
                                    Pair {
                                        label: "message",
                                        value: Expression::Variable(Identifier("msg")),
                                    },
                                ]),
                                subscopes: vec![],
                            }],
                        }],
                    }]),
                ],
            }]),
        };

        let result = format(&technique, 78);
        assert_eq!(
            result,
            trim(
                r#"
journal :

Record everything, with timestamps.

    -   Record event as it happens
        @journalist
        {
            [
                "timestamp" = now()
                "message" = msg
            ]
        }
                "#
            )
        );
    }

    #[test]
    fn nested_scopes() {
        let technique = Technique {
            header: None,
            body: Some(vec![Procedure {
                name: Identifier("before_leaving"),
                parameters: None,
                signature: None,
                elements: vec![
                    Element::Title("Before patient leaves operating room"),
                    Element::Steps(vec![Scope::DependentBlock {
                        ordinal: "1",
                        description: vec![Paragraph(vec![Descriptive::Text("Verbally confirm:")])],
                        responses: vec![],
                        subscopes: vec![
                            Scope::ParallelBlock {
                                bullet: '-',
                                description: vec![Paragraph(vec![Descriptive::Text(
                                    "The name of the surgical procedure(s).",
                                )])],
                                responses: vec![],
                                subscopes: vec![],
                            },
                            Scope::ParallelBlock {
                                bullet: '-',
                                description: vec![Paragraph(vec![Descriptive::Text(
                                    "Completion of instrument, sponge, and needle counts.",
                                )])],
                                responses: vec![],
                                subscopes: vec![],
                            },
                            Scope::ParallelBlock {
                                bullet: '-',
                                description: vec![Paragraph(vec![Descriptive::Text(
                                    "Specimen labelling",
                                )])],
                                responses: vec![],
                                subscopes: vec![Scope::CodeBlock {
                                    expression: Expression::Foreach(
                                        vec![Identifier("specimen")],
                                        Box::new(Expression::Variable(Identifier("specimens"))),
                                    ),
                                    subscopes: vec![Scope::AttributeBlock {
                                        attributes: vec![Attribute::Role(Identifier(
                                            "nursing_team",
                                        ))],
                                        subscopes: vec![Scope::DependentBlock {
                                            ordinal: "a",
                                            description: vec![Paragraph(vec![
                                                Descriptive::Text(
                                                    "Read specimen labels aloud, including patient",
                                                ),
                                                Descriptive::Text("name."),
                                            ])],
                                            responses: vec![],
                                            subscopes: vec![],
                                        }],
                                    }],
                                }],
                            },
                            Scope::ParallelBlock {
                                bullet: '-',
                                description: vec![Paragraph(vec![Descriptive::Text(
                                    "Whether there are any equipment problems to be addressed.",
                                )])],
                                responses: vec![],
                                subscopes: vec![],
                            },
                        ],
                    }]),
                ],
            }]),
        };
        let result = format(&technique, 60);

        assert_eq!(
            result,
            trim(
                r#"
before_leaving :

# Before patient leaves operating room

    1.  Verbally confirm:
        -   The name of the surgical procedure(s).
        -   Completion of instrument, sponge, and needle
            counts.
        -   Specimen labelling
            { foreach specimen in specimens }
                @nursing_team
                    a.  Read specimen labels aloud,
                        including patient name.
        -   Whether there are any equipment problems to be
            addressed.
                "#,
            ),
        );
    }
}
