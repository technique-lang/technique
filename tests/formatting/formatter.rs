#[cfg(test)]
mod verify {
    use technique::formatting::*;
    use technique::language::*;

    fn trim(text: &str) -> &str {
        let head = text.trim_start_matches('\n');
        let tail = head.trim_end_matches(' ');
        tail
    }

    fn combine<'i>(fragments: Vec<(Syntax, std::borrow::Cow<'i, str>)>) -> String {
        let mut result = String::new();
        for fragment in fragments {
            result.push_str(&fragment.1);
        }
        result
    }

    #[test]
    fn header_and_body() {
        let document = Document {
            source: None,
            header: Some(Metadata {
                version: 1,
                license: Some("MIT"),
                copyright: None,
                domain: Some("checklist"),
            }),
            body: None,
        };

        let result = format_with_renderer(&document, 78);
        assert_eq!(
            combine(result),
            trim(
                r#"
% technique v1
! MIT
& checklist
                "#
            )
        );

        let document = Document {
            source: None,
            header: None,
            body: Some(Technique::Procedures(vec![Procedure {
                name: Identifier::dummy("first"),
                parameters: None,
                signature: Some(Signature {
                    requires: Genus::Single(Forma::dummy("A")),
                    provides: Genus::Single(Forma::dummy("B")),
                }),
                elements: vec![],
            }])),
        };

        let result = format_with_renderer(&document, 78);
        assert_eq!(
            combine(result),
            trim(
                r#"
first : A -> B
                "#
            )
        );

        let document = Document {
            source: None,
            header: Some(Metadata {
                version: 1,
                license: Some("PD"),
                copyright: Some("2025 The First Procedure Society, Inc"),
                domain: None,
            }),
            body: Some(Technique::Procedures(vec![
                Procedure {
                    name: Identifier::dummy("first"),
                    parameters: None,
                    signature: Some(Signature {
                        requires: Genus::Single(Forma::dummy("A")),
                        provides: Genus::Single(Forma::dummy("B")),
                    }),
                    elements: vec![],
                },
                Procedure {
                    name: Identifier::dummy("second"),
                    parameters: None,
                    signature: Some(Signature {
                        requires: Genus::List(Forma::dummy("Thing")),
                        provides: Genus::Tuple(vec![Forma::dummy("Who"), Forma::dummy("Where"), Forma::dummy("Why")]),
                    }),
                    elements: vec![],
                },
            ])),
        };

        let result = format_with_renderer(&document, 78);
        assert_eq!(
            combine(result),
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
        let document = Document {
            source: None,
            header: None,
            body: Some(Technique::Procedures(vec![Procedure {
                name: Identifier::dummy("win_le_tour"),
                parameters: None,
                signature: Some(Signature {
                    requires: Genus::Single(Forma::dummy("Bicycle")),
                    provides: Genus::Single(Forma::dummy("YellowJersey")),
                }),
                elements: vec![Element::Steps(vec![
                    Scope::DependentBlock {
                        ordinal: "1",
                        description: vec![Paragraph(vec![Descriptive::Text("Eat breakfast.")])],
                        subscopes: vec![],
                    },
                    Scope::DependentBlock {
                        ordinal: "2",
                        description: vec![Paragraph(vec![Descriptive::Text("Win a stage:")])],
                        subscopes: vec![
                            Scope::DependentBlock {
                                ordinal: "a",
                                description: vec![Paragraph(vec![Descriptive::Text(
                                    "Ride really fast, then",
                                )])],
                                subscopes: vec![],
                            },
                            Scope::DependentBlock {
                                ordinal: "b",
                                description: vec![Paragraph(vec![Descriptive::Text(
                                    "Win the sprint.",
                                )])],
                                subscopes: vec![],
                            },
                        ],
                    },
                    Scope::DependentBlock {
                        ordinal: "3",
                        description: vec![Paragraph(vec![Descriptive::Text("Eat dinner.")])],
                        subscopes: vec![],
                    },
                ])],
            }])),
        };

        let result = format_with_renderer(&document, 78);
        assert_eq!(
            combine(result),
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
        let document = Document {
            source: None,
            header: None,
            body: Some(Technique::Procedures(vec![Procedure {
                name: Identifier::dummy("vibe_coding"),
                parameters: None,
                signature: None,
                elements: vec![Element::CodeBlock(vec![Expression::Execution(Function {
                    target: Identifier::dummy("exec"),
                    parameters: vec![Expression::Multiline(Some("bash"), vec!["rm -rf /"])],
                })])],
            }])),
        };

        let result = format_with_renderer(&document, 78);
        assert_eq!(
            combine(result),
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
        let document = Document {
            source: None,
            header: None,
            body: Some(Technique::Procedures(vec![Procedure {
                name: Identifier::dummy("action"),
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
                                target: Identifier::dummy("exec"),
                                parameters: vec![Expression::Multiline(
                                    Some("bash"),
                                    vec!["rm -rf /"],
                                )],
                            })),
                        ])],
                        subscopes: vec![],
                    }]),
                ],
            }])),
        };

        let result = format_with_renderer(&document, 78);
        assert_eq!(
            combine(result),
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
        let document = Document {
            source: None,
            header: None,
            body: Some(Technique::Procedures(vec![Procedure {
                name: Identifier::dummy("journal"),
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
                        subscopes: vec![Scope::AttributeBlock {
                            attributes: vec![Attribute::Role(Identifier::dummy("journalist"))],
                            subscopes: vec![Scope::CodeBlock {
                                expressions: vec![Expression::Tablet(vec![
                                    Pair {
                                        label: "timestamp",
                                        value: Expression::Execution(Function {
                                            target: Identifier::dummy("now"),
                                            parameters: vec![],
                                        }),
                                    },
                                    Pair {
                                        label: "message",
                                        value: Expression::Variable(Identifier::dummy("msg")),
                                    },
                                ])],
                                subscopes: vec![],
                            }],
                        }],
                    }]),
                ],
            }])),
        };

        let result = format_with_renderer(&document, 78);
        assert_eq!(
            combine(result),
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
        let document = Document {
            source: None,
            header: None,
            body: Some(Technique::Procedures(vec![Procedure {
                name: Identifier::dummy("before_leaving"),
                parameters: None,
                signature: None,
                elements: vec![
                    Element::Title("Before patient leaves operating room"),
                    Element::Steps(vec![Scope::DependentBlock {
                        ordinal: "1",
                        description: vec![Paragraph(vec![Descriptive::Text("Verbally confirm:")])],
                        subscopes: vec![
                            Scope::ParallelBlock {
                                bullet: '-',
                                description: vec![Paragraph(vec![Descriptive::Text(
                                    "The name of the surgical procedure(s).",
                                )])],
                                subscopes: vec![],
                            },
                            Scope::ParallelBlock {
                                bullet: '-',
                                description: vec![Paragraph(vec![Descriptive::Text(
                                    "Completion of instrument, sponge, and needle counts.",
                                )])],
                                subscopes: vec![],
                            },
                            Scope::ParallelBlock {
                                bullet: '-',
                                description: vec![Paragraph(vec![Descriptive::Text(
                                    "Specimen labelling",
                                )])],
                                subscopes: vec![Scope::CodeBlock {
                                    expressions: vec![Expression::Foreach(
                                        vec![Identifier::dummy("specimen")],
                                        Box::new(Expression::Variable(Identifier::dummy("specimens"))),
                                    )],
                                    subscopes: vec![Scope::AttributeBlock {
                                        attributes: vec![Attribute::Role(Identifier::dummy(
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
                                subscopes: vec![],
                            },
                        ],
                    }]),
                ],
            }])),
        };
        let result = format_with_renderer(&document, 60);

        assert_eq!(
            combine(result),
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

    #[test]
    fn section_formatting() {
        let document = Document {
            source: None,
            header: None,
            body: Some(Technique::Procedures(vec![Procedure {
                name: Identifier::dummy("main_procedure"),
                parameters: None,
                signature: None,
                elements: vec![Element::Steps(vec![
                    Scope::SectionChunk {
                        numeral: "I",
                        title: Some(Paragraph(vec![Descriptive::Text("First Section")])),
                        body: Technique::Procedures(vec![]),
                    },
                    Scope::SectionChunk {
                        numeral: "II",
                        title: Some(Paragraph(vec![Descriptive::Text("Second Section")])),
                        body: Technique::Procedures(vec![]),
                    },
                    Scope::SectionChunk {
                        numeral: "III",
                        title: None,
                        body: Technique::Procedures(vec![]),
                    },
                ])],
            }])),
        };

        let result = format_with_renderer(&document, 78);
        assert_eq!(
            combine(result),
            trim(
                r#"
main_procedure :

I. First Section

II. Second Section

III.
                "#
            )
        );
    }

    #[test]
    fn response_formatting() {
        let document = Document {
            source: None,
            header: None,
            body: Some(Technique::Procedures(vec![Procedure {
                name: Identifier::dummy("test_procedure"),
                parameters: None,
                signature: None,
                elements: vec![Element::Steps(vec![
                    Scope::DependentBlock {
                        ordinal: "1",
                        description: vec![Paragraph(vec![Descriptive::Text("Main step")])],
                        subscopes: vec![Scope::DependentBlock {
                            ordinal: "a",
                            description: vec![Paragraph(vec![Descriptive::Text(
                                "Substep with response",
                            )])],
                            subscopes: vec![Scope::ResponseBlock {
                                responses: vec![
                                    Response {
                                        value: "Yes",
                                        condition: None,
                                    },
                                    Response {
                                        value: "No",
                                        condition: None,
                                    },
                                ],
                            }],
                        }],
                    },
                    Scope::DependentBlock {
                        ordinal: "2",
                        description: vec![Paragraph(vec![Descriptive::Text(
                            "Simple step with response",
                        )])],
                        subscopes: vec![Scope::ResponseBlock {
                            responses: vec![Response {
                                value: "Confirmed",
                                condition: None,
                            }],
                        }],
                    },
                ])],
            }])),
        };

        let result = format_with_renderer(&document, 78);
        assert_eq!(
            combine(result),
            trim(
                r#"
test_procedure :

    1.  Main step
        a.  Substep with response
                'Yes' | 'No'
    2.  Simple step with response
            'Confirmed'
                "#
            )
        );
    }
}
