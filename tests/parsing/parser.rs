#[cfg(test)]
mod verify {
    use technique::language::*;
    use technique::parsing::parser::Parser;

    fn trim(s: &str) -> &str {
        s.strip_prefix('\n')
            .unwrap_or(s)
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

    #[test]
    fn procedure_declaration_one() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
making_coffee : (Beans, Milk) -> Coffee

            "#,
        ));

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("making_coffee"),
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Tuple(vec![Forma("Beans"), Forma("Milk")]),
                    range: Genus::Single(Forma("Coffee"))
                }),
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![]
            })
        );
    }

    #[test]
    fn procedure_declaration_two() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
first : A -> B

second : C -> D

            "#,
        ));

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("first"),
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                }),
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![]
            })
        );

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("second"),
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("C")),
                    range: Genus::Single(Forma("D"))
                }),
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![]
            })
        );
    }

    #[test]
    fn procedure_declaration_with_parameters() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
making_coffee(e) : Ingredients -> Coffee

            "#,
        ));

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("making_coffee"),
                parameters: Some(vec![Identifier("e")]),
                signature: Some(Signature {
                    domain: Genus::Single(Forma("Ingredients")),
                    range: Genus::Single(Forma("Coffee"))
                }),
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![]
            })
        );
    }

    #[test]
    fn example_procedure() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
first : A -> B

# The First

This is the first one.

1. Do the first thing in the first one.
2. Do the second thing in the first one.

            "#,
        ));

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("first"),
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                }),
                title: Some("The First"),
                description: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "This is the first one."
                )])],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Do the first thing in the first one."
                        )])],
                        responses: vec![],
                        scopes: vec![]
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Do the second thing in the first one."
                        )])],
                        responses: vec![],
                        scopes: vec![]
                    }
                ]
            })
        );
    }

    #[test]
    fn example_with_responses() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
first : A -> B

# The First

This is the first one.

1. Have you done the first thing in the first one?
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
        ));

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("first"),
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                }),
                title: Some("The First"),
                description: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "This is the first one."
                )])],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Have you done the first thing in the first one?"
                        )])],
                        responses: vec![
                            Response {
                                value: "Yes",
                                condition: None
                            },
                            Response {
                                value: "No",
                                condition: Some("but I have an excuse")
                            }
                        ],
                        scopes: vec![],
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Do the second thing in the first one."
                        )])],
                        responses: vec![],
                        scopes: vec![],
                    }
                ],
            })
        );
    }

    #[test]
    fn example_with_substeps() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
first : A -> B

# The First

This is the first one.

1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
        ));

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("first"),
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                }),
                title: Some("The First"),
                description: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "This is the first one."
                )])],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Have you done the first thing in the first one?"
                        )])],
                        responses: vec![],
                        scopes: vec![Scope {
                            roles: vec![],
                            substeps: vec![Step::Dependent {
                                ordinal: "a",
                                content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                    "Do the first thing. Then ask yourself if you are done:"
                                )])],
                                responses: vec![
                                    Response {
                                        value: "Yes",
                                        condition: None
                                    },
                                    Response {
                                        value: "No",
                                        condition: Some("but I have an excuse")
                                    }
                                ],
                                scopes: vec![]
                            }]
                        }]
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Do the second thing in the first one."
                        )])],
                        responses: vec![],
                        scopes: vec![],
                    }
                ],
            })
        );
    }

    #[test]
    fn realistic_procedure() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
            before_anesthesia :

            # Before induction of anaesthesia

                1.  Has the patient confirmed his/her identity, site, procedure,
                    and consent?
                        'Yes'
                2.  Is the site marked?
                        'Yes' | 'Not Applicable'
                3.  Is the anaesthesia machine and medication check complete?
                        'Yes'
                4.  Is the pulse oximeter on the patient and functioning?
                        'Yes'
                5.  Does the patient have a:
                    - Known allergy?
                            'No' | 'Yes'
                    - Difficult airway or aspiration risk?
                            'No' | 'Yes' and equipment/assistance available
                    - Risk of blood loss > 500 mL?
                            'No' | 'Yes' and two IVs planned and fluids available
            "#,
        ));
        let procedure = input.read_procedure();

        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("before_anesthesia"),
                parameters: None,
                signature: None,
                title: Some("Before induction of anaesthesia"),
                description: vec![],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![Descriptive::Paragraph(vec![
                            Descriptive::Text(
                                "Has the patient confirmed his/her identity, site, procedure,"
                            ),
                            Descriptive::Text("and consent?")
                        ])],
                        responses: vec![Response {
                            value: "Yes",
                            condition: None
                        }],
                        scopes: vec![],
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Is the site marked?"
                        )])],
                        responses: vec![
                            Response {
                                value: "Yes",
                                condition: None
                            },
                            Response {
                                value: "Not Applicable",
                                condition: None
                            }
                        ],
                        scopes: vec![],
                    },
                    Step::Dependent {
                        ordinal: "3",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Is the anaesthesia machine and medication check complete?"
                        )])],
                        responses: vec![Response {
                            value: "Yes",
                            condition: None
                        }],
                        scopes: vec![],
                    },
                    Step::Dependent {
                        ordinal: "4",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Is the pulse oximeter on the patient and functioning?"
                        )])],
                        responses: vec![Response {
                            value: "Yes",
                            condition: None
                        }],
                        scopes: vec![],
                    },
                    Step::Dependent {
                        ordinal: "5",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Does the patient have a:"
                        )])],
                        responses: vec![],
                        scopes: vec![Scope {
                            roles: vec![],
                            substeps: vec![
                                Step::Parallel {
                                    content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                        "Known allergy?"
                                    )])],
                                    responses: vec![
                                        Response {
                                            value: "No",
                                            condition: None
                                        },
                                        Response {
                                            value: "Yes",
                                            condition: None
                                        }
                                    ],
                                    scopes: vec![],
                                },
                                Step::Parallel {
                                    content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                        "Difficult airway or aspiration risk?"
                                    )])],
                                    responses: vec![
                                        Response {
                                            value: "No",
                                            condition: None
                                        },
                                        Response {
                                            value: "Yes",
                                            condition: Some("and equipment/assistance available")
                                        }
                                    ],
                                    scopes: vec![],
                                },
                                Step::Parallel {
                                    content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                        "Risk of blood loss > 500 mL?"
                                    )])],
                                    responses: vec![
                                        Response {
                                            value: "No",
                                            condition: None
                                        },
                                        Response {
                                            value: "Yes",
                                            condition: Some(
                                                "and two IVs planned and fluids available"
                                            )
                                        }
                                    ],
                                    scopes: vec![],
                                }
                            ]
                        }],
                    }
                ],
            })
        );
    }

    #[test]
    fn realistic_procedure_part2() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
label_the_specimens :

    1.  Specimen labelling
                @nursing_team
                    - Label blood tests
                    - Label tissue samples
                @admin_staff
                    a. Prepare the envelopes
            "#,
        ));
        let procedure = input.read_procedure();

        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("label_the_specimens"),
                parameters: None,
                signature: None,
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![Step::Dependent {
                    ordinal: "1",
                    content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                        "Specimen labelling"
                    )])],
                    responses: vec![],
                    scopes: vec![
                        Scope {
                            roles: vec![Attribute::Role(Identifier("nursing_team"))],
                            substeps: vec![
                                Step::Parallel {
                                    content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                        "Label blood tests"
                                    )])],
                                    responses: vec![],
                                    scopes: vec![],
                                },
                                Step::Parallel {
                                    content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                        "Label tissue samples"
                                    )])],
                                    responses: vec![],
                                    scopes: vec![],
                                }
                            ]
                        },
                        Scope {
                            roles: vec![Attribute::Role(Identifier("admin_staff"))],
                            substeps: vec![Step::Dependent {
                                ordinal: "a",
                                content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                    "Prepare the envelopes"
                                )])],
                                responses: vec![],
                                scopes: vec![],
                            }]
                        }
                    ],
                }],
            })
        );
    }

    #[test]
    fn realistic_procedure_part3() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
before_leaving :

# Before patient leaves operating room

    1.  Verbally confirm:
        -   The name of the surgical procedure(s).
        -   Completion of instrument, sponge, and needle counts.
        -   Specimen labelling
            { foreach specimen in specimens }
                @nursing_team
                    a.  Read specimen labels aloud, including patient
                        name.
        -   Whether there are any equipment problems to be addressed.
    2.  Post-operative care:
        @surgeon
            a.  What are the key concerns for recovery and management
                of this patient?
        @anesthetist
            b.  What are the key concerns for recovery and management
                of this patient?
        @nursing_team
            c.  What are the key concerns for recovery and management
                of this patient?
            "#,
        ));
        let procedure = input.read_procedure();

        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("before_leaving"),
                parameters: None,
                signature: None,
                title: Some("Before patient leaves operating room"),
                description: vec![],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![
                            Descriptive::Paragraph(vec![Descriptive::Text("Verbally confirm:")])
                        ],
                        responses: vec![],
                        scopes: vec![
                            Scope {
                                roles: vec![],
                                substeps: vec![
                                    Step::Parallel {
                                        content: vec![
                                            Descriptive::Paragraph(vec![Descriptive::Text("The name of the surgical procedure(s).")])
                                        ],
                                        responses: vec![],
                                        scopes: vec![],
                                    },
                                    Step::Parallel {
                                        content: vec![
                                            Descriptive::Paragraph(vec![Descriptive::Text("Completion of instrument, sponge, and needle counts.")])
                                        ],
                                        responses: vec![],
                                        scopes: vec![],
                                    },
                                    Step::Parallel {
                                        content: vec![Descriptive::Paragraph(vec![
                                            Descriptive::Text("Specimen labelling"),
                                            Descriptive::CodeBlock(Expression::Foreach(
                                                vec![Identifier("specimen")],
                                                Box::new(Expression::Variable(Identifier("specimens")))
                                            )),
                                        ])],
                                        responses: vec![],
                                        scopes: vec![],
                                    }
                                ]
                            },
                            Scope {
                                roles: vec![Attribute::Role(Identifier("nursing_team"))],
                                substeps: vec![
                                    Step::Dependent {
                                        ordinal: "a",
                                        content: vec![
                                            Descriptive::Paragraph(vec![
                                                Descriptive::Text("Read specimen labels aloud, including patient"),
                                                Descriptive::Text("name.")
                                            ])
                                        ],
                                        responses: vec![],
                                        scopes: vec![Scope {
                                            roles: vec![],
                                            substeps: vec![
                                                Step::Parallel {
                                                    content: vec![
                                                        Descriptive::Paragraph(vec![Descriptive::Text("Whether there are any equipment problems to be addressed.")])
                                                    ],
                                                    responses: vec![],
                                                    scopes: vec![],
                                                }
                                            ]
                                        }],
                                    }
                                ]
                            }
                        ],
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![
                            Descriptive::Paragraph(vec![Descriptive::Text("Post-operative care:")])
                        ],
                        responses: vec![],
                        scopes: vec![
                            Scope {
                                roles: vec![Attribute::Role(Identifier("surgeon"))],
                                substeps: vec![
                                    Step::Dependent {
                                        ordinal: "a",
                                        content: vec![
                                            Descriptive::Paragraph(vec![
                                                Descriptive::Text("What are the key concerns for recovery and management"),
                                                Descriptive::Text("of this patient?")
                                            ])
                                        ],
                                        responses: vec![],
                                        scopes: vec![],
                                    }
                                ]
                            },
                            Scope {
                                roles: vec![Attribute::Role(Identifier("anesthetist"))],
                                substeps: vec![
                                    Step::Dependent {
                                        ordinal: "b",
                                        content: vec![
                                            Descriptive::Paragraph(vec![
                                                Descriptive::Text("What are the key concerns for recovery and management"),
                                                Descriptive::Text("of this patient?")
                                            ])
                                        ],
                                        responses: vec![],
                                        scopes: vec![],
                                    }
                                ]
                            },
                            Scope {
                                roles: vec![Attribute::Role(Identifier("nursing_team"))],
                                substeps: vec![
                                    Step::Dependent {
                                        ordinal: "c",
                                        content: vec![
                                            Descriptive::Paragraph(vec![
                                                Descriptive::Text("What are the key concerns for recovery and management"),
                                                Descriptive::Text("of this patient?")
                                            ])
                                        ],
                                        responses: vec![],
                                        scopes: vec![],
                                    }
                                ]
                            }
                        ],
                    }
                ],
            })
        );
    }

    #[test]
    fn parallel_role_assignments() {
        let mut input = Parser::new();

        // Test a step that mirrors the surgical safety checklist pattern
        input.initialize(
            r#"
5. Review anticipated critical events.
        @surgeon
            a. What are the critical or non-routine steps?
            b. How long will the case take?
            c. What is the blood loss expected?
        @anaesthetist
            d. Are there any patient-specific concerns?
        @nursing_team
            e. Has sterility been confirmed?
            f. Has the equipment issues been addressed?
            "#,
        );

        let result = input.read_step_dependent();

        match result {
            Ok(Step::Dependent {
                ordinal,
                content,
                responses,
                scopes,
            }) => {
                assert_eq!(ordinal, "5");
                assert_eq!(
                    content,
                    vec![Descriptive::Paragraph(vec![Descriptive::Text(
                        "Review anticipated critical events."
                    )])]
                );
                assert_eq!(responses, vec![]);
                // Should have 3 scopes: one for each role with their substeps
                assert_eq!(scopes.len(), 3);

                // Check that the first scope has surgeon role
                assert_eq!(
                    scopes[0].roles,
                    vec![Attribute::Role(Identifier("surgeon"))]
                );
                assert_eq!(
                    scopes[0]
                        .substeps
                        .len(),
                    3
                ); // a, b, c

                // Check that the second scope has anaesthetist role
                assert_eq!(
                    scopes[1].roles,
                    vec![Attribute::Role(Identifier("anaesthetist"))]
                );
                assert_eq!(
                    scopes[1]
                        .substeps
                        .len(),
                    1
                ); // d

                // Check that the third scope has nursing_team role
                assert_eq!(
                    scopes[2].roles,
                    vec![Attribute::Role(Identifier("nursing_team"))]
                );
                assert_eq!(
                    scopes[2]
                        .substeps
                        .len(),
                    2
                ); // e, f
            }
            _ => panic!("Expected dependent step with role assignment"),
        }
    }

    #[test]
    fn multiple_roles_with_dependent_substeps() {
        let mut input = Parser::new();

        // Test multiple roles each with their own dependent substeps
        input.initialize(
            r#"
1. Review surgical procedure
        @surgeon
            a. Review patient chart
            b. Verify surgical site
            c. Confirm procedure type
        @anaesthetist
            a. Check patient allergies
            b. Review medication history
        @nursing_team
            a. Prepare instruments
            b. Verify sterility
            c. Confirm patient positioning
            "#,
        );

        let result = input.read_step_dependent();

        match result {
            Ok(Step::Dependent {
                ordinal,
                content,
                responses,
                scopes,
            }) => {
                assert_eq!(ordinal, "1");
                assert_eq!(
                    content,
                    vec![Descriptive::Paragraph(vec![Descriptive::Text(
                        "Review surgical procedure"
                    )])]
                );
                assert_eq!(responses, vec![]);
                assert_eq!(scopes.len(), 3);

                // Check surgeon scope (3 dependent substeps)
                assert_eq!(
                    scopes[0].roles,
                    vec![Attribute::Role(Identifier("surgeon"))]
                );
                assert_eq!(
                    scopes[0]
                        .substeps
                        .len(),
                    3
                );

                // Check anaesthetist scope (2 dependent substeps)
                assert_eq!(
                    scopes[1].roles,
                    vec![Attribute::Role(Identifier("anaesthetist"))]
                );
                assert_eq!(
                    scopes[1]
                        .substeps
                        .len(),
                    2
                );

                // Check nursing_team scope (3 dependent substeps)
                assert_eq!(
                    scopes[2].roles,
                    vec![Attribute::Role(Identifier("nursing_team"))]
                );
                assert_eq!(
                    scopes[2]
                        .substeps
                        .len(),
                    3
                );

                // Verify all substeps are dependent (ordered) steps
                for scope in &scopes {
                    for substep in &scope.substeps {
                        assert!(matches!(substep, Step::Dependent { .. }));
                    }
                }
            }
            _ => panic!("Expected dependent step with multiple role assignments"),
        }
    }

    #[test]
    fn mixed_substeps_in_roles() {
        let mut input = Parser::new();

        input.initialize(
            r#"
1. Emergency response
    @team_lead
        a. Assess situation
        b. Coordinate response
            - Monitor communications
            - Track resources
        c. File report
            "#,
        );
        let result = input.read_step_dependent();

        match result {
            Ok(Step::Dependent {
                ordinal,
                content,
                responses,
                scopes,
            }) => {
                assert_eq!(ordinal, "1");
                assert_eq!(
                    content,
                    vec![Descriptive::Paragraph(vec![Descriptive::Text(
                        "Emergency response"
                    )])]
                );
                assert_eq!(responses, vec![]);
                assert_eq!(scopes.len(), 1);

                // Check team_lead scope
                assert_eq!(
                    scopes[0].roles,
                    vec![Attribute::Role(Identifier("team_lead"))]
                );
                assert_eq!(
                    scopes[0]
                        .substeps
                        .len(),
                    3
                );

                // Verify the sequence: dependent (a), dependent (b with nested parallel), dependent (c)
                assert!(matches!(scopes[0].substeps[0], Step::Dependent { .. }));
                assert!(matches!(scopes[0].substeps[1], Step::Dependent { .. }));
                assert!(matches!(scopes[0].substeps[2], Step::Dependent { .. }));

                // Check substep a
                if let Step::Dependent {
                    ordinal,
                    content,
                    scopes,
                    ..
                } = &scopes[0].substeps[0]
                {
                    assert_eq!(ordinal, &"a");
                    assert_eq!(
                        content,
                        &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Assess situation"
                        )])]
                    );
                    assert_eq!(scopes.len(), 0); // No nested scopes
                }

                // Check substep b - should have nested parallel steps
                if let Step::Dependent {
                    ordinal,
                    content,
                    scopes,
                    ..
                } = &scopes[0].substeps[1]
                {
                    assert_eq!(ordinal, &"b");
                    assert_eq!(
                        content,
                        &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Coordinate response"
                        )])]
                    );
                    assert_eq!(scopes.len(), 1); // Should have nested scope with parallel steps

                    // Check the nested parallel steps
                    assert_eq!(
                        scopes[0]
                            .substeps
                            .len(),
                        2
                    );
                    assert!(matches!(scopes[0].substeps[0], Step::Parallel { .. }));
                    assert!(matches!(scopes[0].substeps[1], Step::Parallel { .. }));

                    if let Step::Parallel { content, .. } = &scopes[0].substeps[0] {
                        assert_eq!(
                            content,
                            &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                "Monitor communications"
                            )])]
                        );
                    }

                    if let Step::Parallel { content, .. } = &scopes[0].substeps[1] {
                        assert_eq!(
                            content,
                            &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                "Track resources"
                            )])]
                        );
                    }
                }

                // Check substep c
                if let Step::Dependent {
                    ordinal,
                    content,
                    scopes,
                    ..
                } = &scopes[0].substeps[2]
                {
                    assert_eq!(ordinal, &"c");
                    assert_eq!(
                        content,
                        &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "File report"
                        )])]
                    );
                    assert_eq!(scopes.len(), 0); // No nested scopes
                }
            }
            _ => panic!("Expected step with mixed substep types"),
        }
    }

    #[test]
    fn substeps_with_responses() {
        let mut input = Parser::new();

        input.initialize(
            r#"
1. Main step
    a. Substep with response
        'Yes' | 'No'
            "#,
        );
        let result = input.read_step_dependent();

        assert_eq!(
            result,
            Ok(Step::Dependent {
                ordinal: "1",
                content: vec![Descriptive::Paragraph(vec![Descriptive::Text("Main step")])],
                responses: vec![],
                scopes: vec![Scope {
                    roles: vec![],
                    substeps: vec![Step::Dependent {
                        ordinal: "a",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Substep with response"
                        )])],
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
                        scopes: vec![],
                    }],
                }],
            })
        );
    }

    #[test]
    fn naked_bindings() {
        let mut input = Parser::new();

        // Test simple naked binding: text ~ variable
        input.initialize("What is the result? ~ answer");
        let descriptive = input.read_descriptive();
        assert_eq!(
            descriptive,
            Ok(vec![Descriptive::Paragraph(vec![Descriptive::Binding(
                Box::new(Descriptive::Text("What is the result?")),
                vec![Identifier("answer")]
            )])])
        );

        // Test naked binding followed by more text. This is probably not a
        // valid usage, but it's good that it parses cleanly.
        input.initialize("Enter your name ~ name\nContinue with next step");
        let descriptive = input.read_descriptive();
        assert_eq!(
            descriptive,
            Ok(vec![Descriptive::Paragraph(vec![
                Descriptive::Binding(
                    Box::new(Descriptive::Text("Enter your name")),
                    vec![Identifier("name")]
                ),
                Descriptive::Text("Continue with next step")
            ])])
        );

        // Test mixed content with function call binding and naked binding.
        // This likewise may turn out to be something that fails compilation,
        // but it's important that it parses right so that the users gets
        // appropriate feedback.
        input.initialize("First <do_something> ~ result then describe the outcome ~ description");
        let descriptive = input.read_descriptive();
        assert_eq!(
            descriptive,
            Ok(vec![Descriptive::Paragraph(vec![
                Descriptive::Text("First"),
                Descriptive::Binding(
                    Box::new(Descriptive::Application(Invocation {
                        target: Target::Local(Identifier("do_something")),
                        parameters: None,
                    })),
                    vec![Identifier("result")]
                ),
                Descriptive::Binding(
                    Box::new(Descriptive::Text("then describe the outcome")),
                    vec![Identifier("description")]
                )
            ])])
        );
    }

    #[test]
    fn role_with_dependent_substeps() {
        let mut input = Parser::new();

        // Test role assignment with multiple dependent substeps that execute in series
        input.initialize(
            r#"
1. Perform procedure
        @surgeon
            a. Make initial incision
            b. Locate target area
            c. Complete procedure
            "#,
        );

        let result = input.read_step_dependent();

        match result {
            Ok(Step::Dependent {
                ordinal,
                content,
                responses,
                scopes,
            }) => {
                assert_eq!(ordinal, "1");
                assert_eq!(
                    content,
                    vec![Descriptive::Paragraph(vec![Descriptive::Text(
                        "Perform procedure"
                    )])]
                );
                assert_eq!(responses, vec![]);
                assert_eq!(scopes.len(), 1);

                // Check that the scope has the surgeon role
                assert_eq!(
                    scopes[0].roles,
                    vec![Attribute::Role(Identifier("surgeon"))]
                );

                // Check that the scope has 3 dependent substeps in order
                assert_eq!(
                    scopes[0]
                        .substeps
                        .len(),
                    3
                );

                if let Step::Dependent {
                    ordinal, content, ..
                } = &scopes[0].substeps[0]
                {
                    assert_eq!(ordinal, &"a");
                    assert_eq!(
                        content,
                        &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Make initial incision"
                        )])]
                    );
                }

                if let Step::Dependent {
                    ordinal, content, ..
                } = &scopes[0].substeps[1]
                {
                    assert_eq!(ordinal, &"b");
                    assert_eq!(
                        content,
                        &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Locate target area"
                        )])]
                    );
                }

                if let Step::Dependent {
                    ordinal, content, ..
                } = &scopes[0].substeps[2]
                {
                    assert_eq!(ordinal, &"c");
                    assert_eq!(
                        content,
                        &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Complete procedure"
                        )])]
                    );
                }
            }
            _ => panic!("Expected dependent step with role assignment and substeps"),
        }
    }
}
