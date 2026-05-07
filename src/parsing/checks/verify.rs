use std::path::Path;
use std::vec;

use super::*;

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
            domain: None,
            span: Span::default(),
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
            domain: Some("checklist"),
            span: Span::default(),
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
            name: Identifier::new("making_coffee"),
            parameters: None,
            signature: Some(Signature {
                requires: Genus::Tuple(vec![Forma::new("Beans"), Forma::new("Milk")]),
                provides: Genus::Single(Forma::new("Coffee"))
            }),
            elements: vec![],
            span: Span::default(),
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
            name: Identifier::new("first"),
            parameters: None,
            signature: Some(Signature {
                requires: Genus::Single(Forma::new("A")),
                provides: Genus::Single(Forma::new("B"))
            }),
            elements: vec![],
            span: Span::default(),
        })
    );

    let procedure = input.read_procedure();
    assert_eq!(
        procedure,
        Ok(Procedure {
            name: Identifier::new("second"),
            parameters: None,
            signature: Some(Signature {
                requires: Genus::Single(Forma::new("C")),
                provides: Genus::Single(Forma::new("D"))
            }),
            elements: vec![],
            span: Span::default(),
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
            name: Identifier::new("making_coffee"),
            parameters: Some(vec![Identifier::new("e")]),
            signature: Some(Signature {
                requires: Genus::Single(Forma::new("Ingredients")),
                provides: Genus::Single(Forma::new("Coffee"))
            }),
            elements: vec![],
            span: Span::default(),
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
            name: Identifier::new("first"),
            parameters: None,
            signature: Some(Signature {
                requires: Genus::Single(Forma::new("A")),
                provides: Genus::Single(Forma::new("B"))
            }),
            elements: vec![
                Element::Title("The First", Span::default()),
                Element::Description(
                    vec![Paragraph::new(vec![Descriptive::Text(
                        "This is the first one."
                    )])],
                    Span::default()
                ),
                Element::Steps(
                    vec![
                        Scope::DependentBlock {
                            ordinal: "1",
                            description: vec![Paragraph::new(vec![Descriptive::Text(
                                "Do the first thing in the first one."
                            )])],

                            subscopes: vec![],
                            span: Span::default(),
                        },
                        Scope::DependentBlock {
                            ordinal: "2",
                            description: vec![Paragraph::new(vec![Descriptive::Text(
                                "Do the second thing in the first one."
                            )])],

                            subscopes: vec![],
                            span: Span::default(),
                        }
                    ],
                    Span::default()
                )
            ],
            span: Span::default(),
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
            name: Identifier::new("first"),
            parameters: None,
            signature: Some(Signature {
                requires: Genus::Single(Forma::new("A")),
                provides: Genus::Single(Forma::new("B"))
            }),
            elements: vec![
                Element::Title("The First", Span::default()),
                Element::Description(
                    vec![Paragraph::new(vec![Descriptive::Text(
                        "This is the first one."
                    )])],
                    Span::default()
                ),
                Element::Steps(
                    vec![
                        Scope::DependentBlock {
                            ordinal: "1",
                            description: vec![Paragraph::new(vec![Descriptive::Text(
                                "Have you done the first thing in the first one?"
                            )])],
                            subscopes: vec![Scope::ResponseBlock {
                                responses: vec![
                                    Response {
                                        value: "Yes",
                                        condition: None,
                                        span: Span::default()
                                    },
                                    Response {
                                        value: "No",
                                        condition: Some("but I have an excuse"),
                                        span: Span::default()
                                    }
                                ],
                                span: Span::default(),
                            }],
                            span: Span::default(),
                        },
                        Scope::DependentBlock {
                            ordinal: "2",
                            description: vec![Paragraph::new(vec![Descriptive::Text(
                                "Do the second thing in the first one."
                            )])],

                            subscopes: vec![],
                            span: Span::default(),
                        }
                    ],
                    Span::default()
                )
            ],
            span: Span::default(),
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
            name: Identifier::new("first"),
            parameters: None,
            signature: Some(Signature {
                requires: Genus::Single(Forma::new("A")),
                provides: Genus::Single(Forma::new("B"))
            }),
            elements: vec![
                Element::Title("The First", Span::default()),
                Element::Description(
                    vec![Paragraph::new(vec![Descriptive::Text(
                        "This is the first one."
                    )])],
                    Span::default()
                ),
                Element::Steps(
                    vec![
                        Scope::DependentBlock {
                            ordinal: "1",
                            description: vec![Paragraph::new(vec![Descriptive::Text(
                                "Have you done the first thing in the first one?"
                            )])],

                            subscopes: vec![Scope::DependentBlock {
                                ordinal: "a",
                                description: vec![Paragraph::new(vec![Descriptive::Text(
                                    "Do the first thing. Then ask yourself if you are done:"
                                )])],
                                subscopes: vec![Scope::ResponseBlock {
                                    responses: vec![
                                        Response {
                                            value: "Yes",
                                            condition: None,
                                            span: Span::default()
                                        },
                                        Response {
                                            value: "No",
                                            condition: Some("but I have an excuse"),
                                            span: Span::default()
                                        }
                                    ],
                                    span: Span::default(),
                                }],
                                span: Span::default(),
                            }],
                            span: Span::default(),
                        },
                        Scope::DependentBlock {
                            ordinal: "2",
                            description: vec![Paragraph::new(vec![Descriptive::Text(
                                "Do the second thing in the first one."
                            )])],

                            subscopes: vec![],
                            span: Span::default(),
                        }
                    ],
                    Span::default()
                )
            ],
            span: Span::default(),
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
    let result = input.read_procedure();
    let procedure = result.expect("a parsed Procedure");

    assert_eq!(
        procedure,
        Procedure {
            name: Identifier::new("before_anesthesia"),
            parameters: None,
            signature: None,
            elements: vec![
                Element::Title("Before induction of anaesthesia", Span::default()),
                Element::Steps(
                    vec![
                        Scope::DependentBlock {
                            ordinal: "1",
                            description: vec![Paragraph::new(vec![
                                Descriptive::Text(
                                    "Has the patient confirmed his/her identity, site, procedure,"
                                ),
                                Descriptive::Text("and consent?")
                            ])],
                            subscopes: vec![Scope::ResponseBlock {
                                responses: vec![Response {
                                    value: "Yes",
                                    condition: None,
                                    span: Span::default()
                                }],
                                span: Span::default(),
                            }],
                            span: Span::default(),
                        },
                        Scope::DependentBlock {
                            ordinal: "2",
                            description: vec![Paragraph::new(vec![Descriptive::Text(
                                "Is the site marked?"
                            )])],
                            subscopes: vec![Scope::ResponseBlock {
                                responses: vec![
                                    Response {
                                        value: "Yes",
                                        condition: None,
                                        span: Span::default()
                                    },
                                    Response {
                                        value: "Not Applicable",
                                        condition: None,
                                        span: Span::default()
                                    }
                                ],
                                span: Span::default(),
                            }],
                            span: Span::default(),
                        },
                        Scope::DependentBlock {
                            ordinal: "3",
                            description: vec![Paragraph::new(vec![Descriptive::Text(
                                "Is the anaesthesia machine and medication check complete?"
                            )])],
                            subscopes: vec![Scope::ResponseBlock {
                                responses: vec![Response {
                                    value: "Yes",
                                    condition: None,
                                    span: Span::default()
                                }],
                                span: Span::default(),
                            }],
                            span: Span::default(),
                        },
                        Scope::DependentBlock {
                            ordinal: "4",
                            description: vec![Paragraph::new(vec![Descriptive::Text(
                                "Is the pulse oximeter on the patient and functioning?"
                            )])],
                            subscopes: vec![Scope::ResponseBlock {
                                responses: vec![Response {
                                    value: "Yes",
                                    condition: None,
                                    span: Span::default()
                                }],
                                span: Span::default(),
                            }],
                            span: Span::default(),
                        },
                        Scope::DependentBlock {
                            ordinal: "5",
                            description: vec![Paragraph::new(vec![Descriptive::Text(
                                "Does the patient have a:"
                            )])],

                            subscopes: vec![
                                Scope::ParallelBlock {
                                    bullet: '-',
                                    description: vec![Paragraph::new(vec![Descriptive::Text(
                                        "Known allergy?"
                                    )])],
                                    subscopes: vec![Scope::ResponseBlock {
                                        responses: vec![
                                            Response {
                                                value: "No",
                                                condition: None,
                                                span: Span::default()
                                            },
                                            Response {
                                                value: "Yes",
                                                condition: None,
                                                span: Span::default()
                                            }
                                        ],
                                        span: Span::default(),
                                    }],
                                    span: Span::default(),
                                },
                                Scope::ParallelBlock {
                                    bullet: '-',
                                    description: vec![Paragraph::new(vec![Descriptive::Text(
                                        "Difficult airway or aspiration risk?"
                                    )])],
                                    subscopes: vec![Scope::ResponseBlock {
                                        responses: vec![
                                            Response {
                                                value: "No",
                                                condition: None,
                                                span: Span::default()
                                            },
                                            Response {
                                                value: "Yes",
                                                condition: Some(
                                                    "and equipment/assistance available"
                                                ),
                                                span: Span::default(),
                                            }
                                        ],
                                        span: Span::default(),
                                    }],
                                    span: Span::default(),
                                },
                                Scope::ParallelBlock {
                                    bullet: '-',
                                    description: vec![Paragraph::new(vec![Descriptive::Text(
                                        "Risk of blood loss > 500 mL?"
                                    )])],
                                    subscopes: vec![Scope::ResponseBlock {
                                        responses: vec![
                                            Response {
                                                value: "No",
                                                condition: None,
                                                span: Span::default()
                                            },
                                            Response {
                                                value: "Yes",
                                                condition: Some(
                                                    "and two IVs planned and fluids available"
                                                ),
                                                span: Span::default(),
                                            }
                                        ],
                                        span: Span::default(),
                                    }],
                                    span: Span::default(),
                                }
                            ],
                            span: Span::default(),
                        }
                    ],
                    Span::default()
                )
            ],
            span: Span::default(),
        }
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
            name: Identifier::new("label_the_specimens"),
            parameters: None,
            signature: None,
            elements: vec![Element::Steps(
                vec![Scope::DependentBlock {
                    ordinal: "1",
                    description: vec![Paragraph::new(vec![Descriptive::Text(
                        "Specimen labelling"
                    )])],

                    subscopes: vec![
                        Scope::AttributeBlock {
                            attributes: vec![Attribute::Role(
                                Identifier::new("nursing_team"),
                                Span::default()
                            )],
                            subscopes: vec![
                                Scope::ParallelBlock {
                                    bullet: '-',
                                    description: vec![Paragraph::new(vec![Descriptive::Text(
                                        "Label blood tests"
                                    )])],

                                    subscopes: vec![],
                                    span: Span::default(),
                                },
                                Scope::ParallelBlock {
                                    bullet: '-',
                                    description: vec![Paragraph::new(vec![Descriptive::Text(
                                        "Label tissue samples"
                                    )])],

                                    subscopes: vec![],
                                    span: Span::default(),
                                }
                            ],
                            span: Span::default(),
                        },
                        Scope::AttributeBlock {
                            attributes: vec![Attribute::Role(
                                Identifier::new("admin_staff"),
                                Span::default()
                            )],
                            subscopes: vec![Scope::DependentBlock {
                                ordinal: "a",
                                description: vec![Paragraph::new(vec![Descriptive::Text(
                                    "Prepare the envelopes"
                                )])],

                                subscopes: vec![],
                                span: Span::default(),
                            }],
                            span: Span::default(),
                        }
                    ],
                    span: Span::default(),
                }],
                Span::default()
            )],
            span: Span::default(),
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
    let result = input.read_procedure();

    let procedure = result.expect("a procedure");
    assert_eq!(
        procedure,
        Procedure {
            name: Identifier::new("before_leaving"),
            parameters: None,
            signature: None,
            elements: vec![
                Element::Title("Before patient leaves operating room", Span::default()),
                Element::Steps(
                    vec![
                        Scope::DependentBlock {
                            ordinal: "1",
                            description: vec![Paragraph::new(vec![Descriptive::Text(
                                "Verbally confirm:"
                            )])],

                            subscopes: vec![
                                Scope::ParallelBlock {
                                    bullet: '-',
                                    description: vec![Paragraph::new(vec![Descriptive::Text(
                                        "The name of the surgical procedure(s)."
                                    )])],

                                    subscopes: vec![],
                                    span: Span::default(),
                                },
                                Scope::ParallelBlock {
                                    bullet: '-',
                                    description: vec![Paragraph::new(vec![Descriptive::Text(
                                        "Completion of instrument, sponge, and needle counts."
                                    )])],

                                    subscopes: vec![],
                                    span: Span::default(),
                                },
                                Scope::ParallelBlock {
                                    bullet: '-',
                                    description: vec![Paragraph::new(vec![Descriptive::Text(
                                        "Specimen labelling"
                                    )])],

                                    subscopes: vec![Scope::CodeBlock {
                                        expressions: vec![Expression::Foreach(
                                            vec![Identifier::new("specimen")],
                                            Box::new(Expression::Variable(
                                                Identifier::new("specimens"),
                                                Span::default()
                                            )),
                                            Span::default(),
                                        )],
                                        subscopes: vec![Scope::AttributeBlock {
                                            attributes: vec![Attribute::Role(
                                                Identifier::new("nursing_team"),
                                                Span::default(),
                                            )],
                                            subscopes: vec![Scope::DependentBlock {
                                                ordinal: "a",
                                                description: vec![Paragraph::new(vec![
                                                Descriptive::Text(
                                                    "Read specimen labels aloud, including patient"
                                                ),
                                                Descriptive::Text("name.")
                                            ])],

                                                subscopes: vec![],
                                                span: Span::default(),
                                            }],
                                            span: Span::default(),
                                        }],
                                        span: Span::default(),
                                    }],
                                    span: Span::default(),
                                },
                                Scope::ParallelBlock {
                                    bullet: '-',
                                    description: vec![Paragraph::new(vec![Descriptive::Text(
                                        "Whether there are any equipment problems to be addressed."
                                    )])],

                                    subscopes: vec![],
                                    span: Span::default(),
                                }
                            ],
                            span: Span::default(),
                        },
                        Scope::DependentBlock {
                            ordinal: "2",
                            description: vec![Paragraph::new(vec![Descriptive::Text(
                                "Post-operative care:"
                            )])],

                            subscopes: vec![
                                Scope::AttributeBlock {
                                    attributes: vec![Attribute::Role(
                                        Identifier::new("surgeon"),
                                        Span::default()
                                    )],
                                    subscopes: vec![Scope::DependentBlock {
                                        ordinal: "a",
                                        description: vec![Paragraph::new(vec![
                                        Descriptive::Text(
                                            "What are the key concerns for recovery and management"
                                        ),
                                        Descriptive::Text("of this patient?")
                                    ])],

                                        subscopes: vec![],
                                        span: Span::default(),
                                    }],
                                    span: Span::default(),
                                },
                                Scope::AttributeBlock {
                                    attributes: vec![Attribute::Role(
                                        Identifier::new("anesthetist"),
                                        Span::default(),
                                    )],
                                    subscopes: vec![Scope::DependentBlock {
                                        ordinal: "b",
                                        description: vec![Paragraph::new(vec![
                                        Descriptive::Text(
                                            "What are the key concerns for recovery and management"
                                        ),
                                        Descriptive::Text("of this patient?")
                                    ])],

                                        subscopes: vec![],
                                        span: Span::default(),
                                    }],
                                    span: Span::default(),
                                },
                                Scope::AttributeBlock {
                                    attributes: vec![Attribute::Role(
                                        Identifier::new("nursing_team"),
                                        Span::default(),
                                    )],
                                    subscopes: vec![Scope::DependentBlock {
                                        ordinal: "c",
                                        description: vec![Paragraph::new(vec![
                                        Descriptive::Text(
                                            "What are the key concerns for recovery and management"
                                        ),
                                        Descriptive::Text("of this patient?")
                                    ])],

                                        subscopes: vec![],
                                        span: Span::default(),
                                    }],
                                    span: Span::default(),
                                }
                            ],
                            span: Span::default(),
                        }
                    ],
                    Span::default()
                )
            ],
            span: Span::default(),
        }
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
        Ok(Scope::DependentBlock {
            ordinal,
            description: content,
            subscopes: scopes,
            ..
        }) => {
            assert_eq!(ordinal, "5");
            assert_eq!(
                content,
                vec![Paragraph::new(vec![Descriptive::Text(
                    "Review anticipated critical events."
                )])]
            );
            // Should have 3 scopes: one for each role with their substeps
            assert_eq!(scopes.len(), 3);

            // Check that the first scope has surgeon role
            if let Scope::AttributeBlock {
                attributes,
                subscopes: substeps,
                ..
            } = &scopes[0]
            {
                assert_eq!(
                    *attributes,
                    vec![Attribute::Role(Identifier::new("surgeon"), Span::default())]
                );
                assert_eq!(substeps.len(), 3); // a, b, c
            } else {
                panic!("Expected AttributedBlock for surgeon");
            }

            // Check that the second scope has anaesthetist role
            if let Scope::AttributeBlock {
                attributes,
                subscopes: substeps,
                ..
            } = &scopes[1]
            {
                assert_eq!(
                    *attributes,
                    vec![Attribute::Role(
                        Identifier::new("anaesthetist"),
                        Span::default()
                    )]
                );
                assert_eq!(substeps.len(), 1); // d
            } else {
                panic!("Expected AttributedBlock for anaesthetist");
            }

            // Check that the third scope has nursing_team role
            if let Scope::AttributeBlock {
                attributes,
                subscopes: substeps,
                ..
            } = &scopes[2]
            {
                assert_eq!(
                    *attributes,
                    vec![Attribute::Role(
                        Identifier::new("nursing_team"),
                        Span::default()
                    )]
                );
                assert_eq!(substeps.len(), 2); // e, f
            } else {
                panic!("Expected AttributedBlock for nursing_team");
            }
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
        Ok(Scope::DependentBlock {
            ordinal,
            description: content,
            subscopes: scopes,
            ..
        }) => {
            assert_eq!(ordinal, "1");
            assert_eq!(
                content,
                vec![Paragraph::new(vec![Descriptive::Text(
                    "Review surgical procedure"
                )])]
            );
            assert_eq!(scopes.len(), 3);

            // Check surgeon scope (3 dependent substeps)
            if let Scope::AttributeBlock {
                attributes,
                subscopes: substeps,
                ..
            } = &scopes[0]
            {
                assert_eq!(
                    *attributes,
                    vec![Attribute::Role(Identifier::new("surgeon"), Span::default())]
                );
                assert_eq!(substeps.len(), 3);
            } else {
                panic!("Expected AttributedBlock for surgeon");
            }

            // Check anaesthetist scope (2 dependent substeps)
            if let Scope::AttributeBlock {
                attributes,
                subscopes: substeps,
                ..
            } = &scopes[1]
            {
                assert_eq!(
                    *attributes,
                    vec![Attribute::Role(
                        Identifier::new("anaesthetist"),
                        Span::default()
                    )]
                );
                assert_eq!(substeps.len(), 2);
            } else {
                panic!("Expected AttributedBlock for anaesthetist");
            }

            // Check nursing_team scope (3 dependent substeps)
            if let Scope::AttributeBlock {
                attributes,
                subscopes: substeps,
                ..
            } = &scopes[2]
            {
                assert_eq!(
                    *attributes,
                    vec![Attribute::Role(
                        Identifier::new("nursing_team"),
                        Span::default()
                    )]
                );
                assert_eq!(substeps.len(), 3);
            } else {
                panic!("Expected AttributedBlock for nursing_team");
            }

            // Verify all substeps are dependent (ordered) steps
            for scope in &scopes {
                match scope {
                    Scope::AttributeBlock {
                        subscopes: substeps,
                        ..
                    } => {
                        for substep in substeps {
                            assert!(matches!(substep, Scope::DependentBlock { .. }));
                        }
                    }
                    _ => panic!("Expected AttributedBlock scopes"),
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

    let step = match result {
        Ok(step) => step,
        _ => panic!("Expected step with mixed substep types"),
    };

    assert_eq!(
        step,
        Scope::DependentBlock {
            ordinal: "1",
            description: vec![Paragraph::new(vec![Descriptive::Text(
                "Emergency response"
            )])],

            subscopes: vec![Scope::AttributeBlock {
                attributes: vec![Attribute::Role(
                    Identifier::new("team_lead"),
                    Span::default()
                )],
                subscopes: vec![
                    Scope::DependentBlock {
                        ordinal: "a",
                        description: vec![Paragraph::new(vec![Descriptive::Text(
                            "Assess situation"
                        )])],

                        subscopes: vec![],
                        span: Span::default(),
                    },
                    Scope::DependentBlock {
                        ordinal: "b",
                        description: vec![Paragraph::new(vec![Descriptive::Text(
                            "Coordinate response"
                        )])],

                        subscopes: vec![
                            Scope::ParallelBlock {
                                bullet: '-',
                                description: vec![Paragraph::new(vec![Descriptive::Text(
                                    "Monitor communications"
                                )])],

                                subscopes: vec![],
                                span: Span::default(),
                            },
                            Scope::ParallelBlock {
                                bullet: '-',
                                description: vec![Paragraph::new(vec![Descriptive::Text(
                                    "Track resources"
                                )])],

                                subscopes: vec![],
                                span: Span::default(),
                            }
                        ],
                        span: Span::default(),
                    },
                    Scope::DependentBlock {
                        ordinal: "c",
                        description: vec![Paragraph::new(vec![Descriptive::Text("File report")])],

                        subscopes: vec![],
                        span: Span::default(),
                    }
                ],
                span: Span::default(),
            }],
            span: Span::default(),
        }
    );
}

#[test]
fn substeps_with_responses() {
    let mut input = Parser::new();

    input.initialize(
        r#"
1. Main step
    a.  Substep with response
        'Yes' | 'No'
            "#,
    );
    let result = input.read_step_dependent();

    assert_eq!(
        result,
        Ok(Scope::DependentBlock {
            ordinal: "1",
            description: vec![Paragraph::new(vec![Descriptive::Text("Main step")])],

            subscopes: vec![Scope::DependentBlock {
                ordinal: "a",
                description: vec![Paragraph::new(vec![Descriptive::Text(
                    "Substep with response"
                )])],
                subscopes: vec![Scope::ResponseBlock {
                    responses: vec![
                        Response {
                            value: "Yes",
                            condition: None,
                            span: Span::default()
                        },
                        Response {
                            value: "No",
                            condition: None,
                            span: Span::default()
                        },
                    ],
                    span: Span::default(),
                }],
                span: Span::default(),
            }],
            span: Span::default(),
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
        Ok(vec![Paragraph::new(vec![Descriptive::Binding(
            Box::new(Descriptive::Text("What is the result?")),
            vec![Identifier::new("answer")]
        )])])
    );

    // Test naked binding followed by more text. This is probably not a
    // valid usage, but it's good that it parses cleanly.
    input.initialize("Enter your name ~ name\nContinue with next step");
    let descriptive = input.read_descriptive();
    assert_eq!(
        descriptive,
        Ok(vec![Paragraph::new(vec![
            Descriptive::Binding(
                Box::new(Descriptive::Text("Enter your name")),
                vec![Identifier::new("name")]
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
        Ok(vec![Paragraph::new(vec![
            Descriptive::Text("First"),
            Descriptive::Binding(
                Box::new(Descriptive::Application(Invocation {
                    target: Target::Local(Identifier::new("do_something")),
                    parameters: None,
                })),
                vec![Identifier::new("result")]
            ),
            Descriptive::Binding(
                Box::new(Descriptive::Text("then describe the outcome")),
                vec![Identifier::new("description")]
            )
        ])])
    );
}

#[test]
fn section_parsing() {
    let result = parse_with_recovery(
        Path::new(""),
        trim(
            r#"
main_procedure :

I. First Section

first_section_first_procedure :

# One dot One

first_section_second_procedure :

# One dot Two

II. Second Section

second_section_first_procedure :

# Two dot One

second_section_second_procedure :

# Two dot Two
            "#,
        ),
    );

    let document = match result {
        Ok(document) => document,
        Err(e) => panic!("Parsing failed: {:?}", e),
    };

    // Verify complete structure
    assert_eq!(
        document,
        Document {
            source: None,
            header: None,
            body: Some(Technique::Procedures(vec![Procedure {
                name: Identifier::new("main_procedure"),
                parameters: None,
                signature: None,
                elements: vec![Element::Steps(
                    vec![
                        Scope::SectionChunk {
                            numeral: "I",
                            title: Some(Paragraph::new(vec![Descriptive::Text("First Section")])),
                            body: Technique::Procedures(vec![
                                Procedure {
                                    name: Identifier::new("first_section_first_procedure"),
                                    parameters: None,
                                    signature: None,
                                    elements: vec![Element::Title("One dot One", Span::default())],
                                    span: Span::default(),
                                },
                                Procedure {
                                    name: Identifier::new("first_section_second_procedure"),
                                    parameters: None,
                                    signature: None,
                                    elements: vec![Element::Title("One dot Two", Span::default())],
                                    span: Span::default(),
                                }
                            ]),
                            span: Span::default(),
                        },
                        Scope::SectionChunk {
                            numeral: "II",
                            title: Some(Paragraph::new(vec![Descriptive::Text("Second Section")])),
                            body: Technique::Procedures(vec![
                                Procedure {
                                    name: Identifier::new("second_section_first_procedure"),
                                    parameters: None,
                                    signature: None,
                                    elements: vec![Element::Title("Two dot One", Span::default())],
                                    span: Span::default(),
                                },
                                Procedure {
                                    name: Identifier::new("second_section_second_procedure"),
                                    parameters: None,
                                    signature: None,
                                    elements: vec![Element::Title("Two dot Two", Span::default())],
                                    span: Span::default(),
                                }
                            ]),
                            span: Span::default(),
                        },
                    ],
                    Span::default()
                )],
                span: Span::default(),
            }])),
        }
    );
}

#[test]
fn section_with_procedures_only() {
    let result = parse_with_recovery(
        Path::new(""),
        trim(
            r#"
main_procedure :

I. First Section

procedure_one : Input -> Output

procedure_two : Other -> Thing

II. Second Section

procedure_three : Concept -> Requirements

procedure_four : Concept -> Architecture
            "#,
        ),
    );

    let document = match result {
        Ok(document) => document,
        Err(e) => panic!("Parsing failed: {:?}", e),
    };

    // Verify that both sections contain their respective procedures
    if let Some(Technique::Procedures(procs)) = document.body {
        let main_proc = &procs[0];
        if let Some(Element::Steps(steps, _)) = main_proc
            .elements
            .first()
        {
            // Should have 2 sections
            assert_eq!(steps.len(), 2);

            // Check first section has 2 procedures
            if let Scope::SectionChunk {
                body: Technique::Procedures(section1_procs),
                ..
            } = &steps[0]
            {
                assert_eq!(section1_procs.len(), 2);
                assert_eq!(section1_procs[0].name, Identifier::new("procedure_one"));
                assert_eq!(section1_procs[1].name, Identifier::new("procedure_two"));
            } else {
                panic!("First section should contain procedures");
            }

            // Check second section has 2 procedures
            if let Scope::SectionChunk {
                body: Technique::Procedures(section2_procs),
                ..
            } = &steps[1]
            {
                assert_eq!(section2_procs.len(), 2);
                assert_eq!(section2_procs[0].name, Identifier::new("procedure_three"));
                assert_eq!(section2_procs[1].name, Identifier::new("procedure_four"));
            } else {
                panic!("Second section should contain procedures");
            }
        } else {
            panic!("Main procedure should have steps");
        }
    } else {
        panic!("Should have procedures");
    }
}

#[test]
fn section_with_procedures() {
    let result = parse_with_recovery(
        Path::new(""),
        trim(
            r#"
main_procedure :

I. Concept

II. Requirements Definition and Architecture

requirements_and_architecture : Concept -> Requirements, Architecture

    2.  Define Requirements <define_requirements>(concept)

    3.  Determine Architecture <determine_architecture>(concept)

define_requirements : Concept -> Requirements

determine_architecture : Concept -> Architecture

III. Implementation
            "#,
        ),
    );

    let document = match result {
        Ok(document) => document,
        Err(e) => panic!("Parsing failed: {:?}", e),
    };

    assert_eq!(
        document,
        Document {
            source: None,
            header: None,
            body: Some(Technique::Procedures(vec![Procedure {
                name: Identifier::new("main_procedure"),
                parameters: None,
                signature: None,
                elements: vec![Element::Steps(
                    vec![
                        Scope::SectionChunk {
                            numeral: "I",
                            title: Some(Paragraph::new(vec![Descriptive::Text("Concept")])),
                            body: Technique::Empty,
                            span: Span::default(),
                        },
                        Scope::SectionChunk {
                            numeral: "II",
                            title: Some(Paragraph::new(vec![Descriptive::Text(
                                "Requirements Definition and Architecture"
                            )])),
                            body: Technique::Procedures(vec![
                                Procedure {
                                    name: Identifier::new("requirements_and_architecture"),
                                    parameters: None,
                                    signature: Some(Signature {
                                        requires: Genus::Single(Forma::new("Concept")),
                                        provides: Genus::Naked(vec![
                                            Forma::new("Requirements"),
                                            Forma::new("Architecture")
                                        ]),
                                    }),
                                    elements: vec![Element::Steps(
                                        vec![
                                            Scope::DependentBlock {
                                                ordinal: "2",
                                                description: vec![Paragraph::new(vec![
                                                    Descriptive::Text("Define Requirements"),
                                                    Descriptive::Application(Invocation {
                                                        target: Target::Local(Identifier::new(
                                                            "define_requirements",
                                                        )),
                                                        parameters: Some(vec![
                                                            Expression::Variable(
                                                                Identifier::new("concept"),
                                                                Span::default(),
                                                            )
                                                        ]),
                                                    }),
                                                ])],

                                                subscopes: vec![],
                                                span: Span::default(),
                                            },
                                            Scope::DependentBlock {
                                                ordinal: "3",
                                                description: vec![Paragraph::new(vec![
                                                    Descriptive::Text("Determine Architecture"),
                                                    Descriptive::Application(Invocation {
                                                        target: Target::Local(Identifier::new(
                                                            "determine_architecture",
                                                        )),
                                                        parameters: Some(vec![
                                                            Expression::Variable(
                                                                Identifier::new("concept"),
                                                                Span::default(),
                                                            )
                                                        ]),
                                                    }),
                                                ])],

                                                subscopes: vec![],
                                                span: Span::default(),
                                            },
                                        ],
                                        Span::default()
                                    )],
                                    span: Span::default(),
                                },
                                Procedure {
                                    name: Identifier::new("define_requirements"),
                                    parameters: None,
                                    signature: Some(Signature {
                                        requires: Genus::Single(Forma::new("Concept")),
                                        provides: Genus::Single(Forma::new("Requirements")),
                                    }),
                                    elements: vec![],
                                    span: Span::default(),
                                },
                                Procedure {
                                    name: Identifier::new("determine_architecture"),
                                    parameters: None,
                                    signature: Some(Signature {
                                        requires: Genus::Single(Forma::new("Concept")),
                                        provides: Genus::Single(Forma::new("Architecture")),
                                    }),
                                    elements: vec![],
                                    span: Span::default(),
                                },
                            ]),
                            span: Span::default(),
                        },
                        Scope::SectionChunk {
                            numeral: "III",
                            title: Some(Paragraph::new(vec![Descriptive::Text("Implementation")])),
                            body: Technique::Empty,
                            span: Span::default(),
                        },
                    ],
                    Span::default()
                )],
                span: Span::default(),
            }])),
        }
    )
}

#[test]
fn spans_are_populated() {
    let source = std::fs::read_to_string("tests/samples/KnownSpanLengths.tq").unwrap();

    let mut input = Parser::new();
    input.initialize(&source);

    let metadata = input
        .read_technique_header()
        .unwrap();
    assert_eq!(metadata.span, Span::new(0, 42));

    input.trim_whitespace();
    let procedure = input
        .read_procedure()
        .unwrap();
    assert_eq!(procedure.span, Span::new(43, 231));
    assert_eq!(
        procedure
            .name
            .span,
        Span::new(43, 5)
    );

    let params = procedure
        .parameters
        .as_ref()
        .unwrap();
    assert_eq!(params[0].span, Span::new(49, 4));

    if let Genus::Single(f) = &procedure
        .signature
        .as_ref()
        .unwrap()
        .requires
    {
        assert_eq!(f.span, Span::new(57, 6));
    }
    if let Genus::Single(f) = &procedure
        .signature
        .as_ref()
        .unwrap()
        .provides
    {
        assert_eq!(f.span, Span::new(67, 8));
    }

    if let Element::Title(_, span) = &procedure.elements[0] {
        assert_eq!(*span, Span::new(77, 14));
    }
    if let Element::Description(_, span) = &procedure.elements[1] {
        assert_eq!(*span, Span::new(92, 23));
    }
    if let Element::Steps(scopes, span) = &procedure.elements[2] {
        assert_eq!(*span, Span::new(119, 155));

        if let Scope::DependentBlock {
            span, subscopes, ..
        } = &scopes[0]
        {
            assert_eq!(*span, Span::new(119, 41));
            if let Scope::AttributeBlock {
                attributes, span, ..
            } = &subscopes[0]
            {
                assert_eq!(*span, Span::new(151, 9));
                if let Attribute::Role(id, attr_span) = &attributes[0] {
                    assert_eq!(*attr_span, Span::new(151, 8));
                    assert_eq!(id.span, Span::new(152, 7));
                }
            }
        }
        if let Scope::DependentBlock {
            span, subscopes, ..
        } = &scopes[1]
        {
            assert_eq!(*span, Span::new(164, 110));

            if let Scope::ResponseBlock { responses, .. } = &subscopes[0] {
                assert_eq!(responses[0].span, Span::new(211, 6));
                assert_eq!(responses[1].span, Span::new(220, 21));
                assert_eq!(responses[2].span, Span::new(244, 29));
            }
        }
    }
}
