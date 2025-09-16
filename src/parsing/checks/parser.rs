use super::*;

#[test]
fn magic_line() {
    let mut input = Parser::new();
    input.initialize("% technique v1");
    assert!(is_magic_line(input.source));

    let result = input.read_magic_line();
    assert_eq!(result, Ok(1));

    input.initialize("%technique v1");
    assert!(is_magic_line(input.source));

    let result = input.read_magic_line();
    assert_eq!(result, Ok(1));

    input.initialize("%techniquev1");
    assert!(is_magic_line(input.source));

    // this is rejected because the technique keyword isn't present.
    let result = input.read_magic_line();
    assert!(result.is_err());
}

#[test]
fn magic_line_wrong_keyword_error_position() {
    // Test that error position points to the first character of the wrong keyword
    assert_eq!(analyze_magic_line("% tecnique v1"), 2); // Points to "t" in "tecnique"
    assert_eq!(analyze_magic_line("%  tecnique v1"), 3); // Points to "t" in "tecnique" with extra space
    assert_eq!(analyze_magic_line("% \ttechniqe v1"), 3); // Points to "t" in "techniqe" with tab
    assert_eq!(analyze_magic_line("%    wrong v1"), 5); // Points to "w" in "wrong" with multiple spaces
    assert_eq!(analyze_magic_line("% foo v1"), 2); // Points to "f" in "foo"
    assert_eq!(analyze_magic_line("% TECHNIQUE v1"), 2); // Points to "T" in uppercase "TECHNIQUE"

    // Test missing keyword entirely - should point to position after %
    assert_eq!(analyze_magic_line("% v1"), 2); // Points to "v" when keyword is missing
    assert_eq!(analyze_magic_line("%  v1"), 3); // Points to "v" when keyword is missing with space
}

#[test]
fn magic_line_wrong_version_error_position() {
    // Test that error position points to the version number after "v" in wrong version strings
    assert_eq!(analyze_magic_line("% technique v0"), 13); // Points to "0" in "v0"
    assert_eq!(analyze_magic_line("% technique  v2"), 14); // Points to "2" in "v2" with extra space
    assert_eq!(analyze_magic_line("% technique\tv0"), 13); // Points to "0" in "v0" with tab
    assert_eq!(analyze_magic_line("% technique   vX"), 15); // Points to "X" in "vX" with multiple spaces
    assert_eq!(analyze_magic_line("% technique v99"), 13); // Points to "9" in "v99"
    assert_eq!(analyze_magic_line("% technique   v0.5"), 15); // Points to "0" in "v0.5" with multiple spaces

    // Test edge case where there's no "v" at all - should point to where version should start
    assert_eq!(analyze_magic_line("% technique 1.0"), 12); // Points to "1" when there's no "v"
    assert_eq!(analyze_magic_line("% technique v1.0"), 14); // Points to "." when there is a "v1" but it has minor version
    assert_eq!(analyze_magic_line("% technique  2"), 13); // Points to "2" when there's no "v" with extra space
    assert_eq!(analyze_magic_line("% technique beta"), 12); // Points to "b" in "beta" when there's no "v"
}

#[test]
fn header_spdx() {
    let mut input = Parser::new();
    input.initialize("! PD");
    assert!(is_spdx_line(input.source));

    let result = input.read_spdx_line();
    assert_eq!(result, Ok((Some("PD"), None)));

    input.initialize("! MIT; (c) ACME, Inc.");
    assert!(is_spdx_line(input.source));

    let result = input.read_spdx_line();
    assert_eq!(result, Ok((Some("MIT"), Some("ACME, Inc."))));

    input.initialize("! MIT; (C) 2024 ACME, Inc.");
    assert!(is_spdx_line(input.source));

    let result = input.read_spdx_line();
    assert_eq!(result, Ok((Some("MIT"), Some("2024 ACME, Inc."))));

    input.initialize("! CC BY-SA 3.0 [IGO]; (c) 2024 ACME, Inc.");
    assert!(is_spdx_line(input.source));

    let result = input.read_spdx_line();
    assert_eq!(
        result,
        Ok((Some("CC BY-SA 3.0 [IGO]"), Some("2024 ACME, Inc.")))
    );
}

#[test]
fn header_template() {
    let mut input = Parser::new();
    input.initialize("& checklist");
    assert!(is_template_line(input.source));

    let result = input.read_template_line();
    assert_eq!(result, Ok(Some("checklist")));

    input.initialize("& nasa-flight-plan,v4.0");
    assert!(is_template_line(input.source));

    let result = input.read_template_line();
    assert_eq!(result, Ok(Some("nasa-flight-plan,v4.0")));
}

// now we test incremental parsing

#[test]
fn check_not_eof() {
    let mut input = Parser::new();
    input.initialize("Hello World");
    assert!(!input.is_finished());

    input.initialize("");
    assert!(input.is_finished());
}

#[test]
fn consume_whitespace() {
    let mut input = Parser::new();
    input.initialize("  hello");
    input.trim_whitespace();
    assert_eq!(input.source, "hello");

    input.initialize("\n \nthere");
    input.trim_whitespace();
    assert_eq!(input.source, "there");
    assert_eq!(input.offset, 3);
}

// It is not clear that we will ever actually need parse_identifier(),
// parse_forma(), parse_genus(), or parse_signature() as they are not
// called directly, but even though they are not used in composition of
// the parse_procedure_declaration() parser, it is highly likely that
// someday we will need to be able to parse them individually, perhaps for
// a future language server or code highlighter. So we test them properly
// here; in any event it exercises the underlying validate_*() codepaths.

#[test]
fn identifier_rules() {
    let mut input = Parser::new();
    input.initialize("p");
    let result = input.read_identifier();
    assert_eq!(result, Ok(Identifier("p")));

    input.initialize("cook_pizza");
    let result = input.read_identifier();
    assert_eq!(result, Ok(Identifier("cook_pizza")));

    input.initialize("cook-pizza");
    let result = input.read_identifier();
    assert!(result.is_err());
}

#[test]
fn signatures() {
    let mut input = Parser::new();
    input.initialize("A -> B");
    let result = input.read_signature();
    assert_eq!(
        result,
        Ok(Signature {
            domain: Genus::Single(Forma("A")),
            range: Genus::Single(Forma("B"))
        })
    );

    input.initialize("Beans -> Coffee");
    let result = input.read_signature();
    assert_eq!(
        result,
        Ok(Signature {
            domain: Genus::Single(Forma("Beans")),
            range: Genus::Single(Forma("Coffee"))
        })
    );

    input.initialize("[Bits] -> Bob");
    let result = input.read_signature();
    assert_eq!(
        result,
        Ok(Signature {
            domain: Genus::List(Forma("Bits")),
            range: Genus::Single(Forma("Bob"))
        })
    );

    input.initialize("Complex -> (Real, Imaginary)");
    let result = input.read_signature();
    assert_eq!(
        result,
        Ok(Signature {
            domain: Genus::Single(Forma("Complex")),
            range: Genus::Tuple(vec![Forma("Real"), Forma("Imaginary")])
        })
    );
}

#[test]
fn declaration_simple() {
    let mut input = Parser::new();
    input.initialize("making_coffee :");

    assert!(is_procedure_declaration(input.source));

    let result = input.parse_procedure_declaration();
    assert_eq!(result, Ok((Identifier("making_coffee"), None, None)));
}

#[test]
fn declaration_full() {
    let mut input = Parser::new();
    input.initialize("f : A -> B");
    assert!(is_procedure_declaration(input.source));

    let result = input.parse_procedure_declaration();
    assert_eq!(
        result,
        Ok((
            Identifier("f"),
            None,
            Some(Signature {
                domain: Genus::Single(Forma("A")),
                range: Genus::Single(Forma("B"))
            })
        ))
    );

    input.initialize("making_coffee : (Beans, Milk) -> [Coffee]");
    assert!(is_procedure_declaration(input.source));

    let result = input.parse_procedure_declaration();
    assert_eq!(
        result,
        Ok((
            Identifier("making_coffee"),
            None,
            Some(Signature {
                domain: Genus::Tuple(vec![Forma("Beans"), Forma("Milk")]),
                range: Genus::List(Forma("Coffee"))
            })
        ))
    );

    let content = "f : B";
    // we still need to detect procedure declarations with malformed
    // signatures; the user's intent will be to declare a procedure though
    // it will fail validation in the parser shortly after.
    assert!(is_procedure_declaration(content));

    let content = r#"
    connectivity_check(e,s) : LocalEnvironment, TargetService -> NetworkHealth
                "#;

    assert!(is_procedure_declaration(content));
}

// At one point we had a bug where parsing was racing ahead and taking too
// much content, which was only uncovered when we expanded to be agnostic
// about whitespace in procedure declarations.
#[test]
fn multiline_declaration() {
    let content = r#"
    making_coffee (b, m) :
       (Beans, Milk)
         -> Coffee

    And now we will make coffee as follows...

        1. Add the beans to the machine
        2. Pour in the milk
                "#;

    assert!(is_procedure_declaration(content));
}

#[test]
fn multiline_signature_parsing() {
    let mut input = Parser::new();
    let content = r#"
making_coffee :
   Ingredients
     -> Coffee
                    "#
    .trim_ascii();

    input.initialize(content);
    let result = input.parse_procedure_declaration();

    assert_eq!(
        result,
        Ok((
            Identifier("making_coffee"),
            None,
            Some(Signature {
                domain: Genus::Single(Forma("Ingredients")),
                range: Genus::Single(Forma("Coffee"))
            })
        ))
    );

    // Test complex multiline signature with parameters and tuple
    let content = r#"
making_coffee(b, m) :
       (Beans, Milk)
         -> Coffee
                    "#
    .trim_ascii();

    input.initialize(content);
    let result = input.parse_procedure_declaration();

    assert_eq!(
        result,
        Ok((
            Identifier("making_coffee"),
            Some(vec![Identifier("b"), Identifier("m")]),
            Some(Signature {
                domain: Genus::Tuple(vec![Forma("Beans"), Forma("Milk")]),
                range: Genus::Single(Forma("Coffee"))
            })
        ))
    );
}

#[test]
fn character_delimited_blocks() {
    let mut input = Parser::new();
    input.initialize("{ todo() }");

    let result = input.take_block_chars("inline code", '{', '}', true, |parser| {
        let text = parser.source;
        assert_eq!(text, " todo() ");
        Ok(true)
    });
    assert_eq!(result, Ok(true));

    // this is somewhat contrived as we would not be using this to parse
    // strings (We will need to preserve whitespace inside strings when
    // we find ourselves parsing them, so subparser() won't work.
    input.initialize("XhelloX world");

    let result = input.take_block_chars("", 'X', 'X', false, |parser| {
        let text = parser.source;
        assert_eq!(text, "hello");
        Ok(true)
    });
    assert_eq!(result, Ok(true));
}

#[test]
fn skip_string_content_flag() {
    let mut input = Parser::new();

    // Test skip_string_content: true - should ignore braces inside strings
    input.initialize(r#"{ "string with { brace" }"#);
    let result = input.take_block_chars("code block", '{', '}', true, |parser| {
        let text = parser.source;
        assert_eq!(text, r#" "string with { brace" "#);
        Ok(true)
    });
    assert_eq!(result, Ok(true));

    // Test skip_string_content: false - should treat braces normally
    input.initialize(r#""string with } brace""#);
    let result = input.take_block_chars("string content", '"', '"', false, |parser| {
        let text = parser.source;
        assert_eq!(text, "string with } brace");
        Ok(true)
    });
    assert_eq!(result, Ok(true));
}

#[test]
fn string_delimited_blocks() {
    let mut input = Parser::new();
    input.initialize("```bash\nls -l\necho hello```");
    assert_eq!(input.offset, 0);

    let result = input.take_block_delimited("```", |parser| {
        let text = parser.source;
        assert_eq!(text, "bash\nls -l\necho hello");
        Ok(true)
    });
    assert_eq!(result, Ok(true));
    assert_eq!(input.source, "");
    assert_eq!(input.offset, 27);

    // Test with different delimiter
    input.initialize("---start\ncontent here\nmore content---end");

    let result = input.take_block_delimited("---", |parser| {
        let text = parser.source;
        assert_eq!(text, "start\ncontent here\nmore content");
        Ok(true)
    });
    assert_eq!(result, Ok(true));

    // Test with whitespace around delimiters
    input.initialize("```  hello world  ``` and now goodbye");

    let result = input.take_block_delimited("```", |parser| {
        let text = parser.source;
        assert_eq!(text, "  hello world  ");
        Ok(true)
    });
    assert_eq!(result, Ok(true));
    assert_eq!(input.source, " and now goodbye");
    assert_eq!(input.offset, 21);
}

#[test]
fn taking_until() {
    let mut input = Parser::new();

    // Test take_until() with an identifier up to a limiting character
    input.initialize("hello,world");
    let result = input.take_until(&[','], |inner| inner.read_identifier());
    assert_eq!(result, Ok(Identifier("hello")));
    assert_eq!(input.source, ",world");

    // Test take_until() with whitespace delimiters
    input.initialize("test \t\nmore");
    let result = input.take_until(&[' ', '\t', '\n'], |inner| inner.read_identifier());
    assert_eq!(result, Ok(Identifier("test")));
    assert_eq!(input.source, " \t\nmore");

    // Test take_until() when no delimiter found (it should take everything)
    input.initialize("onlytext");
    let result = input.take_until(&[',', ';'], |inner| inner.read_identifier());
    assert_eq!(result, Ok(Identifier("onlytext")));
    assert_eq!(input.source, "");
}

#[test]
fn reading_invocations() {
    let mut input = Parser::new();

    // Test simple invocation without parameters
    input.initialize("<hello>");
    let result = input.read_invocation();
    assert_eq!(
        result,
        Ok(Invocation {
            target: Target::Local(Identifier("hello")),
            parameters: None
        })
    );

    // Test invocation with empty parameters
    input.initialize("<hello_world>()");
    let result = input.read_invocation();
    assert_eq!(
        result,
        Ok(Invocation {
            target: Target::Local(Identifier("hello_world")),
            parameters: Some(vec![])
        })
    );

    // Test invocation with multiple parameters
    input.initialize("<greetings>(name, title, occupation)");
    let result = input.read_invocation();
    assert_eq!(
        result,
        Ok(Invocation {
            target: Target::Local(Identifier("greetings")),
            parameters: Some(vec![
                Expression::Variable(Identifier("name")),
                Expression::Variable(Identifier("title")),
                Expression::Variable(Identifier("occupation"))
            ])
        })
    );

    // We don't have real support for this yet, but syntactically we will
    // support the idea of invoking a procedure at an external URL, so we
    // have this case as a placeholder.
    input.initialize("<https://example.com/proc>");
    let result = input.read_invocation();
    assert_eq!(
        result,
        Ok(Invocation {
            target: Target::Remote(External("https://example.com/proc")),
            parameters: None
        })
    );
}

#[test]
fn step_detection() {
    // Test main dependent steps (whitespace agnostic)
    assert!(is_step_dependent("1. First step"));
    assert!(is_step_dependent("  1. Indented step"));
    assert!(is_step_dependent("10. Tenth step"));
    assert!(!is_step_dependent("a. Letter step"));
    assert!(!is_step_dependent("1.No space"));

    // Test dependent substeps (whitespace agnostic)
    assert!(is_substep_dependent("a. Substep"));
    assert!(is_substep_dependent("  a. Indented substep"));
    assert!(!is_substep_dependent("2. Substep can't have number"));
    assert!(!is_substep_dependent("   1. Even if it is indented"));

    // Test parallel substeps (whitespace agnostic)
    assert!(is_substep_parallel("- Parallel substep"));
    assert!(is_substep_parallel("  - Indented parallel"));
    assert!(is_substep_parallel("    - Deeper indented"));
    assert!(!is_substep_parallel("-No space")); // it's possible we may allow this in the future
    assert!(!is_substep_parallel("* Different bullet"));

    // Test top-level parallel steps
    assert!(is_step_parallel("- Top level parallel"));
    assert!(is_step_parallel("  - Indented parallel"));
    assert!(is_step("- Top level parallel")); // general step detection
    assert!(is_step("1. Numbered step"));

    // Test recognition of sub-sub-steps
    assert!(is_subsubstep_dependent("i. One"));
    assert!(is_subsubstep_dependent(" ii. Two"));
    assert!(is_subsubstep_dependent("v. Five"));
    assert!(is_subsubstep_dependent("vi. Six"));
    assert!(is_subsubstep_dependent("ix. Nine"));
    assert!(is_subsubstep_dependent("x. Ten"));
    assert!(is_subsubstep_dependent("xi. Eleven"));
    assert!(is_subsubstep_dependent("xxxix. Thirty-nine"));

    // Test attribute assignments
    assert!(is_attribute_assignment("@surgeon"));
    assert!(is_attribute_assignment("  @nursing_team"));
    assert!(is_attribute_assignment("^kitchen"));
    assert!(is_attribute_assignment("  ^garden  "));
    assert!(is_attribute_assignment("@chef + ^kitchen"));
    assert!(is_attribute_assignment("^room1 + @barista"));
    assert!(!is_attribute_assignment("surgeon"));
    assert!(!is_attribute_assignment("@123invalid"));
    assert!(!is_attribute_assignment("^InvalidPlace"));

    // Test enum responses
    assert!(is_enum_response("'Yes'"));
    assert!(is_enum_response("  'No'"));
    assert!(is_enum_response("'Not Applicable'"));
    assert!(!is_enum_response("Yes"));
    assert!(!is_enum_response("'unclosed"));
}

#[test]
fn read_toplevel_steps() {
    let mut input = Parser::new();

    // Test simple dependent step
    input.initialize("1. First step");
    let result = input.read_step_dependent();
    assert_eq!(
        result,
        Ok(Scope::DependentBlock {
            ordinal: "1",
            description: vec![Paragraph(vec![Descriptive::Text("First step")])],
            subscopes: vec![],
        })
    );

    // Test simple parallel step
    input.initialize(
        r#"
 - a top-level task to be one in parallel with
 - another top-level task
       "#,
    );
    let result = input.read_step_parallel();
    assert_eq!(
        result,
        Ok(Scope::ParallelBlock {
            bullet: '-',
            description: vec![Paragraph(vec![Descriptive::Text(
                "a top-level task to be one in parallel with"
            )]),],
            subscopes: vec![],
        })
    );
    let result = input.read_step_parallel();
    assert_eq!(
        result,
        Ok(Scope::ParallelBlock {
            bullet: '-',
            description: vec![Paragraph(vec![Descriptive::Text("another top-level task")]),],
            subscopes: vec![],
        })
    );

    // Test multi-line dependent step
    input.initialize(
        r#"
    1.  Have you done the first thing in the first one?
            "#,
    );
    let result = input.read_step_dependent();
    assert_eq!(
        result,
        Ok(Scope::DependentBlock {
            ordinal: "1",
            description: vec![Paragraph(vec![Descriptive::Text(
                "Have you done the first thing in the first one?"
            )])],
            subscopes: vec![],
        })
    );

    // Test invalid step
    input.initialize("Not a step");
    let result = input.read_step_dependent();
    assert_eq!(result, Err(ParsingError::InvalidStep(0, 0)));
}

#[test]
fn reading_substeps_basic() {
    let mut input = Parser::new();

    // Test simple dependent sub-step
    input.initialize("a. First subordinate task");
    let result = input.read_substep_dependent();
    assert_eq!(
        result,
        Ok(Scope::DependentBlock {
            ordinal: "a",
            description: vec![Paragraph(vec![Descriptive::Text("First subordinate task")])],
            subscopes: vec![],
        })
    );

    // Test simple parallel sub-step
    input.initialize("- Parallel task");
    let result = input.read_substep_parallel();
    assert_eq!(
        result,
        Ok(Scope::ParallelBlock {
            bullet: '-',
            description: vec![Paragraph(vec![Descriptive::Text("Parallel task")])],
            subscopes: vec![],
        })
    );
}

#[test]
fn single_step_with_dependent_substeps() {
    let mut input = Parser::new();

    input.initialize(
        r#"
1. Main step
    a. First substep
    b. Second substep
            "#,
    );
    let result = input.read_step_dependent();

    assert_eq!(
        result,
        Ok(Scope::DependentBlock {
            ordinal: "1",
            description: vec![Paragraph(vec![Descriptive::Text("Main step")])],
            subscopes: vec![
                Scope::DependentBlock {
                    ordinal: "a",
                    description: vec![Paragraph(vec![Descriptive::Text("First substep")])],
                    subscopes: vec![],
                },
                Scope::DependentBlock {
                    ordinal: "b",
                    description: vec![Paragraph(vec![Descriptive::Text("Second substep")])],
                    subscopes: vec![],
                },
            ],
        })
    );
}

#[test]
fn single_step_with_parallel_substeps() {
    let mut input = Parser::new();

    input.initialize(
        r#"
1. Main step
    - First substep
    - Second substep
            "#,
    );
    let result = input.read_step_dependent();

    assert_eq!(
        result,
        Ok(Scope::DependentBlock {
            ordinal: "1",
            description: vec![Paragraph(vec![Descriptive::Text("Main step")])],
            subscopes: vec![
                Scope::ParallelBlock {
                    bullet: '-',
                    description: vec![Paragraph(vec![Descriptive::Text("First substep")])],
                    subscopes: vec![],
                },
                Scope::ParallelBlock {
                    bullet: '-',
                    description: vec![Paragraph(vec![Descriptive::Text("Second substep")])],
                    subscopes: vec![],
                },
            ],
        })
    );
}

#[test]
fn multiple_steps_with_substeps() {
    let mut input = Parser::new();

    input.initialize(
        r#"
1. First step
    a. Substep
2. Second step
            "#,
    );
    let first_result = input.read_step_dependent();
    let second_result = input.read_step_dependent();

    assert_eq!(
        first_result,
        Ok(Scope::DependentBlock {
            ordinal: "1",
            description: vec![Paragraph(vec![Descriptive::Text("First step")])],
            subscopes: vec![Scope::DependentBlock {
                ordinal: "a",
                description: vec![Paragraph(vec![Descriptive::Text("Substep")])],
                subscopes: vec![],
            }],
        })
    );

    assert_eq!(
        second_result,
        Ok(Scope::DependentBlock {
            ordinal: "2",
            description: vec![Paragraph(vec![Descriptive::Text("Second step")])],
            subscopes: vec![],
        })
    );
}

#[test]
fn is_step_with_failing_input() {
    let test_input = "1. Have you done the first thing in the first one?\n    a. Do the first thing. Then ask yourself if you are done:\n        'Yes' | 'No' but I have an excuse\n2. Do the second thing in the first one.";

    // Test each line that should be a step
    assert!(is_step_dependent(
        "1. Have you done the first thing in the first one?"
    ));
    assert!(is_step_dependent(
        "2. Do the second thing in the first one."
    ));

    // Test lines that should NOT be steps
    assert!(!is_step_dependent(
        "    a. Do the first thing. Then ask yourself if you are done:"
    ));
    assert!(!is_step_dependent(
        "        'Yes' | 'No' but I have an excuse"
    ));

    // Finally, test content over multiple lines
    assert!(is_step_dependent(test_input));
}

#[test]
fn read_step_with_content() {
    let mut input = Parser::new();

    input.initialize(
        r#"
1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
    );

    let result = input.read_step_dependent();

    // Should parse the complete first step with substeps
    assert_eq!(
        result,
        Ok(Scope::DependentBlock {
            ordinal: "1",
            description: vec![Paragraph(vec![Descriptive::Text(
                "Have you done the first thing in the first one?"
            )])],
            subscopes: vec![Scope::DependentBlock {
                ordinal: "a",
                description: vec![Paragraph(vec![Descriptive::Text(
                    "Do the first thing. Then ask yourself if you are done:"
                )])],
                subscopes: vec![Scope::ResponseBlock {
                    responses: vec![
                        Response {
                            value: "Yes",
                            condition: None
                        },
                        Response {
                            value: "No",
                            condition: Some("but I have an excuse")
                        }
                    ]
                }]
            }],
        })
    );

    assert_eq!(
        input.source,
        "2. Do the second thing in the first one.\n            "
    );
}

#[test]
fn read_procedure_step_isolation() {
    let mut input = Parser::new();

    input.initialize(
        r#"
first : A -> B

# The First

This is the first one.

1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
    );

    let result = input.read_procedure();

    // This should pass if read_procedure correctly isolates step content
    match result {
        Ok(procedure) => {
            let steps = procedure
                .elements
                .iter()
                .find_map(|element| match element {
                    Element::Steps(steps) => Some(steps),
                    _ => None,
                });
            assert_eq!(
                steps
                    .unwrap()
                    .len(),
                2
            );
        }
        Err(_e) => {
            panic!("read_procedure failed");
        }
    }
}

#[test]
fn take_block_lines_with_is_step() {
    let mut input = Parser::new();

    input.initialize(
        r#"
1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
    );

    let result = input.take_block_lines(is_step_dependent, is_step_dependent, |inner| {
        Ok(inner.source)
    });

    match result {
        Ok(content) => {
            // Should isolate first step including substeps, stop at second step
            assert!(content.contains("1. Have you done"));
            assert!(content.contains("a. Do the first thing"));
            assert!(!content.contains("2. Do the second thing"));

            // Remaining should be the second step
            assert_eq!(
                input.source,
                "2. Do the second thing in the first one.\n            "
            );
        }
        Err(_) => {
            panic!("take_block_lines() failed");
        }
    }
}

#[test]
fn is_step_line_by_line() {
    // Test is_step on each line of our test content
    let lines = [
        "1. Have you done the first thing in the first one?",
        "    a. Do the first thing. Then ask yourself if you are done:",
        "        'Yes' | 'No' but I have an excuse",
        "2. Do the second thing in the first one.",
    ];

    for (i, line) in lines
        .iter()
        .enumerate()
    {
        let is_step_result = is_step_dependent(line);

        match i {
            0 => assert!(is_step_result, "First step line should match is_step"),
            1 | 2 => assert!(
                !is_step_result,
                "Substep/response lines should NOT match is_step"
            ),
            3 => assert!(is_step_result, "Second step line should match is_step"),
            _ => {}
        }
    }
}

#[test]
fn take_block_lines_title_description_pattern() {
    let mut input = Parser::new();

    // Test the exact pattern used in read_procedure for title/description extraction
    input.initialize(
        r#"
# The First

This is the first one.

1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
    );

    let result = input.take_block_lines(
        |_| true,                       // start predicate (always true)
        |line| is_step_dependent(line), // end predicate (stop at first step)
        |inner| Ok(inner.source),
    );

    match result {
        Ok(content) => {
            // The isolated content should be title + description, stopping at first step
            assert!(content.contains("# The First"));
            assert!(content.contains("This is the first one."));
            assert!(!content.contains("1. Have you done"));

            // The remaining content should include ALL steps and substeps
            let remaining = input
                .source
                .trim_ascii_start();
            assert!(remaining.starts_with("1. Have you done"));
            assert!(remaining.contains("a. Do the first thing"));
            assert!(remaining.contains("2. Do the second thing"));
        }
        Err(_e) => {
            panic!("take_block_lines failed");
        }
    }
}

#[test]
fn test_potential_procedure_declaration_is_superset() {
    // All valid procedure declarations must be matched by potential_procedure_declaration

    // Valid simple declarations
    assert!(is_procedure_declaration("foo : A -> B"));
    assert!(potential_procedure_declaration("foo : A -> B"));

    assert!(is_procedure_declaration("my_proc :"));
    assert!(potential_procedure_declaration("my_proc :"));

    assert!(is_procedure_declaration("step123 : Input -> Output"));
    assert!(potential_procedure_declaration("step123 : Input -> Output"));

    // Valid with parameters
    assert!(is_procedure_declaration("process(a, b) : X -> Y"));
    assert!(potential_procedure_declaration("process(a, b) : X -> Y"));

    assert!(is_procedure_declaration("calc(x) :"));
    assert!(potential_procedure_declaration("calc(x) :"));

    // Invalid that should only match potential_
    assert!(!is_procedure_declaration("MyProcedure :")); // Capital letter
    assert!(potential_procedure_declaration("MyProcedure :"));

    assert!(!is_procedure_declaration("123foo :")); // Starts with digit
    assert!(potential_procedure_declaration("123foo :"));

    // Neither should match sentences with spaces
    assert!(!is_procedure_declaration("Ask these questions :"));
    assert!(!potential_procedure_declaration("Ask these questions :"));

    // Edge cases with whitespace
    assert!(!is_procedure_declaration("  :")); // No name
    assert!(!potential_procedure_declaration("  :"));

    assert!(is_procedure_declaration("  foo  :  ")); // Whitespace around
    assert!(potential_procedure_declaration("  foo  :  "));

    // Verify the superset property systematically
    let test_cases = vec![
        "a :",
        "z :",
        "abc :",
        "test_123 :",
        "foo_bar_baz :",
        "x() :",
        "func(a) :",
        "proc(a, b, c) :",
        "test(x,y,z) :",
        "a_1 :",
        "test_ :",
        "_test :", // Underscores
    ];

    for case in test_cases {
        if is_procedure_declaration(case) {
            assert!(
                potential_procedure_declaration(case),
                "potential_procedure_declaration must match all valid declarations: {}",
                case
            );
        }
    }
}

#[test]
fn test_take_block_lines_procedure_wrapper() {
    let mut input = Parser::new();

    // Test the outer take_block_lines call that wraps read_procedure
    input.initialize(
        r#"
first : A -> B

# The First

This is the first one.

1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
    );

    let result = input.take_block_lines(
        is_procedure_declaration,
        is_procedure_declaration,
        |outer| Ok(outer.source),
    );

    match result {
        Ok(isolated_content) => {
            // Since there's only one procedure, the outer take_block_lines should capture everything
            assert!(isolated_content.contains("first : A -> B"));
            assert!(isolated_content.contains("# The First"));
            assert!(isolated_content.contains("This is the first one."));
            assert!(isolated_content.contains("1. Have you done the first thing in the first one?"));
            assert!(isolated_content.contains("a. Do the first thing"));
            assert!(isolated_content.contains("2. Do the second thing"));
        }
        Err(_e) => {
            panic!("take_block_lines failed");
        }
    }
}

#[test]
fn code_blocks() {
    let mut input = Parser::new();

    // Test simple identifier in code block
    input.initialize("{ count }");
    let result = input.read_code_block();
    assert_eq!(result, Ok(Expression::Variable(Identifier("count"))));

    // Test function with simple parameter
    input.initialize("{ sum(count) }");
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("sum"),
            parameters: vec![Expression::Variable(Identifier("count"))]
        }))
    );

    // Test function with multiple parameters
    input.initialize("{ consume(apple, banana, chocolate) }");
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("consume"),
            parameters: vec![
                Expression::Variable(Identifier("apple")),
                Expression::Variable(Identifier("banana")),
                Expression::Variable(Identifier("chocolate"))
            ]
        }))
    );

    // Test function with text parameter
    input.initialize("{ exec(\"Hello, World\") }");
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("exec"),
            parameters: vec![Expression::String(vec![Piece::Text("Hello, World")])]
        }))
    );

    // Test function with multiline string parameter
    input.initialize(
        r#"{ exec(```bash
ls -l
echo "Done"```) }"#,
    );
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("exec"),
            parameters: vec![Expression::Multiline(
                Some("bash"),
                vec!["ls -l", "echo \"Done\""]
            )]
        }))
    );

    // Test function with quantity parameter (like timer with duration)
    input.initialize("{ timer(3 hr) }");
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("timer"),
            parameters: vec![Expression::Number(Numeric::Scientific(Quantity {
                mantissa: Decimal {
                    number: 3,
                    precision: 0
                },
                uncertainty: None,
                magnitude: None,
                symbol: "hr"
            }))]
        }))
    );

    // Test function with integer quantity parameter
    input.initialize("{ measure(100) }");
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("measure"),
            parameters: vec![Expression::Number(Numeric::Integral(100))]
        }))
    );

    // Test function with decimal quantity parameter
    input.initialize("{ wait(2.5 s, \"yes\") }");
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("wait"),
            parameters: vec![
                Expression::Number(Numeric::Scientific(Quantity {
                    mantissa: Decimal {
                        number: 25,
                        precision: 1
                    },
                    uncertainty: None,
                    magnitude: None,
                    symbol: "s"
                })),
                Expression::String(vec![Piece::Text("yes")])
            ]
        }))
    );
}

#[test]
fn multiline() {
    let mut input = Parser::new();

    // Test multiline with consistent indentation that should be trimmed
    input.initialize(
        r#"{ exec(```bash
        ./stuff

        if [ true ]
        then
            ./other args
        fi```) }"#,
    );
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("exec"),
            parameters: vec![Expression::Multiline(
                Some("bash"),
                vec![
                    "./stuff",
                    "",
                    "if [ true ]",
                    "then",
                    "    ./other args",
                    "fi"
                ]
            )]
        }))
    );

    // Test multiline without language tag
    input.initialize(
        r#"{ exec(```
ls -l
echo "Done"```) }"#,
    );
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("exec"),
            parameters: vec![Expression::Multiline(None, vec!["ls -l", "echo \"Done\""])]
        }))
    );

    // Test multiline with intentional empty lines in the middle
    input.initialize(
        r#"{ exec(```shell
echo "Starting"

echo "Middle section"


echo "Ending"```) }"#,
    );
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("exec"),
            parameters: vec![Expression::Multiline(
                Some("shell"),
                vec![
                    "echo \"Starting\"",
                    "",
                    "echo \"Middle section\"",
                    "",
                    "",
                    "echo \"Ending\""
                ]
            )]
        }))
    );

    // Test that internal indentation relative to the base is preserved,
    // and also that nested parenthesis don't break the enclosing
    // take_block_chars() used to capture the input to the function.
    input.initialize(
        r#"{ exec(```python
    def hello():
        print("Hello")
        if True:
            print("World")

    hello()```) }"#,
    );
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("exec"),
            parameters: vec![Expression::Multiline(
                Some("python"),
                vec![
                    "def hello():",
                    "    print(\"Hello\")",
                    "    if True:",
                    "        print(\"World\")",
                    "",
                    "hello()"
                ]
            )]
        }))
    );

    // Test that a trailing empty line from the closing delimiter is removed
    input.initialize(
        r#"{ exec(```
echo test
```) }"#,
    );
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("exec"),
            parameters: vec![Expression::Multiline(None, vec!["echo test"])]
        }))
    );

    // Test various indentation edge cases
    input.initialize(
        r#"{ exec(```yaml
  name: test
  items:
    - item1
    - item2
  config:
    enabled: true```) }"#,
    );
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Execution(Function {
            target: Identifier("exec"),
            parameters: vec![Expression::Multiline(
                Some("yaml"),
                vec![
                    "name: test",
                    "items:",
                    "  - item1",
                    "  - item2",
                    "config:",
                    "  enabled: true"
                ]
            )]
        }))
    );
}

#[test]
fn tablets() {
    let mut input = Parser::new();

    // Test simple single-entry tablet
    input.initialize(r#"{ ["name" = "Johannes Grammerly"] }"#);
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Tablet(vec![Pair {
            label: "name",
            value: Expression::String(vec![Piece::Text("Johannes Grammerly")])
        }]))
    );

    // Test multiline tablet with string values
    input.initialize(
        r#"{ [
    "name" = "Alice of Chains"
    "age" = "29"
] }"#,
    );
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Tablet(vec![
            Pair {
                label: "name",
                value: Expression::String(vec![Piece::Text("Alice of Chains")])
            },
            Pair {
                label: "age",
                value: Expression::String(vec![Piece::Text("29")])
            }
        ]))
    );

    // Test tablet with mixed value types
    input.initialize(
        r#"{ [
    "answer" = 42
    "message" = msg
    "timestamp" = now()
] }"#,
    );
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Tablet(vec![
            Pair {
                label: "answer",
                value: Expression::Number(Numeric::Integral(42))
            },
            Pair {
                label: "message",
                value: Expression::Variable(Identifier("msg"))
            },
            Pair {
                label: "timestamp",
                value: Expression::Execution(Function {
                    target: Identifier("now"),
                    parameters: vec![]
                })
            }
        ]))
    );

    // Test empty tablet
    input.initialize("{ [ ] }");
    let result = input.read_code_block();
    assert_eq!(result, Ok(Expression::Tablet(vec![])));

    // Test tablet with interpolated string values
    input.initialize(
        r#"{ [
    "context" = "Details about the thing"
    "status" = active
] }"#,
    );
    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Tablet(vec![
            Pair {
                label: "context",
                value: Expression::String(vec![Piece::Text("Details about the thing")])
            },
            Pair {
                label: "status",
                value: Expression::Variable(Identifier("active"))
            }
        ]))
    );
}

#[test]
fn numeric_literals() {
    let mut input = Parser::new();

    // Test simple integer
    input.initialize("{ 42 }");
    let result = input.read_code_block();
    assert_eq!(result, Ok(Expression::Number(Numeric::Integral(42))));

    // Test negative integer
    input.initialize("{ -123 }");
    let result = input.read_code_block();
    assert_eq!(result, Ok(Expression::Number(Numeric::Integral(-123))));

    // Test zero
    input.initialize("{ 0 }");
    let result = input.read_code_block();
    assert_eq!(result, Ok(Expression::Number(Numeric::Integral(0))));
}

#[test]
fn reading_identifiers() {
    let mut input = Parser::new();

    // Parse a basic identifier
    input.initialize("hello");
    let result = input.read_identifier();
    assert_eq!(result, Ok(Identifier("hello")));
    assert_eq!(input.source, "");

    // Parse an identifier with trailing content
    input.initialize("count more");
    let result = input.read_identifier();
    assert_eq!(result, Ok(Identifier("count")));
    assert_eq!(input.source, " more");

    // Parse an identifier with leading whitespace and trailing content
    input.initialize("  \t  test_name  after");
    let result = input.read_identifier();
    assert_eq!(result, Ok(Identifier("test_name")));
    assert_eq!(input.source, "  after");

    // Parse an identifier with various delimiters
    input.initialize("name(param)");
    let result = input.read_identifier();
    assert_eq!(result, Ok(Identifier("name")));
    assert_eq!(input.source, "(param)");
}

#[test]
fn test_foreach_expression() {
    let mut input = Parser::new();
    input.initialize("{ foreach item in items }");

    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Foreach(
            vec![Identifier("item")],
            Box::new(Expression::Variable(Identifier("items")))
        ))
    );
}

#[test]
fn foreach_tuple_pattern() {
    let mut input = Parser::new();
    input.initialize("{ foreach (design, component) in zip(designs, components) }");

    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Foreach(
            vec![Identifier("design"), Identifier("component")],
            Box::new(Expression::Execution(Function {
                target: Identifier("zip"),
                parameters: vec![
                    Expression::Variable(Identifier("designs")),
                    Expression::Variable(Identifier("components"))
                ]
            }))
        ))
    );

    input.initialize("{ foreach (a, b, c) in zip(list1, list2, list3) }");

    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Foreach(
            vec![Identifier("a"), Identifier("b"), Identifier("c")],
            Box::new(Expression::Execution(Function {
                target: Identifier("zip"),
                parameters: vec![
                    Expression::Variable(Identifier("list1")),
                    Expression::Variable(Identifier("list2")),
                    Expression::Variable(Identifier("list3"))
                ]
            }))
        ))
    );
}

#[test]
fn tuple_binding_expression() {
    let mut input = Parser::new();
    input.initialize("{ <get_coordinates>() ~ (x, y) }");

    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Binding(
            Box::new(Expression::Application(Invocation {
                target: Target::Local(Identifier("get_coordinates")),
                parameters: Some(vec![])
            })),
            vec![Identifier("x"), Identifier("y")]
        ))
    );
}

#[test]
fn test_repeat_expression() {
    let mut input = Parser::new();
    input.initialize("{ repeat count }");

    let result = input.read_code_block();
    assert_eq!(
        result,
        Ok(Expression::Repeat(Box::new(Expression::Variable(
            Identifier("count")
        ))))
    );
}

#[test]
fn test_foreach_keyword_boundary() {
    // Test that "foreach" must be a complete word
    let mut input = Parser::new();
    input.initialize("{ foreachitem in items }");

    let result = input.read_code_block();
    // Should fail because "foreachitem" is parsed but "in items" is leftover content
    assert_eq!(result, Err(ParsingError::InvalidCodeBlock(2, 11)));
}

#[test]
fn test_repeat_keyword_boundary() {
    // Test that "repeat" must be a complete word
    let mut input = Parser::new();
    input.initialize("{ repeater }");

    let result = input.read_code_block();
    // Should parse as identifier, not repeat
    assert_eq!(result, Ok(Expression::Variable(Identifier("repeater"))));
}

#[test]
fn test_foreach_in_keyword_boundary() {
    // Test that "in" must be a complete word in foreach
    let mut input = Parser::new();
    input.initialize("{ foreach item instead items }");

    let result = input.read_code_block();
    // Should fail because "instead" doesn't match "in"
    assert!(result.is_err());
}

#[test]
fn splitting_by() {
    let mut input = Parser::new();

    // Test splitting simple comma-separated identifiers
    input.initialize("apple, banana, cherry");
    let result = input.take_split_by(',', |inner| inner.read_identifier());
    assert_eq!(
        result,
        Ok(vec![
            Identifier("apple"),
            Identifier("banana"),
            Identifier("cherry")
        ])
    );
    assert_eq!(input.source, "");

    // Test splitting with extra whitespace
    input.initialize("  un  |  deux  |  trois  ");
    let result = input.take_split_by('|', |inner| inner.read_identifier());
    assert_eq!(
        result,
        Ok(vec![
            Identifier("un"),
            Identifier("deux"),
            Identifier("trois")
        ])
    );

    // Ensure a single item (no delimiter present in input) works
    input.initialize("seulement");
    let result = input.take_split_by(',', |inner| inner.read_identifier());
    assert_eq!(result, Ok(vec![Identifier("seulement")]));

    // an empty chunk causes an error
    input.initialize("un,,trois");
    let result = input.take_split_by(',', |inner| inner.read_identifier());
    assert!(result.is_err());

    // empty trailing chunk causes an error
    input.initialize("un,deux,");
    let result = input.take_split_by(',', |inner| inner.read_identifier());
    assert!(result.is_err());

    // different split character
    input.initialize("'Yes'|'No'|'Maybe'");
    let result = input.take_split_by('|', |inner| {
        validate_response(inner.source).ok_or(ParsingError::IllegalParserState(inner.offset, 0))
    });
    assert_eq!(
        result,
        Ok(vec![
            Response {
                value: "Yes",
                condition: None
            },
            Response {
                value: "No",
                condition: None
            },
            Response {
                value: "Maybe",
                condition: None
            }
        ])
    );
}

#[test]
fn reading_responses() {
    let mut input = Parser::new();

    // Test single response
    input.initialize("'Yes'");
    let result = input.read_responses();
    assert_eq!(
        result,
        Ok(vec![Response {
            value: "Yes",
            condition: None
        }])
    );

    // Test multiple responses
    input.initialize("'Yes' | 'No'");
    let result = input.read_responses();
    assert_eq!(
        result,
        Ok(vec![
            Response {
                value: "Yes",
                condition: None
            },
            Response {
                value: "No",
                condition: None
            }
        ])
    );

    // Test three responses
    input.initialize("'Yes' | 'No' | 'Not Applicable'");
    let result = input.read_responses();
    assert_eq!(
        result,
        Ok(vec![
            Response {
                value: "Yes",
                condition: None
            },
            Response {
                value: "No",
                condition: None
            },
            Response {
                value: "Not Applicable",
                condition: None
            }
        ])
    );

    // Test response with condition
    input.initialize("'Yes' and equipment available");
    let result = input.read_responses();
    assert_eq!(
        result,
        Ok(vec![Response {
            value: "Yes",
            condition: Some("and equipment available")
        }])
    );

    // Test responses with whitespace
    input.initialize("  'Option A'  |  'Option B'  ");
    let result = input.read_responses();
    assert_eq!(
        result,
        Ok(vec![
            Response {
                value: "Option A",
                condition: None
            },
            Response {
                value: "Option B",
                condition: None
            }
        ])
    );
}

#[test]
fn reading_attributes() {
    let mut input = Parser::new();

    // Test simple role
    input.initialize("@chef");
    let result = input.read_attributes();
    assert_eq!(result, Ok(vec![Attribute::Role(Identifier("chef"))]));

    // Test simple place
    input.initialize("^kitchen");
    let result = input.read_attributes();
    assert_eq!(result, Ok(vec![Attribute::Place(Identifier("kitchen"))]));

    // Test multiple roles
    input.initialize("@master_chef + @barista");
    let result = input.read_attributes();
    assert_eq!(
        result,
        Ok(vec![
            Attribute::Role(Identifier("master_chef")),
            Attribute::Role(Identifier("barista"))
        ])
    );

    // Test multiple places
    input.initialize("^kitchen + ^bath_room");
    let result = input.read_attributes();
    assert_eq!(
        result,
        Ok(vec![
            Attribute::Place(Identifier("kitchen")),
            Attribute::Place(Identifier("bath_room"))
        ])
    );

    // Test mixed roles and places
    input.initialize("@chef + ^bathroom");
    let result = input.read_attributes();
    assert_eq!(
        result,
        Ok(vec![
            Attribute::Role(Identifier("chef")),
            Attribute::Place(Identifier("bathroom"))
        ])
    );

    // Test mixed places and roles
    input.initialize("^kitchen + @barista");
    let result = input.read_attributes();
    assert_eq!(
        result,
        Ok(vec![
            Attribute::Place(Identifier("kitchen")),
            Attribute::Role(Identifier("barista"))
        ])
    );

    // Test complex mixed attributes
    input.initialize("@chef + ^kitchen + @barista + ^dining_room");
    let result = input.read_attributes();
    assert_eq!(
        result,
        Ok(vec![
            Attribute::Role(Identifier("chef")),
            Attribute::Place(Identifier("kitchen")),
            Attribute::Role(Identifier("barista")),
            Attribute::Place(Identifier("dining_room"))
        ])
    );

    // Test invalid - uppercase
    input.initialize("^Kitchen");
    let result = input.read_attributes();
    assert!(result.is_err());

    // Test invalid - no marker
    input.initialize("kitchen");
    let result = input.read_attributes();
    assert!(result.is_err());
}

#[test]
fn step_with_role_assignment() {
    let mut input = Parser::new();

    // Test step with role assignment
    input.initialize(
        r#"
1. Check the patient's vital signs
        @nurse
            "#,
    );
    let result = input.read_step_dependent();

    let scope = result.expect("Expected dependent step with role assignment");

    assert_eq!(
        scope,
        Scope::DependentBlock {
            ordinal: "1",

            description: vec![Paragraph(vec![Descriptive::Text(
                "Check the patient's vital signs"
            )])],
            subscopes: vec![Scope::AttributeBlock {
                attributes: vec![Attribute::Role(Identifier("nurse"))],
                subscopes: vec![],
            }]
        }
    );
}

#[test]
fn substep_with_role_assignment() {
    let mut input = Parser::new();

    // Test step with role assignment and substep
    input.initialize(
        r#"
1. Verify patient identity
        @surgeon
            a. Check ID
            "#,
    );
    let result = input.read_step_dependent();

    let scope = result.expect("Expected dependent step with role assignment");

    assert_eq!(
        scope,
        Scope::DependentBlock {
            ordinal: "1",
            description: vec![Paragraph(vec![Descriptive::Text(
                "Verify patient identity"
            )])],
            subscopes: vec![Scope::AttributeBlock {
                attributes: vec![Attribute::Role(Identifier("surgeon"))],
                subscopes: vec![Scope::DependentBlock {
                    ordinal: "a",
                    description: vec![Paragraph(vec![Descriptive::Text("Check ID")])],
                    subscopes: vec![]
                }]
            }]
        }
    );
}

#[test]
fn parallel_step_with_role_assignment() {
    let mut input = Parser::new();

    // Test step with role assignment and parallel substep
    input.initialize(
        r#"
1. Monitor patient vitals
        @nursing_team
            - Check readings
            "#,
    );
    let result = input.read_step_dependent();

    let scope = result.expect("Expected dependent step with role assignment");

    assert_eq!(
        scope,
        Scope::DependentBlock {
            ordinal: "1",
            description: vec![Paragraph(vec![Descriptive::Text("Monitor patient vitals")])],
            subscopes: vec![Scope::AttributeBlock {
                attributes: vec![Attribute::Role(Identifier("nursing_team"))],
                subscopes: vec![Scope::ParallelBlock {
                    bullet: '-',
                    description: vec![Paragraph(vec![Descriptive::Text("Check readings")])],
                    subscopes: vec![]
                }]
            }]
        }
    );
}

#[test]
fn two_roles_with_substeps() {
    let mut input = Parser::new();

    // Test two roles each with one substep
    input.initialize(
        r#"
1. Review events.
        @surgeon
            a. What are the steps?
        @nurse
            b. What are the concerns?
            "#,
    );

    let result = input.read_step_dependent();

    let scope = result.expect("Failed to parse two roles with substeps");

    assert_eq!(
        scope,
        Scope::DependentBlock {
            ordinal: "1",
            description: vec![Paragraph(vec![Descriptive::Text("Review events.")])],
            subscopes: vec![
                Scope::AttributeBlock {
                    attributes: vec![Attribute::Role(Identifier("surgeon"))],
                    subscopes: vec![Scope::DependentBlock {
                        ordinal: "a",
                        description: vec![Paragraph(vec![Descriptive::Text(
                            "What are the steps?"
                        )])],
                        subscopes: vec![]
                    }]
                },
                Scope::AttributeBlock {
                    attributes: vec![Attribute::Role(Identifier("nurse"))],
                    subscopes: vec![Scope::DependentBlock {
                        ordinal: "b",
                        description: vec![Paragraph(vec![Descriptive::Text(
                            "What are the concerns?"
                        )])],
                        subscopes: vec![]
                    }]
                }
            ]
        }
    );
}

#[test]
fn parse_collecting_errors_basic() {
    let mut input = Parser::new();

    // Test with valid content - should have no errors
    input.initialize("% technique v1\nvalid_proc : A -> B\n# Title\nDescription");
    let result = input.parse_collecting_errors();
    match result {
        Ok(document) => {
            assert!(document
                .header
                .is_some());
            assert!(document
                .body
                .is_some());
        }
        Err(_) => panic!("Expected successful parse for valid content"),
    }

    // Test with invalid header - should collect header error
    input.initialize("% wrong v1");
    let result = input.parse_collecting_errors();
    match result {
        Ok(_) => panic!("Expected errors for invalid content"),
        Err(errors) => {
            assert!(errors.len() > 0);
            assert!(errors
                .iter()
                .any(|e| matches!(e, ParsingError::InvalidHeader(_, _))));
        }
    }

    // Test that the method returns Result instead of ParseResult
    input.initialize("some content");
    let _result: Result<Document, Vec<ParsingError>> = input.parse_collecting_errors();
    // If this compiles, the method signature is correct
}

#[test]
fn test_multiple_error_collection() {
    use std::path::Path;

    // Create a string with 3 procedures: 2 with errors and 1 valid
    let content = r#"
broken_proc1 : A ->
    # This procedure has incomplete signature

    1. Do something

valid_proc : A -> B
    # This is a valid procedure

    1. Valid step
    2. Another valid step

broken_proc2 : -> B
    # This procedure has incomplete signature (missing domain)

    1. Do something else
        "#;

    let result = parse_with_recovery(Path::new("test.t"), content);

    // Assert that there are at least 2 errors (from the broken procedures)
    match result {
        Ok(_) => panic!("Result should have errors"),
        Err(errors) => {
            let l = errors.len();
            assert!(l >= 2, "Should have at least 2 errors, got {}", l)
        }
    };
}

#[test]
fn test_redundant_error_removal_needed() {
    use std::path::Path;

    // Create a malformed procedure that could generate multiple errors at the same offset
    let content = r#"
% technique v1

broken :
    This is not a valid signature line

    1. Step one
        "#;

    let result = parse_with_recovery(Path::new("test.tq"), content);

    // Check that we get an error about the invalid signature
    match result {
        Err(errors) => {
            // Debug: print what errors we actually get
            eprintln!("Errors: {:?}", errors);

            // Verify no redundant errors at the same offset
            let mut offsets = errors
                .iter()
                .map(|e| e.offset())
                .collect::<Vec<_>>();
            offsets.sort();
            let original_len = offsets.len();
            offsets.dedup();
            assert_eq!(
                offsets.len(),
                original_len,
                "Found redundant errors at same offset"
            );
        }
        Ok(_) => panic!("Expected errors for malformed content"),
    }
}

#[test]
fn test_redundant_error_removal_unclosed_interpolation() {
    let mut input = Parser::new();

    // Test that UnclosedInterpolation error takes precedence over generic
    // ExpectedMatchingChar
    input.initialize(r#"{ "string with {unclosed interpolation" }"#);
    let result = input.read_code_block();

    // Should get the specific UnclosedInterpolation error, not a generic
    // one
    match result {
        Err(ParsingError::UnclosedInterpolation(_, _)) => {
            // Good - we got the specific error
        }
        Err(other) => {
            panic!("Expected UnclosedInterpolation error, got: {:?}", other);
        }
        Ok(_) => {
            panic!("Expected error for unclosed interpolation, but parsing succeeded");
        }
    }
}

#[test]
fn multiline_code_inline() {
    let mut input = Parser::new();

    // Test multiline code inline in descriptive text
    let source = r#"
This is { exec(a,
 b, c)
 } a valid inline.
            "#;

    input.initialize(source);
    let result = input.read_descriptive();

    assert!(
        result.is_ok(),
        "Multiline code inline should parse successfully"
    );

    let paragraphs = result.unwrap();
    assert_eq!(paragraphs.len(), 1, "Should have exactly one paragraph");

    let descriptives = &paragraphs[0].0;
    assert_eq!(
        descriptives.len(),
        3,
        "Should have 3 descriptive elements: text, code inline, text"
    );

    // First element should be "This is"
    match &descriptives[0] {
        Descriptive::Text(text) => assert_eq!(*text, "This is"),
        _ => panic!("First element should be text"),
    }

    // Second element should be the multiline code inline
    match &descriptives[1] {
        Descriptive::CodeInline(Expression::Execution(func)) => {
            assert_eq!(
                func.target
                    .0,
                "exec"
            );
            assert_eq!(
                func.parameters
                    .len(),
                3
            );
            // Check that all parameters were parsed correctly
            if let Expression::Variable(Identifier(name)) = &func.parameters[0] {
                assert_eq!(*name, "a");
            } else {
                panic!("First parameter should be variable 'a'");
            }
        }
        _ => panic!("Second element should be code inline with function execution"),
    }

    // Third element should be "a valid inline."
    match &descriptives[2] {
        Descriptive::Text(text) => assert_eq!(*text, "a valid inline."),
        _ => panic!("Third element should be text"),
    }
}
