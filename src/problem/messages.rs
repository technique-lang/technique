use crate::problem::Present;
use technique::{formatting::Render, language::*, parsing::parser::ParsingError};

/// Generate problem and detail messages for parsing errors using AST construction
pub fn generate_error_message<'i>(error: &ParsingError, renderer: &dyn Render) -> (String, String) {
    match error {
        ParsingError::IllegalParserState(_) => (
            "Illegal parser state".to_string(),
            "Internal parser error. This should not have happened! Sorry.".to_string(),
        ),
        ParsingError::Unimplemented(_) => (
            "Feature not yet implemented".to_string(),
            "This feature is planned but not yet available.".to_string(),
        ),
        ParsingError::Unrecognized(_) => (
            "Unrecognized input".to_string(),
            "The parser encountered unexpected content".to_string(),
        ),
        ParsingError::Expected(_, value) => (
            format!("Expected {}", value),
            format!(
                "The parser was looking for {} but found something else.",
                value
            ),
        ),
        ParsingError::ExpectedMatchingChar(_, subject, start, end) => (
            format!("Expected matching character '{}'", end),
            format!(
                r#"
The parser was expecting {} enclosed by '{}' and '{}' but
there was no more input remaining in the current scope.
                "#,
                subject, start, end
            )
            .trim_ascii()
            .to_string(),
        ),
        ParsingError::UnclosedInterpolation(_) => (
            "Unclosed string interpolation".to_string(),
            r#"
Every '{' that starts an interpolation within a string must have a
corresponding '}' to mark where the interpolation ends and the string
literal resumes.
            "#
            .trim_ascii()
            .to_string(),
        ),
        ParsingError::InvalidHeader(_) => {
            // Format the sample metadata using the same code as the formatter
            let mut formatted_example = String::new();
            formatted_example
                .push_str(&renderer.style(crate::formatting::Syntax::Header, "% technique v1\n"));
            formatted_example.push_str(&renderer.style(
                crate::formatting::Syntax::Header,
                "! «license»; © «copyright»\n",
            ));
            formatted_example
                .push_str(&renderer.style(crate::formatting::Syntax::Header, "& «template»"));

            (
                "Invalid header".to_string(),
                format!(
                    r#"
The metadata describing a Technique file must follow this format:

{}

The first line are the magic bytes identifying the Technique file format and
current language version.

The second line lists the System Package Data Exchange (SPDX) information
about the ownership of the Technique in this file and permissions associated
with it. The line is optional but if present it starts with a «license»
declaration, conventionally an SPDX identifier like {}, {}, or
{}. A declaration of the «copyright» holder can optionally follow
the license statement, separated from it by a semicolon. Copyright statements
typically list the year and then the name of the person or entity holding the
copyright.

The third line optionally specifies the template to be used when rendering the
Technique. Common templates include {}, {}, and
{}.
            "#,
                    formatted_example,
                    renderer.style(crate::formatting::Syntax::Header, "MIT"),
                    renderer.style(crate::formatting::Syntax::Header, "CC-BY 4.0"),
                    renderer.style(crate::formatting::Syntax::Header, "Proprietary"),
                    renderer.style(crate::formatting::Syntax::Header, "checklist"),
                    renderer.style(crate::formatting::Syntax::Header, "nasa-flight-plan,v4.0"),
                    renderer.style(crate::formatting::Syntax::Header, "recipe")
                ),
            )
        }
        ParsingError::InvalidCharacter(_, c) => (
            format!("Invalid character '{}'", c),
            "This character is not allowed here.".to_string(),
        ),
        ParsingError::UnexpectedEndOfInput(_) => (
            "Unexpected end of input".to_string(),
            "The file ended before the parser expected it to".to_string(),
        ),
        ParsingError::InvalidIdentifier(_, _) => {
            let examples = vec![
                Procedure {
                    name: Identifier("make_coffee"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                },
                Procedure {
                    name: Identifier("attempt1"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                },
                Procedure {
                    name: Identifier("i"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                },
                Procedure {
                    name: Identifier("l33t_hax0r"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                },
            ];

            (
                "Invalid identifier".to_string(),
                format!(
                    r#"
Identifiers must start with a lowercase letter and contain only lower case
letters, numbers, and underscores. Valid examples include:
    {}
    {}
    {}
    {}
                    "#,
                    examples[0].present(renderer),
                    examples[1].present(renderer),
                    examples[2].present(renderer),
                    examples[3].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidForma(_) => {
            let examples = vec![
                Forma("Coffee"),
                Forma("Ingredients"),
                Forma("PatientRecord"),
            ];

            (
                "Invalid forma name".to_string(),
                format!(
                    r#"
The names of Forma (the basic types in Technique) must start with an uppercase
letter and cannot contain dashes, underscores, spaces, or other punctuation.
For example:
    {}
    {}
    {}
                    "#,
                    examples[0].present(renderer),
                    examples[1].present(renderer),
                    examples[2].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidGenus(_) => {
            let examples = vec![
                Genus::Single(Forma("Coffee")),
                Genus::Tuple(vec![Forma("Beans"), Forma("Water")]),
                Genus::Naked(vec![Forma("Beans"), Forma("Water")]),
                Genus::List(Forma("Patient")),
                Genus::Unit,
            ];

            (
                "Invalid genus".to_string(),
                format!(
                    r#"
Genus are the full types in Technique. They are either simple (just the name
of a Forma) or compound (lists or tuples of Forma). Some examples:

    {}
    {}
    {}
    {}
    {}

Tuples can be enclosed in parenthesis or "naked"; semantically they are the
same. The final example is the syntax for the Unit genus, used when something
doesn't have an input or result, per se.
                    "#,
                    examples[0].present(renderer),
                    examples[1].present(renderer),
                    examples[2].present(renderer),
                    examples[3].present(renderer),
                    examples[4].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidSignature(_) => {
            let examples = vec![
                Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B")),
                },
                Signature {
                    domain: Genus::Tuple(vec![Forma("Beans"), Forma("Milk")]),
                    range: Genus::Single(Forma("Coffee")),
                },
                Signature {
                    domain: Genus::List(Forma("FunctionalRequirement")),
                    range: Genus::Single(Forma("Architecture")),
                },
            ];

            (
                "Invalid signature".to_string(),
                format!(
                    r#"
Procedure signatures follow the pattern domain -> range, where domain and
range are genus. Some examples:

    {}
    {}
    {}

Signatures are optional on procedure declarations but if present must follow
this form.
                    "#,
                    examples[0].present(renderer),
                    examples[1].present(renderer),
                    examples[2].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidDeclaration(_) => {
            let examples = vec![
                Procedure {
                    name: Identifier("f"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                },
                Procedure {
                    name: Identifier("implementation"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                },
                Procedure {
                    name: Identifier("make_coffee"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                },
                Procedure {
                    name: Identifier("f"),
                    parameters: None,
                    signature: Some(Signature {
                        domain: Genus::Single(Forma("A")),
                        range: Genus::Single(Forma("B")),
                    }),
                    elements: Vec::new(),
                },
                Procedure {
                    name: Identifier("implementation"),
                    parameters: None,
                    signature: Some(Signature {
                        domain: Genus::Single(Forma("Design")),
                        range: Genus::Single(Forma("Product")),
                    }),
                    elements: Vec::new(),
                },
                Procedure {
                    name: Identifier("make_coffee"),
                    parameters: None,
                    signature: Some(Signature {
                        domain: Genus::Naked(vec![Forma("Beans"), Forma("Milk")]),
                        range: Genus::Single(Forma("Coffee")),
                    }),
                    elements: Vec::new(),
                },
                Procedure {
                    name: Identifier("make_coffee"),
                    parameters: None,
                    signature: Some(Signature {
                        domain: Genus::Tuple(vec![Forma("Beans"), Forma("Milk")]),
                        range: Genus::Single(Forma("Coffee")),
                    }),
                    elements: Vec::new(),
                },
                Procedure {
                    name: Identifier("make_coffee"),
                    parameters: Some(vec![Identifier("b"), Identifier("m")]),
                    signature: Some(Signature {
                        domain: Genus::Naked(vec![Forma("Beans"), Forma("Milk")]),
                        range: Genus::Single(Forma("Coffee")),
                    }),
                    elements: Vec::new(),
                },
            ];

            (
                "Invalid procedure declaration".to_string(),
                format!(
                    r#"
Procedures are declared by specifying an identifier as a name, followed by a
colon:

    {}
    {}
    {}

A procedure can optionally have a signature, as in the following examples:

    {}
    {}
    {}
    {}

Finally, variables can be assigned for the names of the input parameters:

    {}
                    "#,
                    examples[0].present(renderer),
                    examples[1].present(renderer),
                    examples[2].present(renderer),
                    examples[3].present(renderer),
                    examples[4].present(renderer),
                    examples[5].present(renderer),
                    examples[6].present(renderer),
                    examples[7].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidSection(_) => {
            // Roman numeral sections don't have AST representation
            (
                "Invalid section heading".to_string(),
                format!(
                    r#"
Section headings use capital Roman numerals, followed by optional title:

    I. First Section
    II. Second Section
    III.

Conventionally such a title would be in Proper Case but that is left up to the
author of the Technique.
                    "#
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidInvocation(_) => {
            let examples = vec![
                Invocation {
                    target: Target::Local(Identifier("make_coffee")),
                    parameters: None,
                },
                Invocation {
                    target: Target::Local(Identifier("check_vitals")),
                    parameters: Some(vec![Expression::Variable(Identifier("patient"))]),
                },
            ];

            (
                "Invalid procedure invocation".to_string(),
                format!(
                    r#"
To denote the invocation of another procedure, use angle brackets:

    {}

If the procedure takes parameters they can be specified in parenthesis:

    {}
                    "#,
                    examples[0].present(renderer),
                    examples[1].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidFunction(_) => {
            let examples = vec![
                Function {
                    target: Identifier("exec"),
                    parameters: vec![Expression::String(vec![Piece::Text("ls -la")])],
                },
                Function {
                    target: Identifier("now"),
                    parameters: vec![],
                },
                Function {
                    target: Identifier("calculate"),
                    parameters: vec![
                        Expression::Variable(Identifier("a")),
                        Expression::Variable(Identifier("b")),
                    ],
                },
            ];

            (
                "Invalid function call".to_string(),
                format!(
                    r#"
Function calls in code blocks are made by specifying the name of the function
to be executed followed by parentheses, supplying values, variables, or other
expressions as parameters as required:

    {}
    {}
    {}
                    "#,
                    examples[0].present(renderer),
                    examples[1].present(renderer),
                    examples[2].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidCodeBlock(_) => {
            let examples = vec![
                Expression::Execution(Function {
                    target: Identifier("exec"),
                    parameters: vec![Expression::String(vec![Piece::Text("command")])],
                }),
                Expression::Repeat(Box::new(Expression::Number(Numeric::Integral(5)))),
                Expression::Foreach(
                    vec![Identifier("patient")],
                    Box::new(Expression::Variable(Identifier("patients"))),
                ),
            ];

            (
                "Invalid code block".to_string(),
                format!(
                    r#"
Inline code blocks are enclosed in braces:

    {{ {} }}
    {{ {} }}
    {{ {} }}
                    "#,
                    examples[0].present(renderer),
                    examples[1].present(renderer),
                    examples[2].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidMultiline(_) => (
            "Invalid multi-line string".to_string(),
            r#"
Multi-line strings can be written by surrounding the content in triple
backticks:

    ```
    In those days spirits were brave, the stakes were high, men were real men,
    women were real women and small furry creatures from Alpha Centauri were
    real small furry creatures from Alpha Centauri.
    ```

The leading and trailing newline will be trimmed. In addition, any whitespace
indenting the string will be removed. So

    ```bash
        if [ -f /etc/passwd ]
        then
            echo "Found the password file"
        fi
    ```

would result in a string with the `if` on the left margin but `echo` indented
by 4 spaces. This example also shows that the «language» of the content can be
specified. Doing so does not change the meaning of the multi-line string, but
it may be used by output templates when rendering the procedure.
            "#
            .trim_ascii()
            .to_string(),
        ),
        ParsingError::InvalidStep(_) => (
            "Invalid step format".to_string(),
            r#"
Steps must start with a number or lower-case letter (in the case of dependent
steps and sub-steps, respectively) followed by a '.':

    1.  First step
    2.  Second step
        a.  First substep
        b.  Second substep

Steps or substeps that can execute in parallel can instead be marked with a
dash. They can be done in either order, or concurrently:

    -   Do one thing
    -   And another
                "#
            .trim_ascii()
            .to_string(),
        ),
        ParsingError::InvalidSubstep(_) => (
            "Invalid substep format".to_string(),
            r#"
Substeps can be nested below top-level dependent steps or top-level parallel
steps. So both of these are valid:

    1.  First top-level step.
        a.  First substep in first dependent step.
        b.  Second substep in first dependent step.

and

    -   First top-level step to be done in any order.
        a.  First substep in first parallel step.
        b.  Second substep in first parallel step.

The ordinal must be a lowercase letter and not a roman numeral. By convention
substeps are indented by 4 characters, but that is not required.

Note also that the substeps can be consecutively numbered, which allows each
substep to be uniquely identified when they are grouped under different
parallel steps, but again this is not compulsory.
                "#
            .trim_ascii()
            .to_string(),
        ),
        ParsingError::InvalidForeach(_) => {
            let examples = vec![
                Expression::Foreach(
                    vec![Identifier("patient")],
                    Box::new(Expression::Variable(Identifier("patients"))),
                ),
                Expression::Foreach(
                    vec![Identifier("name"), Identifier("value")],
                    Box::new(Expression::Variable(Identifier("data"))),
                ),
            ];

            (
                "Invalid foreach loop".to_string(),
                format!(
                    r#"
Loops follow this pattern:

    {{ {} }}
    {{ {} }}

In the first example `patients` would be a list; in the latter case `data` is
a list of tuples.
                    "#,
                    examples[0].present(renderer),
                    examples[1].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidResponse(_) => {
            let examples = vec![
                vec![
                    Response {
                        value: "Rock",
                        condition: None,
                    },
                    Response {
                        value: "Paper",
                        condition: None,
                    },
                    Response {
                        value: "Scissors",
                        condition: None,
                    },
                ],
                vec![Response {
                    value: "Confirmed",
                    condition: None,
                }],
                vec![
                    Response {
                        value: "Yes",
                        condition: Some("but with explanation"),
                    },
                    Response {
                        value: "No",
                        condition: None,
                    },
                ],
            ];

            (
                "Invalid response format".to_string(),
                format!(
                    r#"
The fixed choices that are valid for the result of a given step can be
enumerated. These responses are each enclosed in single quotes, separated by
the '|' character:

    {}
    {}
    {}

By convention the response values are Proper Case.
                    "#,
                    examples[0]
                        .iter()
                        .map(|r| r.present(renderer))
                        .collect::<Vec<_>>()
                        .join(" | "),
                    examples[1][0].present(renderer),
                    examples[2]
                        .iter()
                        .map(|r| r.present(renderer))
                        .collect::<Vec<_>>()
                        .join(" | ")
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidIntegral(_) => {
            let examples = vec![
                Numeric::Integral(42),
                Numeric::Integral(-123),
                Numeric::Integral(0),
                Numeric::Integral(9223372036854775807),
            ];

            (
                "Invalid integer literal".to_string(),
                format!(
                    r#"
Integer literals must be positive of negative whole numbers:
    {}
    {}
    {}
    {}

Integers cannot contain decimal points or units."#,
                    examples[0].present(renderer),
                    examples[1].present(renderer),
                    examples[2].present(renderer),
                    examples[3].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidQuantity(_) => {
            let examples = vec![
                Numeric::Scientific(Quantity {
                    mantissa: Decimal {
                        number: 42,
                        precision: 1,
                    },
                    uncertainty: None,
                    magnitude: None,
                    symbol: "kg",
                }),
                Numeric::Scientific(Quantity {
                    mantissa: Decimal {
                        number: 100,
                        precision: 0,
                    },
                    uncertainty: None,
                    magnitude: None,
                    symbol: "m/s",
                }),
                Numeric::Scientific(Quantity {
                    mantissa: Decimal {
                        number: 420,
                        precision: 0,
                    },
                    uncertainty: Some(Decimal {
                        number: 10,
                        precision: 0,
                    }),
                    magnitude: None,
                    symbol: "kg",
                }),
                Numeric::Scientific(Quantity {
                    mantissa: Decimal {
                        number: 42,
                        precision: 1,
                    },
                    uncertainty: None,
                    magnitude: Some(2),
                    symbol: "kg",
                }),
            ];

            (
                "Invalid quantity format".to_string(),
                format!(
                    r#"
Quantities are measurements with units:
    {}
    {}

They can optionally have uncertainty:
    {}

a magnitude:
    {}"#,
                    examples[0].present(renderer),
                    examples[1].present(renderer),
                    examples[2].present(renderer),
                    examples[3].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidQuantityDecimal(_) => (
            "Invalid number in quantity".to_string(),
            r#"
The numeric part of a quantity may be positive or negative, and may have a
decimal point:
    42
    -123.45
    0.001

Values less than 1 must have a leading '0' before the decimal."#
                .trim_ascii()
                .to_string(),
        ),
        ParsingError::InvalidQuantityUncertainty(_) => (
            "Invalid uncertainty in quantity".to_string(),
            r#"
Uncertainty values must be positive numbers:
    4.2 ± 0.1 kg    (valid)
    100 +/- 5 m/s   (valid)

You can use '±' or `+/-`, followed by a decimal."#
                .trim_ascii()
                .to_string(),
        ),
        ParsingError::InvalidQuantityMagnitude(_) => (
            "Invalid magnitude format".to_string(),
            r#"
The magnitude of a quantity can be expressed in the usual scientific format
× 10ⁿ; for ease of writing you can use ASCII to write * or x and 10^n, as in
the following examples:
    × 10^24
    × 10²⁴
    * 10^-6
    x 10^12

The base must be 10, and the exponent must be an integer."#
                .trim_ascii()
                .to_string(),
        ),
        ParsingError::InvalidQuantitySymbol(_) => {
            let examples = vec![
                Numeric::Scientific(Quantity {
                    mantissa: Decimal {
                        number: 84,
                        precision: 0,
                    },
                    uncertainty: None,
                    magnitude: None,
                    symbol: "kg",
                }),
                Numeric::Scientific(Quantity {
                    mantissa: Decimal {
                        number: 30,
                        precision: 1,
                    },
                    uncertainty: None,
                    magnitude: Some(8),
                    symbol: "m/s",
                }),
                Numeric::Scientific(Quantity {
                    mantissa: Decimal {
                        number: 16,
                        precision: 0,
                    },
                    uncertainty: Some(Decimal {
                        number: 15,
                        precision: 1,
                    }),
                    magnitude: None,
                    symbol: "°C",
                }),
                Numeric::Scientific(Quantity {
                    mantissa: Decimal {
                        number: 3126,
                        precision: 1,
                    },
                    uncertainty: None,
                    magnitude: None,
                    symbol: "μs",
                }),
            ];

            (
                "Invalid character in quantity units".to_string(),
                format!(
                    r#"
Symbols used to denote units can contain:

    Letters 'A'..'z'    {}
    Rates '/':          {}
    Degrees '°':        {}
    SI prefixes 'μ':    {}

Hyphens, underscores, spaces, or subscripts are not valid in unit symbols.
                    "#,
                    examples[0].present(renderer),
                    examples[1].present(renderer),
                    examples[2].present(renderer),
                    examples[3].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
    }
}
