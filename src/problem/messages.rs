use crate::problem::Present;
use technique::{
    formatting::Render, language::*, linking::LinkingError, parsing::ParsingError,
    resolution::ResolutionError, runner::RunnerError, translation::TranslationError,
};

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
        ParsingError::MissingParenthesis(_) => {
            let examples = vec![Descriptive::Binding(
                Box::new(Descriptive::Application(Invocation {
                    target: Target::Local(Identifier::new("mix_pangalactic_gargle_blaster")),
                    parameters: None,
                })),
                vec![Identifier::new("zaphod"), Identifier::new("trillian")],
            )];

            (
                "Lists of binding variables must be enclosed in parentheses".to_string(),
                format!(
                    r#"
If you bind the result of an invocation to more than one variable, you must
enclose those names in parenthesis. For example:

    {}
                    "#,
                    examples[0].present(renderer),
                )
                .trim_ascii()
                .to_string(),
            )
        }
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
                .push_str(&renderer.style(crate::formatting::Syntax::Header, "& «domain»"));

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

The third line optionally specifies the domain or kind of Technique this is,
to be used when rendering the Technique. Common domains include
{}, {}, and {}.
            "#,
                    formatted_example,
                    renderer.style(crate::formatting::Syntax::Header, "MIT"),
                    renderer.style(crate::formatting::Syntax::Header, "CC-BY 4.0"),
                    renderer.style(crate::formatting::Syntax::Header, "Proprietary"),
                    renderer.style(crate::formatting::Syntax::Header, "checklist"),
                    renderer.style(crate::formatting::Syntax::Header, "nasa-esa-iss,v4.0"),
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
                    name: Identifier::new("make_coffee"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("attempt1"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("i"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("l33t_hax0r"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                    span: Span::default(),
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
                Forma::new("Coffee"),
                Forma::new("Ingredients"),
                Forma::new("PatientRecord"),
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
                Genus::Single(Forma::new("Coffee")),
                Genus::Tuple(vec![Forma::new("Beans"), Forma::new("Water")]),
                Genus::Naked(vec![Forma::new("Beans"), Forma::new("Water")]),
                Genus::List(Forma::new("Patient")),
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
                    requires: Genus::Single(Forma::new("A")),
                    provides: Genus::Single(Forma::new("B")),
                },
                Signature {
                    requires: Genus::Tuple(vec![Forma::new("Beans"), Forma::new("Milk")]),
                    provides: Genus::Single(Forma::new("Coffee")),
                },
                Signature {
                    requires: Genus::List(Forma::new("FunctionalRequirement")),
                    provides: Genus::Single(Forma::new("Architecture")),
                },
            ];

            (
                "Invalid signature".to_string(),
                format!(
                    r#"
Procedure signatures follow the pattern requires -> provides, where requires
and provides are genus. Some examples:

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
                    name: Identifier::new("f"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("implementation"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("make_coffee"),
                    parameters: None,
                    signature: None,
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("f"),
                    parameters: None,
                    signature: Some(Signature {
                        requires: Genus::Single(Forma::new("A")),
                        provides: Genus::Single(Forma::new("B")),
                    }),
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("implementation"),
                    parameters: None,
                    signature: Some(Signature {
                        requires: Genus::Single(Forma::new("Design")),
                        provides: Genus::Single(Forma::new("Product")),
                    }),
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("make_coffee"),
                    parameters: None,
                    signature: Some(Signature {
                        requires: Genus::Naked(vec![Forma::new("Beans"), Forma::new("Milk")]),
                        provides: Genus::Single(Forma::new("Coffee")),
                    }),
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("make_coffee"),
                    parameters: None,
                    signature: Some(Signature {
                        requires: Genus::Tuple(vec![Forma::new("Beans"), Forma::new("Milk")]),
                        provides: Genus::Single(Forma::new("Coffee")),
                    }),
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("make_coffee"),
                    parameters: Some(vec![Identifier::new("b"), Identifier::new("m")]),
                    signature: Some(Signature {
                        requires: Genus::Naked(vec![Forma::new("Beans"), Forma::new("Milk")]),
                        provides: Genus::Single(Forma::new("Coffee")),
                    }),
                    elements: Vec::new(),
                    span: Span::default(),
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
        ParsingError::InvalidParameters(_) => {
            let examples = vec![
                Procedure {
                    name: Identifier::new("create_bypass"),
                    parameters: Some(vec![Identifier::new("a"), Identifier::new("b")]),
                    signature: None,
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("bulldoze"),
                    parameters: Some(vec![Identifier::new("c")]),
                    signature: None,
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("lawsuit"),
                    parameters: None,
                    signature: Some(Signature {
                        requires: Genus::Single(Forma::new("Council")),
                        provides: Genus::List(Forma::new("Penny")),
                    }),
                    elements: Vec::new(),
                    span: Span::default(),
                },
                Procedure {
                    name: Identifier::new("lawsuit"),
                    parameters: Some(vec![Identifier::new("c")]),
                    signature: Some(Signature {
                        requires: Genus::Single(Forma::new("Council")),
                        provides: Genus::List(Forma::new("Penny")),
                    }),
                    elements: Vec::new(),
                    span: Span::default(),
                },
            ];

            (
                "Parameters must be enclosed in parentheses".to_string(),
                format!(
                    r#"
Parameters to a procedure must be variables, and enclosed in parentheses. For
example:

    {}
    {}

Naming the input genus is optional, however; these are both valid procedure
declarations (and in fact the same):

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
        ParsingError::MixedSectionContent(_) => (
            "Section mixes steps and procedures".to_string(),
            r#"
A section contains either steps or procedures, but not both. A section that
begins with steps cannot then declare a procedure:

    I. Mix Drink

        1.  Take the juice from one bottle
        2.  Pour in one measure of water

    prepare_mega_gin :

Writing steps directly under a heading is a convenient shorthand and perfectly
valid Technique, but to add a helper procedure alongside them you will need to
upgrade to the main part of the section being a named procedure, and then you
can write a helper procedure that follows

    I. Mix Drink

    mix_drink :

        1.  Take the juice from one bottle
        2.  Pour in one measure of water
        3.  Allow three cubes to melt <prepare_mega_gin>

    prepare_mega_gin :
                "#
            .trim_ascii()
            .to_string(),
        ),
        ParsingError::InvalidInvocation(_) => {
            let examples = vec![
                Invocation {
                    target: Target::Local(Identifier::new("make_coffee")),
                    parameters: None,
                },
                Invocation {
                    target: Target::Local(Identifier::new("check_vitals")),
                    parameters: Some(vec![Expression::Variable(
                        Identifier::new("patient"),
                        Span::default(),
                    )]),
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
                    target: Identifier::new("exec"),
                    parameters: vec![Expression::String(
                        vec![Piece::Text("ls -la")],
                        Span::default(),
                    )],
                },
                Function {
                    target: Identifier::new("now"),
                    parameters: vec![],
                },
                Function {
                    target: Identifier::new("calculate"),
                    parameters: vec![
                        Expression::Variable(Identifier::new("a"), Span::default()),
                        Expression::Variable(Identifier::new("b"), Span::default()),
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
        ParsingError::InvalidTuple(_) => {
            let examples = vec![
                Expression::Tuple(
                    vec![
                        Expression::Number(Numeric::Integral(2), Span::default()),
                        Expression::String(vec![Piece::Text("mice")], Span::default()),
                    ],
                    Span::default(),
                ),
                Expression::Unit(Span::default()),
            ];

            (
                "Invalid tuple syntax".to_string(),
                format!(
                    r#"
A tuple needs two or more values separated by commas, like {}.
Parentheses around a single value aren't necessary; just write the
value on its own. For an empty value use unit, written {}.
                    "#,
                    examples[0].present(renderer),
                    examples[1].present(renderer),
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidCodeBlock(_) => {
            let examples = vec![
                Expression::Execution(
                    Function {
                        target: Identifier::new("exec"),
                        parameters: vec![Expression::String(
                            vec![Piece::Text("command")],
                            Span::default(),
                        )],
                    },
                    Span::default(),
                ),
                Expression::Repeat(
                    Box::new(Expression::Number(Numeric::Integral(5), Span::default())),
                    Span::default(),
                ),
                Expression::Foreach(
                    vec![Identifier::new("patient")],
                    Box::new(Expression::Variable(
                        Identifier::new("patients"),
                        Span::default(),
                    )),
                    Span::default(),
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
        ParsingError::InvalidAttribute(_) => {
            let examples = vec![
                Scope::AttributeBlock {
                    attributes: vec![
                        Attribute::Role(
                            Identifier::new("president_of_the_galaxy"),
                            Span::default(),
                        ),
                        Attribute::Role(Identifier::new("femme_fatale"), Span::default()),
                    ],
                    subscopes: vec![],
                    span: Span::default(),
                },
                Scope::AttributeBlock {
                    attributes: vec![
                        Attribute::Place(Identifier::new("milliways"), Span::default()),
                        Attribute::Role(Identifier::new("waiter"), Span::default()),
                        Attribute::Role(Identifier::new("dish_of_the_day"), Span::default()),
                    ],
                    subscopes: vec![],
                    span: Span::default(),
                },
            ];

            (
                "Invalid attribute assignment".to_string(),
                format!(
                    r#"
Multiple attributes (be they role or place assignments) must be joined using
the '+' operator, for example:

    {}
    {}

Note that an attribute creates a scope, so sub-steps and code blocks can be
nested underneath a role or place assignment.
                "#,
                    examples[0].present(renderer),
                    examples[1].present(renderer)
                )
                .trim_ascii()
                .to_string(),
            )
        }
        ParsingError::InvalidForeach(_) => {
            let examples = vec![
                Expression::Foreach(
                    vec![Identifier::new("patient")],
                    Box::new(Expression::Variable(
                        Identifier::new("patients"),
                        Span::default(),
                    )),
                    Span::default(),
                ),
                Expression::Foreach(
                    vec![Identifier::new("name"), Identifier::new("value")],
                    Box::new(Expression::Variable(
                        Identifier::new("data"),
                        Span::default(),
                    )),
                    Span::default(),
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
                        span: Span::default(),
                    },
                    Response {
                        value: "Paper",
                        condition: None,
                        span: Span::default(),
                    },
                    Response {
                        value: "Scissors",
                        condition: None,
                        span: Span::default(),
                    },
                ],
                vec![Response {
                    value: "Confirmed",
                    condition: None,
                    span: Span::default(),
                }],
                vec![
                    Response {
                        value: "Yes",
                        condition: Some("but with explanation"),
                        span: Span::default(),
                    },
                    Response {
                        value: "No",
                        condition: None,
                        span: Span::default(),
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

/// Generate problem and detail messages for errors occurring during the
/// translation phase.
pub fn generate_translation_error<'i>(
    error: &TranslationError<'i>,
    _renderer: &dyn Render,
) -> (String, String) {
    match error {
        TranslationError::DuplicateProcedure(Identifier { value: name, .. }) => (
            format!("Duplicate procedure name '{}'", name),
            "A procedure with this name has already been declared in this document.".to_string(),
        ),
        TranslationError::DuplicateTitle {
            procedure: Identifier { value: name, .. },
            ..
        } => (
            format!("Duplicate title in procedure '{}'", name),
            "A procedure can have at most one title.".to_string(),
        ),
        TranslationError::InterleavedDescription {
            procedure: Identifier { value: name, .. },
            ..
        } => (
            format!("Description out of place in procedure '{}'", name),
            r#"
A procedure's free-text description must appear immediately after the
title and before any steps or code blocks.
            "#
            .trim_ascii()
            .to_string(),
        ),
        TranslationError::SignatureParameterMismatch {
            procedure: Identifier { value: name, .. },
            parameters,
            requires,
        } => (
            format!(
                "Parameter list of '{}' disagrees with its signature. {} names but {} required inputs",
                name, parameters, requires
            ),
            r#"
When a procedure declares both a parameter list and a signature, the named
parameters must correspond one-to-one with the inputs the signature
requires.
            "#
            .trim_ascii()
            .to_string(),
        ),
        TranslationError::BoundRepeat { .. } => (
            "Cannot use the result of `repeat`".to_string(),
            r#"
A `repeat` runs indefinitely and produces no value, so its result
cannot be bound to a variable.
            "#
            .trim_ascii()
            .to_string(),
        ),
        TranslationError::HeterogenousList { .. } => (
            "Mixed List and Tablet syntax".to_string(),
            r#"
A `[...]` literal must be either a Tablet (every entry in the list a
`"label" = value` pair) or a List (entries are actual values in
sequence), not a mix of the two.
            "#
            .trim_ascii()
            .to_string(),
        ),
    }
}

/// Generate problem and detail messages for errors occurring when a program's
/// internal references and variable scoping are resolved.
pub fn generate_resolution_error<'i>(
    error: &ResolutionError<'i>,
    _renderer: &dyn Render,
) -> (String, String) {
    match error {
        ResolutionError::UnresolvedProcedure(Identifier { value: name, .. }) => (
            format!("Unresolved procedure '{}'", name),
            r#"
A `<name>` invocation must refer to a procedure declared in this
document. Built-in functions use the `name(...)` form (without angle
brackets).
            "#
            .trim_ascii()
            .to_string(),
        ),
        ResolutionError::ProcedureArityMismatch {
            procedure: Identifier { value: name, .. },
            expected,
            actual,
        } => (
            format!(
                "Wrong number of arguments to <{}>. Expected {}, but called with {}",
                name, expected, actual
            ),
            r#"
A parenthesised argument list must supply one argument for each input the
signature requires (or, absent a signature, each declared parameter). Omit
the parentheses entirely to defer every argument.
            "#
            .trim_ascii()
            .to_string(),
        ),
        ResolutionError::UnboundVariable {
            variable: Identifier { value: name, .. },
        } => (
            format!("Unbound variable {}", name),
            r#"
The variable names nothing in scope: neither a parameter of the procedure nor a
result bound by a binding or loop within it.
            "#
            .trim_ascii()
            .to_string(),
        ),
    }
}

/// Generate problem and detail messages for errors occurring when function
/// references are linked against the available function table.
pub fn generate_linking_error<'i>(
    error: &LinkingError<'i>,
    renderer: &dyn Render,
) -> (String, String) {
    let examples = vec![
        Expression::Execution(
            Function {
                target: Identifier::new("panic"),
                parameters: vec![],
            },
            Span::default(),
        ),
        Expression::Execution(
            Function {
                target: Identifier::new("operate_kettle"),
                parameters: vec![Expression::Number(
                    Numeric::Scientific(Quantity {
                        mantissa: Decimal {
                            number: 100,
                            precision: 0,
                        },
                        uncertainty: None,
                        magnitude: None,
                        symbol: "°C",
                    }),
                    Span::default(),
                )],
            },
            Span::default(),
        ),
        Expression::Execution(
            Function {
                target: Identifier::new("enumerate"),
                parameters: vec![
                    Expression::String(vec![Piece::Text("men")], Span::default()),
                    Expression::String(vec![Piece::Text("women")], Span::default()),
                    Expression::String(
                        vec![Piece::Text("small furry creatures from Alpha Centauri")],
                        Span::default(),
                    ),
                ],
            },
            Span::default(),
        ),
    ];

    match error {
        LinkingError::ArityMismatch {
            function: Identifier { value: name, .. },
            expected,
            actual,
        } => (
            format!(
                "Wrong number of arguments to {}(), expected {} but called with {}",
                name, expected, actual
            ),
            format!(
                r#"
A function must be called with the correct number of arguments. Some example
functions calls:

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
        ),
        LinkingError::UnresolvedFunction {
            function: Identifier { value: name, .. },
        } => (
            format!("Unknown function {}()", name),
            "The function is neither builtin nor provided by the selected domain.".to_string(),
        ),
    }
}

/// Generate problem and detail messages for errors occurring when a procedure
/// is being evaluated by the runner.
pub fn generate_runner_error(error: &RunnerError, _renderer: &dyn Render) -> (String, String) {
    match error {
        RunnerError::NoSuchRun(run_id) => (
            format!("No such run '{:06}'", run_id.0),
            "The directory for this run identifier was not found in the local state store."
                .to_string(),
        ),
        RunnerError::StoreError { path, error } => (
            format!("I/O error with local state store at {}", path.display()),
            format!("{}", error),
        ),
        RunnerError::MalformedRecord { run_id, .. } => (
            format!("Malformed record for run '{:06}'", run_id.0),
            "The PFFTT state file for this run could not be parsed.".to_string(),
        ),
        RunnerError::StartMissing(run_id) => (
            format!("Start record missing in run '{:06}'", run_id.0),
            r#"
The state file is present but its first record (the Start event) is
missing or malformed.
            "#
            .trim_ascii()
            .to_string(),
        ),
        RunnerError::InvalidRunId(text) => (
            format!("Invalid run identifier '{}'", text),
            r#"
Run identifiers are integer values, conventionally rendered as six
zero-padded digits.
            "#
            .trim_ascii()
            .to_string(),
        ),
        RunnerError::MissingEntryProcedure => (
            "No entry procedure".to_string(),
            r#"
The document has neither procedure declarations nor top-level steps so
the runner can't start its walk.
            "#
            .trim_ascii()
            .to_string(),
        ),
        RunnerError::UnboundVariable(name) => (
            format!("Unbound variable '{}'", name),
            "The procedure has not yet supplied a value for this variable!".to_string(),
        ),
        RunnerError::BindArityMismatch { expected, actual } => (
            format!(
                "Binding arity mismatch: {} names but {} values",
                expected, actual
            ),
            r#"
Binding multiple variables requires the procedure being invoked or
function being called to return a tuple of the same size.
            "#
            .trim_ascii()
            .to_string(),
        ),
        RunnerError::BindNotTuple { expected } => (
            format!(
                "Binding requires a tuple: {} names but the value is not a tuple",
                expected
            ),
            r#"
Binding multiple variables requires the procedure being invoked or
function being called to return a tuple of the same size.
            "#
            .trim_ascii()
            .to_string(),
        ),
        RunnerError::ParameterArityMismatch {
            procedure,
            parameters,
            actual,
        } => (
            format!(
                "Wrong number of arguments: {} expects {} ({}) but {} given",
                procedure,
                parameters.len(),
                parameters.join(", "),
                actual
            ),
            r#"
Arguments after the filename are passed as the parameters for the entry
procedure at the top of the Technique document.
            "#
            .trim_ascii()
            .to_string(),
        ),
        RunnerError::ParameterUnexpected { procedure, actual } => (
            format!(
                "Unexpected arguments: {} takes no parameters but {} given",
                procedure, actual
            ),
            format!(
                r#"
Arguments were supplied on the command-line but {} doesn't take any
parameters.
                "#,
                procedure
            )
            .trim_ascii()
            .to_string(),
        ),
        RunnerError::NotIterable => (
            "Iteration requires a list".to_string(),
            r#"
The foreach loop control structure requires a list to iterate over, but the
value supplied isn't one. A tablet is a dictionary, not a sequence. If you want
to use the values from a tablet convert them into a list first with the
values() function. There is also a labels() function to get each of the
tablet's labels, and pairs() to get a sequence of tuples of labels and values
you can iterate over.
            "#
            .trim_ascii()
            .to_string(),
        ),
        RunnerError::InvalidArgument { function, expected } => (
            format!("Wrong argument type passed to {}()", function),
            format!(
                "The {}() function expected {} but was given something else.",
                function, expected
            ),
        ),
        RunnerError::UnknownFunction(function) => (
            format!("Unknown function {}()", function),
            format!(
                r#"
The function {}() is undefined. This should have been caught during the
linking phase!
                "#,
                function
            )
            .trim_ascii()
            .to_string(),
        ),
        RunnerError::FunctionArityMismatch {
            function,
            expected,
            actual,
        } => (
            format!("Wrong number of arguments to {}()", function),
            format!(
                "The function {}() expects {} but {} given.",
                function, expected, actual
            ),
        ),
        RunnerError::ExecError(error) => (
            "Could not run external command".to_string(),
            format!(
                "Launching or reading from the external command failed: {}.",
                error
            ),
        ),
        RunnerError::CommandFailed(code) => (
            format!("External command exited with status {}", code),
            "The shell command run by exec() finished with a non-zero exit status.".to_string(),
        ),
        RunnerError::IncompatibleCombination { left, right } => (
            format!("Cannot combine {} with {}", left, right),
            format!(
                r#"
Combining Values requires compatible kinds; a {} and a {} can't be added
together.
                "#,
                left, right
            )
            .trim_ascii()
            .to_string(),
        ),
        RunnerError::TerminalRequired => (
            "Running interactively requires a terminal".to_string(),
            r#"
An interactive run writes its prompts to the terminal and reads user input
direclty, so its output can't be redirected to a file or pipe. Use `technique
run` in a terminal, or use `--mode=automatic` to run on auto; you can then
safely redirect the output.
            "#
            .trim_ascii()
            .to_string(),
        ),
        RunnerError::UserQuit => (
            "Interrupted".to_string(),
            r#"
The user quit before the procedure was completed. Use `technique resume
<id>` to continue.
            "#
            .trim_ascii()
            .to_string(),
        ),
    }
}
