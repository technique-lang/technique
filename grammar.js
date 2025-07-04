module.exports = grammar({
  name: "technique",

  // We define whitespace (excluding newlines) and comments as "extras".
  // This means they can appear anywhere in the source code without being
  // explicitly part of the grammar rules. The newline character is omitted
  // here because it is a significant part of our grammar—it terminates a
  // procedure declaration.
  extras: ($) => [
    /[ \t\r]/, // All whitespace characters except newline
    $.comment,
  ],

  rules: {
    // A source file is composed of zero or more procedure declarations,
    // each of which must be properly terminated.
    source_file: ($) => repeat($._terminated_procedure_declaration),

    // To enforce that every declaration is on its own line, we create a
    // wrapper rule that pairs a `procedure_declaration` with a newline token.
    // This makes the grammar cleaner and more robust.
    _terminated_procedure_declaration: ($) =>
      seq($.procedure_declaration, "\n"),

    // A procedure declaration can be one of two valid forms, which we
    // represent using `choice`.
    procedure_declaration: ($) =>
      choice(
        // Form 1: A name followed by a colon. e.g., `my_procedure :`
        seq(field("name", $.identifier), ":"),
        // Form 2: A name with a full type signature. e.g., `add : Int -> Int`
        seq(
          field("name", $.identifier),
          ":",
          field("parameters", $.type),
          "->",
          field("return_type", $.type),
        ),
      ),

    // A 'type' can be a simple name, a list, or a tuple.
    type: ($) => choice($.simple_type, $.list_type, $.tuple_type),

    // A simple type is just an identifier (e.g., "Coffee").
    simple_type: ($) => $.identifier,

    // A list type is a type enclosed in square brackets (e.g., "[Beans]").
    list_type: ($) => seq("[", $.type, "]"),

    // A tuple type is a parenthesized, comma-separated list of types.
    // This grammar supports empty tuples `()`, single-element tuples `(A)`,
    // and multi-element tuples `(A, B)`, with an optional trailing comma.
    tuple_type: ($) =>
      seq(
        "(",
        optional(seq($.type, repeat(seq(",", $.type)), optional(","))),
        ")",
      ),

    // An identifier is used for names and types. It must start with a letter
    // or underscore, and can be followed by letters, numbers, or underscores.
    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    // A comment is a line starting with `//` and extending to the end of the line.
    comment: ($) => token(seq("//", /.*/)),
  },
});
