module.exports = grammar({
  name: "technique",

  // Only spaces, tabs, and carriage returns are extras; newlines are significant.
  extras: ($) => [/[ \t\r]/],

  rules: {
    // A source file is one or more procedure declarations, separated by newlines.
    source_file: ($) =>
      seq(
        $.procedure_declaration,
        repeat(seq($.newline, $.procedure_declaration)),
        optional($.newline),
      ),

    procedure_declaration: ($) =>
      seq(
        field("name", $.identifier),
        ":",
        optional(
          seq(
            field("parameters", $.genus),
            "->",
            field("return_type", $.genus),
          ),
        ),
      ),

    // Genus: Simple, List, or Tuple
    genus: ($) => choice($.simple_genus, $.list_genus, $.tuple_genus),
    simple_genus: ($) => $.forma,
    list_genus: ($) => seq("[", $.forma, "]"),
    tuple_genus: ($) => seq("(", $.forma, repeat(seq(",", $.forma)), ")"),

    // Forma: must start with uppercase letter, then letters or digits
    forma: ($) => /[A-Z][a-zA-Z0-9]*/,

    // Identifiers for procedure names
    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    // Newline as a literal
    newline: ($) => "\n",
  },
});
