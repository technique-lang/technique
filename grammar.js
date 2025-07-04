module.exports = grammar({
  name: "technique",

  // Only spaces, tabs, and carriage returns are extras; newlines are significant.
  extras: ($) => [/[ \t\r]/],

  rules: {
    // A source file: optional header_block (if present, must be first), then declarations.
    source_file: ($) =>
      seq(
        optional(seq($.header_block, $.newline)),
        $.procedure_declaration,
        repeat(seq($.newline, $.procedure_declaration)),
        optional($.newline),
      ),

    // Header block: magic line (required), then optional SPDX and template lines.
    header_block: ($) =>
      seq(
        $.magic_line,
        optional($.spdx_line),
        optional($.template_line)
      ),

    magic_line: ($) =>
      seq("%", /[ \t]*/, "technique", /[ \t]+/, /v[0-9]+/, $.newline),
    spdx_line: ($) => seq("!", /[^\n]+/, $.newline),
    template_line: ($) => seq("&", /[^\n]+/, $.newline),

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

    // Genus: Unit, Simple, List, or Tuple
    genus: ($) =>
      choice($.unit_genus, $.simple_genus, $.list_genus, $.tuple_genus),
    unit_genus: ($) => seq("(", ")"),
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
