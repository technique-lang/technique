module.exports = grammar({
  name: "technique",

  // Only spaces, tabs, and carriage returns are extras; newlines are significant.
  extras: ($) => [/[ \t\r]/],

  rules: {
    // A source file is one or more lines of content.
    source_file: ($) =>
      seq(
        $.procedure_declaration_full_signature,
        repeat(seq($.newline, $.procedure_declaration_full_signature)),
        optional($.newline),
      ),

    // A line can be either a procedure declaration or a blank line.
    _line: ($) =>
      choice($.procedure_declaration_full_signature, $.blank_line),

    // Procedure Declaration (full signature form)
    procedure_declaration_full_signature: ($) =>
      seq(
        field("name", $.identifier),
        $.colon,
        field("parameters", $.identifier),
        $.arrow,
        field("return_type", $.identifier),
      ),

    // A type is, for now, just a simple identifier.
    type: ($) => $.identifier,

    // An identifier is a standard C-style identifier.
    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    // A blank line: only whitespace (or empty) followed by a newline.
    blank_line: ($) => /[ \t\r]*\n/,

    // --- Explicit Tokens ---
    colon: ($) => ":",
    arrow: ($) => "->",
    newline: ($) => "\n",
  },
});
