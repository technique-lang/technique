# Project Overview

We are developing the Technique procedure language, a programming language for
describing procedures in a structured but human readable form. You can
consider it a domain-specific language (DSL) for writing procedures and
checklists.

The current version is Technique v1, written in Rust.

Strictly the input source as a whole is a Technique "document" (and the
top-level definition in the grammar is a `Technique`), but the term
"procedure" is often loosely used to refer to a given Technique as a whole (in
the Technique language a `Procedure` is a named collection of steps,
optionally with a title, description, and types for input and output).

# Build and Development Commands

The main CLI has three major groups of subcommands.

- Syntax and type-checking, code formatting;

- Rendering procedures to PDF via Typst; and

- Running procedures.

## Compiling and checking

To syntax and type-check a procedure file:

- `cargo run -- check File.tq`

The internal AST that results from parsing can be seen using

- `cargo run -- check --output=native File.tq`

which is helpful if assessing the correctness of the parser.

The translated `Program` produced by the translator can be inspected with

- `cargo run -- check --until=translation --output=native File.tq`

Compiler phases are `parsing`, `translation`, `resolution`, and `linking`. By
default `check` runs the whole pipeline. The `--until` option allows you to
stop at an earlier phase so intermediate results can be inspected.

## Formatting

Format a Technique file with carefully refined syntax highlighting:

- `cargo run -- format File.tq`

This includes re-wrapping of descriptive paragraphs and properly laying out
nested steps, substeps, and sub-substeps (see @src/formatting/formatter.rs;
syntax highlighting lives in @src/highlighting/).

If redirected to a file or pipe ANSI colours are stripped from the output.

## Rendering

Technique documents can be rendered to PDF.

A template will be selected based on the "domain" the document is written in
(domains live in @src/domain/, templates in @src/templating/).

By default rendering a Technique will use the domain listed on a document's
"domain line" metadata header, if present, otherwise will fallback to the
`source` domain which renders the input document with syntax highlighting but
in PDF form.

The domain can be overridden with `--domain`. For example, to render a document
with the `procedure` domain instead, do:

- `cargo run -- render --domain=procedure File.tq`

To override the default built-in template with _./custom.typ_ specify the `--template` option:

- `cargo run -- render --domain=procedure --template=custom.typ File.tq`

To keep the intermediate files use `--keep`

- `cargo run -- render --keep --domain=procedure --template=custom.typ File.tq`

which leaves _.procedure.typ_ and _.File.typ_ in the same directory as source document _File.tq_, allowing them to be inspected.

finally, `--output` can switch the output from writing the `pdf` to disk to writing to `typst` to the terminal for debugging:

- `cargo run -- render --output=typst File.tq`

# Executing Technique

Technique documents can be _executed_.

## Running interactively

- `cargo run -- run File.tq`

This initiates a depth-first walk of the tree represented by the input
document (the runner lives in @src/runner/). Each step, substep, sub-substep,
scope, and enclosing procedure,
section, and the document as a whole has a Result, which is {`Done`, `Skip`,
or `Fail` } and a Value, often unit "Unitus" `()`, string literals "Literali"
`"Some content here"`, or numeric literals "Quanticle" `42`. Arrays "Arraeum",
key/value tables "Tabularum", and other more complex types are defined in
@src/value/types.rs

As the document is evaluated the current scope is printed and the user is
prompted a result, usually by pressing `<Enter>`. There is a menu available
via the user pressing `<Esc>`.

Results are written to `.store/` keyed by the "run id" identifying the
instantiation, 42 in this example, printed at start and at finish or
interruption (see [Result store](#result-store) below). An interrupted run can
be continued

- `cargo run -- resume 42`

where `42` is the run identifier.

## Running automatically as a script

A document can instead be run automatically

- `cargo run -- run --mode=automatic File.tq`

which will proceed to call any functions that create effects, placing the
result in the `Done` value, and otherwise will `Skip` prose that an automated
program cannot act on. External processes that do not return `0` will result
in `Fail`.

## Inspecting results

As with the parser, the internal representation of what would be written the
state store can instead be serialized to the terminal:

- `cargo run -- run --output=native File.tq`

which can be more direct that attempting to read the PFFTT file in the .store/
directory.

# Language Server

Technique has a language server implementation available for integration with
editors and IDEs via:

- `cargo run -- language`

It accepts commands and code over stdin and returns compilation errors and
other diagnostics in accordance with the Language Server Protocol (LSP). It is
not run by developers directly.

# Language Design

These files are trivial example procedures created during testing:

- tests/samples/parsing/HeaderAndDeclaration.tq
- examples/minimal/ExampleOfEverything.tq
- examples/minimal/SimpleList.tq
- tests/samples/parsing/TabletOfQuantity.tq
- tests/golden/parsing/ManyAttributes.tq
- tests/golden/runner/DemolitionBeams.tq


These files are complete real-world procedures:

- examples/prototype/SurgicalSafetyChecklist.tq
- examples/prototype/SystemsEngineeringProcess.tq
- examples/prototype/NetworkProbe.tq
- examples/prototype/DontPanic.tq
- examples/prototype/GovernmentForm.tq
- examples/prototype/DatabaseUpgrade.tq

These files contain deliberate mistakes for testing parser failures and error
message output:

- tests/broken/parsing/BadDeclaration.tq
- tests/broken/parsing/IllegalUnitSymbol.tq
- tests/broken/parsing/UnclosedInterpolation.tq
- tests/broken/translation/DuplicateProcedure.tq
- tests/broken/resolution/UnboundVariable.tq

Within all these examples, and reviewing the Abstract Syntax Tree found in
@src/language/types.rs, you can see:

## Metadata header:

- If files have a metadata header, they start with the magic string  
  `% technique v1` indicating the file format version 1.
- The optional SPDX line begins with `!` and expresses a license and
  optionally copyright declaration
- The optional domain line begins with `&` and the name indicating which kind
  of Technique this is.

## Technique

Valid Technique is either:

- a series of steps, or
- a procedure (which can itself contain sections enclosing further procedures).

The first procedure in a document is effectively the "main" procedure, the
entry point.

## Sections

Documents can be split into sections, marked with uppercase Roman numerals
(`I.`, `II.`, `III.`, ...). These are effectively sub-techniques unto
themselves.

This allows an author to either write a procedure, a series of steps, a series
of sections with steps (and substeps) as an outline, or a fully complex series
of sections each with a series of procedures within them.

## Procedures

- A procedure is marked as starting with a declaration of the form `name :
Input -> Output`. The signature part `Input -> Output` is optional. The `:`
  is required. There are optional parameters after the name, in the form
  `name(a, b, c)`.
- Procedures have that declaration, followed by an optional title beginning
  with `#`, followed by an optional free text description, followed by zero or
  more steps.

## Steps

- Steps are either:
  "dependent steps", typically with ordinals `1.`, `2.`,
  `3.`, ... with each step needing to be completed before proceeding to the
  next step (with substeps following the conventional pattern of `a.` , `b.`,
  `c.`, ..., and a third level of sub-substeps using lowercase Roman numerals
  `i.`, `ii.`, `iii.`, ...), or
  "parallel steps", marked with `-`.
- Attributes scope a step to a role (a person or function, marked with `@`)
  and/or a place (a location, marked with `^`), as in `@hitchhiker` or
  `^sleeping_quarters`. Multiple attributes can be combined with `+` (e.g.
  `@waiter + ^milliways`); the special `@*` resets the scope. Attributes
  are effectively parallel steps and create scopes within which parallel or
  dependent steps can be nested.
- This is well-formed but semantically incorrect:

```technique
invalid :
  - Top level parallel step
      - nested parallel substep
```
  the author's intent was to nest parallel steps but this is not possible to
  articulate in Technique because; there is no way for the parser to
  differentiate between the two. Unfortunately we cannot generate an error for
  this because of being whitespace agnostic.
- The free form descriptive text can be escaped to code using an inline code block, delimited with braces `{ ... }`
- Within code blocks there are basic control flow: `repeat`, `foreach` loops.
- Within code blocks there are function calls to builtin functions: `exec()`
- Other procedures in the file can be invoked with the syntax `<function_name>(params)`.
- Variable binding is done with `~` operator, with the variable following the
  result being bound. Bindings can occur in Expressions (that is CodeBlock and
  CodeInline) or naked, in Descriptives.
- Data structures are called tablets, with syntax as:

```technique
    [
        "label" = value,
        "label2" = value2
    ]
```

- Multiple choice responses like `'Yes' | 'No'` for an enum with two text values.

# Running

## Result store

Each step has a "result", and that result is recorded in the Procedure
interchange Format For Transferring Techniques (PFFTT), file extension
_.pfftt_.

This is a line-oriented file format where each line is of the form

```pfftt
2026-06-14T05:58:08.773Z 000042 /high_pressure_cleaning:/I/setup_machine:/1/b Done ()
```

where the fields are

- `2026-06-14T05:58:08.773Z` is the timestamp
- `000042` is the run identifier
- `/high_pressure_cleaning:/I/setup_machine:/1/b` is the fully qualified path,
  in this case top-level procedure `high_pressure_cleaning :` containing
  section `I`, containing a procedure `setup_machine :`, containing steps of
  which step `1` contains a substep `b`.
- `Done`, the state reflected by this PFFTT line
- `()` the value of the step or state, if any, in this case Unitus.

Results are written to the @./.store/ directory, and serve both as the
permanent record of a step having been completed and also as a trace allowing
the procedure to be resumed if interrupted. The line format is serialized by
@src/runner/state.rs, which is authoritative.

Note that the local .store/ is an implementation detail, and that a more
robust solution will in the future involve a proper database, either running
locally or via an external service.

# Implementation notes

## Code navigation

For Rust symbol lookups (definitions, references, call hierarchy, types), use
the deferred `LSP` tool (load via `ToolSearch select:LSP`). Use `Grep` tool
only for non-symbol text.

## Whitespace

The parser is whitespace agnostic. Indentation is not significant. The
formatter outputs a Technique in a canonical form (with scopes indented by 4
spaces, for example) but when reading input the parser is liberal in what it
accepts.

## Key modules

- @src/language/types.rs Technique language abstract syntax tree
  (AST); and
- @src/parsing/parser.rs parser implementation.

## Key types

- Scope, the recursive nesting node — steps, sections, attributes, response
  enums, and code blocks all live here as variants.
- Paragraph, a container of Descriptives, generally corresponding to a
  paragraph of text describing a procedure or step. Descriptive in turn
  contains text, invocations, code inlines, and variable bindings.

## Parser design

The parser is implemented as a stateful object with the reference to the
source and the offset of bytes consumed within that source. This allows the
parser to be zero-copy.

Each concept has a recognition function, for example `is_code_block()` and
`is_substep_dependent()` which is used to guard invocation of the relevant
parser consuming methods, such as `read_code_block()` and
`read_substep_dependent()` respectively. This is done as follows:

```rust
    let title = if is_procedure_title(self.source) {
        self.read_procedure_title()?;
    }
```

thus ensuring the reading function is guaranteed to actually be facing the
content it is expected to consume avoiding the need to test for start
conditions within that `read_*()` function.

There are a variety of `take_*()` methods, notably `take_block_lines()` which
consumes from a starting predicate to an ending predicate. This has been
extensively tested and should be considered 100% reliable.

## Translation, Resolution, and Linking

After parsing, a translation phase lowers the AST into a `Program` that the
runner walks (@src/program/types.rs is authoritative for the Intermediate
Representation).

- @src/translation/translator.rs — `translate()` and `TranslationError`
  (analog of `parser.rs` holding both `parse()` and `ParsingError`).
- @src/translation/checks/{translate,errors}.rs — hand-written test
  suite, source strings parsed inline through the real parser.

Key design decisions:

- Every translated node is an `Operation<'i>`. See @src/program/types.rs for
  the variants.
- Where the parsed AST already carries the right shape (descriptive
  paragraphs, signatures, response values, attribute lists), translation-side
  types borrow from the AST via `&'i ...` references. The executable spine is
  owned because that's where translation adds information (resolved subroutine
  references, for example).
- Translation-side types use names distinct from AST counterparts to
  minimise namespace overlap: `Subroutine` vs `language::Procedure`,
  `Fragment` vs `language::Piece`, `Entry` vs `language::Pair`.
- References to AST types are always qualified with `language::`
  (`language::Identifier`, `language::Paragraph`, ...). Don't import
  with a glob; don't bring single types into scope unqualified.
- `Loop` unifies `foreach`/`repeat` via `Loop { names, over:
  Option<Box<Operation>>, body }`; `over: None` means forever.
- Procedure invocations carry a `SubroutineRef::Unresolved(Identifier)`
  out of translation; the subsequent `resolution` phase
  @src/resolution/ replaces these with
  `SubroutineRef::Resolved(SubroutineId)`.
- Tests use multi-line raw strings parsed through the real parser, with
  `.trim_ascii()` so the `% technique v1` header lands at byte 0. Match the
  convention in @src/parsing/checks/parser.rs

# Resources

## Formal grammar

Formal definition of the language grammar (not used for parsing):

- https://github.com/technique-lang/specification/blob/main/technique.bnf possibly at @../specification/technique.bnf

Ambiguities are resolved in favour of the actual parser implementation here.

There is also a formal definition of the PFFTT format:

- https://github.com/technique-lang/specification/blob/main/pfftt.bnf possibly at @../specification/pfftt.bnf

## Syntax highlighting for IDEs

There is a Tree Sitter grammar used for doing syntax highlighting (only):

- https://github.com/technique-lang/tree-sitter-technique/ possibly at @../tree-sitter-technique/grammar.js

## Language Website

Prose descriptions and introductory material are available on the public website:

- https://www.technique-lang.org/tutorial/ a basic language and tool tutorial
- https://www.technique-lang.org/reference/ language reference and resources
- https://engrxiv.org/preprint/view/5911 formal paper on the fundamental nature of procedures