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

includes re-wrapping of descriptive paragraphs and properly laying out nested
steps, substeps, and sub-substeps.

If redirected to a file or pipe ANSI colours are stripped from the output.

## Rendering

Technique documents can be rendered to PDF.

A template will be selected based on the "domain" the document is written in.

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
document. Each step, substep, sub-substep, scope, and enclosing procedure,
section, and the document as a whole has a Result, which is {`Done`, `Skip`,
or `Fail` } and a Value, often unit "Unitus" `()`, string "Literali" literals
 `"Some content here"`, or numeric "Quanticle" literals `42`. Arrays
 "Arraeum", key/value tables "Tabularum", and other more complex types are
 defined in @src/value/types.rs

As the document is evaluated the current scope is printed and the user is
prompted a result, usually by pressing `<Enter>`. There is a menu available
via the user pressing `<Esc>`.

Results are recorded in a state store under _.store/_ in the current
directory. Files are named for the input document with a _.pfftt_ extension
under the "run id" identifying the instantiation, 42 in this example, printed
at start and at finish or interruption.

./.store/000042/File.pfftt

An interrupted run can be continued

- `cargo run -- resume 42`

where `42` is the run identifier.

## Running automatically as a script

A document can instead be run automatically

- `cargo run -- run --mode=automatic File.tq`

which will proceed to call any functions that create effects, placing the
result in the `Done` value, and otherwise will `Skip` prose that an automated
program cannot act on. External processes that do not return `0` will result
in `Fail`.
