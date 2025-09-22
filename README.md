# Technique

This is the Technique Procedure Language, a programming language for
describing procedures in a structured but human-readable form. You can
consider it a domain-specific language (DSL) for writing procedures and
checklists.

This language design has evolved over a long period, 20+ years, starting with
on-paper procedures for systems operations tasks, and then going through
different iterations of program and approach. The current version is Technique
v1, written in Rust.

The Technique language allows you to write instructions to be read and
followed other humans. As a result, Technique doesn't look much like
programming code, but it is nevertheless a formally defined specification for
writing procedures and clear rules for executing them and recording their
outcomes.

This repository contains the _technique_ binary, which is the compiler for
Technique v1, along with a code formatter, and machinery to render procedures
as PDFs. Syntax highlighting is available for Vim, the Zed Editor, Sublime
Text, and the Typst typesetter. There's a language server, and an extension
for Zed, with VS Code and NeoVim on the way.

- Syntax checking, code formatter, renderer (this repository) \
  <https://github.com/technique-lang/technique>
- Formal specification \
  <https://github.com/technique-lang/specification>
- Zed Editor support  \
  <https://github.com/technique-lang/extension.zed>
- Tree Sitter grammar (for syntax highlighting) \
  <https://github.com/technique-lang/tree-sitter-technique>
- Typst and Vim support  \
  <https://github.com/technique-lang/highlighting>
