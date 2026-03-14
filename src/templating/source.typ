// Source domain template for Technique.
//
// Per-syntax-tag formatting functions called from Rust-generated markup.
// Each function is independently overridable via `--template`.

// -- Formatting functions ----------------------------------------------------

#let render-neutral(c) = raw(c)
#let render-indent(c) = raw(c)
#let render-newline() = linebreak()
#let render-header(c) = text(fill: rgb(0x75, 0x50, 0x7b), raw(c))
#let render-declaration(c) = text(fill: rgb(0x34, 0x65, 0xa4), weight: "bold", raw(c))
#let render-description(c) = raw(c)
#let render-forma(c) = text(fill: rgb(0x8f, 0x59, 0x02), weight: "bold", raw(c))
#let render-stepitem(c) = text(weight: "bold", raw(c))
#let render-codeblock(c) = text(fill: rgb(0x99, 0x99, 0x99), weight: "bold", raw(c))
#let render-variable(c) = text(fill: rgb(0x72, 0x9f, 0xcf), weight: "bold", raw(c))
#let render-section(c) = raw(c)
#let render-string(c) = text(fill: rgb(0x4e, 0x9a, 0x06), weight: "bold", raw(c))
#let render-numeric(c) = text(fill: rgb(0xad, 0x7f, 0xa8), weight: "bold", raw(c))
#let render-response(c) = text(fill: rgb(0xf5, 0x79, 0x00), weight: "bold", raw(c))
#let render-invocation(c) = text(fill: rgb(0x3b, 0x5d, 0x7d), weight: "bold", raw(c))
#let render-title(c) = text(weight: "bold", raw(c))
#let render-keyword(c) = text(fill: rgb(0x75, 0x50, 0x7b), weight: "bold", raw(c))
#let render-function(c) = text(fill: rgb(0x34, 0x65, 0xa4), weight: "bold", raw(c))
#let render-multiline(c) = text(fill: rgb(0x4e, 0x9a, 0x06), weight: "bold", raw(c))
#let render-label(c) = text(fill: rgb(0x60, 0x98, 0x9a), weight: "bold", raw(c))
#let render-operator(c) = text(fill: red, raw(c))
#let render-quote(c) = text(fill: rgb(0x99, 0x99, 0x99), weight: "bold", raw(c))
#let render-language(c) = text(fill: rgb(0xc4, 0xa0, 0x00), weight: "bold", raw(c))
#let render-attribute(c) = text(weight: "bold", raw(c))
#let render-structure(c) = text(fill: rgb(0x99, 0x99, 0x99), weight: "bold", raw(c))

// -- Default template --------------------------------------------------------

#let template(body) = {
    set text(font: "Inconsolata")
    show raw: set block(breakable: true)
    body
}
