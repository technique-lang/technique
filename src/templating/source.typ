// Built-in source template for Technique.
//
// Expects a `technique` dictionary with shape:
//   (fragments: ((syntax, content), ...))
//
// Each fragment carries a syntax tag and a content string.
// The syntax tag determines the colour and weight applied.

#let render-fragment(f) = {
    if f.syntax == "Newline" { linebreak() }
    else if f.syntax == "Header" { text(fill: rgb(0x75, 0x50, 0x7b), raw(f.content)) }
    else if f.syntax == "Declaration" { text(fill: rgb(0x34, 0x65, 0xa4), weight: "bold", raw(f.content)) }
    else if f.syntax == "Forma" { text(fill: rgb(0x8f, 0x59, 0x02), weight: "bold", raw(f.content)) }
    else if f.syntax == "StepItem" { text(weight: "bold", raw(f.content)) }
    else if f.syntax == "CodeBlock" { text(fill: rgb(0x99, 0x99, 0x99), weight: "bold", raw(f.content)) }
    else if f.syntax == "Variable" { text(fill: rgb(0x72, 0x9f, 0xcf), weight: "bold", raw(f.content)) }
    else if f.syntax == "String" { text(fill: rgb(0x4e, 0x9a, 0x06), weight: "bold", raw(f.content)) }
    else if f.syntax == "Numeric" { text(fill: rgb(0xad, 0x7f, 0xa8), weight: "bold", raw(f.content)) }
    else if f.syntax == "Response" { text(fill: rgb(0xf5, 0x79, 0x00), weight: "bold", raw(f.content)) }
    else if f.syntax == "Invocation" { text(fill: rgb(0x3b, 0x5d, 0x7d), weight: "bold", raw(f.content)) }
    else if f.syntax == "Title" { text(weight: "bold", raw(f.content)) }
    else if f.syntax == "Keyword" { text(fill: rgb(0x75, 0x50, 0x7b), weight: "bold", raw(f.content)) }
    else if f.syntax == "Function" { text(fill: rgb(0x34, 0x65, 0xa4), weight: "bold", raw(f.content)) }
    else if f.syntax == "Multiline" { text(fill: rgb(0x4e, 0x9a, 0x06), weight: "bold", raw(f.content)) }
    else if f.syntax == "Label" { text(fill: rgb(0x60, 0x98, 0x9a), weight: "bold", raw(f.content)) }
    else if f.syntax == "Operator" { text(fill: red, raw(f.content)) }
    else if f.syntax == "Quote" { text(fill: rgb(0x99, 0x99, 0x99), weight: "bold", raw(f.content)) }
    else if f.syntax == "Language" { text(fill: rgb(0xc4, 0xa0, 0x00), weight: "bold", raw(f.content)) }
    else if f.syntax == "Attribute" { text(weight: "bold", raw(f.content)) }
    else if f.syntax == "Structure" { text(fill: rgb(0x99, 0x99, 0x99), weight: "bold", raw(f.content)) }
    else { raw(f.content) }
}

#let render(technique) = [
    #show text: set text(font: "Inconsolata")
    #show raw: set block(breakable: true)

    #for f in technique.fragments {
        render-fragment(f)
    }
]
