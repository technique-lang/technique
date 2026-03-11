// Built-in source template for Technique.
//
// Expects a `technique` dictionary with shape:
//   (fragments: ((syntax, content), ...))
//
// Each fragment carries a syntax tag and a content string.
// The syntax tag determines the colour and weight applied.

#let palette = (
    Neutral: (c) => raw(c),
    Indent: (c) => raw(c),
    Newline: (_) => linebreak(),
    Header: (c) => text(fill: rgb(0x75, 0x50, 0x7b), raw(c)),
    Declaration: (c) => text(fill: rgb(0x34, 0x65, 0xa4), weight: "bold", raw(c)),
    Description: (c) => raw(c),
    Forma: (c) => text(fill: rgb(0x8f, 0x59, 0x02), weight: "bold", raw(c)),
    StepItem: (c) => text(weight: "bold", raw(c)),
    CodeBlock: (c) => text(fill: rgb(0x99, 0x99, 0x99), weight: "bold", raw(c)),
    Variable: (c) => text(fill: rgb(0x72, 0x9f, 0xcf), weight: "bold", raw(c)),
    Section: (c) => raw(c),
    String: (c) => text(fill: rgb(0x4e, 0x9a, 0x06), weight: "bold", raw(c)),
    Numeric: (c) => text(fill: rgb(0xad, 0x7f, 0xa8), weight: "bold", raw(c)),
    Response: (c) => text(fill: rgb(0xf5, 0x79, 0x00), weight: "bold", raw(c)),
    Invocation: (c) => text(fill: rgb(0x3b, 0x5d, 0x7d), weight: "bold", raw(c)),
    Title: (c) => text(weight: "bold", raw(c)),
    Keyword: (c) => text(fill: rgb(0x75, 0x50, 0x7b), weight: "bold", raw(c)),
    Function: (c) => text(fill: rgb(0x34, 0x65, 0xa4), weight: "bold", raw(c)),
    Multiline: (c) => text(fill: rgb(0x4e, 0x9a, 0x06), weight: "bold", raw(c)),
    Label: (c) => text(fill: rgb(0x60, 0x98, 0x9a), weight: "bold", raw(c)),
    Operator: (c) => text(fill: red, raw(c)),
    Quote: (c) => text(fill: rgb(0x99, 0x99, 0x99), weight: "bold", raw(c)),
    Language: (c) => text(fill: rgb(0xc4, 0xa0, 0x00), weight: "bold", raw(c)),
    Attribute: (c) => text(weight: "bold", raw(c)),
    Structure: (c) => text(fill: rgb(0x99, 0x99, 0x99), weight: "bold", raw(c)),
)

#let render-fragment(f) = {
    let styler = palette.at(f.syntax, default: (c) => raw(c))
    styler(f.content)
}

#let render(technique) = [
    #show text: set text(font: "Inconsolata")
    #show raw: set block(breakable: true)

    #for f in technique.fragments {
        render-fragment(f)
    }
]
