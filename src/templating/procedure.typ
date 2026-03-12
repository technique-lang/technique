// Procedure domain template for Technique.
//
// Exports `render` and `template`.

// -- Render helpers --------------------------------------------------------

#let render-responses(responses) = {
    for r in responses {
        if r.condition != none [- _#r.value #r.condition _]
        else [- _#r.value _]
    }
    if responses.len() > 0 { parbreak() }
}

#let render-invocations(invocations) = {
    if invocations.len() > 0 {
        text(size: 7pt)[`#invocations.join(", ")`]
        linebreak()
    }
}

#let ordinal-start(children) = {
    let first = children.at(0, default: none)
    if first != none and first.type == "sequential" {
        let o = first.at("ordinal", default: "a")
        let c = o.codepoints().at(0, default: "a")
        if "abcdefghijklmnopqrstuvwxyz".contains(c) {
            "abcdefghijklmnopqrstuvwxyz".position(c) + 1
        } else { 1 }
    } else { 1 }
}

#let render-child(node) = {
    if node.type == "sequential" or node.type == "parallel" {
        if node.at("title", default: none) != none [+ #node.title]
    }
}

#let render-role(node) = {
    [- *#node.name*]
    if node.children.len() > 0 {
        let start = ordinal-start(node.children)
        pad(left: 20pt)[
            #set par(leading: 0.5em)
            #set enum(numbering: "a.", start: start, spacing: 0.8em)
            #for child in node.children {
                if child.type == "attribute" {
                    render-role(child)
                } else {
                    render-child(child)
                }
            }
        ]
    }
}

#let render-node(node) = {
    if node.type == "section" {
        heading(level: 1, numbering: none,
            [#node.ordinal. #h(8pt) #if node.at("heading", default: none) != none { node.heading }])
        for child in node.children { render-node(child) }

    } else if node.type == "procedure" {
        text(size: 7pt)[`#node.name`]
        linebreak()
        if node.at("title", default: none) != none {
            heading(level: 2, numbering: none, outlined: false, node.title)
        }
        for para in node.description {
            [#para]
            parbreak()
        }
        if node.children.len() > 0 {
            pad(left: 8pt)[#for child in node.children { render-node(child) }]
        }

    } else if node.type == "sequential" or node.type == "parallel" {
        render-invocations(node.invocations)
        let ordinal = if node.type == "sequential" { node.ordinal } else { none }
        if ordinal != none and node.at("title", default: none) != none [
            *#ordinal.* #h(4pt) *#node.title*
        ] else if ordinal != none [
            *#ordinal.*
        ] else if node.at("title", default: none) != none [
            *#node.title*
        ]
        parbreak()
        for para in node.body {
            [#para]
            parbreak()
        }
        render-responses(node.responses)
        if node.children.len() > 0 {
            pad(left: 16pt)[#for child in node.children { render-node(child) }]
        }

    } else if node.type == "attribute" {
        render-role(node)
    }
}

#let has-sections(body) = {
    body.any(n => n.type == "section")
}

#let render-outline(body) = {
    grid(columns: (auto, 1fr), column-gutter: 6pt, row-gutter: 0.3em,
        ..body.filter(n => n.type == "section").map(n => {
            let heading = n.at("heading", default: none)
            ([#n.ordinal.], [#if heading != none { heading }])
        }).flatten()
    )
}

// -- Render function -------------------------------------------------------

#let render(technique) = [
    #block(width: 100%, stroke: 0.1pt, inset: 10pt)[
        #if technique.at("title", default: none) != none [
            #text(size: 15pt)[*#technique.title*]

        ]
        #if technique.description.len() > 0 or has-sections(technique.body) [
            _Overview_

            #for para in technique.description [
                #para
            ]
            #if has-sections(technique.body) {
                render-outline(technique.body)
            }
        ]
        #heading(level: 3, numbering: none, outlined: false, [Procedure])

        #for (i, node) in technique.body.enumerate() {
            render-node(node)
            if i + 1 < technique.body.len() and node.type == "section" {
                line(length: 100%, stroke: (thickness: 0.5pt, paint: rgb("#003366"), dash: ("dot", 2pt, 4pt, 2pt)))
            }
        }
    ]
]

// -- Default template ------------------------------------------------------

#let template(body) = {
    set page(margin: 1.5cm)
    set par(justify: false)
    set text(size: 9pt, font: "TeX Gyre Heros")

    show heading.where(level: 1): set text(size: 14pt)
    show heading.where(level: 2): set text(size: 11pt)
    show heading.where(level: 3): it => {
        block(width: 100%, fill: rgb("#006699"), inset: 5pt,
            text(fill: white, weight: "bold", it.body))
    }

    body
}
