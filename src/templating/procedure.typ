// Procedure domain template for Technique.
//
// Thin formatting functions called from Rust-generated markup.
// Each function is independently overridable via `--template`.

// -- Formatting functions ----------------------------------------------------

#let render-document(title: none, description: (), children: none) = [
    #block(width: 100%, stroke: 0.1pt, inset: 10pt)[
        #if title != none [
            #text(size: 15pt)[*#title*]

        ]
        #if description.len() > 0 or children != none [
            _Overview_

            #for para in description [
                #para
            ]
        ]
        #if children != none {
            children
        }
    ]
]

#let render-outline(sections: ()) = {
    grid(columns: (auto, 1fr), column-gutter: 6pt, row-gutter: 0.3em,
        ..sections.map(s => {
            let heading = s.at("heading", default: none)
            ([#s.ordinal.], [#if heading != none { heading }])
        }).flatten()
    )
    heading(level: 3, numbering: none, outlined: false, [Procedure])
}

#let render-section(ordinal: none, heading: none, children: none) = {
    std.heading(level: 1, numbering: none,
        [#ordinal. #h(8pt) #if heading != none { heading }])
    if children != none { children }
}

#let section-divider() = {
    line(length: 100%, stroke: (thickness: 0.5pt, paint: rgb("#003366"), dash: ("dot", 2pt, 4pt, 2pt)))
}

#let render-procedure(name: none, title: none, description: (), children: none) = {
    if title != none {
        std.heading(level: 2, numbering: none, outlined: false, title)
    }
    text(size: 7pt, fill: rgb("#999999"), raw(name))
    linebreak()

    for para in description {
        [#para]
        parbreak()
    }
    if children != none {
        pad(left: 8pt, children)
    }
}

#let render-step(ordinal: none, title: none, body: (), invocations: (), responses: none, children: none) = {
  block(breakable: false, {
    if invocations.len() > 0 {
        text(size: 7pt, raw(invocations.join(", ")))
        linebreak()
    }
    if ordinal != none and title != none [
        *#ordinal.* #h(4pt) *#title*
    ] else if ordinal != none [
        *#ordinal.*
    ] else if title != none [
        *#title*
    ]
    parbreak()
    for para in body {
        [#para]
        parbreak()
    }
    if responses != none {
        responses
        parbreak()
    }
    if children != none {
        pad(left: 16pt, children)
    }
  })
}

#let render-response(value: none, condition: none) = {
    if condition != none [- _#value #condition _]
    else [- _#value _]
}

#let render-attribute(name: none, children: none) = {
    [- *#name*]
    if children != none {
        pad(left: 20pt, children)
    }
}

// -- Default template --------------------------------------------------------

#let template(body) = {
    set page(margin: 1.5cm)
    set par(justify: false)
    set text(size: 9pt, font: "TeX Gyre Heros")

    show heading.where(level: 1): set text(size: 14pt)
    show heading.where(level: 2): it => {
        block(width: 100%, below: 0.4em,
            text(size: 11pt, weight: "bold", it.body))
    }
    show heading.where(level: 3): it => {
        block(width: 100%, fill: rgb("#006699"), inset: 5pt,
            text(fill: white, weight: "bold", it.body))
    }

    body
}
