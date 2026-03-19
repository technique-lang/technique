// Checklist domain template for Technique.
//
// Thin formatting functions called from Rust-generated markup.
// Each function is independently overridable via `--template`.

// -- Formatting functions ----------------------------------------------------

#let check = box(stroke: 0.5pt, width: 0.8em, height: 0.8em)
#let small-check = box(stroke: 0.5pt, width: 0.6em, height: 0.6em)

#let render-document(name: none, title: none) = {
    if title != none {
        std.heading(level: 1, numbering: none, title)
    } else if name != none {
        std.heading(level: 1, numbering: none, raw(name))
    }
}

#let render-section(ordinal: none, heading: none, children: none) = {
    if ordinal != none and heading != none {
        std.heading(level: 2, numbering: none, [#ordinal. #heading])
    } else if ordinal != none {
        std.heading(level: 2, numbering: none, [#ordinal.])
    } else if heading != none {
        std.heading(level: 2, numbering: none, heading)
    }
    if children != none { children }
}

#let render-procedure(name: none, title: none, body: (), role: none, responses: none, children: none) = {
    if title != none {
        std.heading(level: 3, numbering: none, title)
    } else if name != none {
        std.heading(level: 3, numbering: none, raw(name))
    }
    if children != none { children }
}

#let render-response(value: none, condition: none) = {
    small-check
    if condition != none [ _#value #condition _]
    else [ _#value _]
}

#let render-step(ordinal: none, title: none, body: (), role: none, responses: none, children: none) = {
  block(breakable: false, {
    if role != none {
        text(weight: "bold")[#role]
        parbreak()
    }
    check
    if ordinal != none [  *#ordinal.*  ]
    if title != none [ #title]
    parbreak()
    for para in body {
        [#para]
        parbreak()
    }
    if responses != none {
        responses
        parbreak()
    }
    if children != none { pad(left: 16pt, children) }
  })
}

// -- Default template --------------------------------------------------------

#let template(body) = {
    set page(margin: 1.5cm)
    set text(size: 10pt)
    body
}
