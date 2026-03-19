// Checklist domain template for Technique.
//
// Thin formatting functions called from Rust-generated markup.
// Each function is independently overridable via `--template`.

// -- Formatting functions ----------------------------------------------------

#let check = box(stroke: 0.5pt, width: 0.8em, height: 0.8em)
#let small-check = box(stroke: 0.5pt, width: 0.6em, height: 0.6em)

#let render-section(ordinal: none, heading: none, children: none) = {
    let level = if ordinal != none { 2 } else { 1 }
    if ordinal != none and heading != none {
        std.heading(level: level, numbering: none, [#ordinal. #heading])
    } else if ordinal != none {
        std.heading(level: level, numbering: none, [#ordinal.])
    } else if heading != none {
        std.heading(level: level, numbering: none, heading)
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
    if children != none { children }
  })
}

// -- Default template --------------------------------------------------------

#let template(body) = {
    set page(margin: 1.5cm)
    set text(size: 10pt)
    body
}
