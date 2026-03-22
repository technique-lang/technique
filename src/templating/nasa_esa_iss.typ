// NASA/ESA ISS Crew Procedure domain template for Technique.
//
// Renders procedures in the style of ISS flight plan documents:
// bordered table layout, role designators in left margin, underlined
// procedure titles, boxed display names, bold commands, and
// verification checks.

// -- Formatting functions ----------------------------------------------------

#let render-document(source: none, name: none, title: none, description: (), children: none) = [
    #if title != none [
        #text(size: 11pt, weight: "bold")[#upper(title)]
        #v(0.5em)
    ]
    #block(width: 100%, stroke: 0.75pt, {
        // Column header
        block(width: 100%, fill: rgb("#e8e8e8"), inset: 4pt, above: 0pt,
            text(size: 9pt, weight: "bold", align(center, [PROCEDURE])))

        // Notes from description
        if description.len() > 0 {
            block(width: 100%, inset: (x: 10pt, y: 6pt), {
                align(center, text(size: 8pt, weight: "bold", underline([NOTE])))
                v(2pt)
                for para in description {
                    text(size: 8pt, para)
                    parbreak()
                }
            })
            line(length: 100%, stroke: 0.5pt)
        }

        // Body content
        if children != none {
            block(width: 100%, inset: (x: 10pt, y: 6pt), children)
        }
    })
]

#let render-outline(sections: ()) = {}

#let render-section(ordinal: none, heading: none, children: none) = {
    std.heading(level: 1, numbering: none,
        [#ordinal. #h(8pt) #if heading != none { heading }])
    if children != none { children }
}

#let section-divider() = {
    line(length: 100%, stroke: 0.5pt)
}

#let render-procedure(name: none, title: none, description: (), children: none) = {
    let ordinal-width = 1.6em
    block(above: 0.8em, below: 0.6em, {
        set par(hanging-indent: ordinal-width + 0.2em)
        box(width: ordinal-width, text(size: 9pt, weight: "bold", [#name.]))
        h(0.2em)
        if title != none {
            text(size: 9pt, weight: "bold", underline(offset: 2pt, title))
        }
    })
    pad(left: ordinal-width + 0.2em, {
        for para in description {
            block(above: 0.2em, below: 0.2em, {
                set text(size: 8pt)
                para
            })
        }
        if children != none {
            children
        }
    })
}

#let render-step(ordinal: none, title: none, body: (), invocations: (), responses: none, children: none) = {
    let ordinal-width = 1.2em

    block(above: 0.4em, below: 0.4em, breakable: false, {
        set par(spacing: 0.5em, hanging-indent: ordinal-width + 0.2em)

        if ordinal != none {
            box(width: ordinal-width, text(size: 8pt, [#ordinal.]))
            h(0.2em)
        } else if title != none or invocations.len() > 0 {
            box(width: ordinal-width)[\u{2013}]
            h(0.2em)
        }

        if title != none {
            text(size: 8pt, title)
        }

        if invocations.len() > 0 {
            if title != none { h(4pt) }
            invocations.map(i => {
                text(size: 8pt, fill: rgb("#666666"), raw("<"))
                text(size: 8pt, fill: rgb("#3b5d7d"), raw(i))
                text(size: 8pt, fill: rgb("#666666"), raw(">"))
            }).join(text(fill: rgb("#666666"), raw(", ")))
        }

        if body.len() > 0 {
            parbreak()
            for para in body {
                text(size: 8pt, [#para])
                parbreak()
            }
        }

        if responses != none {
            v(2pt)
            responses
        }

        if children != none {
            pad(left: 14pt, children)
        }
    })
}

#let render-response(value: none, condition: none) = {
    h(4pt)
    text(size: 8pt, [\u{221a}])
    text(size: 8pt, {
        if condition != none [ #value -- #condition ]
        else [ #value ]
    })
}

#let render-attribute(name: none, children: none) = {
    block(above: 0.3em, below: 0.3em, {
        text(size: 8pt, weight: "bold", upper(name))
        if children != none {
            pad(left: 8pt, children)
        }
    })
}

#let render-code-block(expression: none, body: (), responses: none, children: none) = {
    if expression != none {
        block(above: 0.3em, below: 0.3em, {
            if expression.starts-with("cmd ") {
                let arg = expression.slice(4)
                text(size: 8pt, weight: "bold", [cmd ])
                text(size: 8pt, arg)
            } else {
                text(size: 8pt, fill: rgb("#444444"), raw(expression))
            }
        })
    }
    if body.len() > 0 {
        pad(left: 14pt, {
            for line in body {
                text(size: 8pt, fill: rgb("#4e9a06"), weight: "bold", raw(line))
                linebreak()
            }
        })
    }
    if responses != none or children != none {
        pad(left: 14pt, {
            if responses != none {
                responses
            }
            if children != none {
                children
            }
        })
    }
}

// -- Default template --------------------------------------------------------

#let template(body) = {
    set page(
        paper: "us-letter",
        margin: (top: 1.5cm, bottom: 1.5cm, left: 1.5cm, right: 1.5cm),
        numbering: "1",
        number-align: center + bottom,
    )
    set par(justify: false)
    set text(size: 8pt, font: "Liberation Sans")

    body
}
