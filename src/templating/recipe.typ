// Recipe domain template for Technique.
//
// Thin formatting functions called from Rust-generated markup.
// Each function is independently overridable via `--template`.

// -- Formatting functions ----------------------------------------------------

#let render-document(title: none, description: (), ingredients: none, method: none) = [
    #if title != none [
        #text(size: 18pt, weight: "bold")[#title]
        #v(0.3em)
    ]
    #if description.len() > 0 [
        #for para in description {
            [
                #set text(font: "Libertinus Serif", size: 11pt)
                #set par(leading: 0.5em)
                #para
            ]
            parbreak()
        }
        #v(0.5em)
    ]
    #if ingredients != none [
        #heading(level: 1, numbering: none, outlined: false, [Ingredients])
        #ingredients
    ]
    #if method != none [
        #heading(level: 1, numbering: none, outlined: false, [Method])
        #method
    ]
]

#let render-ingredients(heading: none, description: (), children: none) = {
    block(breakable: false, {
        if heading != none {
            block(above: 0.8em, below: 0.8em,
                text(size: 11pt, weight: "bold", heading))
        }
        for para in description {
            [
                #set text(font: "Libertinus Serif", size: 11pt)
                #set par(leading: 0.5em)
                #para
            ]
            parbreak()
        }
        if children != none {
            if description.len() > 0 { v(0.5em) }
            children
        }
    })
}

#let render-ingredient(label: none, quantity: none, source: none) = {
    block(above: 0.3em, below: 0.3em, {
        h(0.5em)
        box(width: 8em)[#label]
        if quantity != none [
            --- #h(6pt) #quantity
        ]
        if source != none [
            #h(6pt) #text(fill: rgb("#999999"), size: 0.9em)[(#source)]
        ]
    })
}

#let render-role-heading(name: none) = {
    if name != none {
        block(above: 0.6em, below: 0.4em,
            text(size: 9pt, fill: rgb("#666666"), style: "italic", name))
    }
}

#let render-step(ordinal: none, title: none, description: (), role: none, children: none) = {
    let ordinal-width = if ordinal != none and ordinal.len() > 1 { 1.5em } else { 1em }

    block(breakable: false, {
        if ordinal != none or title != none {
            block(above: 0.5em, below: 0.5em, {
                set par(hanging-indent: ordinal-width + 0.2em)
                if ordinal != none {
                    box(width: ordinal-width)[*#ordinal.*]
                    h(0.2em)
                }
                if title != none {
                    if ordinal != none { title } else if role == none {
                        heading(level: 2, numbering: none, title)
                    } else {
                        [\u{2013} ] + title
                    }
                }
            })
        }
        if description.len() > 0 {
            v(0.5em)
            for para in description {
                [
                    #set text(font: "Libertinus Serif", size: 11pt)
                    #set par(leading: 0.5em)
                    #para
                ]
                parbreak()
            }
        }
        if children != none {
            if ordinal != none {
                v(0.35em)
                pad(left: 16pt, children)
            } else {
                children
            }
        }
    })
}

// -- Default template --------------------------------------------------------

#let template(body) = {
    set page(margin: 1.5cm)
    set par(justify: false)
    set text(size: 10pt, font: "TeX Gyre Heros")

    show heading.where(level: 1): set text(size: 15pt)
    show heading.where(level: 2): set text(size: 12pt)

    body
}
