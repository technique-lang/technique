// Built-in checklist template for Technique.
//
// Expects a `technique` dictionary with shape:
//   (sections: ((ordinal, heading, steps: ((ordinal, title, body, role,
//   responses, children), ...)), ...))

#let check = box(stroke: 0.5pt, width: 0.8em, height: 0.8em)
#let small-check = box(stroke: 0.5pt, width: 0.6em, height: 0.6em)

#let render-responses(responses) = {
    for (i, r) in responses.enumerate() {
        if i > 0 [ | ]
        small-check
        if r.condition != none [ _#r.value #r.condition _]
        else [ _#r.value _]
    }
    if responses.len() > 0 { parbreak() }
}

#let render-step(step) = {
    if step.role != none {
        text(weight: "bold")[#step.role]
        parbreak()
    }
    check
    if step.ordinal != none [  *#step.ordinal.*  ]
    if step.title != none [ #step.title]
    parbreak()
    for para in step.body {
        [#para]
        parbreak()
    }
    render-responses(step.responses)
    for child in step.children {
        render-step(child)
    }
}

#let render(technique) = [
    #set page(margin: 1.5cm)
    #set text(size: 10pt)

    #for section in technique.sections [
        #if section.ordinal != none and section.heading != none [
            == #section.ordinal. #section.heading
        ] else if section.ordinal != none [
            == #section.ordinal.
        ] else if section.heading != none [
            == #section.heading
        ]
        #for step in section.steps {
            render-step(step)
        }
    ]
]
