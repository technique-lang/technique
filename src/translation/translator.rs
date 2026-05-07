// Translation of the parser's abstract syntax tree into a runnable Program.

use std::collections::HashMap;

use crate::language;
use crate::language::{Document, Span};

use super::types::{Operation, Ordinal, Program, Subroutine, SubroutineId};

pub fn translate<'i>(document: &'i Document<'i>) -> Result<Program<'i>, Vec<TranslationError<'i>>> {
    let mut program = Program::new();
    let mut errors = Vec::new();
    let mut known: HashMap<&'i str, SubroutineId> = HashMap::new();

    if let Some(body) = &document.body {
        if let language::Technique::Steps(scopes) = body {
            // Top-level Steps-only document is wrapped in a synthetic
            // anonymous subroutine at index 0, so downstream code can
            // assume a uniform Vec<Subroutine>.
            let mut wrapper = Subroutine::anonymous();
            wrapper.body = translate_subscopes(scopes, &[]);
            program
                .subroutines
                .push(wrapper);
        }
        collect_technique(body, &mut program, &mut known, &mut errors);
    }

    if errors.is_empty() {
        Ok(program)
    } else {
        Err(errors)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TranslationError<'i> {
    DuplicateProcedure(language::Identifier<'i>),
    DuplicateTitle {
        procedure: language::Identifier<'i>,
        at: Span,
    },
    InterleavedDescription {
        procedure: language::Identifier<'i>,
        at: Span,
    },
    OrphanResponse(Span),
}

// Walk a Technique node, registering any procedures it declares directly or
// transitively through nested sections. Procedures are hoisted into the flat
// Program.subroutines list regardless of where in the section tree they were
// declared. The top-level synthetic anonymous wrapper (for a
// Technique::Steps-only document) is added by translate(), not here.
fn collect_technique<'i>(
    technique: &'i language::Technique<'i>,
    program: &mut Program<'i>,
    known: &mut HashMap<&'i str, SubroutineId>,
    errors: &mut Vec<TranslationError<'i>>,
) {
    match technique {
        language::Technique::Procedures(procedures) => {
            for procedure in procedures {
                if let Some(id) = register_procedure(procedure, program, known, errors) {
                    translate_procedure(id, procedure, program, errors);
                }

                // Element::Steps scopes may contain Sections, whose bodies
                // can in turn declare further procedures. Walk the
                // procedure's scopes so those nested declarations are
                // discovered; the walk is independent of whether this
                // procedure itself was a duplicate.
                for element in &procedure.elements {
                    if let language::Element::Steps(scopes, _) = element {
                        for scope in scopes {
                            collect_scope(scope, program, known, errors);
                        }
                    }
                }
            }
        }
        language::Technique::Steps(scopes) => {
            for scope in scopes {
                collect_scope(scope, program, known, errors);
            }
        }
        language::Technique::Empty => {}
    }
}

// Pass 1: gather a procedure's name and reserve its slot in
// Program.subroutines.
fn register_procedure<'i>(
    procedure: &'i language::Procedure<'i>,
    program: &mut Program<'i>,
    known: &mut HashMap<&'i str, SubroutineId>,
    errors: &mut Vec<TranslationError<'i>>,
) -> Option<SubroutineId> {
    let name = procedure
        .name
        .value;
    let span = procedure
        .name
        .span;

    if known.contains_key(name) {
        errors.push(TranslationError::DuplicateProcedure(language::Identifier {
            value: name,
            span,
        }));
        return None;
    }

    let id = SubroutineId(
        program
            .subroutines
            .len(),
    );
    known.insert(name, id);
    program
        .subroutines
        .push(Subroutine::new(language::Identifier { value: name, span }));
    Some(id)
}

// Pass 2: populate a registered subroutine with its title, description,
// parameters, signature, and body.
fn translate_procedure<'i>(
    id: SubroutineId,
    procedure: &'i language::Procedure<'i>,
    program: &mut Program<'i>,
    errors: &mut Vec<TranslationError<'i>>,
) {
    let (title, description) = extract_title_and_description(procedure, errors);

    // build body
    let mut ops = Vec::new();
    for element in &procedure.elements {
        if let language::Element::Steps(scopes, _) = element {
            for scope in scopes {
                append_attributes(&mut ops, scope, &[]);
            }
        }
    }
    let body = Operation::Sequence(ops);

    let entry = &mut program.subroutines[id.0];
    entry.title = title;
    entry.description = description;
    entry.parameters = procedure
        .parameters
        .as_ref()
        .map(Vec::as_slice);
    entry.signature = procedure
        .signature
        .as_ref();
    entry.body = body;
}

fn translate_scope<'i>(
    scope: &'i language::Scope<'i>,
    attrs: &[&'i [language::Attribute<'i>]],
) -> Operation<'i> {
    match scope {
        language::Scope::SectionChunk {
            numeral,
            title,
            body,
            ..
        } => {
            let inner = match body {
                language::Technique::Steps(scopes) => translate_subscopes(scopes, attrs),
                // Procedures declared inside a section are hoisted into
                // Program.subroutines by the collect pass; the section
                // node carries no executable body for them.
                language::Technique::Procedures(_) => Operation::Sequence(Vec::new()),
                language::Technique::Empty => Operation::Sequence(Vec::new()),
            };
            Operation::Section {
                numeral,
                title: title.as_ref(),
                body: Box::new(inner),
            }
        }
        language::Scope::DependentBlock {
            ordinal,
            description,
            subscopes,
            ..
        } => Operation::Step {
            ordinal: Ordinal::Dependent(ordinal),
            attributes: attrs.to_vec(),
            description: description.as_slice(),
            body: Box::new(translate_subscopes(subscopes, attrs)),
            expects: None,
        },
        language::Scope::ParallelBlock {
            description,
            subscopes,
            ..
        } => Operation::Step {
            ordinal: Ordinal::Parallel,
            attributes: attrs.to_vec(),
            description: description.as_slice(),
            body: Box::new(translate_subscopes(subscopes, attrs)),
            expects: None,
        },
        // AttributeBlock is not represented as an Operation; it disappears
        // from the output, contributing only its attribute list onto every
        // enclosed Step. translate_subscopes / append_attributes intercept it
        // before reaching here.
        language::Scope::AttributeBlock { .. } => {
            unreachable!("AttributeBlock handled by append_attributes")
        }
        language::Scope::CodeBlock { .. } => todo!("code block"),
        language::Scope::ResponseBlock { .. } => todo!("response block"),
    }
}

// Translate a single scope into operations appended to `ops`. Most scopes
// contribute exactly one Operation; AttributeBlock contributes no Operation
// of its own and inlines its (recursively-translated) children with its
// attribute list pushed onto the enclosing-attribute stack.
fn append_attributes<'i>(
    ops: &mut Vec<Operation<'i>>,
    scope: &'i language::Scope<'i>,
    attrs: &[&'i [language::Attribute<'i>]],
) {
    if let language::Scope::AttributeBlock {
        attributes,
        subscopes,
        ..
    } = scope
    {
        let mut nested: Vec<&'i [language::Attribute<'i>]> = attrs.to_vec();
        nested.push(attributes.as_slice());
        for sub in subscopes {
            append_attributes(ops, sub, &nested);
        }
    } else {
        ops.push(translate_scope(scope, attrs));
    }
}

fn translate_subscopes<'i>(
    scopes: &'i [language::Scope<'i>],
    attrs: &[&'i [language::Attribute<'i>]],
) -> Operation<'i> {
    let mut ops = Vec::new();
    for scope in scopes {
        append_attributes(&mut ops, scope, attrs);
    }
    Operation::Sequence(ops)
}

// Walk a procedure's elements to extract the procedure-shell title and
// description, surfacing the two structural errors:
//   DuplicateTitle - a second Element::Title in this procedure.
//   InterleavedDescription - an Element::Description after a Steps or
//     CodeBlock element.
// Multiple Element::Description occurrences before any Steps/CodeBlock are
// not an error; the first wins.
fn extract_title_and_description<'i>(
    procedure: &'i language::Procedure<'i>,
    errors: &mut Vec<TranslationError<'i>>,
) -> (Option<&'i str>, &'i [language::Paragraph<'i>]) {
    let mut title: Option<&'i str> = None;
    let mut description: Option<&'i [language::Paragraph<'i>]> = None;
    let mut blocked = false;

    for element in &procedure.elements {
        match element {
            language::Element::Title(value, span) => {
                if title.is_some() {
                    errors.push(TranslationError::DuplicateTitle {
                        procedure: language::Identifier {
                            value: procedure
                                .name
                                .value,
                            span: procedure
                                .name
                                .span,
                        },
                        at: *span,
                    });
                } else {
                    title = Some(*value);
                }
            }
            language::Element::Description(paragraphs, span) => {
                if blocked {
                    errors.push(TranslationError::InterleavedDescription {
                        procedure: language::Identifier {
                            value: procedure
                                .name
                                .value,
                            span: procedure
                                .name
                                .span,
                        },
                        at: *span,
                    });
                } else if description.is_none() {
                    description = Some(paragraphs.as_slice());
                }
            }
            language::Element::Steps(_, _) | language::Element::CodeBlock(_, _) => {
                blocked = true;
            }
        }
    }

    (title, description.unwrap_or(&[]))
}

fn collect_scope<'i>(
    scope: &'i language::Scope<'i>,
    program: &mut Program<'i>,
    known: &mut HashMap<&'i str, SubroutineId>,
    errors: &mut Vec<TranslationError<'i>>,
) {
    match scope {
        language::Scope::SectionChunk { body, .. } => {
            collect_technique(body, program, known, errors);
        }
        language::Scope::DependentBlock { subscopes, .. }
        | language::Scope::ParallelBlock { subscopes, .. }
        | language::Scope::AttributeBlock { subscopes, .. }
        | language::Scope::CodeBlock { subscopes, .. } => {
            for sub in subscopes {
                collect_scope(sub, program, known, errors);
            }
        }
        language::Scope::ResponseBlock { .. } => {}
    }
}
