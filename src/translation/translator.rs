// Translation of the internal parser abstract syntax tree to an internal
// intermediate representation.

use std::collections::HashMap;

use crate::language;
use crate::language::Document;

use super::types::{Operation, Procedure, ProcedureId, Program};

pub fn translate<'i>(document: &Document<'i>) -> Result<Program<'i>, Vec<TranslationError<'i>>> {
    let mut program = Program::new();
    let mut errors = Vec::new();
    let mut known: HashMap<&'i str, ProcedureId> = HashMap::new();

    if let Some(body) = &document.body {
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
    DuplicateTitle(language::Identifier<'i>),
    InterleavedDescription(language::Identifier<'i>),
    OrphanResponse,
}

// Walk a Technique node, registering any procedures it declares directly or
// transitively through nested sections. Procedures are hoisted into the
// flat Program.procedures list regardless of where in the section tree they
// were declared.
fn collect_technique<'i>(
    technique: &language::Technique<'i>,
    program: &mut Program<'i>,
    known: &mut HashMap<&'i str, ProcedureId>,
    errors: &mut Vec<TranslationError<'i>>,
) {
    match technique {
        language::Technique::Procedures(procedures) => {
            for procedure in procedures {
                register_procedure(procedure, program, known, errors);

                // Element::Steps scopes may contain Sections, whose bodies
                // can in turn declare further procedures. Walk the
                // procedure's scopes so those nested declarations are
                // discovered; the walk is independent of whether this
                // procedure itself was a duplicate.
                for element in &procedure.elements {
                    if let language::Element::Steps(scopes) = element {
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

fn register_procedure<'i>(
    procedure: &language::Procedure<'i>,
    program: &mut Program<'i>,
    known: &mut HashMap<&'i str, ProcedureId>,
    errors: &mut Vec<TranslationError<'i>>,
) {
    let name = procedure
        .name
        .0;

    if known.contains_key(name) {
        errors.push(TranslationError::DuplicateProcedure(language::Identifier(
            name,
        )));
        return;
    }

    let id = ProcedureId(
        program
            .procedures
            .len(),
    );
    known.insert(name, id);
    program
        .procedures
        .push(Procedure {
            name: Some(language::Identifier(name)),
            title: None,
            description: &[],
            parameters: None,
            signature: None,
            body: Operation::Sequence(Vec::new()),
        });
}

fn collect_scope<'i>(
    scope: &language::Scope<'i>,
    program: &mut Program<'i>,
    known: &mut HashMap<&'i str, ProcedureId>,
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
