// Hand-written check suite for translation: surface AST -> IR.
//
// Modelled on `src/parsing/checks/parser.rs`. Source strings parsed through
// the real parser inline matches what the runner sees in production.

use std::path::Path;

use crate::language;
use crate::parsing;
use crate::translation::translate;

#[test]
fn empty_input_yields_empty_program() {
    let source = "";
    let path = Path::new("test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");
    assert!(program
        .procedures
        .is_empty());
}

#[test]
fn single_procedure_registered() {
    let source = r#"
% technique v1

make_coffee :
        "#
    .trim_ascii();
    let path = Path::new("test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    assert_eq!(
        program
            .procedures
            .len(),
        1
    );
    assert_eq!(
        program.procedures[0].name,
        Some(language::Identifier("make_coffee"))
    );
}

#[test]
fn multiple_procedures_registered_in_order() {
    let source = r#"
% technique v1

first :

second :

third :
        "#
    .trim_ascii();
    let path = Path::new("test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let names: Vec<_> = program
        .procedures
        .iter()
        .map(|p| {
            p.name
                .as_ref()
                .map(|id| id.0)
        })
        .collect();
    assert_eq!(names, vec![Some("first"), Some("second"), Some("third")]);
}

#[test]
fn procedure_inside_section_registered() {
    let source = r#"
% technique v1

outer :

I. Section One

inner : () -> ()
        "#
    .trim_ascii();
    let path = Path::new("test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let names: Vec<_> = program
        .procedures
        .iter()
        .map(|p| {
            p.name
                .as_ref()
                .map(|id| id.0)
        })
        .collect();
    assert_eq!(names, vec![Some("outer"), Some("inner")]);
}
