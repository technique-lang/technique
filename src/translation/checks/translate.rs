// Hand-written check suite for translation phase
//
// Modelled on `src/parsing/checks/parser.rs`. Source strings parsed through
// the real parser inline matches what the runner sees in production.

use std::path::Path;

use crate::language;
use crate::parsing;
use crate::translation::{translate, Operation};

#[test]
fn empty_input_yields_empty_program() {
    let source = "";
    let path = Path::new("test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");
    assert!(program
        .subroutines
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
            .subroutines
            .len(),
        1
    );
    assert_eq!(
        program.subroutines[0].name,
        Some(language::Identifier::new("make_coffee"))
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
        .subroutines
        .iter()
        .map(|p| {
            p.name
                .as_ref()
                .map(|id| id.value)
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
        .subroutines
        .iter()
        .map(|p| {
            p.name
                .as_ref()
                .map(|id| id.value)
        })
        .collect();
    assert_eq!(names, vec![Some("outer"), Some("inner")]);
}

#[test]
fn procedure_title_extracted() {
    let source = r#"
% technique v1

make_coffee :

# Coffee Time
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    assert_eq!(program.subroutines[0].title, Some("Coffee Time"));
}

#[test]
fn procedure_description_extracted() {
    let source = r#"
% technique v1

make_coffee :

# Coffee Time

This is how to make coffee.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    assert_eq!(
        program.subroutines[0]
            .description
            .len(),
        1
    );
}

#[test]
fn procedure_parameters_borrowed() {
    let source = r#"
% technique v1

make_coffee(beans, water) :
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let params = program.subroutines[0]
        .parameters
        .expect("parameters present");
    assert_eq!(params.len(), 2);
    assert_eq!(params[0].value, "beans");
    assert_eq!(params[1].value, "water");
}

#[test]
fn procedure_signature_borrowed() {
    let source = r#"
% technique v1

make_coffee : Beans -> Coffee
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    assert!(program.subroutines[0]
        .signature
        .is_some());
}

#[test]
fn anonymous_wrapper_for_top_level_steps() {
    let source = r#"
1.  First step

2.  Second step
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    assert_eq!(
        program
            .subroutines
            .len(),
        1
    );
    assert_eq!(program.subroutines[0].name, None);
}

#[test]
fn section_with_empty_body_translated() {
    let source = r#"
% technique v1

outer :

I. First section
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let body = &program.subroutines[0].body;
    let Operation::Sequence(ops) = body else {
        panic!("expected Sequence, got {:?}", body);
    };
    assert_eq!(ops.len(), 1);
    let Operation::Section {
        numeral,
        title,
        body: section_body,
    } = &ops[0]
    else {
        panic!("expected Section, got {:?}", ops[0]);
    };
    assert_eq!(*numeral, "I");
    assert!(title.is_some());
    let Operation::Sequence(inner) = section_body.as_ref() else {
        panic!("expected inner Sequence");
    };
    assert!(inner.is_empty());
}

#[test]
fn section_holding_procedures_yields_empty_body() {
    let source = r#"
% technique v1

outer :

I. Section One

inner : () -> ()
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    // The inner procedure was hoisted into the flat list.
    assert_eq!(
        program
            .subroutines
            .len(),
        2
    );

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    assert_eq!(ops.len(), 1);
    let Operation::Section {
        body: section_body, ..
    } = &ops[0]
    else {
        panic!("expected Section");
    };
    let Operation::Sequence(inner) = section_body.as_ref() else {
        panic!("expected inner Sequence");
    };
    // Procedures-bodied section carries no executable operations of its own.
    assert!(inner.is_empty());
}

#[test]
fn nested_sections_translate_recursively() {
    let source = r#"
% technique v1

outer :

I. Outer

II. Sibling
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let program = translate(&document).expect("translate");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    assert_eq!(ops.len(), 2);
    let numerals: Vec<&str> = ops
        .iter()
        .map(|op| match op {
            Operation::Section { numeral, .. } => *numeral,
            _ => panic!("expected Section"),
        })
        .collect();
    assert_eq!(numerals, vec!["I", "II"]);
}
