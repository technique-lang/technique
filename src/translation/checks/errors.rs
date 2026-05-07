// Error-case checks for translation. Source strings parsed through the
// real parser inline match what the runner sees in production.

use std::path::Path;

use crate::language;
use crate::language::Span;
use crate::parsing;
use crate::translation::{translate, TranslationError};

#[test]
fn translation_error_variants_construct() {
    let _ = TranslationError::OrphanResponse(Span::default());
}

#[test]
fn duplicate_procedure_name_is_error() {
    let source = r#"
% technique v1

make_coffee :

make_coffee :
        "#
    .trim_ascii();
    let path = Path::new("test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let errors = translate(&document).expect_err("translate should fail");

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0],
        TranslationError::DuplicateProcedure(language::Identifier::new("make_coffee"))
    );
}

#[test]
fn duplicate_title_is_error() {
    let source = r#"
% technique v1

make_coffee :

# First Title

# Second Title
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let errors = translate(&document).expect_err("translate should fail");

    assert_eq!(errors.len(), 1);
    let TranslationError::DuplicateTitle { procedure, at } = &errors[0] else {
        panic!("expected DuplicateTitle, got {:?}", errors[0]);
    };
    assert_eq!(procedure.value, "make_coffee");
    let expected = source
        .rfind("# Second Title")
        .expect("second title in source");
    assert_eq!(at.offset, expected);
    assert!(at.length >= "# Second Title".len());
}

#[test]
fn description_after_code_block_is_error() {
    let source = r#"
% technique v1

make_coffee :

{
    journal("step 1")
}

This text comes too late.
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let errors = translate(&document).expect_err("translate should fail");

    assert_eq!(errors.len(), 1);
    let TranslationError::InterleavedDescription { procedure, at } = &errors[0] else {
        panic!("expected InterleavedDescription, got {:?}", errors[0]);
    };
    assert_eq!(procedure.value, "make_coffee");
    let expected = source
        .find("This text comes too late.")
        .expect("description in source");
    assert_eq!(at.offset, expected);
    assert!(at.length >= "This text comes too late.".len());
}

#[test]
fn orphan_response_is_error() {
    // ResponseBlock under an AttributeBlock with no enclosing Step.
    let source = r#"
% technique v1

check :

@chef
    'Yes' | 'No'
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let errors = translate(&document).expect_err("translate should fail");

    assert_eq!(errors.len(), 1);
    let TranslationError::OrphanResponse(at) = &errors[0] else {
        panic!("expected OrphanResponse, got {:?}", errors[0]);
    };
    let expected = source
        .find("'Yes' | 'No'")
        .expect("response in source");
    assert_eq!(at.offset, expected);
    assert!(at.length >= "'Yes' | 'No'".len());
}
