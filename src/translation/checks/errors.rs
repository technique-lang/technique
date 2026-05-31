// Error-case checks for translation. Source strings parsed through the
// real parser inline match what the runner sees in production.

use std::path::Path;

use crate::parsing;
use crate::translation::{translate, TranslationError};

#[test]
fn duplicate_procedure_name() {
    let source = r#"
% technique v1

make_coffee :

make_coffee :
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let errors = translate(&document).expect_err("translate should fail");

    assert_eq!(errors.len(), 1);
    let TranslationError::DuplicateProcedure(id) = &errors[0] else {
        panic!("expected DuplicateProcedure, got {:?}", errors[0]);
    };
    assert_eq!(id.value, "make_coffee");
    let expected = source
        .rfind("make_coffee")
        .expect("second declaration in source");
    assert_eq!(
        id.span
            .offset,
        expected,
        "span points at the duplicate declaration"
    );
}

#[test]
fn bound_repeat() {
    let source = r#"
% technique v1

making :

    1.  { repeat <coffee>(e) ~ cups }

coffee : E -> C
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let errors = translate(&document).expect_err("translate should fail");

    assert_eq!(errors.len(), 1);
    let TranslationError::BoundRepeat { at } = &errors[0] else {
        panic!("expected BoundRepeat, got {:?}", errors[0]);
    };
    assert_eq!(
        at.offset,
        source
            .find("repeat")
            .expect("repeat in source"),
        "span points at the bound repeat expression"
    );
}

#[test]
fn duplicate_title() {
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
fn description_after_code_block() {
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
fn second_description_split_by_title() {
    // Two prose blocks separated by a `# Title` parse as
    // [Description, Title, Description, Steps]. The procedure shell
    // allows at most one description, and the second one fires
    // InterleavedDescription regardless of whether body elements have
    // appeared yet.
    let source = r#"
% technique v1

make_coffee :

First paragraph of preamble.

# Coffee Time

Second paragraph would silently disappear.

1.  Grind beans.
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
        .find("Second paragraph")
        .expect("second description in source");
    assert_eq!(at.offset, expected);
}

#[test]
fn description_after_multiple_body_elements() {
    // Pin down the rule "a description must come before any body element":
    // once `blocked` is set by a body element, it stays set across further
    // body elements, and a later Element::Description still trips the
    // guard. (A Steps-then-Description case is unreachable today because
    // the parser absorbs trailing prose into the previous step's
    // description)
    let source = r#"
% technique v1

make_coffee :

{
    journal("a")
}

{
    journal("b")
}

This text is past the body.
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
        .find("This text is past the body.")
        .expect("description in source");
    assert_eq!(at.offset, expected);
}

#[test]
fn unresolved_procedure_invocation() {
    let source = r#"
% technique v1

main :

{
    <does_not_exist>(x)
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let errors = translate(&document).expect_err("translate should fail");

    assert_eq!(errors.len(), 1);
    let TranslationError::UnresolvedProcedure(id) = &errors[0] else {
        panic!("expected UnresolvedProcedure, got {:?}", errors[0]);
    };
    assert_eq!(id.value, "does_not_exist");
    let expected = source
        .find("does_not_exist")
        .expect("identifier in source");
    assert_eq!(
        id.span
            .offset,
        expected,
        "span points at the offending identifier"
    );
}

#[test]
fn unresolved_procedure_in_section_title() {
    // After section title hoisting, an `<Application>` embedded in the
    // title goes through the resolve pass. This ensures section titles
    // can't silently reference nonexistent procedures.
    let source = r#"
% technique v1

main :

I. Lead with <does_not_exist>
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let errors = translate(&document).expect_err("translate should fail");

    assert_eq!(errors.len(), 1);
    let TranslationError::UnresolvedProcedure(id) = &errors[0] else {
        panic!("expected UnresolvedProcedure, got {:?}", errors[0]);
    };
    assert_eq!(id.value, "does_not_exist");
}
