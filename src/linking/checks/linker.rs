// Hand-written check suite for the linking phase. Source strings are parsed
// and translated through the real pipeline, then linked against a stub
// `Library`, matching what the runner sees in production.

use std::path::Path;

use crate::linking::{link, LinkingError};
use crate::parsing;
use crate::program::{Executable, ExecutableRef, Operation};
use crate::runner::Library;
use crate::translation::translate;

fn first_execute<'a, 'i>(op: &'a Operation<'i>) -> Option<&'a Executable<'i>> {
    match op {
        Operation::Execute(executable) => Some(executable),
        Operation::Sequence(ops) => ops
            .iter()
            .find_map(first_execute),
        Operation::Section { body, .. } => first_execute(body),
        Operation::Step { body, .. } => first_execute(body),
        Operation::Loop { over, body, .. } => over
            .as_deref()
            .and_then(first_execute)
            .or_else(|| first_execute(body)),
        Operation::Bind { value, .. } => first_execute(value),
        Operation::Tablet(entries) => entries
            .iter()
            .find_map(|entry| first_execute(&entry.value)),
        Operation::List(items) => items
            .iter()
            .find_map(first_execute),
        _ => None,
    }
}

#[test]
fn known_function_resolves_to_reference() {
    let source = r#"
% technique v1

powerdown :
    1.  Inhibit the node { cmd("Inhibit") }
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");

    link(&mut program, &Library::stub()).expect("link");

    let executable = first_execute(&program.subroutines[0].body).expect("an Execute in the body");
    let ExecutableRef::Resolved(_) = executable.target else {
        panic!("expected Resolved, got {:?}", executable.target);
    };
}

#[test]
fn unknown_function_left_unresolved() {
    let source = r#"
% technique v1

probe :
    1.  Run a mystery { mystery("x") }
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");

    link(&mut program, &Library::stub()).expect("link");

    let executable = first_execute(&program.subroutines[0].body).expect("an Execute in the body");
    let ExecutableRef::Unresolved(target) = &executable.target else {
        panic!("expected Unresolved, got {:?}", executable.target);
    };
    assert_eq!(target.value, "mystery");
}

#[test]
fn wrong_arity_is_an_error() {
    let source = r#"
% technique v1

powerdown :
    1.  Inhibit too much { cmd("Inhibit", "Extra") }
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");

    let errors = link(&mut program, &Library::stub()).expect_err("arity error");
    assert_eq!(errors.len(), 1);
    let LinkingError::ArityMismatch {
        function,
        expected,
        actual,
    } = &errors[0];
    assert_eq!(function.value, "cmd");
    assert_eq!(*expected, 1);
    assert_eq!(*actual, 2);
}
