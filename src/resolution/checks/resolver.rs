// Hand-written check suite for the resolution phase. Source strings are parsed
// and translated through the real pipeline, then resolved — matching what the
// runner sees in production.

use std::path::Path;

use crate::language;
use crate::parsing;
use crate::program::{ExecutableRef, Operation, SubroutineId, SubroutineRef};
use crate::resolution::{ResolutionError, resolve};
use crate::translation::translate;

#[test]
fn procedure_arity_mismatch() {
    // observe's arity is 1 (one signature input). A bare `<observe>` would
    // defer it, but a written argument list must match exactly — two arguments
    // is a mismatch.
    let source = r#"
% technique v1

main(a, b) :

{
    <observe>(a, b)
}

observe : Situation -> Context
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    let errors = resolve(&mut program).expect_err("resolve should fail");

    assert_eq!(errors.len(), 1);
    let ResolutionError::ProcedureArityMismatch {
        procedure,
        expected,
        actual,
    } = &errors[0]
    else {
        panic!("expected ProcedureArityMismatch, got {:?}", errors[0]);
    };
    assert_eq!(procedure.value, "observe");
    assert_eq!(*expected, 1);
    assert_eq!(*actual, 2);
    assert_eq!(
        procedure
            .span
            .offset,
        source
            .find("observe")
            .expect("observe in source"),
        "span points at the invocation"
    );
}

#[test]
fn empty_parens_must_match_arity() {
    // `<other>()` writes an explicit (empty) list, so it must match arity — a
    // mismatch against arity 1.
    let source = r#"
% technique v1

run :

{
    <other>()
}

other : X -> Y
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    let errors = resolve(&mut program).expect_err("resolve should fail");

    let ResolutionError::ProcedureArityMismatch {
        expected, actual, ..
    } = &errors[0]
    else {
        panic!("expected ProcedureArityMismatch, got {:?}", errors[0]);
    };
    assert_eq!(*expected, 1);
    assert_eq!(*actual, 0);
}

#[test]
fn unresolved_procedure_invocation() {
    let source = r#"
% technique v1

main(x) :

{
    <does_not_exist>(x)
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    let errors = resolve(&mut program).expect_err("resolve should fail");

    assert_eq!(errors.len(), 1);
    let ResolutionError::UnresolvedProcedure(id) = &errors[0] else {
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
    // After section title hoisting, an `<Application>` embedded in the title
    // goes through the resolve pass. This ensures section titles can't
    // silently reference nonexistent procedures.
    let source = r#"
% technique v1

main :

I. Lead with <does_not_exist>
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    let errors = resolve(&mut program).expect_err("resolve should fail");

    assert_eq!(errors.len(), 1);
    let ResolutionError::UnresolvedProcedure(id) = &errors[0] else {
        panic!("expected UnresolvedProcedure, got {:?}", errors[0]);
    };
    assert_eq!(id.value, "does_not_exist");
}

#[test]
fn invoke_resolves_forward_reference() {
    // helper is declared *after* main, but resolution should still wire it up
    // because every name is registered before any body is resolved.
    let source = r#"
% technique v1

main(x) :

{
    <helper>(x)
}

helper : X -> Y
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let helper_idx = program
        .subroutines
        .iter()
        .position(|s| {
            s.name
                .as_ref()
                .map(|n| n.value)
                == Some("helper")
        })
        .expect("helper present");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Invoke(invocable) = &ops[0] else {
        panic!("expected Invoke");
    };
    let SubroutineRef::Resolved(SubroutineId(idx)) = invocable.target else {
        panic!("expected Resolved, got {:?}", invocable.target);
    };
    assert_eq!(idx, helper_idx);
}

#[test]
fn invoke_resolves_backward_reference() {
    // helper is declared *before* main; same expectation.
    let source = r#"
% technique v1

helper : X -> Y

main(x) :

{
    <helper>(x)
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let main_idx = program
        .subroutines
        .iter()
        .position(|s| {
            s.name
                .as_ref()
                .map(|n| n.value)
                == Some("main")
        })
        .expect("main present");

    let Operation::Sequence(ops) = &program.subroutines[main_idx].body else {
        panic!("expected Sequence");
    };
    let Operation::Invoke(invocable) = &ops[0] else {
        panic!("expected Invoke");
    };
    let SubroutineRef::Resolved(_) = invocable.target else {
        panic!("expected Resolved, got {:?}", invocable.target);
    };
}

#[test]
fn executable_target_left_alone_by_resolve() {
    // Function calls (the `name(...)` form, no angle brackets) live in a
    // separate namespace — they're built-in or host-provided and are resolved
    // later by linking against the executing domain. Resolution must leave
    // Operation::Execute targets untouched.
    let source = r#"
% technique v1

run :

{
    journal("started")
}
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let Operation::Sequence(ops) = &program.subroutines[0].body else {
        panic!("expected Sequence");
    };
    let Operation::Execute(executable) = &ops[0] else {
        panic!("expected Execute, got {:?}", ops[0]);
    };
    let ExecutableRef::Unresolved(target) = &executable.target else {
        panic!("expected Unresolved, got {:?}", executable.target);
    };
    assert_eq!(target.value, "journal");
}

#[test]
fn unbound_variable_is_an_error() {
    let source = r#"
% technique v1

select_account : Authority -> Account
    1.  specify Account ID { account_number } here
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    let errors = resolve(&mut program).expect_err("unbound variable");

    assert_eq!(errors.len(), 1);
    let ResolutionError::UnboundVariable { variable } = &errors[0] else {
        panic!("expected UnboundVariable, got {:?}", errors[0]);
    };
    assert_eq!(variable.value, "account_number");
}

#[test]
fn callee_cannot_read_caller_binding() {
    // A name bound in `main` is not in `peek`'s scope: each procedure resolves
    // against its own parameters and bindings, never the caller's. `peek`'s
    // read of `secret` is therefore unbound — the isolation guarantee enforced
    // statically rather than only as a runtime fault.
    let source = r#"
% technique v1

main :

1.  remember the value ~ secret

2.  { <peek>() }

peek :

1.  Value is { secret }
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    let errors = resolve(&mut program).expect_err("resolve should fail");

    assert_eq!(errors.len(), 1);
    let ResolutionError::UnboundVariable { variable } = &errors[0] else {
        panic!("expected UnboundVariable, got {:?}", errors[0]);
    };
    assert_eq!(variable.value, "secret");
}

#[test]
fn read_before_binding_is_an_error() {
    // `a` is read in step 1 but only bound in step 2. The runner walks steps
    // in order and would find `a` unbound at step 1, so resolution rejects it:
    // the scope check is order-sensitive, not merely "bound somewhere".
    let source = r#"
% technique v1

task :

1.  Read { a }.

2.  The value is ~ a
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    let errors = resolve(&mut program).expect_err("read before binding");

    assert_eq!(errors.len(), 1);
    let ResolutionError::UnboundVariable { variable } = &errors[0] else {
        panic!("expected UnboundVariable, got {:?}", errors[0]);
    };
    assert_eq!(variable.value, "a");
}

#[test]
fn parameter_and_bound_reads_resolve_cleanly() {
    // A read of a named parameter, and a read of a name bound by an earlier
    // step, both resolve — neither is flagged.
    let source = r#"
% technique v1

settle(amount) : Owed -> Paid
    1.  record { amount } ~ noted
    2.  confirm { noted }
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");

    resolve(&mut program).expect("resolve");
}

// Find the first single-name `Bind` whose name matches, returning its inferred
// genus.
fn inferred_for<'a>(op: &'a Operation<'a>, name: &str) -> Option<&'a Option<language::Genus<'a>>> {
    match op {
        Operation::Bind {
            names, inferred, ..
        } if names.len() == 1 && names[0].value == name => Some(inferred),
        Operation::Bind { value, .. } | Operation::Step { body: value, .. } => {
            inferred_for(value, name)
        }
        Operation::Sequence(ops) | Operation::List(ops) => ops
            .iter()
            .find_map(|op| inferred_for(op, name)),
        Operation::Loop { over, body, .. } => over
            .as_ref()
            .and_then(|over| inferred_for(over, name))
            .or_else(|| inferred_for(body, name)),
        Operation::Section { body, .. } => inferred_for(body, name),
        _ => None,
    }
}

#[test]
fn iterated_binding_is_inferred_list() {
    // `regions` is bound by a descriptive step and then iterated by a foreach,
    // so resolution marks it `[*]`.
    let source = r#"
% technique v1

sweep :
    1.  enumerate the regions ~ regions
    2.  { foreach region in regions }
        -   note { region }
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let inferred =
        inferred_for(&program.subroutines[0].body, "regions").expect("regions bind present");
    assert_eq!(
        inferred,
        &Some(language::Genus::List(language::Forma::new("*")))
    );
}

#[test]
fn non_iterated_binding_stays_unknown() {
    // `noted` is bound but never iterated, so its shape is left `None`.
    let source = r#"
% technique v1

record :
    1.  write it down ~ noted
    2.  confirm { noted }
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let inferred = inferred_for(&program.subroutines[0].body, "noted").expect("noted bind present");
    assert_eq!(inferred, &None);
}

#[test]
fn multi_name_binding_stays_unknown() {
    // A tuple binding is never marked, even if one of its names is iterated:
    // only single-name binds carry an inferred list shape.
    let source = r#"
% technique v1

split :
    1.  divide it ~ (regions, rest)
    2.  { foreach region in regions }
        -   note { region }
        "#
    .trim_ascii();
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    // The tuple bind is skipped, so no single-name `regions` bind exists to mark.
    assert!(inferred_for(&program.subroutines[0].body, "regions").is_none());
}
