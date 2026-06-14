use crate::runner::context::Context;
use crate::runner::library::Library;
use crate::runner::runner::RunnerError;
use crate::value::{Numeric, Value};

fn int(n: i64) -> Value {
    Value::Quanticle(Numeric::Integral(n))
}

fn text(s: &str) -> Value {
    Value::Literali(s.to_string())
}

// Invoke a builtin by name through the core Library, the same path the
// evaluator takes once a call is resolved.
fn call(name: &str, args: &[Value]) -> Result<Value, RunnerError> {
    let library = Library::core();
    let context = Context::native();
    let id = library
        .resolve(name)
        .expect("builtin registered");
    library.call(id, &context, args)
}

// Invoke a system-layer builtin (exec, now) through an assembled core+system
// Library, the way an interactive or automatic run does.
fn call_system(name: &str, args: &[Value]) -> Result<Value, RunnerError> {
    let mut library = Library::core();
    library.extend(Library::system());
    let context = Context::native();
    let id = library
        .resolve(name)
        .expect("builtin registered");
    library.call(id, &context, args)
}

#[test]
fn seq_builds_inclusive_range() {
    let result = call("seq", &[int(1), int(4)]).expect("seq");
    assert_eq!(result, Value::Arraeum(vec![int(1), int(2), int(3), int(4)]));
}

#[test]
fn seq_is_empty_when_descending() {
    let result = call("seq", &[int(6), int(1)]).expect("seq");
    assert_eq!(result, Value::Arraeum(Vec::new()));
}

#[test]
fn seq_rejects_non_integer() {
    let result = call("seq", &[text("a"), int(4)]);
    let Err(RunnerError::InvalidArgument { function, .. }) = result else {
        panic!("expected InvalidArgument, got {:?}", result);
    };
    assert_eq!(function, "seq");
}

#[test]
fn call_with_wrong_arity_errors_rather_than_panicking() {
    let result = call("seq", &[int(1)]);
    let Err(RunnerError::FunctionArityMismatch {
        function,
        expected,
        actual,
    }) = result
    else {
        panic!("expected FunctionArityMismatch, got {:?}", result);
    };
    assert_eq!(function, "seq");
    assert_eq!(expected, 2);
    assert_eq!(actual, 1);
}

#[test]
fn zip_pairs_truncating_to_shorter() {
    let xs = Value::Arraeum(vec![int(1), int(2), int(3)]);
    let ys = Value::Arraeum(vec![text("a"), text("b")]);
    let result = call("zip", &[xs, ys]).expect("zip");
    assert_eq!(
        result,
        Value::Arraeum(vec![
            Value::Parametriq(vec![int(1), text("a")]),
            Value::Parametriq(vec![int(2), text("b")]),
        ])
    );
}

#[test]
fn tablet_projections() {
    let form = Value::Tabularum(vec![
        ("primary".to_string(), text("1.1.1.1")),
        ("secondary".to_string(), text("8.8.8.8")),
    ]);

    let values = call("values", std::slice::from_ref(&form)).expect("values");
    assert_eq!(
        values,
        Value::Arraeum(vec![text("1.1.1.1"), text("8.8.8.8")])
    );

    let labels = call("labels", std::slice::from_ref(&form)).expect("labels");
    assert_eq!(
        labels,
        Value::Arraeum(vec![text("primary"), text("secondary")])
    );

    let pairs = call("pairs", std::slice::from_ref(&form)).expect("pairs");
    assert_eq!(
        pairs,
        Value::Arraeum(vec![
            Value::Parametriq(vec![text("primary"), text("1.1.1.1")]),
            Value::Parametriq(vec![text("secondary"), text("8.8.8.8")]),
        ])
    );
}

#[test]
fn projections_reject_non_tablet() {
    let result = call("values", &[int(3)]);
    let Err(RunnerError::InvalidArgument { function, .. }) = result else {
        panic!("expected InvalidArgument, got {:?}", result);
    };
    assert_eq!(function, "values");
}

#[test]
fn exec_captures_stdout_as_literal() {
    let result = call_system("exec", &[text("printf 'hello world'")]).expect("exec");
    assert_eq!(result, text("hello world"));
}

#[test]
fn exec_rejects_non_string_argument() {
    let result = call_system("exec", &[int(3)]);
    let Err(RunnerError::InvalidArgument { function, .. }) = result else {
        panic!("expected InvalidArgument, got {:?}", result);
    };
    assert_eq!(function, "exec");
}

#[test]
fn exec_nonzero_exit_is_command_failed() {
    let result = call_system("exec", &[text("exit 3")]);
    let Err(RunnerError::CommandFailed(code)) = result else {
        panic!("expected CommandFailed, got {:?}", result);
    };
    assert_eq!(code, 3);
}
