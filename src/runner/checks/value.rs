use crate::language::{Identifier, Numeric as LangNumeric};
use crate::program::{Entry, Fragment, Operation};
use crate::runner::runner::RunnerError;
use crate::runner::value::{reduce, Environment};
use crate::value::{Numeric, Value};

#[test]
fn unbound_variable_errors() {
    let op = Operation::Variable(Identifier::new("missing"));
    let mut env = Environment::new();
    match reduce(&op, &mut env) {
        Err(RunnerError::UnboundVariable(name)) => assert_eq!(name, "missing"),
        other => panic!("expected UnboundVariable, got {:?}", other),
    }
}

#[test]
fn bound_variable_looks_up() {
    let mut env = Environment::new();
    env.extend("name".to_string(), Value::Literali("World".to_string()));
    let op = Operation::Variable(Identifier::new("name"));
    let v = reduce(&op, &mut env).expect("reduce");
    assert_eq!(v, Value::Literali("World".to_string()));
}

#[test]
fn number_reduces_to_quanticle() {
    let op = Operation::Number(LangNumeric::Integral(42));
    let mut env = Environment::new();
    let v = reduce(&op, &mut env).expect("reduce");
    assert_eq!(v, Value::Quanticle(Numeric::Integral(42)));
}

#[test]
fn string_fragments_interpolate_bound_variable() {
    let mut env = Environment::new();
    env.extend("name".to_string(), Value::Literali("World".to_string()));
    let op = Operation::String(vec![
        Fragment::Text("Hello, "),
        Fragment::Interpolation(Operation::Variable(Identifier::new("name"))),
        Fragment::Text("!"),
    ]);
    let v = reduce(&op, &mut env).expect("reduce");
    assert_eq!(v, Value::Literali("Hello, World!".to_string()));
}

#[test]
fn string_interpolation_propagates_unbound_error() {
    let op = Operation::String(vec![
        Fragment::Text("hi "),
        Fragment::Interpolation(Operation::Variable(Identifier::new("nope"))),
    ]);
    let mut env = Environment::new();
    match reduce(&op, &mut env) {
        Err(RunnerError::UnboundVariable(name)) => assert_eq!(name, "nope"),
        other => panic!("expected UnboundVariable, got {:?}", other),
    }
}

#[test]
fn multiline_joins_with_newlines() {
    let op = Operation::Multiline(None, vec!["foo", "bar", "baz"]);
    let mut env = Environment::new();
    let v = reduce(&op, &mut env).expect("reduce");
    assert_eq!(v, Value::Literali("foo\nbar\nbaz".to_string()));
}

#[test]
fn tablet_entries_reduce() {
    let op = Operation::Tablet(vec![
        Entry {
            label: "name",
            value: Operation::String(vec![Fragment::Text("Kowalski")]),
        },
        Entry {
            label: "count",
            value: Operation::Number(LangNumeric::Integral(7)),
        },
    ]);
    let mut env = Environment::new();
    let v = reduce(&op, &mut env).expect("reduce");
    assert_eq!(
        v,
        Value::Tabularum(vec![
            ("name".to_string(), Value::Literali("Kowalski".to_string())),
            ("count".to_string(), Value::Quanticle(Numeric::Integral(7))),
        ])
    );
}

#[test]
fn bind_extends_env_for_subsequent_lookup() {
    let names = [Identifier::new("greeting")];
    let bind = Operation::Bind {
        names: &names,
        value: Box::new(Operation::String(vec![Fragment::Text("Hello")])),
    };
    let lookup = Operation::Variable(Identifier::new("greeting"));
    let seq = Operation::Sequence(vec![bind, lookup]);
    let mut env = Environment::new();
    let v = reduce(&seq, &mut env).expect("reduce");
    assert_eq!(v, Value::Literali("Hello".to_string()));
}

#[test]
fn sequence_returns_last_value() {
    let seq = Operation::Sequence(vec![
        Operation::Number(LangNumeric::Integral(1)),
        Operation::Number(LangNumeric::Integral(2)),
        Operation::Number(LangNumeric::Integral(3)),
    ]);
    let mut env = Environment::new();
    let v = reduce(&seq, &mut env).expect("reduce");
    assert_eq!(v, Value::Quanticle(Numeric::Integral(3)));
}

#[test]
fn empty_sequence_returns_unitus() {
    let seq = Operation::Sequence(vec![]);
    let mut env = Environment::new();
    let v = reduce(&seq, &mut env).expect("reduce");
    assert_eq!(v, Value::Unitus);
}
