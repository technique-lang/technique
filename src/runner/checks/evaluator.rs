use crate::language::{Identifier, Numeric as LangNumeric};
use crate::program::{Entry, Fragment, Operation};
use crate::runner::evaluator::{evaluate, Environment};
use crate::runner::runner::RunnerError;
use crate::value;

#[test]
fn variable_lookup() {
    let op = Operation::Variable(Identifier::new("missing"));
    let mut env = Environment::new();
    match evaluate(&mut env, &op) {
        Err(RunnerError::UnboundVariable(name)) => assert_eq!(name, "missing"),
        other => panic!("expected UnboundVariable, got {:?}", other),
    }

    let mut env = Environment::new();
    env.extend(
        "name".to_string(),
        value::Value::Literali("World".to_string()),
    );
    let op = Operation::Variable(Identifier::new("name"));
    let v = evaluate(&mut env, &op).expect("evaluated");
    assert_eq!(v, value::Value::Literali("World".to_string()));
}

#[test]
fn number_evaluates_to_quanticle() {
    let op = Operation::Number(LangNumeric::Integral(42));
    let mut env = Environment::new();
    let v = evaluate(&mut env, &op).expect("evaluated");
    assert_eq!(v, value::Value::Quanticle(value::Numeric::Integral(42)));
}

#[test]
fn string_interpolation() {
    let mut env = Environment::new();
    env.extend(
        "name".to_string(),
        value::Value::Literali("World".to_string()),
    );
    let op = Operation::String(vec![
        Fragment::Text("Hello, "),
        Fragment::Interpolation(Operation::Variable(Identifier::new("name"))),
        Fragment::Text("!"),
    ]);
    let v = evaluate(&mut env, &op).expect("evaluated");
    assert_eq!(v, value::Value::Literali("Hello, World!".to_string()));

    let op = Operation::String(vec![
        Fragment::Text("hi "),
        Fragment::Interpolation(Operation::Variable(Identifier::new("nope"))),
    ]);
    let mut env = Environment::new();
    match evaluate(&mut env, &op) {
        Err(RunnerError::UnboundVariable(name)) => assert_eq!(name, "nope"),
        other => panic!("expected UnboundVariable, got {:?}", other),
    }
}

#[test]
fn multiline_joins_with_newlines() {
    let op = Operation::Multiline(None, vec!["foo", "bar", "baz"]);
    let mut env = Environment::new();
    let v = evaluate(&mut env, &op).expect("evaluated");
    assert_eq!(v, value::Value::Literali("foo\nbar\nbaz".to_string()));
}

#[test]
fn tablet_entries_evaluate() {
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
    let v = evaluate(&mut env, &op).expect("evaluated");
    assert_eq!(
        v,
        value::Value::Tabularum(vec![
            (
                "name".to_string(),
                value::Value::Literali("Kowalski".to_string())
            ),
            (
                "count".to_string(),
                value::Value::Quanticle(value::Numeric::Integral(7))
            ),
        ])
    );
}

#[test]
fn list_elements_evaluate() {
    let op = Operation::List(vec![
        Operation::Number(LangNumeric::Integral(1)),
        Operation::Number(LangNumeric::Integral(4)),
        Operation::Number(LangNumeric::Integral(9)),
    ]);
    let mut env = Environment::new();
    let v = evaluate(&mut env, &op).expect("evaluated");
    assert_eq!(
        v,
        value::Value::Arraeum(vec![
            value::Value::Quanticle(value::Numeric::Integral(1)),
            value::Value::Quanticle(value::Numeric::Integral(4)),
            value::Value::Quanticle(value::Numeric::Integral(9)),
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
    let v = evaluate(&mut env, &seq).expect("evaluated");
    assert_eq!(v, value::Value::Literali("Hello".to_string()));
}

#[test]
fn sequence_evaluation() {
    let seq = Operation::Sequence(vec![
        Operation::Number(LangNumeric::Integral(1)),
        Operation::Number(LangNumeric::Integral(2)),
        Operation::Number(LangNumeric::Integral(3)),
    ]);
    let mut env = Environment::new();
    let v = evaluate(&mut env, &seq).expect("evaluated");
    assert_eq!(v, value::Value::Quanticle(value::Numeric::Integral(3)));

    let seq = Operation::Sequence(vec![]);
    let mut env = Environment::new();
    let v = evaluate(&mut env, &seq).expect("evaluated");
    assert_eq!(v, value::Value::Unitus);
}

#[test]
fn multi_name_bind_destructures_parametriq() {
    // Build a Parametriq of three values by reducing a wrapped construction.
    // Simplest path: pre-stuff env with a Parametriq, then bind a tuple of
    // names to a Variable that looks it up.
    let mut env = Environment::new();
    env.extend(
        "triple".to_string(),
        value::Value::Parametriq(vec![
            value::Value::Literali("one".to_string()),
            value::Value::Literali("two".to_string()),
            value::Value::Quanticle(value::Numeric::Integral(3)),
        ]),
    );
    let names = [
        Identifier::new("a"),
        Identifier::new("b"),
        Identifier::new("c"),
    ];
    let bind = Operation::Bind {
        names: &names,
        value: Box::new(Operation::Variable(Identifier::new("triple"))),
    };
    let result = evaluate(&mut env, &bind).expect("evaluated");
    assert_eq!(result, value::Value::Unitus);
    assert_eq!(
        env.lookup("a"),
        Some(&value::Value::Literali("one".to_string()))
    );
    assert_eq!(
        env.lookup("b"),
        Some(&value::Value::Literali("two".to_string()))
    );
    assert_eq!(
        env.lookup("c"),
        Some(&value::Value::Quanticle(value::Numeric::Integral(3)))
    );
}

#[test]
fn multi_name_bind_wrong_arity_errors() {
    let mut env = Environment::new();
    env.extend(
        "pair".to_string(),
        value::Value::Parametriq(vec![
            value::Value::Literali("one".to_string()),
            value::Value::Literali("two".to_string()),
        ]),
    );
    let names = [
        Identifier::new("a"),
        Identifier::new("b"),
        Identifier::new("c"),
    ];
    let bind = Operation::Bind {
        names: &names,
        value: Box::new(Operation::Variable(Identifier::new("pair"))),
    };
    match evaluate(&mut env, &bind) {
        Err(RunnerError::BindArityMismatch { expected, actual }) => {
            assert_eq!(expected, 3);
            assert_eq!(actual, 2);
        }
        other => panic!("expected BindArityMismatch, got {:?}", other),
    }
}

#[test]
fn multi_name_bind_against_scalar_errors_as_not_tuple() {
    let mut env = Environment::new();
    env.extend(
        "scalar".to_string(),
        value::Value::Literali("just one".to_string()),
    );
    let names = [Identifier::new("a"), Identifier::new("b")];
    let bind = Operation::Bind {
        names: &names,
        value: Box::new(Operation::Variable(Identifier::new("scalar"))),
    };
    match evaluate(&mut env, &bind) {
        Err(RunnerError::BindNotTuple { expected }) => {
            assert_eq!(expected, 2);
        }
        other => panic!("expected BindNotTuple, got {:?}", other),
    }
}
