use crate::value::{Numeric, Value};

#[test]
fn value_display() {
    assert_eq!(Value::Unitus.to_string(), "");

    let v = Value::Literali("just text".to_string());
    assert_eq!(v.to_string(), "\"just text\"");

    let v = Value::Quanticle(Numeric::Integral(42));
    assert_eq!(v.to_string(), "42");

    let v = Value::Tabularum(vec![
        ("first".to_string(), Value::Literali("Anna".to_string())),
        ("last".to_string(), Value::Literali("Kowalski".to_string())),
    ]);
    assert_eq!(v.to_string(), "[first = \"Anna\", last = \"Kowalski\"]");

    let v = Value::Parametriq(vec![
        Value::Literali("a".to_string()),
        Value::Literali("b".to_string()),
        Value::Quanticle(Numeric::Integral(3)),
    ]);
    assert_eq!(v.to_string(), "[\"a\", \"b\", 3]");

    assert_eq!(Value::Futurae("name".to_string()).to_string(), "{name}");
}
