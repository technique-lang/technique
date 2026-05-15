use crate::value::{Numeric, Value};

#[test]
fn integral_renders_via_formatter() {
    let v = Value::Quanticle(Numeric::Integral(42));
    assert_eq!(v.to_string(), "42");
}

#[test]
fn render_tabularum_formats_as_bracketed_pairs() {
    let v = Value::Tabularum(vec![
        ("first".to_string(), Value::Literali("Anna".to_string())),
        ("last".to_string(), Value::Literali("Kowalski".to_string())),
    ]);
    assert_eq!(v.to_string(), "[first = \"Anna\", last = \"Kowalski\"]");
}

#[test]
fn render_unitus_is_empty() {
    assert_eq!(Value::Unitus.to_string(), "");
}

#[test]
fn render_futurae_is_braced() {
    assert_eq!(Value::Futurae("name".to_string()).to_string(), "{name}");
}

#[test]
fn render_literali_is_quoted() {
    let v = Value::Literali("just text".to_string());
    assert_eq!(v.to_string(), "\"just text\"");
}

#[test]
fn render_parametriq_formats_as_bracketed_list() {
    let v = Value::Parametriq(vec![
        Value::Literali("a".to_string()),
        Value::Literali("b".to_string()),
        Value::Quanticle(Numeric::Integral(3)),
    ]);
    assert_eq!(v.to_string(), "[\"a\", \"b\", 3]");
}
