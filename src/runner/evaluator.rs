//! Variable bindings and the evaluator that turns value-bearing
//! Operations into Values for description rendering and binding.

use std::borrow::Cow;
use std::collections::HashMap;

use super::context::Context;
use super::library::Library;
use super::runner::RunnerError;
use crate::formatting::{Substitutions, Syntax};
use crate::program::{ExecutableRef, Fragment, Operation};
use crate::value::{Numeric, Value};

/// Variable bindings established by the walker as `Bind` operations
/// evaluate. Lookup is by identifier name.
#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct Environment {
    bindings: HashMap<String, Value>,
}

#[allow(dead_code)]
impl Environment {
    pub fn new() -> Self {
        Environment {
            bindings: HashMap::new(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Value> {
        self.bindings
            .get(name)
    }

    pub fn extend(&mut self, name: String, value: Value) {
        self.bindings
            .insert(name, value);
    }

    /// Pre-styled fragments for each bound value, for splicing into a step's
    /// prose where it interpolates that variable. See `render_value`.
    pub fn substitutions(&self) -> Substitutions {
        let mut subs = Substitutions::new();
        for (name, value) in &self.bindings {
            if let Some(fragments) = render_value(value) {
                subs.insert(name.clone(), fragments);
            }
        }
        subs
    }
}

/// Pre-styled fragments for splicing a bound value into a step's prose,
/// highlighted as it would be in source: strings quoted, numbers bare. Value
/// kinds with no sensible inline prose form yield None, leaving the variable's
/// `{ name }` interpolation to render as written.
fn render_value(value: &Value) -> Option<Vec<(Syntax, Cow<'static, str>)>> {
    match value {
        Value::Literali(text) => Some(vec![
            (Syntax::Quote, Cow::Borrowed("\"")),
            (Syntax::String, Cow::Owned(text.clone())),
            (Syntax::Quote, Cow::Borrowed("\"")),
        ]),
        Value::Quanticle(numeric) => Some(vec![(Syntax::Numeric, Cow::Owned(numeric.to_string()))]),
        _ => None,
    }
}

/// The monoidal append operation for the Value type.
///
/// Combine two Values into one, with `Unitus` as the identity. Within-kind
/// pairings combine (strings concatenate, lists append, tablets merge with
/// last-write-wins on duplicate keys); cross-kind and not-yet-defined
/// within-kind pairings are a hard error.
///
/// Note that this is deliberately *not* the value of a `Sequence`: a
/// sequence is statement composition and takes its last member's value,
/// while `+` accumulates — `{ "a"; "b" }` is `"b"`, but `"a" + "b"` is
/// `"ab"`.
#[allow(dead_code)]
pub fn combine(left: Value, right: Value) -> Result<Value, RunnerError> {
    match (left, right) {
        (Value::Unitus, other) | (other, Value::Unitus) => Ok(other),
        (Value::Literali(mut a), Value::Literali(b)) => {
            a.push_str(&b);
            Ok(Value::Literali(a))
        }
        (Value::Arraeum(mut a), Value::Arraeum(b)) => {
            a.extend(b);
            Ok(Value::Arraeum(a))
        }
        (Value::Tabularum(mut a), Value::Tabularum(b)) => {
            for (key, value) in b {
                match a
                    .iter_mut()
                    .find(|(existing, _)| *existing == key)
                {
                    Some(entry) => entry.1 = value,
                    None => a.push((key, value)),
                }
            }
            Ok(Value::Tabularum(a))
        }
        (left, right) => Err(RunnerError::IncompatibleCombination {
            left: kind(&left),
            right: kind(&right),
        }),
    }
}

/// Human-facing kind name of a Value, for combination error messages.
fn kind(value: &Value) -> &'static str {
    match value {
        Value::Unitus => "unit",
        Value::Literali(_) => "string",
        Value::Enumerati(_) => "response",
        Value::Quanticle(_) => "quantity",
        Value::Intratempse(_) => "resource",
        Value::Tabularum(_) => "tablet",
        Value::Arraeum(_) => "list",
        Value::Parametriq(_) => "tuple",
        Value::Futurae(_) => "future",
    }
}

/// Evaluate an `Operation` to a `Value`.
///
/// Fails with `UnboundVariable` etc if the operation cannot be resolved;
/// specifically at this point values of variables need to be known from the
/// `Environment` otherwise the `Operation` can't be evaluated.
///
/// A resolved `Execute` dispatches through the passed in `Library` to its
/// builtin, evaluating its arguments before doing so.
#[allow(dead_code)]
pub fn evaluate<'i>(
    library: &Library,
    context: &Context,
    env: &mut Environment,
    op: &Operation<'i>,
) -> Result<Value, RunnerError> {
    match op {
        Operation::Variable(id) => env
            .lookup(id.value)
            .cloned()
            .ok_or_else(|| {
                RunnerError::UnboundVariable(
                    id.value
                        .to_string(),
                )
            }),
        Operation::Number(n) => Ok(Value::Quanticle(Numeric::from(n))),
        Operation::Response(value) => Ok(Value::Enumerati(value.to_string())),
        Operation::String(fragments) => {
            let mut text = String::new();
            for fragment in fragments {
                match fragment {
                    Fragment::Text(t) => text.push_str(t),
                    Fragment::Interpolation(inner) => {
                        match evaluate(library, context, env, inner)? {
                            Value::Literali(s) => text.push_str(&s),
                            other => text.push_str(&other.to_string()),
                        }
                    }
                }
            }
            Ok(Value::Literali(text))
        }
        Operation::Multiline(_, lines) => Ok(Value::Literali(lines.join("\n"))),
        Operation::Tablet(entries) => {
            let mut pairs = Vec::with_capacity(entries.len());
            for entry in entries {
                let v = evaluate(library, context, env, &entry.value)?;
                pairs.push((
                    entry
                        .label
                        .to_string(),
                    v,
                ));
            }
            Ok(Value::Tabularum(pairs))
        }
        Operation::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(evaluate(library, context, env, item)?);
            }
            Ok(Value::Arraeum(values))
        }
        Operation::Tuple(items) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(evaluate(library, context, env, item)?);
            }
            Ok(Value::Parametriq(values))
        }
        Operation::Bind { names, value, .. } => {
            let v = evaluate(library, context, env, value)?;
            bind_names(env, names, v)?;
            Ok(Value::Unitus)
        }
        Operation::Sequence(ops) => {
            let mut last = Value::Unitus;
            for child in ops {
                last = evaluate(library, context, env, child)?;
            }
            Ok(last)
        }
        Operation::Execute(executable) => dispatch(library, context, env, executable, None),
        // A `?` reached outside a procedure invocation has no parameter name
        // to defer against; it stands for an as-yet-unsupplied value.
        Operation::Hole => Ok(Value::Futurae(String::new())),
        Operation::Unit => Ok(Value::Unitus),
        Operation::Prose(_)
        | Operation::Prologue(_)
        | Operation::Section { .. }
        | Operation::Step { .. }
        | Operation::Loop { .. }
        | Operation::Within { .. }
        | Operation::Cost(_)
        | Operation::Invoke(_) => Ok(Value::Unitus),
    }
}

/// Reduce a value to the elements a `foreach` iterates. A list yields its
/// members; `Unit` (the absence of a value) is empty; a blank string (an empty
/// prompt answer) is likewise empty, so a `foreach` over it runs zero times; a
/// non-blank string may be a `[a, b]` literal, which parses into its elements,
/// else it is a one-element list; a bare quantity widens likewise. A tablet,
/// tuple, or future is not iterable.
pub(super) fn coerce_to_list(value: Value) -> Result<Vec<Value>, RunnerError> {
    match value {
        Value::Arraeum(items) => Ok(items),
        Value::Unitus => Ok(Vec::new()),
        Value::Literali(text)
            if text
                .trim()
                .is_empty() =>
        {
            Ok(Vec::new())
        }
        Value::Literali(text) => match parse_list_literal(&text) {
            Some(items) => Ok(items),
            None => Ok(vec![Value::Literali(text)]),
        },
        value @ Value::Quanticle(_) => Ok(vec![value]),
        _ => Err(RunnerError::NotIterable),
    }
}

/// Coerce a raw user-supplied string — a command-line argument or an unquoted
/// list element — into its natural Value type: a `[ … ]` literal becomes a
/// list, a number becomes a quantity, anything else stays a string.
pub(super) fn parse_value(text: &str) -> Value {
    let trimmed = text.trim();
    if let Some(items) = parse_list_literal(trimmed) {
        return Value::Arraeum(items);
    }
    if let Some(numeric) = crate::parsing::parse_numeric(trimmed) {
        return Value::Quanticle(Numeric::from(&numeric));
    }
    Value::Literali(text.to_string())
}

/// Parse a `[ "a", b, ... ]` literal into its elements. A quoted element is a
/// string verbatim; an unquoted one takes its natural type via `parse_value`.
/// Returns `None` for text that is not bracketed. TODO This splits naively on
/// ',' so commas inside element text are not supported.
fn parse_list_literal(text: &str) -> Option<Vec<Value>> {
    let inner = text
        .trim()
        .strip_prefix('[')?
        .strip_suffix(']')?;
    if inner
        .trim()
        .is_empty()
    {
        return Some(Vec::new());
    }
    let items = inner
        .split(',')
        .map(|element| {
            let element = element.trim();
            match element
                .strip_prefix('"')
                .and_then(|e| e.strip_suffix('"'))
            {
                Some(unquoted) => Value::Literali(unquoted.to_string()),
                None => parse_value(element),
            }
        })
        .collect();
    Some(items)
}

/// Bind names to a value, shared by `Bind` evaluation and `foreach`
/// iteration. One name takes the whole value; multiple names destructure a
/// `Parametriq` of matching arity.
pub(super) fn bind_names(
    env: &mut Environment,
    names: &[crate::language::Identifier<'_>],
    value: Value,
) -> Result<(), RunnerError> {
    match names.len() {
        0 => unreachable!(), // bind_names requires at least one name
        1 => env.extend(
            names[0]
                .value
                .to_string(),
            value,
        ),
        n => {
            let Value::Parametriq(values) = value else {
                return Err(RunnerError::BindNotTuple { expected: n });
            };
            if values.len() != n {
                return Err(RunnerError::BindArityMismatch {
                    expected: n,
                    actual: values.len(),
                });
            }
            for (name, value) in names
                .iter()
                .zip(values)
            {
                env.extend(
                    name.value
                        .to_string(),
                    value,
                );
            }
        }
    }
    Ok(())
}

/// Run a builtin function. When `override_args` is `None`, arguments are
/// evaluated from the executable's AST. When `Some`, the pre-evaluated
/// values are used directly (the Action path, where the user may have
/// edited the command before confirming). This is the single site that
/// calls into the Library.
#[allow(dead_code)]
pub fn dispatch<'i>(
    library: &Library,
    context: &Context,
    env: &mut Environment,
    executable: &crate::program::Executable<'i>,
    override_args: Option<&[Value]>,
) -> Result<Value, RunnerError> {
    match &executable.target {
        ExecutableRef::Resolved(id) => {
            if let Some(args) = override_args {
                library.call(*id, context, args)
            } else {
                let mut args = Vec::with_capacity(
                    executable
                        .arguments
                        .len(),
                );
                for arg in &executable.arguments {
                    args.push(evaluate(library, context, env, arg)?);
                }
                library.call(*id, context, &args)
            }
        }
        ExecutableRef::Unresolved(target) => Err(RunnerError::UnknownFunction(
            target
                .value
                .to_string(),
        )),
    }
}

#[cfg(test)]
#[path = "checks/evaluator.rs"]
mod check;
