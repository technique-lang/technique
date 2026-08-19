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
/// Concatenate the fragments of a quoted literal, evaluating any
/// interpolation as it is reached. Shared by string values and the labels of
/// tablet entries, which parse the same way.
fn evaluate_fragments<'i>(
    library: &Library,
    context: &Context,
    env: &mut Environment,
    fragments: &[Fragment<'i>],
) -> Result<String, RunnerError> {
    let mut text = String::new();

    for fragment in fragments {
        match fragment {
            Fragment::Text(t) => text.push_str(t),
            Fragment::Escaped(c) => text.push(*c),
            Fragment::Interpolation(inner) => match evaluate(library, context, env, inner)? {
                Value::Literali(s) => text.push_str(&s),
                other => text.push_str(&other.to_string()),
            },
        }
    }

    Ok(text)
}

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
        Operation::Variable(id, _) => env
            .lookup(id.value)
            .cloned()
            .ok_or_else(|| {
                RunnerError::UnboundVariable(
                    id.value
                        .to_string(),
                )
            }),
        Operation::Number(n, _) => Ok(Value::Quanticle(Numeric::from(n))),
        Operation::Response(value, _) => Ok(Value::Enumerati(value.to_string())),
        Operation::String(fragments, _) => Ok(Value::Literali(evaluate_fragments(
            library, context, env, fragments,
        )?)),
        Operation::Multiline(_, lines, _) => Ok(Value::Literali(lines.join("\n"))),
        Operation::Tablet(entries, _) => {
            let mut pairs = Vec::with_capacity(entries.len());
            for entry in entries {
                let v = evaluate(library, context, env, &entry.value)?;
                pairs.push((evaluate_fragments(library, context, env, &entry.label)?, v));
            }
            Ok(Value::Tabularum(pairs))
        }
        Operation::List(items, _) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(evaluate(library, context, env, item)?);
            }
            Ok(Value::Arraeum(values))
        }
        Operation::Tuple(items, _) => {
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
        Operation::Sequence(ops, _) => {
            let mut last = Value::Unitus;
            for child in ops {
                last = evaluate(library, context, env, child)?;
            }
            Ok(last)
        }
        Operation::Execute(executable, _) => dispatch(library, context, env, executable, None),
        // A `?` reached outside a procedure invocation has no parameter name
        // to defer against; it stands for an as-yet-unsupplied value.
        Operation::Hole(_) => Ok(Value::Futurae(String::new())),
        Operation::Unit(_) => Ok(Value::Unitus),
        Operation::Prose(_, _)
        | Operation::Prologue(_, _)
        | Operation::Section { .. }
        | Operation::Step { .. }
        | Operation::Loop { .. }
        | Operation::Within { .. }
        | Operation::Cost(_, _)
        | Operation::Invoke(_, _) => Ok(Value::Unitus),
    }
}

/// Reduce a value to the elements a `foreach` iterates. A list yields its
/// members; `Unit` (the absence of a value) is empty; a blank string (an empty
/// prompt answer) is likewise empty, so a `foreach` over it runs zero times; a
/// non-blank string may be a `[a, b]` literal, which parses into its elements
/// and is an error if it doesn't, else it is a one-element list; a bare
/// quantity widens likewise. A tablet, tuple, or future is not iterable.
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
        Value::Literali(text) if is_list_literal(&text) => {
            parse_list_literal(&text).ok_or(RunnerError::MalformedList { text })
        }
        Value::Literali(text) => Ok(vec![Value::Literali(text)]),
        value @ Value::Quanticle(_) => Ok(vec![value]),
        _ => Err(RunnerError::NotIterable),
    }
}

/// Coerce a raw user-supplied string (a command-line argument or an unquoted
/// list element) into its natural Value type. A `[ ... ]` literal becomes a
/// list, a number becomes a quantity, anything else stays a string. `None` if
/// the text appears to be a list but does not parse.
pub(super) fn parse_value(text: &str) -> Option<Value> {
    let trimmed = text.trim();
    if is_list_literal(trimmed) {
        return parse_list_literal(trimmed).map(Value::Arraeum);
    }
    if let Some(numeric) = crate::parsing::parse_numeric(trimmed) {
        return Some(Value::Quanticle(Numeric::from(&numeric)));
    }
    Some(Value::Literali(text.to_string()))
}

/// Whether text reads as a list literal, the guard distinguishing a malformed
/// list from ordinary text that was never one.
pub(super) fn is_list_literal(text: &str) -> bool {
    let text = text.trim();
    text.starts_with('[') && text.ends_with(']')
}

/// Parse a user-input `[ "a", b, ... ]` string into its elements. Elements
/// are separated at top level only, so a ',' inside a quoted element or a
/// nested bracket does not split. A quoted element is a string carrying the
/// record format's escapes; an unquoted one takes its natural type via
/// `parse_value`. Returns `None` for text that is not bracketed, and for text
/// that is malformed (an unbalanced quote or bracket, or an unknown escape).
pub(super) fn parse_list_literal(text: &str) -> Option<Vec<Value>> {
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
    let mut elements = crate::engraving::split_top_level(inner, ',').ok()?;
    // A trailing separator is admitted, but does not add an element.
    if let Some(last) = elements.last() {
        if last
            .trim()
            .is_empty()
        {
            elements.pop();
        }
    }
    elements
        .into_iter()
        .map(|element| {
            let element = element.trim();
            match element
                .strip_prefix('"')
                .and_then(|e| e.strip_suffix('"'))
            {
                Some(quoted) => crate::engraving::unescape_literal(quoted)
                    .ok()
                    .map(Value::Literali),
                None => parse_value(element),
            }
        })
        .collect()
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
