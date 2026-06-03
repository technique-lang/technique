//! Variable bindings and the evaluator that turns value-bearing
//! Operations into Values for description rendering and binding.

use std::collections::HashMap;

use super::library::Library;
use super::runner::RunnerError;
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
        Operation::String(fragments) => {
            let mut text = String::new();
            for fragment in fragments {
                match fragment {
                    Fragment::Text(t) => text.push_str(t),
                    Fragment::Interpolation(inner) => match evaluate(library, env, inner)? {
                        Value::Literali(s) => text.push_str(&s),
                        other => text.push_str(&other.to_string()),
                    },
                }
            }
            Ok(Value::Literali(text))
        }
        Operation::Multiline(_, lines) => Ok(Value::Literali(lines.join("\n"))),
        Operation::Tablet(entries) => {
            let mut pairs = Vec::with_capacity(entries.len());
            for entry in entries {
                let v = evaluate(library, env, &entry.value)?;
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
                values.push(evaluate(library, env,item)?);
            }
            Ok(Value::Arraeum(values))
        }
        Operation::Bind { names, value } => {
            let v = evaluate(library, env,value)?;
            bind_names(env, names, v)?;
            Ok(Value::Unitus)
        }
        Operation::Sequence(ops) => {
            let mut last = Value::Unitus;
            for child in ops {
                last = evaluate(library, env,child)?;
            }
            Ok(last)
        }
        Operation::Execute(executable) => match &executable.target {
            ExecutableRef::Resolved(id) => {
                let mut args = Vec::with_capacity(
                    executable
                        .arguments
                        .len(),
                );
                for arg in &executable.arguments {
                    args.push(evaluate(library, env,arg)?);
                }
                library.call(*id, &args)
            }
            ExecutableRef::Unresolved(target) => Err(RunnerError::UnresolvedFunction(
                target
                    .value
                    .to_string(),
            )),
        },
        Operation::Section { .. }
        | Operation::Step { .. }
        | Operation::Loop { .. }
        | Operation::Invoke(_) => Ok(Value::Unitus),
    }
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

#[cfg(test)]
#[path = "checks/evaluator.rs"]
mod check;
