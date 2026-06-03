//! The function table for the evaluator.

use std::io::Read;
use std::process::{Command, Stdio};

use super::context::Context;
use super::runner::RunnerError;
use crate::program::ExecutableId;
use crate::value::{Numeric, Value};

/// A native function: implemented in Rust, taking an execution Context (host
/// capabilities) and the already-evaluated arguments. Pure builtins disregard
/// the Context; effectful functions from the host domain (e.g. `exec`) use
/// it.
pub type Native = fn(&Context, &[Value]) -> Result<Value, RunnerError>;

/// A function in the Library's table
pub struct Builtin {
    pub name: &'static str,
    pub arity: usize,
    pub function: Native,
}

/// The set of functions available to a program, indexed by `ExecutableId`. A
/// Library is built by combining core builtins then adding whatever the given
/// domain contributes. It is used by the linking phase to perform lookups of
/// function pointers, then ownership is passed to the runner.
pub struct Library {
    functions: Vec<Builtin>,
}

impl Library {
    /// The domain-independent builtins present under every domain. Populated
    /// with the pure functions that coerce our value types; effectful
    /// functions (functions supplied by the host environment) are declared
    /// and implemented by the relevant domain the Technique is executing in.
    pub fn core() -> Self {
        Library {
            functions: vec![
                Builtin {
                    name: "seq",
                    arity: 2,
                    function: seq,
                },
                Builtin {
                    name: "zip",
                    arity: 2,
                    function: zip,
                },
                Builtin {
                    name: "values",
                    arity: 1,
                    function: values,
                },
                Builtin {
                    name: "labels",
                    arity: 1,
                    function: labels,
                },
                Builtin {
                    name: "pairs",
                    arity: 1,
                    function: pairs,
                },
            ],
        }
    }

    /// The system layer: effectful, world-touching functions (process
    /// execution and the clock). Kept out of `core` so a pure, isolated,
    /// deterministic Technique can be run without them; an interactive run
    /// adds this layer on top of `core`.
    pub fn system() -> Vec<Builtin> {
        vec![
            Builtin {
                name: "exec",
                arity: 1,
                function: exec,
            },
            Builtin {
                name: "now",
                arity: 0,
                function: now,
            },
        ]
    }

    /// Add functions to the table, after the core builtins — the system layer
    /// or a domain's own host functions.
    pub fn extend(&mut self, builtins: impl IntoIterator<Item = Builtin>) {
        self.functions
            .extend(builtins);
    }

    /// Resolve a function name to its index, or `None` if no entry matches.
    pub fn resolve(&self, name: &str) -> Option<ExecutableId> {
        self.functions
            .iter()
            .position(|entry| entry.name == name)
            .map(ExecutableId)
    }

    /// The declared arity of the function at `id`.
    pub fn arity(&self, id: ExecutableId) -> usize {
        self.functions[id.0].arity
    }

    /// The name of the function at `id`.
    pub fn name(&self, id: ExecutableId) -> &'static str {
        self.functions[id.0].name
    }

    /// Call the function at `id` with the execution context and (already
    /// evaluated) arguments.
    pub fn call(
        &self,
        id: ExecutableId,
        context: &Context,
        args: &[Value],
    ) -> Result<Value, RunnerError> {
        (self.functions[id.0].function)(context, args)
    }
}

/// `seq(a, b)` — the inclusive integer range from `a` to `b` as a list,
/// empty when `a > b`.
fn seq(_context: &Context, args: &[Value]) -> Result<Value, RunnerError> {
    let a = as_integer("seq", &args[0])?;
    let b = as_integer("seq", &args[1])?;
    let range = (a..=b)
        .map(|n| Value::Quanticle(Numeric::Integral(n)))
        .collect();
    Ok(Value::Arraeum(range))
}

/// `zip(xs, ys)` — a list of `(x, y)` pairs, one per position, truncated to
/// the shorter input.
fn zip(_context: &Context, args: &[Value]) -> Result<Value, RunnerError> {
    let xs = as_list("zip", &args[0])?;
    let ys = as_list("zip", &args[1])?;
    let pairs = xs
        .iter()
        .zip(ys.iter())
        .map(|(x, y)| Value::Parametriq(vec![x.clone(), y.clone()]))
        .collect();
    Ok(Value::Arraeum(pairs))
}

/// `values(form)` — the values of a tablet's entries, in order, as a list.
fn values(_context: &Context, args: &[Value]) -> Result<Value, RunnerError> {
    let entries = as_tablet("values", &args[0])?;
    let values = entries
        .iter()
        .map(|(_, value)| value.clone())
        .collect();
    Ok(Value::Arraeum(values))
}

/// `labels(form)` — the labels of a tablet's entries, in order, as a list of
/// text values.
fn labels(_context: &Context, args: &[Value]) -> Result<Value, RunnerError> {
    let entries = as_tablet("labels", &args[0])?;
    let labels = entries
        .iter()
        .map(|(label, _)| Value::Literali(label.clone()))
        .collect();
    Ok(Value::Arraeum(labels))
}

/// `pairs(form)` — a tablet's entries as a list of `(label, value)` pairs,
/// so `foreach (k, v) in pairs(form)` destructures through the usual rule.
fn pairs(_context: &Context, args: &[Value]) -> Result<Value, RunnerError> {
    let entries = as_tablet("pairs", &args[0])?;
    let pairs = entries
        .iter()
        .map(|(label, value)| {
            Value::Parametriq(vec![Value::Literali(label.clone()), value.clone()])
        })
        .collect();
    Ok(Value::Arraeum(pairs))
}

fn as_integer(function: &'static str, value: &Value) -> Result<i64, RunnerError> {
    if let Value::Quanticle(Numeric::Integral(n)) = value {
        Ok(*n)
    } else {
        Err(RunnerError::InvalidArgument {
            function,
            expected: "an integer",
        })
    }
}

fn as_list<'a>(function: &'static str, value: &'a Value) -> Result<&'a [Value], RunnerError> {
    if let Value::Arraeum(items) = value {
        Ok(items)
    } else {
        Err(RunnerError::InvalidArgument {
            function,
            expected: "a list",
        })
    }
}

fn as_tablet<'a>(
    function: &'static str,
    value: &'a Value,
) -> Result<&'a [(String, Value)], RunnerError> {
    if let Value::Tabularum(entries) = value {
        Ok(entries)
    } else {
        Err(RunnerError::InvalidArgument {
            function,
            expected: "a tablet",
        })
    }
}

#[cfg(test)]
impl Library {
    pub fn stub() -> Self {
        fn unit(_: &Context, _: &[Value]) -> Result<Value, RunnerError> {
            Ok(Value::Unitus)
        }
        let entry = |name, arity| Builtin {
            name,
            arity,
            function: unit as Native,
        };
        Library {
            functions: vec![
                entry("seq", 2),
                entry("zip", 2),
                entry("exec", 1),
                entry("cmd", 1),
                entry("now", 0),
                entry("uuid", 0),
                entry("timer", 1),
                entry("journal", 1),
                entry("click", 1),
                entry("navigate", 1),
                entry("select", 1),
                entry("deselect", 1),
            ],
        }
    }
}

#[cfg(test)]
#[path = "checks/library.rs"]
mod check;
