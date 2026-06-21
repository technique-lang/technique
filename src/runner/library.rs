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

/// Whether a builtin is a pure function that computes a value or whether it
/// is an action which performs a side-effect in the real world.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Nature {
    Pure,
    Action,
}

/// A function in the Library's table
pub struct Builtin {
    pub name: &'static str,
    pub arity: usize,
    pub nature: Nature,
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
                    nature: Nature::Pure,
                    function: seq,
                },
                Builtin {
                    name: "zip",
                    arity: 2,
                    nature: Nature::Pure,
                    function: zip,
                },
                Builtin {
                    name: "values",
                    arity: 1,
                    nature: Nature::Pure,
                    function: values,
                },
                Builtin {
                    name: "labels",
                    arity: 1,
                    nature: Nature::Pure,
                    function: labels,
                },
                Builtin {
                    name: "pairs",
                    arity: 1,
                    nature: Nature::Pure,
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
                nature: Nature::Action,
                function: exec,
            },
            Builtin {
                name: "now",
                arity: 0,
                nature: Nature::Pure,
                function: now,
            },
        ]
    }

    /// Interactions with a web browser (or rather, instructions that a user
    /// do these interactions in their web browser).
    pub fn browser() -> Vec<Builtin> {
        ["click", "select", "deselect", "scroll", "type", "task"]
            .into_iter()
            .map(|name| Builtin {
                name,
                arity: 1,
                nature: Nature::Action,
                function: interact,
            })
            .collect()
    }

    /// Add functions to the table, after the core builtins — the system layer
    /// or an injected library's host functions.
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

    /// Whether the function at `id` is a commanded `Action` or a `Pure`
    /// computation.
    pub fn nature(&self, id: ExecutableId) -> Nature {
        self.functions[id.0].nature
    }

    /// Call the function at `id` with the execution context and (already
    /// evaluated) arguments.
    pub fn call(
        &self,
        id: ExecutableId,
        context: &Context,
        args: &[Value],
    ) -> Result<Value, RunnerError> {
        let builtin = &self.functions[id.0];
        if args.len() != builtin.arity {
            return Err(RunnerError::FunctionArityMismatch {
                function: builtin.name,
                expected: builtin.arity,
                actual: args.len(),
            });
        }
        (builtin.function)(context, args)
    }
}

/// Resolve a named library to the host functions it contributes, or `None`
/// if the name matches no known library. The orthogonal counterpart to a
/// document's domain: a library is selected with `--library` regardless of
/// which domain the Technique declares.
pub fn library_for(name: &str) -> Option<Vec<Builtin>> {
    match name {
        "system" => Some(Library::system()),
        "browser" => Some(Library::browser()),
        _ => None,
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

/// `exec(script)` — run a shell script, teeing its stdout through the Context
/// to the operator as it streams while accumulating it as the return value.
/// Output is held as bytes until the end so a chunk split mid-UTF-8 is
/// harmless and only one String is allocated. Trailing newlines are trimmed
/// from the captured value (matching the usual experience when doing shell
/// `$(...)` substitution). A non-zero exit is an error.
fn exec(context: &Context, args: &[Value]) -> Result<Value, RunnerError> {
    let script = match &args[0] {
        Value::Literali(script) => script,
        _ => {
            return Err(RunnerError::InvalidArgument {
                function: "exec",
                expected: "a shell script string",
            })
        }
    };

    let mut child = Command::new("bash")
        .arg("-c")
        .arg(script)
        .stdout(Stdio::piped())
        .spawn()
        .map_err(RunnerError::ExecError)?;

    let mut stdout = child
        .stdout
        .take()
        .expect("child stdout was piped");
    let mut captured = Vec::new();
    let mut buffer = [0u8; 8192];
    loop {
        let count = stdout
            .read(&mut buffer)
            .map_err(RunnerError::ExecError)?;
        if count == 0 {
            break;
        }
        context
            .write(&buffer[..count])
            .map_err(RunnerError::ExecError)?;
        captured.extend_from_slice(&buffer[..count]);
    }

    let status = child
        .wait()
        .map_err(RunnerError::ExecError)?;
    if !status.success() {
        return Err(RunnerError::CommandFailed(
            status
                .code()
                .unwrap_or(-1),
        ));
    }

    let output = String::from_utf8_lossy(&captured);
    Ok(Value::Literali(
        output
            .trim_end_matches(['\n', '\r'])
            .to_string(),
    ))
}

/// `now()` — the current wall-clock time as an ISO 8601 string. A read of
/// external state, hence part of the system layer rather than `core`.
fn now(_context: &Context, _args: &[Value]) -> Result<Value, RunnerError> {
    Ok(Value::Literali(super::runner::now_iso8601()))
}

/// A browser-library action: the operator performs the UI manipulation when
/// the runner commands the step, so the call settles to unit.
fn interact(_context: &Context, _args: &[Value]) -> Result<Value, RunnerError> {
    Ok(Value::Unitus)
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
        Library {
            functions: vec![
                Builtin {
                    name: "seq",
                    arity: 2,
                    nature: Nature::Pure,
                    function: unit,
                },
                Builtin {
                    name: "zip",
                    arity: 2,
                    nature: Nature::Pure,
                    function: unit,
                },
                Builtin {
                    name: "exec",
                    arity: 1,
                    nature: Nature::Action,
                    function: unit,
                },
                Builtin {
                    name: "cmd",
                    arity: 1,
                    nature: Nature::Action,
                    function: unit,
                },
                Builtin {
                    name: "now",
                    arity: 0,
                    nature: Nature::Pure,
                    function: unit,
                },
                Builtin {
                    name: "uuid",
                    arity: 0,
                    nature: Nature::Pure,
                    function: unit,
                },
                Builtin {
                    name: "timer",
                    arity: 1,
                    nature: Nature::Pure,
                    function: unit,
                },
                Builtin {
                    name: "journal",
                    arity: 1,
                    nature: Nature::Pure,
                    function: unit,
                },
                Builtin {
                    name: "click",
                    arity: 1,
                    nature: Nature::Action,
                    function: unit,
                },
                Builtin {
                    name: "navigate",
                    arity: 1,
                    nature: Nature::Action,
                    function: unit,
                },
                Builtin {
                    name: "select",
                    arity: 1,
                    nature: Nature::Action,
                    function: unit,
                },
                Builtin {
                    name: "deselect",
                    arity: 1,
                    nature: Nature::Action,
                    function: unit,
                },
            ],
        }
    }
}

#[cfg(test)]
#[path = "checks/library.rs"]
mod check;
