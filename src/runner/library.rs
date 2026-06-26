//! The function table for the evaluator.

use std::io::{self, Read};
use std::os::fd::AsFd;
use std::process::{Command, Stdio};

use nix::poll::{poll, PollFd, PollFlags, PollTimeout};

use super::context::{Context, Stream};
use super::runner::RunnerError;
use crate::program::ExecutableId;
use crate::value::{Numeric, Value};

/// A native function: implemented in Rust, taking an execution Context (host
/// capabilities) and the already-evaluated arguments. Pure builtins disregard
/// the Context; effectful functions from the host domain (e.g. `exec`) use
/// it.
pub type Native = fn(&Context, &[Value]) -> Result<Value, RunnerError>;

/// How a builtin is presented to the user. `Pure` computes a value and just
/// runs. `Command` is executed by the host environment; the user vets it on an
/// editable prompt before it runs (`exec`). `Action` is a physical interaction
/// the user performs themselves (`click`, `select`): shown read-only to
/// confirm, never edited.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Nature {
    Pure,
    Command,
    Action,
}

/// A function in the Library's table
pub struct Builtin {
    pub name: &'static str,
    /// The imperative verb an `Action` shows the user, e.g. `Click`. `None` for
    /// `Pure`/`Command` entries, which are never presented this way.
    pub display: Option<&'static str>,
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
                    display: None,
                    arity: 2,
                    nature: Nature::Pure,
                    function: seq,
                },
                Builtin {
                    name: "zip",
                    display: None,
                    arity: 2,
                    nature: Nature::Pure,
                    function: zip,
                },
                Builtin {
                    name: "values",
                    display: None,
                    arity: 1,
                    nature: Nature::Pure,
                    function: values,
                },
                Builtin {
                    name: "labels",
                    display: None,
                    arity: 1,
                    nature: Nature::Pure,
                    function: labels,
                },
                Builtin {
                    name: "pairs",
                    display: None,
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
                display: None,
                arity: 1,
                nature: Nature::Command,
                function: exec,
            },
            Builtin {
                name: "now",
                display: None,
                arity: 0,
                nature: Nature::Pure,
                function: now,
            },
        ]
    }

    /// Interactions with a web browser (or rather, instructions that a user
    /// do these interactions in their web browser).
    pub fn browser() -> Vec<Builtin> {
        [
            ("click", "Click"),
            ("select", "Select"),
            ("deselect", "De-select"),
            ("scroll", "Scroll to"),
            ("navigate", "Navigate to"),
            ("type", "Type"),
            ("task", "Task"),
        ]
        .into_iter()
        .map(|(name, display)| Builtin {
            name,
            display: Some(display),
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

    /// The imperative verb the `Action` at `id` shows the user, if any.
    pub fn display(&self, id: ExecutableId) -> Option<&'static str> {
        self.functions[id.0].display
    }

    /// How the function at `id` is presented: `Pure`, host `Command`, or
    /// physical `Action`.
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

/// Run a shell script, teeing its output to the user chunk by chunk as it
/// streams while accumulating the output as the return value. The child's
/// stdout and stderr are kept on separate pipes so stderr can be shown in
/// red; both are drained together using `poll()` so neither deadlocks. Bytes
/// are decoded at the end, so a chunk split mid-UTF-8 is harmless; trailing
/// newlines are trimmed (matching shell substitution). A non-zero exit is an
/// error.
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
        .stderr(Stdio::piped())
        .spawn()
        .map_err(RunnerError::ExecError)?;

    let mut out = child
        .stdout
        .take()
        .expect("child stdout was piped");
    let mut err = child
        .stderr
        .take()
        .expect("child stderr was piped");

    // The captured return value stays plain; the Context reddens stderr on a
    // colour terminal as it writes. Each stream keeps a `pending` buffer
    // holding partial lines, as bytes are only flushed to terminal at newline
    // to avoid the two stream stomping on each other.
    let mut captured = Vec::new();
    let mut out_pending = Vec::new();
    let mut err_pending = Vec::new();
    let mut out_open = true;
    let mut err_open = true;
    let mut buffer = [0u8; 8192];

    while out_open || err_open {
        // Ask poll() which still-open stream is readable, copying the readiness
        // out into flags so the borrowed fds are released before the readers
        // are borrowed mutably below.
        let (mut out_ready, mut err_ready) = (false, false);
        {
            let mut fds = Vec::with_capacity(2);
            let mut tags = Vec::with_capacity(2);
            if out_open {
                fds.push(PollFd::new(out.as_fd(), PollFlags::POLLIN));
                tags.push(true);
            }
            if err_open {
                fds.push(PollFd::new(err.as_fd(), PollFlags::POLLIN));
                tags.push(false);
            }
            poll(&mut fds, PollTimeout::NONE)
                .map_err(|e| RunnerError::ExecError(io::Error::from(e)))?;
            for (fd, is_out) in fds
                .iter()
                .zip(&tags)
            {
                if !fd
                    .revents()
                    .unwrap_or(PollFlags::empty())
                    .is_empty()
                {
                    if *is_out {
                        out_ready = true;
                    } else {
                        err_ready = true;
                    }
                }
            }
        }

        // A single read after a readiness signal never blocks: it yields the
        // available bytes, or zero once the stream has closed.
        if out_ready {
            let count = out
                .read(&mut buffer)
                .map_err(RunnerError::ExecError)?;
            if count == 0 {
                out_open = false;
            } else {
                tee(
                    &buffer[..count],
                    &mut out_pending,
                    Stream::Stdout,
                    context,
                    &mut captured,
                )?;
            }
        }
        if err_ready {
            let count = err
                .read(&mut buffer)
                .map_err(RunnerError::ExecError)?;
            if count == 0 {
                err_open = false;
            } else {
                tee(
                    &buffer[..count],
                    &mut err_pending,
                    Stream::Stderr,
                    context,
                    &mut captured,
                )?;
            }
        }
    }

    // Flush each stream's final partial content, if any is remaining.
    if !out_pending.is_empty() {
        captured.extend_from_slice(&out_pending);
        context
            .write_run(&out_pending, Stream::Stdout)
            .map_err(RunnerError::ExecError)?;
    }
    if !err_pending.is_empty() {
        captured.extend_from_slice(&err_pending);
        context
            .write_run(&err_pending, Stream::Stderr)
            .map_err(RunnerError::ExecError)?;
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

/// Tee a chunk to the user, recording it plain in `captured` and writing it
/// through the Context. Only whole lines are written through; the trailing
/// partial line waits in `pending` for its newline (or the stream's close), so
/// a `Stream::Stderr` run never lands inside a `Stream::Stdout` line. A newline
/// is always a UTF-8 boundary, so this never splits a character either.
fn tee(
    bytes: &[u8],
    pending: &mut Vec<u8>,
    stream: Stream,
    context: &Context,
    captured: &mut Vec<u8>,
) -> Result<(), RunnerError> {
    pending.extend_from_slice(bytes);
    let ready = match pending
        .iter()
        .rposition(|b| *b == b'\n' || *b == b'\r')
    {
        Some(last) => last + 1,
        None => return Ok(()),
    };
    captured.extend_from_slice(&pending[..ready]);
    context
        .write_run(&pending[..ready], stream)
        .map_err(RunnerError::ExecError)?;
    pending.drain(..ready);
    Ok(())
}

/// `now()` — the current wall-clock time as an ISO 8601 string. A read of
/// external state, hence part of the system layer rather than `core`.
fn now(_context: &Context, _args: &[Value]) -> Result<Value, RunnerError> {
    Ok(Value::Literali(super::runner::now_iso8601()))
}

/// A browser-library action: the user performs the UI manipulation when the
/// runner presents the step, so the call settles to unit.
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
                    display: None,
                    arity: 2,
                    nature: Nature::Pure,
                    function: unit,
                },
                Builtin {
                    name: "zip",
                    display: None,
                    arity: 2,
                    nature: Nature::Pure,
                    function: unit,
                },
                Builtin {
                    name: "exec",
                    display: None,
                    arity: 1,
                    nature: Nature::Command,
                    function: unit,
                },
                Builtin {
                    name: "cmd",
                    display: None,
                    arity: 1,
                    nature: Nature::Command,
                    function: unit,
                },
                Builtin {
                    name: "now",
                    display: None,
                    arity: 0,
                    nature: Nature::Pure,
                    function: unit,
                },
                Builtin {
                    name: "uuid",
                    display: None,
                    arity: 0,
                    nature: Nature::Pure,
                    function: unit,
                },
                Builtin {
                    name: "timer",
                    display: None,
                    arity: 1,
                    nature: Nature::Pure,
                    function: unit,
                },
                Builtin {
                    name: "journal",
                    display: None,
                    arity: 1,
                    nature: Nature::Pure,
                    function: unit,
                },
                Builtin {
                    name: "click",
                    display: Some("Click"),
                    arity: 1,
                    nature: Nature::Action,
                    function: unit,
                },
                Builtin {
                    name: "navigate",
                    display: Some("Navigate"),
                    arity: 1,
                    nature: Nature::Action,
                    function: unit,
                },
                Builtin {
                    name: "select",
                    display: Some("Select"),
                    arity: 1,
                    nature: Nature::Action,
                    function: unit,
                },
                Builtin {
                    name: "deselect",
                    display: Some("Deselect"),
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
