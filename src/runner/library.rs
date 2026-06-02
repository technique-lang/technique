//! The function table for the evaluator.

use super::runner::RunnerError;
use crate::program::ExecutableId;
use crate::value::Value;

/// A native function: implemented in Rust, taking already-evaluated
/// arguments.
pub type Native = fn(&[Value]) -> Result<Value, RunnerError>;

/// A function in the Library's table
struct Entry {
    name: &'static str,
    arity: usize,
    pointer: Native,
}

/// The set of functions available to a program, indexed by `ExecutableId`. A
/// Library is built by combining core builtins then adding whatever the given
/// domain contributes. It is used by the linking phase to perform lookups of
/// function pointers, then ownership is passed to the runner.
pub struct Library {
    functions: Vec<Entry>,
}

impl Library {
    /// The domain-independent builtins present under every domain. Populated
    /// with the pure functions that coerce our value types; effectful
    /// functions (functions supplied by the host environment) are declared
    /// and implemented by the relevant domain the Technique is executing in.
    pub fn core() -> Self {
        Library {
            functions: Vec::new(),
        }
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

    /// Call the function at `id` with (already evaluated) arguments.
    pub fn call(&self, id: ExecutableId, args: &[Value]) -> Result<Value, RunnerError> {
        (self.functions[id.0].pointer)(args)
    }
}

#[cfg(test)]
impl Library {
    pub fn stub() -> Self {
        fn unit(_: &[Value]) -> Result<Value, RunnerError> {
            Ok(Value::Unitus)
        }
        let entry = |name, arity| Entry {
            name,
            arity,
            pointer: unit as Native,
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
