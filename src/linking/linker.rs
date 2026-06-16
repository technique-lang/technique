use crate::language::{self, Span};
use crate::program::{ExecutableRef, Fragment, Operation, Program};
use crate::runner::Library;

#[derive(Debug)]
pub enum LinkingError<'i> {
    /// A function called with a number of arguments that doesn't match its
    /// declared arity in the function table.
    ArityMismatch {
        function: language::Identifier<'i>,
        expected: usize,
        actual: usize,
    },
    /// A function call naming nothing in the function table — neither a core
    /// nor system builtin nor a function the selected domain provides.
    UnresolvedFunction { function: language::Identifier<'i> },
}

impl<'i> LinkingError<'i> {
    pub fn span(&self) -> Span {
        match self {
            LinkingError::ArityMismatch { function, .. } => function.span,
            LinkingError::UnresolvedFunction { function } => function.span,
        }
    }
}

/// Resolve every `Execute` target in the program against `library`. A target
/// naming a table entry becomes `Resolved` once its argument count matches the
/// entry's arity; a target naming nothing stays `Unresolved`. Returns the
/// collected arity errors, if any.
pub fn link<'i>(program: &mut Program<'i>, library: &Library) -> Result<(), Vec<LinkingError<'i>>> {
    let mut problems = Vec::new();
    for subroutine in &mut program.subroutines {
        link_operation(&mut subroutine.body, library, &mut problems);
    }
    if problems.is_empty() {
        Ok(())
    } else {
        Err(problems)
    }
}

// Walks the same Operation arms as the translator's `resolve_operation`;
// executable content is hoisted into `body` during translation, so only
// bodies need walking.
fn link_operation<'i>(
    op: &mut Operation<'i>,
    library: &Library,
    problems: &mut Vec<LinkingError<'i>>,
) {
    match op {
        Operation::Execute(executable) => {
            if let ExecutableRef::Unresolved(id) = &executable.target {
                match library.resolve(id.value) {
                    Some(exec_id) => {
                        let expected = library.arity(exec_id);
                        let actual = executable
                            .arguments
                            .len();
                        if actual == expected {
                            executable.target = ExecutableRef::Resolved(exec_id);
                        } else {
                            problems.push(LinkingError::ArityMismatch {
                                function: *id,
                                expected,
                                actual,
                            });
                        }
                    }
                    None => problems.push(LinkingError::UnresolvedFunction { function: *id }),
                }
            }
            for arg in &mut executable.arguments {
                link_operation(arg, library, problems);
            }
        }
        Operation::Invoke(invocable) => {
            for arg in &mut invocable.arguments {
                link_operation(arg, library, problems);
            }
        }
        Operation::Sequence(ops) => {
            for op in ops {
                link_operation(op, library, problems);
            }
        }
        Operation::Section { body, .. } => link_operation(body, library, problems),
        Operation::Step { body, .. } => link_operation(body, library, problems),
        Operation::Loop { over, body, .. } => {
            if let Some(over) = over {
                link_operation(over, library, problems);
            }
            link_operation(body, library, problems);
        }
        Operation::Bind { value, .. } => link_operation(value, library, problems),
        Operation::String(fragments) => {
            for fragment in fragments {
                if let Fragment::Interpolation(op) = fragment {
                    link_operation(op, library, problems);
                }
            }
        }
        Operation::Tablet(entries) => {
            for entry in entries {
                link_operation(&mut entry.value, library, problems);
            }
        }
        Operation::List(items) => {
            for item in items {
                link_operation(item, library, problems);
            }
        }
        Operation::Variable(_)
        | Operation::Number(_)
        | Operation::Multiline(_, _)
        | Operation::Hole => {}
    }
}

#[cfg(test)]
#[path = "checks/linker.rs"]
mod check;
