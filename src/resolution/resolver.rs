use std::collections::{HashMap, HashSet};

use crate::language::{self, Span};
use crate::program::{Fragment, Operation, Program, Subroutine, SubroutineId, SubroutineRef};

#[derive(Debug, Eq, PartialEq)]
pub enum ResolutionError<'i> {
    /// A local procedure invocation of the form `<name>` or `<name>(...)`
    /// whose name doesn't match any procedure declared in this document.
    UnresolvedProcedure(language::Identifier<'i>),
    /// A procedure invocation `<name>(...)` whose argument count doesn't match
    /// the arity of the procedure it resolves to.
    ProcedureArityMismatch {
        procedure: language::Identifier<'i>,
        expected: usize,
        actual: usize,
    },
    /// A read that doesn't match any variable in scope (neither a parameter
    /// of the procedure nor a name bound by a binding or loop within it).
    UnboundVariable { variable: language::Identifier<'i> },
}

impl<'i> ResolutionError<'i> {
    pub fn span(&self) -> Span {
        match self {
            ResolutionError::UnresolvedProcedure(id) => id.span,
            ResolutionError::ProcedureArityMismatch { procedure, .. } => procedure.span,
            ResolutionError::UnboundVariable { variable } => variable.span,
        }
    }
}

/// Resolve a program's internal references. Every `Invoke` target bound to a
/// declared procedure (with its argument count checked), and every `Variable`
/// read bound in scope. This only checks references within the Program;
/// references to functions provided by the host are resolved later in the
/// linking phase.
///
/// Mutates `Invoke` targets from `Unresolved` to `Resolved`.
pub fn resolve<'i>(program: &mut Program<'i>) -> Result<(), Vec<ResolutionError<'i>>> {
    let known: HashMap<&'i str, SubroutineId> = program
        .subroutines
        .iter()
        .enumerate()
        .filter_map(|(index, subroutine)| {
            subroutine
                .name
                .as_ref()
                .map(|name| (name.value, SubroutineId(index)))
        })
        .collect();
    let arities: Vec<usize> = program
        .subroutines
        .iter()
        .map(Subroutine::arity)
        .collect();

    let mut problems = Vec::new();
    for subroutine in &mut program.subroutines {
        resolve_operation(&mut subroutine.body, &known, &arities, &mut problems);
        infer_iterated(&mut subroutine.body);
    }
    for subroutine in &program.subroutines {
        check_bindings(subroutine, &mut problems);
    }
    if problems.is_empty() {
        Ok(())
    } else {
        Err(problems)
    }
}

fn resolve_operation<'i>(
    op: &mut Operation<'i>,
    known: &HashMap<&'i str, SubroutineId>,
    arities: &[usize],
    problems: &mut Vec<ResolutionError<'i>>,
) {
    match op {
        Operation::Invoke(invocable) => {
            if let SubroutineRef::Unresolved(id) = &invocable.target {
                match known.get(id.value) {
                    Some(sub_id) => {
                        // A bare call (`<thing>` with no parenthesis) defers
                        // all arguments with implicit Holes, so it is exempt,
                        // but a parenthesised list must match the target
                        // procedure's arity exactly.
                        let expected = arities[sub_id.0];
                        let actual = invocable
                            .arguments
                            .len();
                        if !invocable.elided && expected != actual {
                            problems.push(ResolutionError::ProcedureArityMismatch {
                                procedure: *id,
                                expected,
                                actual,
                            });
                        }
                        invocable.target = SubroutineRef::Resolved(*sub_id);
                    }
                    None => problems.push(ResolutionError::UnresolvedProcedure(*id)),
                }
            }
            for arg in &mut invocable.arguments {
                resolve_operation(arg, known, arities, problems);
            }
        }
        Operation::Sequence(ops) | Operation::List(ops) | Operation::Prologue(ops) => {
            for op in ops {
                resolve_operation(op, known, arities, problems);
            }
        }
        Operation::Section { title, body, .. } => {
            if let Some(title) = title {
                resolve_operation(title, known, arities, problems);
            }
            resolve_operation(body, known, arities, problems);
        }
        Operation::Step { body, .. } => resolve_operation(body, known, arities, problems),
        Operation::Loop { over, body, .. } => {
            if let Some(over) = over {
                resolve_operation(over, known, arities, problems);
            }
            resolve_operation(body, known, arities, problems);
        }
        Operation::Bind { value, .. } => resolve_operation(value, known, arities, problems),
        Operation::Execute(executable) => {
            for arg in &mut executable.arguments {
                resolve_operation(arg, known, arities, problems);
            }
        }
        Operation::String(fragments) => {
            for fragment in fragments {
                if let Fragment::Interpolation(op) = fragment {
                    resolve_operation(op, known, arities, problems);
                }
            }
        }
        Operation::Tablet(entries) => {
            for entry in entries {
                resolve_operation(&mut entry.value, known, arities, problems);
            }
        }
        Operation::Variable(_)
        | Operation::Number(_)
        | Operation::Multiline(_, _)
        | Operation::Prose(_)
        | Operation::Hole => {}
    }
}

// Annotate each single-name `Bind` whose name is later iterated by a `foreach`
// with an inferred `[*]` genus, so the runner can offer list entry when
// prompting for it. Two passes: gather the names used as bare loop collections,
// then mark the matching binds. The gathered set is scope-insensitive — a name
// reused across procedures would mark every same-named single bind — so the
// analysis is confined to one subroutine body, where names are normally unique.
fn infer_iterated(body: &mut Operation) {
    let mut iterated = HashSet::new();
    gather_iterated(body, &mut iterated);
    mark_iterated(body, &iterated);
}

fn gather_iterated<'i>(op: &Operation<'i>, iterated: &mut HashSet<&'i str>) {
    match op {
        Operation::Loop { over, body, .. } => {
            if let Some(over) = over {
                if let Operation::Variable(id) = over.as_ref() {
                    iterated.insert(id.value);
                }
                gather_iterated(over, iterated);
            }
            gather_iterated(body, iterated);
        }
        Operation::Bind { value, .. } => gather_iterated(value, iterated),
        Operation::Sequence(ops) | Operation::List(ops) | Operation::Prologue(ops) => {
            for op in ops {
                gather_iterated(op, iterated);
            }
        }
        Operation::Section { title, body, .. } => {
            if let Some(title) = title {
                gather_iterated(title, iterated);
            }
            gather_iterated(body, iterated);
        }
        Operation::Step { body, .. } => gather_iterated(body, iterated),
        Operation::Invoke(invocable) => {
            for arg in &invocable.arguments {
                gather_iterated(arg, iterated);
            }
        }
        Operation::Execute(executable) => {
            for arg in &executable.arguments {
                gather_iterated(arg, iterated);
            }
        }
        Operation::String(fragments) => {
            for fragment in fragments {
                if let Fragment::Interpolation(op) = fragment {
                    gather_iterated(op, iterated);
                }
            }
        }
        Operation::Tablet(entries) => {
            for entry in entries {
                gather_iterated(&entry.value, iterated);
            }
        }
        Operation::Variable(_)
        | Operation::Number(_)
        | Operation::Multiline(_, _)
        | Operation::Prose(_)
        | Operation::Hole => {}
    }
}

fn mark_iterated<'i>(op: &mut Operation<'i>, iterated: &HashSet<&str>) {
    match op {
        Operation::Bind {
            names,
            value,
            inferred,
        } => {
            if names.len() == 1 && iterated.contains(names[0].value) {
                *inferred = Some(language::Genus::List(language::Forma::new("*")));
            }
            mark_iterated(value, iterated);
        }
        Operation::Loop { over, body, .. } => {
            if let Some(over) = over {
                mark_iterated(over, iterated);
            }
            mark_iterated(body, iterated);
        }
        Operation::Sequence(ops) | Operation::List(ops) | Operation::Prologue(ops) => {
            for op in ops {
                mark_iterated(op, iterated);
            }
        }
        Operation::Section { title, body, .. } => {
            if let Some(title) = title {
                mark_iterated(title, iterated);
            }
            mark_iterated(body, iterated);
        }
        Operation::Step { body, .. } => mark_iterated(body, iterated),
        Operation::Invoke(invocable) => {
            for arg in &mut invocable.arguments {
                mark_iterated(arg, iterated);
            }
        }
        Operation::Execute(executable) => {
            for arg in &mut executable.arguments {
                mark_iterated(arg, iterated);
            }
        }
        Operation::String(fragments) => {
            for fragment in fragments {
                if let Fragment::Interpolation(op) = fragment {
                    mark_iterated(op, iterated);
                }
            }
        }
        Operation::Tablet(entries) => {
            for entry in entries {
                mark_iterated(&mut entry.value, iterated);
            }
        }
        Operation::Variable(_)
        | Operation::Number(_)
        | Operation::Multiline(_, _)
        | Operation::Prose(_)
        | Operation::Hole => {}
    }
}

// Flag any Variable read that names nothing yet in scope. Scope starts with
// the subroutine's parameters and grows as the walk passes each `Bind`/`Loop`
// in execution order — so a read is bound only if its name was bound by an
// earlier operation, catching use-before-binding. The runner gives a
// procedure one Environment for its whole body, so a name bound in one step
// stays visible to every later step; scope therefore accumulates and is never
// narrowed on leaving a step, substep, or section.
fn check_bindings<'i>(subroutine: &Subroutine<'i>, problems: &mut Vec<ResolutionError<'i>>) {
    let mut scope = HashSet::new();
    if let Some(parameters) = subroutine.parameters {
        for parameter in parameters {
            scope.insert(parameter.value);
        }
    }
    check_scope(&subroutine.body, &mut scope, problems);
}

fn check_scope<'i>(
    op: &Operation<'i>,
    scope: &mut HashSet<&'i str>,
    problems: &mut Vec<ResolutionError<'i>>,
) {
    match op {
        Operation::Variable(id) => {
            if !scope.contains(id.value) {
                problems.push(ResolutionError::UnboundVariable { variable: *id });
            }
        }
        // A binding's value is evaluated before its name is bound, so the
        // value is checked against the scope as it stands; only then do the
        // names come into scope for what follows.
        Operation::Bind { names, value, .. } => {
            check_scope(value, scope, problems);
            for name in *names {
                scope.insert(name.value);
            }
        }
        // The collection is evaluated first; the loop names then come into
        // scope for the body (and stay, as the runner's binding does).
        Operation::Loop {
            names, over, body, ..
        } => {
            if let Some(over) = over {
                check_scope(over, scope, problems);
            }
            for name in *names {
                scope.insert(name.value);
            }
            check_scope(body, scope, problems);
        }
        Operation::Sequence(ops) | Operation::List(ops) | Operation::Prologue(ops) => {
            for op in ops {
                check_scope(op, scope, problems);
            }
        }
        Operation::Section { title, body, .. } => {
            if let Some(title) = title {
                check_scope(title, scope, problems);
            }
            check_scope(body, scope, problems);
        }
        Operation::Step { body, .. } => check_scope(body, scope, problems),
        Operation::Invoke(invocable) => {
            for arg in &invocable.arguments {
                check_scope(arg, scope, problems);
            }
        }
        Operation::Execute(executable) => {
            for arg in &executable.arguments {
                check_scope(arg, scope, problems);
            }
        }
        Operation::String(fragments) => {
            for fragment in fragments {
                if let Fragment::Interpolation(op) = fragment {
                    check_scope(op, scope, problems);
                }
            }
        }
        Operation::Tablet(entries) => {
            for entry in entries {
                check_scope(&entry.value, scope, problems);
            }
        }
        Operation::Number(_)
        | Operation::Multiline(_, _)
        | Operation::Prose(_)
        | Operation::Hole => {}
    }
}

#[cfg(test)]
#[path = "checks/resolver.rs"]
mod check;
