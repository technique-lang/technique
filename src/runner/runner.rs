//! Interactive walker over a translated Program.

use std::collections::HashSet;
use std::io;
use std::path::PathBuf;

use super::context::Context;
use super::driver::{Driver, UserInput};
use super::evaluator::Environment;
use super::library::{Library, Nature};
use super::path::{PathSegment, QualifiedPath};
use super::state::{
    Appender, InvokeTarget, Record, RecordError, RunId, State, Value as RecordValue,
};
use crate::language;
use crate::program::{
    Executable, ExecutableRef, Invocable, Operation, Ordinal, Program, SubroutineRef,
};
use crate::value::Value;

/// What executing an Operation (or evaluating a Step at any scale) produced.
/// `Done(Value)` is the natural success — for a leaf Step the operator's
/// recorded value, for a Sequence / Section / procedure body the unit value
/// once the whole subtree is finished. `Skipped` and `Failed` are operator
/// verdicts on individual Steps. `Stopped` is a control signal that
/// propagates immediately up the call stack to halt the walk; the deliberate
/// quit was already recorded as a `State::Stop` lifecycle event, so this
/// carries no payload — a `technique resume` picks up from the first step
/// with no recorded outcome.
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Outcome {
    Done(Value),
    Skipped,
    Failed(Failure),
    Stopped,
}

/// Why a Step failed.
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Failure {
    Aborted(String),
}

/// Anything that can go wrong while preparing or running a Technique.
/// Variants are populated as the implementing steps land; the formatter
/// in `crate::problem` knows how to render each one.
#[allow(dead_code)]
#[derive(Debug)]
pub enum RunnerError {
    NoSuchRun(RunId),
    StoreError {
        path: PathBuf,
        error: io::Error,
    },
    MalformedRecord {
        run_id: RunId,
        error: RecordError,
    },
    StartMissing(RunId),
    InvalidRunId(String),
    MissingEntryProcedure,
    UnboundVariable(String),
    BindArityMismatch {
        expected: usize,
        actual: usize,
    },
    BindNotTuple {
        expected: usize,
    },
    NotIterable,
    InvalidArgument {
        function: &'static str,
        expected: &'static str,
    },
    UnknownFunction(String),
    ExecError(io::Error),
    CommandFailed(i32),
    IncompatibleCombination {
        left: &'static str,
        right: &'static str,
    },
    ParameterArityMismatch {
        expected: usize,
        actual: usize,
    },
    ParameterUnexpected {
        actual: usize,
    },
    TerminalRequired,
    UserQuit,
}

/// Execute a Technique interactively by walking the `Program` tree. Tracks
/// the position in the document via a `QualifiedPath` stack, carries an
/// `Environment` with known result values. Maintains a set of
/// already-completed step FQNs, an append handle to write results, and the
/// prompt the operator interacts through.
#[allow(dead_code)]
pub struct Runner<'i, D: Driver> {
    program: &'i Program<'i>,
    appender: Appender,
    completed: HashSet<String>,
    driver: D,
    path: QualifiedPath<'i>,
    library: Library,
    context: Context,
}

impl<'i, D: Driver> Runner<'i, D> {
    pub fn new(
        program: &'i Program<'i>,
        appender: Appender,
        completed: HashSet<String>,
        driver: D,
        library: Library,
    ) -> Self {
        Runner {
            program,
            appender,
            completed,
            driver,
            path: QualifiedPath::new(),
            library,
            context: Context::native(),
        }
    }

    /// Consume the runner and return the inner driver. Tests use this
    /// to assert on the Mock's event log after a run completes.
    #[cfg(test)]
    pub fn into_driver(self) -> D {
        self.driver
    }

    /// Walk the entry procedure top to bottom. Entry-procedure
    /// selection here is `program.subroutines[0]` — the synthetic
    /// anonymous wrapper if the document is top-level Steps, otherwise
    /// the first declared procedure.
    pub fn run(&mut self, mut env: Environment) -> Result<Outcome, RunnerError> {
        let entry = self
            .program
            .subroutines
            .first()
            .ok_or(RunnerError::MissingEntryProcedure)?;
        let name = entry
            .name
            .as_ref()
            .map(|n| n.value);
        if let Some(name) = name {
            self.path
                .push(PathSegment::Procedure(name));
        }
        let result = self.walk(&mut env, &entry.body);
        // A named entry procedure is a structural scope: a completed run closes
        // with a final sign-off prompt at its path. A Quit or error walk skips
        // it — the run did not finish. An anonymous entry (a bare series of
        // steps) has no procedure to accept, so it just ends.
        let result = if name.is_some() {
            let qualified = self
                .path
                .render();
            let sealed = match result {
                Ok(Outcome::Stopped) => Ok(Outcome::Stopped),
                Ok(outcome) => self.seal_scope(&qualified, outcome),
                Err(error) => Err(error),
            };
            self.path
                .pop();
            sealed
        } else {
            result
        };
        result
    }

    fn walk(
        &mut self,
        env: &mut Environment,
        op: &'i Operation<'i>,
    ) -> Result<Outcome, RunnerError> {
        match op {
            Operation::Sequence(ops) => self.walk_sequence(env, ops),
            Operation::Section {
                numeral,
                title,
                body,
                ..
            } => self.walk_section(env, numeral, title.as_deref(), body),
            Operation::Step { .. } => {
                // Dependent vs Parallel ordinal index needs the
                // surrounding Sequence's parallel counter; a Step
                // encountered outside a Sequence (i.e. as the entire
                // body of a procedure) is treated as Dependent.
                self.walk_step(env, op, 0)
            }
            Operation::Loop {
                names, over, body, ..
            } => self.walk_loop(env, names, over.as_deref(), body),
            Operation::Invoke(invocable) => self.walk_invoke(env, invocable),
            Operation::Execute(executable) => {
                let function = self.executable_name(&executable.target);
                let qualified = self
                    .path
                    .render();
                let run_id = self
                    .appender
                    .run_id();
                let record = Record {
                    recorded: now_iso8601(),
                    run_id,
                    path: qualified.clone(),
                    state: State::Execute {
                        function: function.clone(),
                    },
                };
                self.appender
                    .append(&record)?;
                // An `Action` builtin (e.g. `exec`, `click`) is a command the
                // user must command: show the script and run it only on their
                // say-so. Skip or Fail declines the run and settles the step;
                // Quit stops.
                //
                // `Pure` builtins (coercions, reading the clock) just announce
                // and run.
                let is_action = match &executable.target {
                    ExecutableRef::Resolved(id) => {
                        self.library
                            .nature(*id)
                            == Nature::Action
                    }
                    _ => false,
                };
                if is_action {
                    let script = self.script_text(env, executable)?;
                    match self
                        .driver
                        .command(&qualified, &script)
                    {
                        UserInput::Done(chosen) => {
                            let value = super::evaluator::dispatch(
                                &self.library,
                                &self.context,
                                env,
                                executable,
                                Some(&[chosen]),
                            )?;
                            Ok(Outcome::Done(value))
                        }
                        UserInput::Skip => Ok(Outcome::Skipped),
                        UserInput::Fail(reason) => Ok(Outcome::Failed(Failure::Aborted(reason))),
                        UserInput::Quit => {
                            let suspend = Record {
                                recorded: now_iso8601(),
                                run_id,
                                path: "/".to_string(),
                                state: State::Stop,
                            };
                            self.appender
                                .append(&suspend)?;
                            Ok(Outcome::Stopped)
                        }
                    }
                } else {
                    self.driver
                        .announce(&describe_execute(&function));
                    let value = super::evaluator::dispatch(
                        &self.library,
                        &self.context,
                        env,
                        executable,
                        None,
                    )?;
                    Ok(Outcome::Done(value))
                }
            }
            Operation::Bind { .. }
            | Operation::Variable(_)
            | Operation::Number(_)
            | Operation::String(_)
            | Operation::Multiline(_, _)
            | Operation::Tablet(_)
            | Operation::List(_) => {
                let value = super::evaluator::evaluate(&self.library, &self.context, env, op)?;
                Ok(Outcome::Done(value))
            }
        }
    }

    /// The name of a function target. FIXME an unresolved one (awaiting
    /// domain linking) carries its identifier still.
    fn executable_name(&self, target: &ExecutableRef<'_>) -> String {
        match target {
            ExecutableRef::Resolved(id) => self
                .library
                .name(*id)
                .to_string(),
            ExecutableRef::Unresolved(id) => id
                .value
                .to_string(),
        }
    }

    /// The shell script an `exec` will run, rendered for the operator to see
    /// before they command it. The command's first argument is the script.
    fn script_text(
        &self,
        env: &mut Environment,
        executable: &'i Executable<'i>,
    ) -> Result<String, RunnerError> {
        match executable
            .arguments
            .first()
        {
            Some(arg) => {
                match super::evaluator::evaluate(&self.library, &self.context, env, arg)? {
                    Value::Literali(s) => Ok(s),
                    other => Ok(other.to_string()),
                }
            }
            None => Ok(String::new()),
        }
    }

    fn walk_invoke(
        &mut self,
        env: &mut Environment,
        invocable: &'i Invocable<'i>,
    ) -> Result<Outcome, RunnerError> {
        match &invocable.target {
            SubroutineRef::Resolved(id) => {
                let subroutine = &self
                    .program
                    .subroutines[id.0];

                // Evaluate the call arguments in the caller's environment, then
                // bind them positionally into a fresh environment for the
                // callee. The callee sees only its parameters, not the caller's
                // bindings.
                let params = subroutine
                    .parameters
                    .unwrap_or(&[]);
                let expected = params.len();
                let actual = invocable
                    .arguments
                    .len();
                if expected == 0 && actual > 0 {
                    return Err(RunnerError::ParameterUnexpected { actual });
                }
                if expected != actual {
                    return Err(RunnerError::ParameterArityMismatch { expected, actual });
                }
                let mut local = Environment::new();
                for (param, arg) in params
                    .iter()
                    .zip(&invocable.arguments)
                {
                    let value = super::evaluator::evaluate(&self.library, &self.context, env, arg)?;
                    local.extend(
                        param
                            .value
                            .to_string(),
                        value,
                    );
                }

                let name = subroutine
                    .name
                    .as_ref()
                    .map(|n| n.value);
                if let Some(name) = name {
                    let qualified = self
                        .path
                        .render();
                    let run_id = self
                        .appender
                        .run_id();
                    let record = Record {
                        recorded: now_iso8601(),
                        run_id,
                        path: qualified,
                        state: State::Invoke(InvokeTarget::Procedure(name.to_string())),
                    };
                    self.appender
                        .append(&record)?;
                    self.path
                        .push(PathSegment::Procedure(name));
                    let descended = self
                        .path
                        .render();
                    self.driver
                        .enter(&descended, "");
                }

                // Walk the body against the callee's own environment; `local`
                // is dropped on return, leaving the caller's `env` untouched.
                let result = self.walk(&mut local, &subroutine.body);

                // An invoked procedure is a structural scope: the operator signs
                // it off at its close, like a Section, before control returns to
                // the caller. A Quit or error walk skips the prompt — the
                // procedure did not complete.
                if name.is_some() {
                    let qualified = self
                        .path
                        .render();
                    self.path
                        .pop();
                    match result {
                        Ok(Outcome::Stopped) => Ok(Outcome::Stopped),
                        Ok(outcome) => self.seal_scope(&qualified, outcome),
                        Err(error) => Err(error),
                    }
                } else {
                    result
                }
            }
            SubroutineRef::Unresolved(id) => {
                self.driver
                    .announce(&format!("<{}>", id.value));
                Ok(Outcome::Done(Value::Unitus))
            }
        }
    }

    /// Evaluate a control structure. A `foreach` evalutates its body once for
    /// each element of the input collection, binding the loop name(s) to each
    /// element in turn and pushing an `Iteration` scope segment. The
    /// collection must evaluate to a list; a bare primitive widens to a
    /// one-element list, but a tuple or tablet is a runtime error. A
    /// `repeat` keyword (an iterable with `over: None`) is unbounded: it
    /// evaluates its body over and over, each pass an iteration scope, and in
    /// theory never returns though in practice, stops if a Quit or Abort is
    /// registered.
    fn walk_loop(
        &mut self,
        env: &mut Environment,
        names: &'i [language::Identifier<'i>],
        over: Option<&'i Operation<'i>>,
        body: &'i Operation<'i>,
    ) -> Result<Outcome, RunnerError> {
        self.driver
            .announce(&describe_loop(names, over));
        match over {
            None => {
                let mut number = 1;
                loop {
                    self.path
                        .push(PathSegment::Iteration(number));
                    let result = self.walk(env, body);
                    self.path
                        .pop();

                    if let Outcome::Stopped = result? {
                        return Ok(Outcome::Stopped);
                    }
                    number += 1;
                }
            }
            Some(expr) => {
                let items =
                    match super::evaluator::evaluate(&self.library, &self.context, env, expr)? {
                        Value::Arraeum(items) => items,
                        // A scalar in list context is a singleton list.
                        value @ (Value::Literali(_) | Value::Quanticle(_)) => vec![value],
                        // A tablet is a record, not a sequence, so it does not
                        // iterate directly.
                        _ => return Err(RunnerError::NotIterable),
                    };
                for (i, item) in items
                    .into_iter()
                    .enumerate()
                {
                    super::evaluator::bind_names(env, names, item)?;

                    let number = i + 1;
                    self.path
                        .push(PathSegment::Iteration(number));
                    let result = self.walk(env, body);
                    self.path
                        .pop();

                    if let Outcome::Stopped = result? {
                        return Ok(Outcome::Stopped);
                    }
                }
                Ok(Outcome::Done(Value::Unitus))
            }
        }
    }

    fn walk_sequence(
        &mut self,
        env: &mut Environment,
        ops: &'i [Operation<'i>],
    ) -> Result<Outcome, RunnerError> {
        let mut parallel_idx: usize = 0;
        let mut last = Value::Unitus;
        for op in ops {
            let outcome = match op {
                Operation::Step { ordinal, .. } => {
                    let index = match ordinal {
                        Ordinal::Parallel => {
                            parallel_idx += 1;
                            parallel_idx
                        }
                        Ordinal::Dependent(_) => 0,
                    };
                    self.walk_step(env, op, index)?
                }
                _ => self.walk(env, op)?,
            };
            match outcome {
                Outcome::Done(value) => last = value,
                Outcome::Stopped => return Ok(Outcome::Stopped),
                Outcome::Skipped | Outcome::Failed(_) => {}
            }
        }
        Ok(Outcome::Done(last))
    }

    fn walk_section(
        &mut self,
        env: &mut Environment,
        numeral: &'i str,
        title: Option<&'i Operation<'i>>,
        body: &'i Operation<'i>,
    ) -> Result<Outcome, RunnerError> {
        self.path
            .push(PathSegment::Section(numeral));
        let result = self.perform_section(env, numeral, title, body);
        let qualified = self
            .path
            .render();
        self.path
            .pop();
        // A section is a structural scope: the operator signs it off at its
        // close before the next sibling runs. A Quit or error walk skips the
        // prompt — the section did not complete.
        match result {
            Ok(Outcome::Stopped) => Ok(Outcome::Stopped),
            Ok(outcome) => self.seal_scope(&qualified, outcome),
            Err(error) => Err(error),
        }
    }

    fn perform_section(
        &mut self,
        env: &mut Environment,
        numeral: &'i str,
        title: Option<&'i Operation<'i>>,
        body: &'i Operation<'i>,
    ) -> Result<Outcome, RunnerError> {
        let qualified = self
            .path
            .render();
        let title_text = match title {
            Some(op) => match super::evaluator::evaluate(&self.library, &self.context, env, op)? {
                Value::Literali(s) => s,
                other => other.to_string(),
            },
            None => String::new(),
        };
        self.driver
            .section(&qualified, numeral, &title_text);
        self.walk(env, body)
    }

    fn walk_step(
        &mut self,
        env: &mut Environment,
        op: &'i Operation<'i>,
        parallel_index: usize,
    ) -> Result<Outcome, RunnerError> {
        let Operation::Step {
            ordinal,
            attributes,
            description,
            body,
            responses,
        } = op
        else {
            unreachable!("walk_step called with non-Step operation");
        };

        for frame in attributes {
            self.path
                .push(PathSegment::Attributes(frame));
        }
        let segment = match ordinal {
            Ordinal::Dependent(s) => PathSegment::DependentStep(s),
            Ordinal::Parallel => PathSegment::ParallelStep(parallel_index),
        };
        self.path
            .push(segment);
        let qualified = self
            .path
            .render();

        let result = self.perform_step(env, &qualified, body, description, responses);

        self.path
            .pop();
        for _ in attributes {
            self.path
                .pop();
        }

        result
    }

    fn perform_step(
        &mut self,
        env: &mut Environment,
        qualified: &str,
        body: &'i Operation<'i>,
        description: &'i [Operation<'i>],
        responses: &[&'i language::Response<'i>],
    ) -> Result<Outcome, RunnerError> {
        if self
            .completed
            .contains(qualified)
        {
            return Ok(Outcome::Done(Value::Unitus));
        }

        // Mark the start of work on this step before walking its body,
        // so any Invoke/Execute records emitted by the body land between
        // this Begin and the eventual outcome record.
        let run_id = self
            .appender
            .run_id();
        let begin = Record {
            recorded: now_iso8601(),
            run_id,
            path: qualified.to_string(),
            state: State::Begin,
        };
        self.appender
            .append(&begin)?;

        // Show the step's heading and description before walking its body,
        // so any output the body streams (e.g. exec) appears beneath the step
        // it belongs to rather than ahead of it. The description interpolates
        // only values bound by enclosing scopes, so it reads cleanly here.
        let mut description_text = String::new();
        for op in description {
            if !description_text.is_empty() {
                description_text.push('\n');
            }
            match super::evaluator::evaluate(&self.library, &self.context, env, op)? {
                Value::Literali(s) => description_text.push_str(&s),
                other => description_text.push_str(&other.to_string()),
            }
        }

        self.driver
            .step(qualified, &description_text);

        let produced = match self.walk(env, body)? {
            Outcome::Stopped => return Ok(Outcome::Stopped),
            Outcome::Done(value) => value,
            // The body declined its exec command beat (Skip / Fail), which
            // settles the step: there is no result to judge, so record the
            // outcome and return without the verdict prompt.
            settled => {
                let record = Record {
                    recorded: now_iso8601(),
                    run_id,
                    path: qualified.to_string(),
                    state: record_state(&settled),
                };
                self.appender
                    .append(&record)?;
                self.completed
                    .insert(qualified.to_string());
                return Ok(settled);
            }
        };

        let choices: Vec<&str> = responses
            .iter()
            .map(|r| r.value)
            .collect();
        let input = self
            .driver
            .ask(qualified, &choices, produced);

        // Quit halts the walk and is recorded as a Stop lifecycle event at the
        // root path, distinguishing a deliberate stop from a crash (which records
        // nothing). This step's Begin stands without a matching outcome, so
        // resume re-runs it.
        if let UserInput::Quit = input {
            let suspend = Record {
                recorded: now_iso8601(),
                run_id,
                path: "/".to_string(),
                state: State::Stop,
            };
            self.appender
                .append(&suspend)?;
            return Ok(Outcome::Stopped);
        }

        let outcome = outcome_from(input);
        let record = Record {
            recorded: now_iso8601(),
            run_id,
            path: qualified.to_string(),
            state: record_state(&outcome),
        };
        self.appender
            .append(&record)?;
        self.completed
            .insert(qualified.to_string());
        Ok(outcome)
    }

    /// Sign off a completed structural scope — a Section at its close, or the
    /// whole run at the entry procedure.
    fn seal_scope(&mut self, qualified: &str, outcome: Outcome) -> Result<Outcome, RunnerError> {
        let produced = match outcome {
            Outcome::Done(value) => value,
            _ => Value::Unitus,
        };
        let run_id = self
            .appender
            .run_id();
        let input = self
            .driver
            .seal(qualified, produced);
        if let UserInput::Quit = input {
            let suspend = Record {
                recorded: now_iso8601(),
                run_id,
                path: "/".to_string(),
                state: State::Stop,
            };
            self.appender
                .append(&suspend)?;
            return Ok(Outcome::Stopped);
        }
        let outcome = outcome_from(input);
        let record = Record {
            recorded: now_iso8601(),
            run_id,
            path: qualified.to_string(),
            state: record_state(&outcome),
        };
        self.appender
            .append(&record)?;
        self.completed
            .insert(qualified.to_string());
        Ok(outcome)
    }
}

fn describe_loop(
    names: &[crate::language::Identifier<'_>],
    over: Option<&Operation<'_>>,
) -> String {
    let joined: Vec<&str> = names
        .iter()
        .map(|n| n.value)
        .collect();
    match over {
        None => "repeat".to_string(),
        Some(_) if joined.is_empty() => "foreach".to_string(),
        Some(_) => format!("foreach {}", joined.join(", ")),
    }
}

fn describe_execute(function: &str) -> String {
    format!("{}()", function)
}

/// Lift a `UserInput` from the prompt into the runner's `Outcome`.
fn outcome_from(input: UserInput) -> Outcome {
    match input {
        UserInput::Done(value) => Outcome::Done(value),
        UserInput::Skip => Outcome::Skipped,
        UserInput::Fail(reason) => Outcome::Failed(Failure::Aborted(reason)),
        UserInput::Quit => Outcome::Stopped,
    }
}

/// Project the runner's in-memory `Outcome` into the on-disk `State` for the
/// PFFTT file. A single-line input (a chosen response or whatever the user
/// typed) records as a literal string. Multi-line literals (raw exec output)
/// record as unit. The in-memory `Outcome` still carries the full value, so a
/// value bound with `~` remains available in scope regardless. Quit is
/// unreachable here: the caller filters it out before recording.
fn record_state(outcome: &Outcome) -> State {
    match outcome {
        Outcome::Done(Value::Literali(text)) if !text.contains('\n') => {
            State::Done(Some(RecordValue::Literal(text.clone())))
        }
        Outcome::Done(_) => State::Done(Some(RecordValue::Unit)),
        Outcome::Skipped => State::Skip,
        Outcome::Failed(Failure::Aborted(reason)) => State::Fail(Some(RecordValue::Tablet(
            format!("[ reason = \"{}\" ]", reason),
        ))),
        Outcome::Stopped => {
            unreachable!("Stop is recorded as a lifecycle event, not a step result")
        }
    }
}

/// Build an `Environment` seeded with the entry procedure's parameters
/// bound to the supplied CLI arguments.
pub(super) fn bind_parameters(
    program: &Program<'_>,
    arguments: &[String],
) -> Result<Environment, RunnerError> {
    let entry = program
        .subroutines
        .first()
        .ok_or(RunnerError::MissingEntryProcedure)?;
    let params = entry
        .parameters
        .unwrap_or(&[]);
    let expected = params.len();
    let actual = arguments.len();
    if expected == 0 && actual > 0 {
        return Err(RunnerError::ParameterUnexpected { actual });
    }
    if expected != actual {
        return Err(RunnerError::ParameterArityMismatch { expected, actual });
    }
    let mut env = Environment::new();
    for (param, argument) in params
        .iter()
        .zip(arguments)
    {
        env.extend(
            param
                .value
                .to_string(),
            Value::Literali(argument.clone()),
        );
    }
    Ok(env)
}

/// Current UTC time as an RFC3339 millisecond-precision string, used
/// for the `recorded` field of every Result tablet. The fraction is
/// truncated (not rounded) — sub-millisecond resolution is dropped —
/// and the millisecond field is always rendered as three digits, even
/// when trailing zeros would otherwise be elided.
pub(super) fn now_iso8601() -> String {
    let now = time::OffsetDateTime::now_utc();
    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}Z",
        now.year(),
        u8::from(now.month()),
        now.day(),
        now.hour(),
        now.minute(),
        now.second(),
        now.millisecond(),
    )
}

#[cfg(test)]
#[path = "checks/runner.rs"]
mod check;
