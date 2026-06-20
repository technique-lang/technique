//! Interactive walker over a translated Program.

use std::collections::HashMap;
use std::io;
use std::path::PathBuf;

use super::context::Context;
use super::driver::{Driver, UserInput};
use super::evaluator::Environment;
use super::library::{Library, Nature};
use super::path::{PathSegment, QualifiedPath};
use super::state::{Appender, InvokeTarget, Record, RecordError, RunId, State};
use crate::language;
use crate::program::{
    Executable, ExecutableRef, Invocable, Locale, Operation, Ordinal, Program, Subroutine,
    SubroutineRef,
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
    /// Carries the body's computed value for block semantics; recorded as no value.
    Skipped(Value),
    Failed(Failure),
    /// A failure thrown mid-body (a failed exec); propagates up to the
    /// enclosing step, which catches it and settles as Fail.
    Throw(Failure),
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
    FunctionArityMismatch {
        function: &'static str,
        expected: usize,
        actual: usize,
    },
    ExecError(io::Error),
    CommandFailed(i32),
    IncompatibleCombination {
        left: &'static str,
        right: &'static str,
    },
    ParameterArityMismatch {
        procedure: String,
        parameters: Vec<String>,
        actual: usize,
    },
    ParameterUnexpected {
        procedure: String,
        actual: usize,
    },
    TerminalRequired,
    UserQuit,
}

/// Execute a Technique interactively by walking the `Program` tree. Tracks
/// the position in the document via a `QualifiedPath` stack, carries an
/// `Environment` with known result values. Holds the set of step FQNs already
/// completed in a *prior* run — the resume snapshot plus an append handle to
/// write results and the prompt the operator interacts through.
pub struct Runner<'i, D: Driver> {
    program: &'i Program<'i>,
    appender: Appender,
    completed: HashMap<String, Value>,
    driver: D,
    path: QualifiedPath<'i>,
    library: Library,
    context: Context,
}

impl<'i, D: Driver> Runner<'i, D> {
    pub fn new(
        program: &'i Program<'i>,
        appender: Appender,
        completed: HashMap<String, Value>,
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

    /// Consume the runner and return the inner driver after a run completes.
    /// Used to read a `Headless` driver's result count or to assert on the
    /// Mock's event log.
    pub fn into_driver(self) -> D {
        self.driver
    }

    /// Consume the runner and return the inner appender after a run completes.
    /// Used to read the recorded trail of an in-memory `Appender`.
    pub fn into_appender(self) -> Appender {
        self.appender
    }

    /// Walk the entry procedure top to bottom. Entry-procedure
    /// selection here is `program.subroutines[0]` — the synthetic
    /// anonymous wrapper if the document is top-level Steps, otherwise
    /// the first declared procedure.
    pub fn run(&mut self, mut env: Environment) -> Result<Outcome, RunnerError> {
        if let Some(metadata) = self
            .program
            .prelude
        {
            let header = crate::formatting::formatter::render_header(
                metadata,
                self.driver
                    .renderer(),
            );
            self.driver
                .display(&header);
        }
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
            let qualified = self
                .path
                .render();
            self.begin_scope(&qualified)?;
            let params = entry
                .parameters
                .unwrap_or(&[]);
            if params.is_empty() {
                self.driver
                    .enter(&qualified);
            } else {
                let echo = render_argument_echo(name, params, &env);
                self.driver
                    .enter(&echo);
            }
            let declaration = crate::formatting::formatter::render_declaration(
                name,
                entry.parameters,
                entry.signature,
                self.driver
                    .renderer(),
            );
            self.driver
                .display(&declaration);
            if let Some(t) = entry.title {
                let title_text = crate::formatting::formatter::render_title(
                    t,
                    self.driver
                        .renderer(),
                );
                self.driver
                    .display(&title_text);
            }
            if !entry
                .description
                .is_empty()
            {
                let description = crate::formatting::formatter::render_description(
                    entry.description,
                    self.driver
                        .renderer(),
                );
                self.driver
                    .display(&description);
            }
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
            let computable = computable(&entry.body);
            let sealed = match result {
                Ok(Outcome::Stopped) => Ok(Outcome::Stopped),
                Ok(outcome) => self.seal_scope(&qualified, outcome, computable),
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
            // Every body the translator emits is a `Sequence`, so a Step is
            // always reached as one of its members, where `walk_sequence`
            // supplies the parallel ordinal counter. A bare Step never reaches
            // `walk` directly.
            Operation::Step { .. } => {
                unreachable!("a Step is always walked as a Sequence member")
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
                            match super::evaluator::dispatch(
                                &self.library,
                                &self.context,
                                env,
                                executable,
                                Some(&[chosen]),
                            ) {
                                Ok(value) => Ok(Outcome::Done(value)),
                                // A non-zero exit throws to fail the step rather
                                // than aborting the run; the walk continues.
                                Err(RunnerError::CommandFailed(code)) => {
                                    Ok(Outcome::Throw(Failure::Aborted(format!(
                                        "External command exited with status {}",
                                        code
                                    ))))
                                }
                                Err(other) => Err(other),
                            }
                        }
                        UserInput::Skip => Ok(Outcome::Skipped(Value::Unitus)),
                        UserInput::Fail(reason) => Ok(Outcome::Throw(Failure::Aborted(reason))),
                        UserInput::Quit => self.record_stop(),
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
            Operation::Bind { names, value } => self.walk_bind(env, names, value),
            Operation::Variable(_)
            | Operation::Number(_)
            | Operation::String(_)
            | Operation::Multiline(_, _)
            | Operation::Tablet(_)
            | Operation::List(_)
            | Operation::Hole => {
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
        &mut self,
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
                let expected = subroutine.arity();
                let actual = invocable
                    .arguments
                    .len();
                // A bare call defers every argument and is exempt; a written
                // argument list must match arity exactly.
                if !invocable.elided {
                    let procedure = subroutine
                        .name
                        .as_ref()
                        .map(|n| n.value)
                        .unwrap_or("the procedure")
                        .to_string();
                    if expected == 0 && actual > 0 {
                        return Err(RunnerError::ParameterUnexpected { procedure, actual });
                    }
                    if expected != actual {
                        return Err(RunnerError::ParameterArityMismatch {
                            procedure,
                            parameters: describe_parameters(params, subroutine.signature),
                            actual,
                        });
                    }
                }
                let mut local = Environment::new();
                let name = subroutine
                    .name
                    .as_ref()
                    .map(|n| n.value);
                if let Some(name) = name {
                    // Steps record under the callee's lexical address, not the
                    // call site they were reached from.
                    let lexical_segments: Vec<PathSegment> = subroutine
                        .locale
                        .iter()
                        .map(|locale| match *locale {
                            Locale::Procedure(n) => PathSegment::Procedure(n),
                            Locale::Section(n) => PathSegment::Section(n),
                        })
                        .collect();
                    let lexical = super::path::render_path(&lexical_segments);
                    if self
                        .completed
                        .contains_key(&lexical)
                    {
                        return Ok(Outcome::Done(Value::Unitus));
                    }

                    // Acquire deferred arguments at the call site, in the
                    // invocation's `<name>` form, before any Invoke is recorded.
                    let caller = self
                        .path
                        .render();
                    let invoked = format!("{} <{}>", caller, name);

                    let formae = subroutine
                        .signature
                        .map(|s| {
                            s.requires
                                .formae()
                        })
                        .unwrap_or_default();
                    if invocable.elided {
                        for i in 0..subroutine.arity() {
                            let bind = params
                                .get(i)
                                .map(|p| p.value);
                            let forma = formae
                                .get(i)
                                .map(|f| f.value);
                            let value = match self
                                .driver
                                .acquire(&invoked, bind, forma)
                            {
                                UserInput::Done(value) => value,
                                other => return self.abandon(&lexical, other),
                            };
                            if let Some(bind) = bind {
                                local.extend(bind.to_string(), value);
                            }
                        }
                    } else {
                        for (i, arg) in invocable
                            .arguments
                            .iter()
                            .enumerate()
                        {
                            let bind = params
                                .get(i)
                                .map(|p| p.value);
                            let value = if let Operation::Hole = arg {
                                let forma = formae
                                    .get(i)
                                    .map(|f| f.value);
                                match self
                                    .driver
                                    .acquire(&invoked, bind, forma)
                                {
                                    UserInput::Done(value) => value,
                                    other => return self.abandon(&lexical, other),
                                }
                            } else {
                                super::evaluator::evaluate(&self.library, &self.context, env, arg)?
                            };
                            if let Some(bind) = bind {
                                local.extend(bind.to_string(), value);
                            }
                        }
                    }

                    // Record the Invoke at the call site, then descend onto the
                    // callee's lexical address, restored on return.
                    let run_id = self
                        .appender
                        .run_id();
                    self.appender
                        .append(&Record {
                            recorded: now_iso8601(),
                            run_id,
                            path: caller,
                            state: State::Invoke(InvokeTarget::Procedure(name.to_string())),
                        })?;

                    self.begin_scope(&lexical)?;

                    let saved = self
                        .path
                        .replace(lexical_segments);
                    self.announce_procedure(subroutine, name, &lexical);

                    // Walk the callee's body in its own `local` environment,
                    // then sign off its scope; a Quit or error skips the
                    // sign-off, leaving the procedure unfinished.
                    let result = self.walk(&mut local, &subroutine.body);
                    let computable = computable(&subroutine.body);
                    let sealed = match result {
                        Ok(Outcome::Stopped) => Ok(Outcome::Stopped),
                        Ok(outcome) => self.seal_scope(&lexical, outcome, computable),
                        Err(error) => Err(error),
                    };
                    self.path
                        .replace(saved);
                    sealed
                } else {
                    self.walk(&mut local, &subroutine.body)
                }
            }
            SubroutineRef::Unresolved(id) => {
                self.driver
                    .announce(&format!("<{}>", id.value));
                Ok(Outcome::Done(Value::Unitus))
            }
            SubroutineRef::Deferred(ext) => {
                // An external target lives in another document or system, so
                // this run cannot descend into it. Record the call site, then
                // present the invocation as its own node for the operator to
                // settle: Done if they performed (or recorded elsewhere) the
                // external procedure, otherwise Skip or Fail. An unattended
                // (automatic) run records Skip — nothing executed it and no one
                // is present to attest it, so it is not marked Done.
                let run_id = self
                    .appender
                    .run_id();
                let caller = self
                    .path
                    .render();
                self.appender
                    .append(&Record {
                        recorded: now_iso8601(),
                        run_id,
                        path: caller,
                        state: State::Invoke(InvokeTarget::Uri(
                            ext.value
                                .to_string(),
                        )),
                    })?;

                self.path
                    .push(PathSegment::External(ext.value));
                let qualified = self
                    .path
                    .render();
                if self
                    .completed
                    .contains_key(&qualified)
                {
                    self.path
                        .pop();
                    return Ok(Outcome::Done(Value::Unitus));
                }

                self.begin_scope(&qualified)?;
                self.driver
                    .announce(&format!("<{}>", ext.value));
                let input = self
                    .driver
                    .external(&qualified);
                if let UserInput::Quit = input {
                    self.path
                        .pop();
                    return self.record_stop();
                }

                self.driver
                    .settle("→", &qualified, &input);
                let outcome = outcome_from(input);
                self.appender
                    .append(&Record {
                        recorded: now_iso8601(),
                        run_id,
                        path: qualified,
                        state: record_state(&outcome),
                    })?;
                self.path
                    .pop();
                Ok(outcome)
            }
        }
    }

    /// Establish a binding. A descriptive binding of an action in a
    /// prose-only paragraph, for example
    ///
    /// ```technique
    ///     4.  Enumerate all the geographies ~ regions
    /// ```
    ///
    /// carries no computable; the value of regions will be the result the
    /// user enters, acquired from the driver.
    ///
    /// A binding whose value is an invocation or inline code block is
    /// computable and is invoked or evaluated first.
    fn walk_bind(
        &mut self,
        env: &mut Environment,
        names: &'i [language::Identifier<'i>],
        value: &'i Operation<'i>,
    ) -> Result<Outcome, RunnerError> {
        let descriptive = if let Operation::Sequence(ops) = value {
            ops.is_empty()
        } else {
            false
        };
        if descriptive {
            let qualified = self
                .path
                .render();
            let name = names
                .first()
                .map(|n| n.value);
            match self
                .driver
                .acquire(&qualified, name, None)
            {
                UserInput::Done(value) => {
                    super::evaluator::bind_names(env, names, value.clone())?;
                    Ok(Outcome::Done(value))
                }
                UserInput::Skip => {
                    super::evaluator::bind_names(env, names, Value::Unitus)?;
                    Ok(Outcome::Skipped(Value::Unitus))
                }
                UserInput::Fail(reason) => Ok(Outcome::Failed(Failure::Aborted(reason))),
                UserInput::Quit => self.record_stop(),
            }
        } else {
            let value = super::evaluator::evaluate(&self.library, &self.context, env, value)?;
            super::evaluator::bind_names(env, names, value.clone())?;
            Ok(Outcome::Done(value))
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
        match over {
            None => {
                let mut number = 1;
                loop {
                    if let Outcome::Stopped = self.walk_iteration(env, number, body)? {
                        return Ok(Outcome::Stopped);
                    }
                    number += 1;
                }
            }
            Some(expr) => {
                let value = super::evaluator::evaluate(&self.library, &self.context, env, expr)?;
                let items = super::evaluator::coerce_to_list(value)?;
                for (i, item) in items
                    .into_iter()
                    .enumerate()
                {
                    super::evaluator::bind_names(env, names, item)?;

                    let number = i + 1;
                    if let Outcome::Stopped = self.walk_iteration(env, number, body)? {
                        return Ok(Outcome::Stopped);
                    }
                }
                Ok(Outcome::Done(Value::Unitus))
            }
        }
    }

    /// Walk one pass of a loop body within its `[number]` iteration scope,
    /// bracketing it with `↘`/`↙` chrome.
    fn walk_iteration(
        &mut self,
        env: &mut Environment,
        number: usize,
        body: &'i Operation<'i>,
    ) -> Result<Outcome, RunnerError> {
        self.path
            .push(PathSegment::Iteration(number));
        let qualified = self
            .path
            .render();
        self.driver
            .enter(&qualified);
        let result = self.walk(env, body);
        let verdict = match &result {
            Ok(Outcome::Done(_)) => Some(UserInput::Done(Value::Unitus)),
            Ok(Outcome::Skipped(_)) => Some(UserInput::Skip),
            Ok(Outcome::Failed(Failure::Aborted(reason))) => Some(UserInput::Fail(reason.clone())),
            _ => None,
        };
        if let Some(verdict) = verdict {
            self.driver
                .settle("↙", &qualified, &verdict);
        }
        self.path
            .pop();
        result
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
                Outcome::Done(value) | Outcome::Skipped(value) => last = value,
                Outcome::Stopped => return Ok(Outcome::Stopped),
                // A Throw is a hard failure mid-body; it propagates up to the
                // enclosing step. A plain Fail is a step's recorded verdict and
                // the sequence continues to its siblings.
                Outcome::Throw(failure) => return Ok(Outcome::Throw(failure)),
                Outcome::Failed(_) => {}
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
        let qualified = self
            .path
            .render();
        if self
            .completed
            .contains_key(&qualified)
        {
            self.path
                .pop();
            return Ok(Outcome::Done(Value::Unitus));
        }
        self.begin_scope(&qualified)?;
        let result = self.perform_section(env, numeral, title, body);
        self.path
            .pop();
        let computable = computable(body);
        // A section is a structural scope: the user signs it off at its
        // close before the next sibling runs. A Quit or error walk skips the
        // prompt — the section did not complete.
        match result {
            Ok(Outcome::Stopped) => Ok(Outcome::Stopped),
            Ok(outcome) => self.seal_scope(&qualified, outcome, computable),
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
            source,
            body,
            responses,
            ..
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

        let result = self.perform_step(env, &qualified, ordinal, body, source, responses);

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
        _ordinal: &Ordinal<'i>,
        body: &'i Operation<'i>,
        source: &'i language::Scope<'i>,
        responses: &[&'i language::Response<'i>],
    ) -> Result<Outcome, RunnerError> {
        if let Some(value) = self
            .completed
            .get(qualified)
        {
            let value = value.clone();
            // A replayed step does not re-run its body, so we re-establish
            // any results it made by re-binding the step's names to their
            // recorded values. A step that nests further work (a `foreach`
            // loop, substeps) is re-walked instead: if its descendants are
            // all completed too, they short-circuit without re-prompting
            // while re-hydrating bindings made inside the loop body.
            if nests_work(body) {
                self.walk(env, body)?;
            } else if let Some(names) = binding_names(body) {
                super::evaluator::bind_names(env, names, value.clone())?;
            }
            return Ok(Outcome::Done(value));
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

        let step_text = crate::formatting::formatter::render_step(
            source,
            self.driver
                .renderer(),
        );

        self.driver
            .step(qualified, &step_text);

        let produced = match self.walk(env, body)? {
            Outcome::Stopped => return Ok(Outcome::Stopped),
            Outcome::Done(value) => value,
            // The body settled itself — a declined command beat (Skip / Fail)
            // or a thrown exec failure, which catches here as a Fail. Record
            // and show its verdict without an acceptance prompt.
            settled => {
                let settled = match settled {
                    Outcome::Throw(failure) => Outcome::Failed(failure),
                    other => other,
                };
                let record = Record {
                    recorded: now_iso8601(),
                    run_id,
                    path: qualified.to_string(),
                    state: record_state(&settled),
                };
                self.appender
                    .append(&record)?;
                let verdict = match &settled {
                    Outcome::Skipped(_) => UserInput::Skip,
                    Outcome::Failed(Failure::Aborted(reason)) => UserInput::Fail(reason.clone()),
                    _ => unreachable!("only Skip and Fail reach the settled branch"),
                };
                self.driver
                    .settle("→", qualified, &verdict);
                return Ok(settled);
            }
        };

        let choices: Vec<&str> = responses
            .iter()
            .map(|r| r.value)
            .collect();
        let computable = computable(body);
        // `ask` consumes `produced`; keep a copy for a Skip to propagate.
        let propagate = produced.clone();
        let input = self
            .driver
            .ask(qualified, &choices, produced, computable);

        // Quit halts the walk; this step's Begin stands without a matching
        // outcome, so resume re-runs it.
        if let UserInput::Quit = input {
            return self.record_stop();
        }

        self.driver
            .settle("→", qualified, &input);
        let outcome = match input {
            UserInput::Skip => Outcome::Skipped(propagate),
            other => outcome_from(other),
        };
        let record = Record {
            recorded: now_iso8601(),
            run_id,
            path: qualified.to_string(),
            state: record_state(&outcome),
        };
        self.appender
            .append(&record)?;
        Ok(outcome)
    }

    /// Show a named procedure's heading on descent: the driver's `↘` enter line
    /// followed by the procedure's declaration, title, and description. Shared by
    /// the entry procedure and every invoked one.
    fn announce_procedure(
        &mut self,
        subroutine: &'i Subroutine<'i>,
        name: &'i str,
        qualified: &str,
    ) {
        self.driver
            .enter(qualified);
        let declaration = crate::formatting::formatter::render_declaration(
            name,
            subroutine.parameters,
            subroutine.signature,
            self.driver
                .renderer(),
        );
        self.driver
            .display(&declaration);
        if let Some(t) = subroutine.title {
            let title_text = crate::formatting::formatter::render_title(
                t,
                self.driver
                    .renderer(),
            );
            self.driver
                .display(&title_text);
        }
        if !subroutine
            .description
            .is_empty()
        {
            let description = crate::formatting::formatter::render_description(
                subroutine.description,
                self.driver
                    .renderer(),
            );
            self.driver
                .display(&description);
        }
    }

    /// Open a structural scope — the entry procedure, a Section, or an invoked
    /// procedure — pairing with the `Done` its `seal_scope` records on close, so
    /// every scope's address is bracketed `Begin`…`Done` just as a step's is.
    fn begin_scope(&mut self, qualified: &str) -> Result<(), RunnerError> {
        let run_id = self
            .appender
            .run_id();
        self.appender
            .append(&Record {
                recorded: now_iso8601(),
                run_id,
                path: qualified.to_string(),
                state: State::Begin,
            })?;
        Ok(())
    }

    /// Sign off a completed structural scope — a Section at its close, or the
    /// whole run at the entry procedure.
    fn seal_scope(
        &mut self,
        qualified: &str,
        outcome: Outcome,
        computable: bool,
    ) -> Result<Outcome, RunnerError> {
        let produced = match outcome {
            Outcome::Done(value) | Outcome::Skipped(value) => value,
            _ => Value::Unitus,
        };
        let propagate = produced.clone();
        let run_id = self
            .appender
            .run_id();
        let input = self
            .driver
            .seal(qualified, produced, computable);
        if let UserInput::Quit = input {
            return self.record_stop();
        }
        self.driver
            .settle("↙", qualified, &input);
        let outcome = match input {
            UserInput::Skip => Outcome::Skipped(propagate),
            other => outcome_from(other),
        };
        let record = Record {
            recorded: now_iso8601(),
            run_id,
            path: qualified.to_string(),
            state: record_state(&outcome),
        };
        self.appender
            .append(&record)?;
        Ok(outcome)
    }

    /// Settle an invocation declined at its acquire prompt: Skip and Fail
    /// record the call's outcome at `qualified`; Quit stops the run.
    fn abandon(&mut self, qualified: &str, input: UserInput) -> Result<Outcome, RunnerError> {
        if let UserInput::Quit = input {
            return self.record_stop();
        }
        let outcome = outcome_from(input);
        let run_id = self
            .appender
            .run_id();
        self.appender
            .append(&Record {
                recorded: now_iso8601(),
                run_id,
                path: qualified.to_string(),
                state: record_state(&outcome),
            })?;
        Ok(outcome)
    }

    /// Record a deliberate Stop at the root path and unwind the walk.
    fn record_stop(&mut self) -> Result<Outcome, RunnerError> {
        let run_id = self
            .appender
            .run_id();
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

fn describe_execute(function: &str) -> String {
    format!("{}()", function)
}

/// This is true when a body holds work to perform (that can be evaluated); a
/// step whose definition is purely descriptive prose can and will have a
/// Result, but is not computable.
fn computable(op: &Operation) -> bool {
    match op {
        // A pure-prose body is not computable (this is fine!).
        Operation::Sequence(ops) if ops.is_empty() => false,
        Operation::Sequence(ops) => ops
            .iter()
            .any(computable),
        Operation::Step { body, .. } | Operation::Section { body, .. } => computable(body),
        _ => true,
    }
}

/// Lift a `UserInput` from the prompt into the runner's `Outcome`.
fn outcome_from(input: UserInput) -> Outcome {
    match input {
        UserInput::Done(value) => Outcome::Done(value),
        UserInput::Skip => Outcome::Skipped(Value::Unitus),
        UserInput::Fail(reason) => Outcome::Failed(Failure::Aborted(reason)),
        UserInput::Quit => Outcome::Stopped,
    }
}

/// Project the runner's in-memory `Outcome` into the on-disk `State` for the
/// PFFTT file. A `Done` records its full value (serialized by the state codec),
/// so a value bound with `~` rehydrates on resume. Quit is unreachable here:
/// the caller filters it out before recording.
fn record_state(outcome: &Outcome) -> State {
    match outcome {
        Outcome::Done(value) => State::Done(Some(value.clone())),
        Outcome::Skipped(_) => State::Skip,
        Outcome::Failed(Failure::Aborted(reason)) | Outcome::Throw(Failure::Aborted(reason)) => {
            if reason.is_empty() {
                // The user failed the step without giving a reason; record the
                // failure with no reason rather than an empty-string one.
                State::Fail(None)
            } else {
                State::Fail(Some(super::state::fail_reason(reason)))
            }
        }
        Outcome::Stopped => {
            unreachable!("Stop is recorded as a lifecycle event, not a step result")
        }
    }
}

/// The names a step body binds, if any: the first `Bind` reached without
/// descending through a nested step, loop, or section. Used on resume to
/// rebind a replayed step's value into the environment.
fn binding_names<'i>(op: &Operation<'i>) -> Option<&'i [language::Identifier<'i>]> {
    match op {
        Operation::Bind { names, .. } => Some(names),
        Operation::Sequence(ops) => ops
            .iter()
            .find_map(binding_names),
        _ => None,
    }
}

/// Whether a step body nests further executable scopes — a `foreach`/`repeat`
/// loop or substeps — as opposed to being a leaf of prose, a binding, or a
/// call. A completed step that nests work is re-walked on resume so bindings
/// made inside it rehydrate; a leaf is restored from its recorded value alone.
fn nests_work(op: &Operation) -> bool {
    match op {
        Operation::Loop { .. } | Operation::Step { .. } => true,
        Operation::Sequence(ops) => ops
            .iter()
            .any(nests_work),
        _ => false,
    }
}

/// Render the entry call with arguments bound to each parameter in
/// `value ~ name` form, e.g. `connectivity_check([] ~ e, 0 ~ s)`.
fn render_argument_echo(name: &str, params: &[language::Identifier], env: &Environment) -> String {
    let bindings: Vec<String> = params
        .iter()
        .map(|p| {
            let value = match env.lookup(p.value) {
                Some(Value::Literali(text)) => text.clone(),
                Some(other) => other.to_string(),
                None => String::new(),
            };
            format!("{} ~ {}", value, p.value)
        })
        .collect();
    format!("{}: ({})", name, bindings.join(", "))
}

/// Describe a procedure's expected parameters as `name : Type` fragments for
/// an arity error, falling back to whichever of name or forma is known.
fn describe_parameters(
    params: &[language::Identifier],
    signature: Option<&language::Signature>,
) -> Vec<String> {
    let formae = signature
        .map(|s| {
            s.requires
                .formae()
        })
        .unwrap_or_default();
    let count = params
        .len()
        .max(formae.len());
    (0..count)
        .map(|i| {
            let name = params
                .get(i)
                .map(|p| p.value);
            let forma = formae
                .get(i)
                .map(|f| f.value);
            match (name, forma) {
                (Some(n), Some(t)) => format!("{} : {}", n, t),
                (Some(n), None) => n.to_string(),
                (None, Some(t)) => t.to_string(),
                (None, None) => "?".to_string(),
            }
        })
        .collect()
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
    let procedure = entry
        .name
        .as_ref()
        .map(|n| n.value)
        .unwrap_or("the entry procedure")
        .to_string();
    if expected == 0 && actual > 0 {
        return Err(RunnerError::ParameterUnexpected { procedure, actual });
    }
    if expected != actual {
        return Err(RunnerError::ParameterArityMismatch {
            procedure,
            parameters: describe_parameters(params, entry.signature),
            actual,
        });
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
