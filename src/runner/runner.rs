//! Interactive walker over a translated Program.

use std::collections::HashMap;
use std::io;
use std::path::PathBuf;

use super::context::Context;
use super::driver::{Driver, UserInput};
use super::evaluator::Environment;
use super::library::{Library, Nature};
use super::path::{PathSegment, QualifiedPath};
use super::state::{Appender, InvokeTarget, Record, RecordError, RunId, State, Supplied};
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
#[derive(Debug, Clone, PartialEq)]
pub enum Failure {
    Aborted(String),
}

/// Anything that can go wrong while preparing or running a Technique.
/// Variants are populated as the implementing steps land; the formatter
/// in `crate::problem` knows how to render each one.
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
    inputs: HashMap<String, Vec<Supplied>>,
    driver: D,
    path: QualifiedPath<'i>,
    library: Library,
    context: Context,
    document: Option<String>,
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
            inputs: HashMap::new(),
            driver,
            path: QualifiedPath::new(),
            library,
            context: Context::native(),
            document: None,
        }
    }

    /// Name the source document so the run brackets its walk double arrow
    /// marked trace lines.
    pub fn with_document(mut self, document: String) -> Self {
        self.document = Some(document);
        self
    }

    /// Seed the runner with the inputs recorded by a prior run — the values
    /// supplied to the entry procedure and to each invocation — so a resume
    /// restores them rather than re-prompting. Empty on a fresh run.
    pub fn with_inputs(mut self, inputs: HashMap<String, Vec<Supplied>>) -> Self {
        self.inputs = inputs;
        self
    }

    /// Override the host context builtins write through (default: the terminal).
    pub fn with_context(mut self, context: Context) -> Self {
        self.context = context;
        self
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
        if let Some(document) = &self.document {
            let label = format!(
                "/ {},1 #{}",
                document,
                self.appender
                    .run_id()
                    .render()
            );
            self.driver
                .commence(&label);
        }
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
            let params = entry
                .parameters
                .unwrap_or(&[]);
            self.restore_or_record_inputs(&mut env, &qualified, params)?;
            self.begin_scope(&qualified)?;
            if params.is_empty() {
                self.driver
                    .enter(&qualified);
            } else {
                let echo = render_argument_echo(&qualified, params, &env);
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
        // A run that walked to its end closes with a `Finish` record at the
        // root and the double arrow marker.
        if let Ok(outcome) = &result {
            if let Outcome::Stopped = outcome {
            } else {
                self.record_finish()?;
                if self
                    .document
                    .is_some()
                {
                    let label = format!(
                        "/ #{}",
                        self.appender
                            .run_id()
                            .render()
                    );
                    self.driver
                        .conclude(&label);
                }
            }
        }
        result
    }

    fn walk(
        &mut self,
        env: &mut Environment,
        op: &'i Operation<'i>,
    ) -> Result<Outcome, RunnerError> {
        match op {
            Operation::Sequence(ops) => self.walk_sequence(env, ops),
            Operation::Prologue(ops) => {
                self.path
                    .push(PathSegment::Prologue);
                let qualified = self
                    .path
                    .render();
                let result = self.perform_prologue(env, &qualified, ops);
                self.path
                    .pop();
                result
            }
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
                // A `Command` builtin (e.g. `exec`) is executed by the host;
                // the user vets it: show the editable script and run it only on
                // their say-so. An `Action` (e.g. `click`) is a physical
                // interaction the user performs themselves: show the call
                // read-only to confirm. Either way Skip or Fail declines and
                // settles the step; Quit stops. `Pure` builtins just announce
                // and run.
                let nature = match &executable.target {
                    ExecutableRef::Resolved(id) => self
                        .library
                        .nature(*id),
                    _ => Nature::Pure,
                };
                // Pure builtins record nothing; only effectful calls are traced.
                let effectful = if let Nature::Pure = nature {
                    false
                } else {
                    true
                };
                if effectful {
                    self.appender
                        .append(&Record {
                            recorded: now_iso8601(),
                            run_id,
                            path: qualified.clone(),
                            state: State::Execute {
                                function: function.clone(),
                            },
                        })?;
                }
                let outcome = match nature {
                    Nature::Command => {
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
                                    // A non-zero exit throws to fail the step
                                    // rather than aborting the run; the walk
                                    // continues.
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
                    }
                    Nature::Action => {
                        let (verb, label) = self.action_parts(env, executable)?;
                        match self
                            .driver
                            .action(&qualified, &function, &verb, &label)
                        {
                            UserInput::Done(_) => {
                                let value = super::evaluator::dispatch(
                                    &self.library,
                                    &self.context,
                                    env,
                                    executable,
                                    None,
                                )?;
                                Ok(Outcome::Done(value))
                            }
                            UserInput::Skip => Ok(Outcome::Skipped(Value::Unitus)),
                            UserInput::Fail(reason) => Ok(Outcome::Throw(Failure::Aborted(reason))),
                            UserInput::Quit => self.record_stop(),
                        }
                    }
                    Nature::Pure => {
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
                }?;
                // Pair the Execute with a Return carrying its value; a stopped
                // run leaves the enter unpaired.
                let stopped = if let Outcome::Stopped = outcome {
                    true
                } else {
                    false
                };
                if effectful && !stopped {
                    let returned = if let Outcome::Done(value) = &outcome {
                        Some(value.clone())
                    } else {
                        None
                    };
                    self.appender
                        .append(&Record {
                            recorded: now_iso8601(),
                            run_id,
                            path: qualified.clone(),
                            state: State::Return(returned),
                        })?;
                }
                Ok(outcome)
            }
            Operation::Bind {
                names,
                value,
                inferred,
            } => self.walk_bind(env, names, value, inferred.as_ref()),
            Operation::Variable(_)
            | Operation::Number(_)
            | Operation::String(_)
            | Operation::Multiline(_, _)
            | Operation::Tablet(_)
            | Operation::List(_)
            | Operation::Prose(_)
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

    /// An action's parts for the user to confirm: its imperative verb (the
    /// library's `display` name, e.g. `Click`) and the bare label its single
    /// argument evaluates to, with string literals shown unquoted.
    fn action_parts(
        &mut self,
        env: &mut Environment,
        executable: &'i Executable<'i>,
    ) -> Result<(String, String), RunnerError> {
        let verb = match &executable.target {
            ExecutableRef::Resolved(id) => self
                .library
                .display(*id)
                .map(str::to_string)
                .unwrap_or_else(|| self.executable_name(&executable.target)),
            _ => self.executable_name(&executable.target),
        };
        let label = match executable
            .arguments
            .first()
        {
            Some(arg) => {
                match super::evaluator::evaluate(&self.library, &self.context, env, arg)? {
                    Value::Literali(text) => text,
                    other => other.to_string(),
                }
            }
            None => String::new(),
        };
        Ok((verb, label))
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

                    let formae = render_parameter_formae(subroutine.signature);

                    // A prior run's recorded inputs for this callee. A
                    // prompted argument (an elided call or a `?` hole) is
                    // restored from here on resume, in prompted order, rather
                    // than re-acquired. An argument the author supplied as a
                    // source expression is re-evaluated, so a loop variable
                    // still varies — and, being re-derivable, does not need
                    // to be recorded.
                    let recorded = self
                        .inputs
                        .get(&lexical)
                        .cloned();
                    let mut prompted: Vec<Supplied> = Vec::new();
                    let mut taken = 0usize;
                    if invocable.elided {
                        for i in 0..subroutine.arity() {
                            let bind = params
                                .get(i)
                                .map(|p| p.value);
                            let forma = formae
                                .get(i)
                                .map(|s| s.as_str());
                            let value = match recorded
                                .as_ref()
                                .and_then(|r| r.get(taken))
                            {
                                Some(s) => s
                                    .value
                                    .clone(),
                                None => match self
                                    .driver
                                    .acquire(&invoked, bind, forma)
                                {
                                    UserInput::Done(value) => value,
                                    other => return self.abandon(&lexical, other),
                                },
                            };
                            taken += 1;
                            if let Some(bind) = bind {
                                local.extend(bind.to_string(), value.clone());
                            }
                            prompted.push(Supplied {
                                value,
                                name: bind.map(|b| b.to_string()),
                            });
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
                            if let Operation::Hole = arg {
                                let value = match recorded
                                    .as_ref()
                                    .and_then(|r| r.get(taken))
                                {
                                    Some(s) => s
                                        .value
                                        .clone(),
                                    None => {
                                        let forma = formae
                                            .get(i)
                                            .map(|s| s.as_str());
                                        match self
                                            .driver
                                            .acquire(&invoked, bind, forma)
                                        {
                                            UserInput::Done(value) => value,
                                            other => return self.abandon(&lexical, other),
                                        }
                                    }
                                };
                                taken += 1;
                                if let Some(bind) = bind {
                                    local.extend(bind.to_string(), value.clone());
                                }
                                prompted.push(Supplied {
                                    value,
                                    name: bind.map(|b| b.to_string()),
                                });
                            } else {
                                let value = super::evaluator::evaluate(
                                    &self.library,
                                    &self.context,
                                    env,
                                    arg,
                                )?;
                                if let Some(bind) = bind {
                                    local.extend(bind.to_string(), value);
                                }
                            }
                        }
                    }

                    // Record the dispatch once the arguments are in hand —
                    // declining at the prompt above returns before this, so a
                    // declined call records no Invoke. Recorded at answer-time,
                    // so the gap from the previous event is the operator wait.
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

                    // Record the prompted inputs (answered just now) before
                    // Begin, unless they were restored from a prior run (already
                    // in the trail).
                    if recorded.is_none() {
                        self.record_inputs(&lexical, prompted)?;
                    }

                    self.begin_scope(&lexical)?;

                    let saved = self
                        .path
                        .replace(lexical_segments);
                    self.announce_procedure(subroutine, name, &lexical, &local);

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
                    .settle("⇒", &qualified, &input);
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
        inferred: Option<&'i language::Genus<'i>>,
    ) -> Result<Outcome, RunnerError> {
        let descriptive = if let Operation::Sequence(ops) = value {
            ops.is_empty()
        } else {
            false
        };
        if descriptive {
            // A descriptive binding has no expression to compute its value, so
            // each name is solicited from the user in turn. A tuple binding
            // `text ~ (a, b)` prompts once per name and binds each; the step's
            // value is the single value for one name, or a tuple of them.
            let qualified = self
                .path
                .render();
            let rendered = inferred
                .map(|genus| crate::formatting::render_genus(genus, &crate::formatting::Identity));
            let forma = match &rendered {
                Some(text) => Some(text.as_str()),
                None => None,
            };
            // Set the acquired `(name : forma)` off from the path with a
            // trailing space; an invocation prompt instead glues its arguments
            // straight to the `<callee>`.
            let prompt = format!("{qualified} ");
            let mut acquired = Vec::with_capacity(names.len());
            for name in names {
                match self
                    .driver
                    .acquire(&prompt, Some(name.value), forma)
                {
                    UserInput::Done(value) => acquired.push(value),
                    UserInput::Skip => {
                        for name in names {
                            super::evaluator::bind_names(
                                env,
                                std::slice::from_ref(name),
                                Value::Unitus,
                            )?;
                        }
                        return Ok(Outcome::Skipped(Value::Unitus));
                    }
                    UserInput::Fail(reason) => {
                        return Ok(Outcome::Failed(Failure::Aborted(reason)))
                    }
                    UserInput::Quit => return self.record_stop(),
                }
            }
            for (name, value) in names
                .iter()
                .zip(&acquired)
            {
                super::evaluator::bind_names(env, std::slice::from_ref(name), value.clone())?;
            }
            let produced = if acquired.len() == 1 {
                acquired
                    .into_iter()
                    .next()
                    .unwrap()
            } else {
                Value::Parametriq(acquired)
            };
            Ok(Outcome::Done(produced))
        } else {
            // Walk rather than evaluate: the bound value may be an effectful
            // spine operation — an `Invoke` that must descend into its callee
            // interactively, an `Execute` that must be gated, a `Loop` — which
            // the evaluator would mishandle as Unit. Walking a pure value is
            // equivalent to evaluating it.
            match self.walk(env, value)? {
                Outcome::Done(value) => {
                    super::evaluator::bind_names(env, names, value.clone())?;
                    Ok(Outcome::Done(value))
                }
                Outcome::Skipped(value) => {
                    super::evaluator::bind_names(env, names, Value::Unitus)?;
                    Ok(Outcome::Skipped(value))
                }
                other => Ok(other),
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
        match over {
            None => {
                let mut number = 1;
                loop {
                    if let Outcome::Stopped = self.walk_iteration(env, names, number, body)? {
                        return Ok(Outcome::Stopped);
                    }
                    number += 1;
                }
            }
            Some(expr) => {
                // A collection naming an as-yet-unbound variable — e.g. a list
                // a zero-iteration or skipped earlier loop never populated —
                // iterates nothing rather than aborting the run. The name is
                // statically in scope (resolution guarantees it); it simply has
                // no value yet at runtime.
                if let Operation::Variable(id) = expr {
                    if env
                        .lookup(id.value)
                        .is_none()
                    {
                        return Ok(Outcome::Done(Value::Unitus));
                    }
                }
                let value = super::evaluator::evaluate(&self.library, &self.context, env, expr)?;
                let items = super::evaluator::coerce_to_list(value)?;
                for (i, item) in items
                    .into_iter()
                    .enumerate()
                {
                    super::evaluator::bind_names(env, names, item)?;

                    let number = i + 1;
                    if let Outcome::Stopped = self.walk_iteration(env, names, number, body)? {
                        return Ok(Outcome::Stopped);
                    }
                }
                Ok(Outcome::Done(Value::Unitus))
            }
        }
    }

    /// Walk one pass of a loop body within its `[number]` iteration scope,
    /// bracketing it with `↘`/`↙` chrome. The `↘` line echoes the loop
    /// variable(s) bound for this pass, in the same `value ~ name` form used
    /// for a procedure call's arguments.
    fn walk_iteration(
        &mut self,
        env: &mut Environment,
        names: &'i [language::Identifier<'i>],
        number: usize,
        body: &'i Operation<'i>,
    ) -> Result<Outcome, RunnerError> {
        self.path
            .push(PathSegment::Iteration(number));
        let qualified = self
            .path
            .render();
        let echo = render_iteration_echo(&qualified, names, env);
        self.driver
            .enter(&echo);
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

    // Walk the anonymous step-0 scope. Its `/0` address is bracketed
    // Begin…Done like a step's, so a completed prologue short-circuits on
    // resume (and rehydrates any bindings it made) rather than re-running its
    // commands; unlike a step it takes no sign-off of its own, folding its
    // outcome into the enclosing procedure's seal.
    fn perform_prologue(
        &mut self,
        env: &mut Environment,
        qualified: &str,
        ops: &'i [Operation<'i>],
    ) -> Result<Outcome, RunnerError> {
        if let Some(value) = self
            .completed
            .get(qualified)
        {
            let value = value.clone();
            if ops
                .iter()
                .any(nests_work)
            {
                for op in ops {
                    self.walk(env, op)?;
                }
            } else if let Some(names) = ops
                .iter()
                .find_map(binding_names)
            {
                super::evaluator::bind_names(env, names, value.clone())?;
            }
            return Ok(Outcome::Done(value));
        }

        self.begin_scope(qualified)?;
        let outcome = self.walk_sequence(env, ops)?;
        if let Outcome::Stopped = outcome {
            return Ok(outcome);
        }
        let record = Record {
            recorded: now_iso8601(),
            run_id: self
                .appender
                .run_id(),
            path: qualified.to_string(),
            state: record_state(&outcome),
        };
        self.appender
            .append(&record)?;
        Ok(outcome)
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

        let subs = env.substitutions();
        let step_text = crate::formatting::formatter::render_step(
            source,
            &subs,
            self.driver
                .renderer(),
        );

        let depth = self
            .path
            .depth();
        self.driver
            .step(qualified, &step_text, depth);

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
        env: &Environment,
    ) {
        let params = subroutine
            .parameters
            .unwrap_or(&[]);
        if params.is_empty() {
            self.driver
                .enter(qualified);
        } else {
            let echo = render_argument_echo(qualified, params, env);
            self.driver
                .enter(&echo);
        }
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

    /// Record the values supplied to a procedure's parameters at its own path,
    /// so a resume restores them rather than re-prompting. A procedure with no
    /// parameters records nothing.
    fn record_inputs(
        &mut self,
        qualified: &str,
        supplied: Vec<Supplied>,
    ) -> Result<(), RunnerError> {
        if supplied.is_empty() {
            return Ok(());
        }
        let run_id = self
            .appender
            .run_id();
        self.appender
            .append(&Record {
                recorded: now_iso8601(),
                run_id,
                path: qualified.to_string(),
                state: State::Input(supplied),
            })
    }

    /// At a procedure's entry, restore its parameter bindings from a prior
    /// run's recorded inputs if present (resume), otherwise record the inputs
    /// it was called with (a fresh run). Used for the entry procedure, whose
    /// arguments come from the command line.
    fn restore_or_record_inputs(
        &mut self,
        env: &mut Environment,
        qualified: &str,
        params: &[language::Identifier<'i>],
    ) -> Result<(), RunnerError> {
        if let Some(supplied) = self
            .inputs
            .get(qualified)
            .cloned()
        {
            for item in supplied {
                if let Some(name) = item.name {
                    env.extend(name, item.value);
                }
            }
            return Ok(());
        }
        let supplied = params
            .iter()
            .map(|p| Supplied {
                value: env
                    .lookup(p.value)
                    .cloned()
                    .unwrap_or(Value::Unitus),
                name: Some(
                    p.value
                        .to_string(),
                ),
            })
            .collect();
        self.record_inputs(qualified, supplied)
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

    /// Record a `Finish` at the root path, closing a run that walked to its end.
    fn record_finish(&mut self) -> Result<(), RunnerError> {
        let run_id = self
            .appender
            .run_id();
        let record = Record {
            recorded: now_iso8601(),
            run_id,
            path: "/".to_string(),
            state: State::Finish,
        };
        self.appender
            .append(&record)
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
        Operation::Sequence(ops) | Operation::Prologue(ops) => ops
            .iter()
            .any(computable),
        Operation::Step { body, .. } | Operation::Section { body, .. } => computable(body),
        Operation::Prose(_) => false,
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

/// Render a procedure's qualified path with arguments bound to each parameter
/// in `value ~ name` form, e.g. `connectivity_check: ([] ~ e, 0 ~ s)`. The
/// qualified path already carries its trailing `:`.
fn render_argument_echo(
    qualified: &str,
    params: &[language::Identifier],
    env: &Environment,
) -> String {
    format!("{} ({})", qualified, render_bindings(params, env))
}

/// Append a loop iteration's bound variable(s) to its path in `value ~ name`
/// form, e.g. `cleanup_ec2:/3/[1] ("i-1234" ~ instance)` — a string value
/// shows quoted. A `repeat` with no iteration variable echoes the bare path.
fn render_iteration_echo(
    qualified: &str,
    names: &[language::Identifier],
    env: &Environment,
) -> String {
    if names.is_empty() {
        qualified.to_string()
    } else {
        format!("{} ({})", qualified, render_bindings(names, env))
    }
}

/// Comma-join a set of bindings in `value ~ name` form with each value read
/// from the environment.
fn render_bindings(names: &[language::Identifier], env: &Environment) -> String {
    let bindings: Vec<String> = names
        .iter()
        .map(|n| {
            let value = match env.lookup(n.value) {
                Some(value) => value.to_string(),
                None => String::new(),
            };
            format!("{} ~ {}", value, n.value)
        })
        .collect();
    bindings.join(", ")
}

/// Render each parameter's forma as a prompt display string. A single declared
/// list parameter (`[Region]`) renders bracketed so the driver offers list
/// entry; every other genus renders its bare element formae.
fn render_parameter_formae(signature: Option<&language::Signature>) -> Vec<String> {
    match signature.map(|s| &s.requires) {
        Some(genus @ language::Genus::List(_)) => {
            vec![crate::formatting::render_genus(
                genus,
                &crate::formatting::Identity,
            )]
        }
        Some(genus) => genus
            .formae()
            .iter()
            .map(|f| {
                f.value
                    .to_string()
            })
            .collect(),
        None => Vec::new(),
    }
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
            super::evaluator::parse_value(argument),
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
