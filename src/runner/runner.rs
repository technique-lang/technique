//! Interactive walker over a translated Program.

use std::collections::HashSet;
use std::io;

use super::context::Context;
use super::driver::{Driver, Kind, Offer, Question, Review, Standing, UserInput};
use super::evaluator::Environment;
use super::library::{Library, Nature};
use super::path::{PathSegment, QualifiedPath};
use crate::engraving::{
    Appender, InvokeTarget, Journal, Ledger, Position, Record, Serial, State, StoreError, Supplied,
};
use crate::language;
use crate::program::{
    Executable, ExecutableRef, Fragment, Invocable, Locale, Operation, Ordinal, Program,
    Subroutine, SubroutineRef,
};
use crate::value::Value;

/// A step's result. `Done(Value)` is the natural success — for a leaf Step the
/// user's recorded value, for a Sequence / Section / procedure body the unit
/// value once the whole subtree is finished. `Skip` and `Fail` are the user's
/// verdicts on individual Steps.
#[derive(Debug, Clone, PartialEq)]
pub enum Outcome {
    Done(Value),
    /// Carries the body's computed value for block semantics; recorded as no value.
    Skip(Value),
    Fail(Failure),
}

/// Wraps an `Outcome`, plus the control signals that propagate up the walk. A
/// failure thrown mid-body (i.e. a failed exec) is `Throwing`; it propagates
/// up to the enclosing step, which catches it and records a Fail. `Stopping`
/// shows that we are halting the walk immediately.
#[derive(Debug, Clone, PartialEq)]
pub enum Conclusion {
    Completed(Outcome),
    Throwing(Failure),
    Stopping,
    /// The user amended a recorded value. The walk unwinds without recording
    /// anything, and the run starts again from the top, replaying what still
    /// stands. Distinct from `Stopping`: it is not a stop, and it must not
    /// write the `Stop` lifecycle record that a stop writes mid-unwind.
    Restarting,
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
    Store(StoreError),
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
    InvalidCost,
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
    MalformedArgument {
        parameter: String,
        argument: String,
    },
    MalformedList {
        text: String,
    },
    TerminalRequired,
    UserQuit,
}

impl From<StoreError> for RunnerError {
    fn from(error: StoreError) -> Self {
        RunnerError::Store(error)
    }
}

/// Execute a Technique interactively by walking the `Program` tree. Tracks
/// the position in the document via a `QualifiedPath` stack, carries an
/// `Environment` with known result values. Holds the set of step FQNs already
/// completed in a *prior* run — the resume snapshot plus an append handle to
/// write results and the prompt the user interacts through.
pub struct Runner<'i, D: Driver> {
    program: &'i Program<'i>,
    appender: Appender,
    ledger: Ledger,
    driver: D,
    path: QualifiedPath<'i>,
    /// The scope the walk is standing in. Saved and restored around a descent
    /// exactly as `path` is.
    serial: Serial,
    /// Scopes this walk has entered itself. A recorded completion short-circuits
    /// a resume, but a scope the current walk just executed is not a prior
    /// recording — two calls to one procedure from the same scope share a route,
    /// and so an entry, yet both must run.
    entered: HashSet<Serial>,
    /// Bindings the current scope has made, flushed as its `Bind` record
    /// immediately before its outcome.
    bound: Vec<Supplied>,
    /// What a revoked step recorded binding, kept so the acquire prompts in its
    /// body open on the old value rather than an empty buffer. Taken before the
    /// step's `Begin`, which overwrites the entry.
    seeds: Vec<Supplied>,
    /// Every record this run has written, in order, seeded from the journal on
    /// resume. What review moves over. Never consulted to decide what to skip.
    records: Vec<Record>,
    /// The marker the next `Begin` belongs to, set by whichever path is about
    /// to record it.
    opening: &'static str,
    /// A verdict chosen at a reviewed position, waiting for the replay to reach
    /// it. Survives the restart, which is the whole point of it.
    amending: Option<(Serial, UserInput)>,
    /// How deep inside completed scopes the walk is replaying. While non-zero
    /// it descends and displays but takes no prompt, writes no record, and
    /// announces an `Execute` rather than dispatching it.
    replaying: usize,
    constraints: Vec<Value>,
    library: Library,
    context: Context,
    document: Option<String>,
}

impl<'i, D: Driver> Runner<'i, D> {
    pub fn new(
        program: &'i Program<'i>,
        appender: Appender,
        ledger: Ledger,
        driver: D,
        library: Library,
    ) -> Self {
        Runner {
            program,
            appender,
            ledger,
            driver,
            path: QualifiedPath::new(),
            serial: Serial::LIFECYCLE,
            entered: HashSet::new(),
            bound: Vec::new(),
            seeds: Vec::new(),
            records: Vec::new(),
            opening: "\u{2198}",
            amending: None,
            replaying: 0,
            constraints: Vec::new(),
            library,
            context: Context::native(false),
            document: None,
        }
    }

    /// Seed the journal review moves over: on a resume, every record the run has
    /// already written, so the walk can be looked back through as far as it
    /// goes rather than only as far as this session reached.
    pub fn with_records(mut self, records: Vec<Record>) -> Self {
        self.records = records;
        self
    }

    /// Name the source document so the run brackets its walk double arrow
    /// marked trail lines.
    pub fn with_document(mut self, document: String) -> Self {
        self.document = Some(document);
        self
    }

    /// Override the host context builtins write through (default: the terminal).
    pub fn with_context(mut self, context: Context) -> Self {
        self.context = context;
        self
    }

    /// Rebuild the runner for a fresh walk from the top, after the user amended
    /// a recorded value. What the process holds is kept — the driver's terminal
    /// state, the open append handle, the library, the context, and the folded
    /// ledger — and the position is reset.
    ///
    /// A restart rather than resuming at the amended step because the walker's
    /// position is a Rust call stack plus an `Environment` of every binding made
    /// along the way, and there is no way to construct that directly. Replaying
    /// from the top rebuilds it, short-circuiting whatever still stands.
    pub fn restart(self) -> Self {
        Runner {
            program: self.program,
            appender: self.appender,
            ledger: self.ledger,
            driver: self.driver,
            path: QualifiedPath::new(),
            serial: Serial::LIFECYCLE,
            entered: HashSet::new(),
            bound: Vec::new(),
            seeds: Vec::new(),
            records: self.records,
            opening: "\u{2198}",
            // Carried across: it is the answer given in review, and the walk
            // restarts precisely so it can be delivered.
            amending: self.amending,
            replaying: 0,
            constraints: Vec::new(),
            library: self.library,
            context: self.context,
            document: self.document,
        }
    }

    /// Consume the runner and return the inner driver after a run completes.
    /// Used to read a `Headless` driver's result count or to assert on the
    /// Mock's event log.
    pub fn into_driver(self) -> D {
        self.driver
    }

    /// Consume the runner and return the inner appender after a run completes.
    /// Used to read the recorded journal of an in-memory `Appender`.
    pub fn into_appender(self) -> Appender {
        self.appender
    }

    /// Walk the entry procedure top to bottom. Entry-procedure
    /// selection here is `program.subroutines[0]` — the synthetic
    /// anonymous wrapper if the document is top-level Steps, otherwise
    /// the first declared procedure.
    pub fn run(&mut self, mut env: Environment) -> Result<Conclusion, RunnerError> {
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
        }
        // An anonymous entry — a document that is a bare series of steps — has
        // no procedure segment, so it brackets itself at the root path.
        let qualified = self
            .path
            .render();
        let params = entry
            .parameters
            .unwrap_or(&[]);
        let supplied = self.restore_or_collect_inputs(&mut env, &qualified, params)?;
        // A run already sealed at its entry replays for display alone: beginning
        // it again would state a second execution of the whole run and unsettle
        // the outcome standing here. The walk from here down is a replay, so it
        // takes no prompt and writes nothing — neither a `Done` of its own nor a
        // second `Finish`.
        match self.recall(&qualified, &supplied) {
            Recall::Valid(serial, ..) => {
                self.enter_replayed(serial);
                self.replaying += 1;
            }
            _ => self.begin_scope(&qualified, supplied)?,
        }
        if let Some(name) = name {
            if params.is_empty() {
                self.driver
                    .enter(&qualified, "");
            } else {
                let echo = render_argument_echo(params, &env);
                self.driver
                    .enter(&qualified, &echo);
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
        // The entry is a structural scope: a completed run closes with a final
        // closing prompt at its path. A Quit or error walk skips it — the run
        // did not finish.
        let kind = self.kind_of_scope(&entry.body);
        let result = match result {
            Ok(Conclusion::Stopping) => Ok(Conclusion::Stopping),
            Ok(Conclusion::Restarting) => Ok(Conclusion::Restarting),
            Ok(Conclusion::Completed(outcome)) => self.seal_scope(&qualified, outcome, kind),
            Ok(Conclusion::Throwing(failure)) => {
                self.seal_scope(&qualified, Outcome::Fail(failure), kind)
            }
            Err(error) => Err(error),
        };
        if name.is_some() {
            self.path
                .pop();
        }
        // A run that walked to its end closes with a `Finish` record at the
        // root and the double arrow marker.
        if let Ok(conclusion) = &result {
            if let Conclusion::Stopping | Conclusion::Restarting = conclusion {
            } else {
                self.record_finish()?;
                if let Some(document) = &self.document {
                    let label = format!(
                        "/ {},1 #{}",
                        document,
                        self.appender
                            .run_id()
                            .render()
                    );
                    let verdict = verdict_from(conclusion);
                    self.driver
                        .conclude(&label, &verdict);
                }
            }
        }
        result
    }

    fn walk(
        &mut self,
        env: &mut Environment,
        op: &'i Operation<'i>,
    ) -> Result<Conclusion, RunnerError> {
        match op {
            Operation::Sequence(ops, _) => self.walk_sequence(env, ops),
            Operation::Prologue(ops, _) => {
                self.path
                    .push(PathSegment::Prologue);
                let qualified = self
                    .path
                    .render();
                let outer = self.serial;
                let result = self.perform_prologue(env, &qualified, ops);
                self.serial = outer;
                self.path
                    .pop();
                result
            }
            Operation::Section {
                numeral,
                title,
                body,
                ..
            } => {
                let outer = self.serial;
                let result = self.walk_section(env, numeral, title.as_deref(), body);
                self.serial = outer;
                result
            }
            // Every body the translator emits is a `Sequence`, so a Step is
            // always reached as one of its members, where `walk_sequence`
            // supplies the parallel ordinal counter. A bare Step never reaches
            // `walk` directly.
            Operation::Step { .. } => {
                unreachable!() // a Step is always walked as a Sequence member
            }
            Operation::Loop {
                names, over, body, ..
            } => self.walk_loop(env, names, over.as_deref(), body),
            Operation::Within { bound, body, .. } => self.walk_within(env, bound, body),
            // Walk the inner expression (so a `$(<call>)` runs), then
            // construct the Cost value from its result.
            Operation::Cost(inner, _) => match self.walk(env, inner)? {
                Conclusion::Completed(Outcome::Done(Value::Quanticle(numeric))) => Ok(
                    Conclusion::Completed(Outcome::Done(Value::Intratempse(numeric))),
                ),
                Conclusion::Completed(Outcome::Done(_)) => Err(RunnerError::InvalidCost),
                other => Ok(other),
            },
            Operation::Invoke(invocable, _) => {
                let outer = self.serial;
                let replaying = self.replaying;
                let result = self.walk_invoke(env, invocable);
                self.serial = outer;
                self.replaying = replaying;
                result
            }
            Operation::Execute(executable, _) => {
                let function = self.executable_name(&executable.target);
                let qualified = self
                    .path
                    .render();
                // A host call inside a completed scope has already happened.
                // Announce it so the replay reads as the run it retraces, and
                // do not dispatch it a second time.
                if self.replaying() {
                    self.driver
                        .announce(&describe_execute(&function));
                    return Ok(Conclusion::Completed(Outcome::Done(Value::Unitus)));
                }
                // `Command` (e.g. `exec()`) and `Instant` (e.g. `now()`)
                // builtins run on the host; `Command` is vetted on an
                // editable prompt, `Instant` runs unvetted (see below).
                // `Action` is a physical step the user confirms read-only.
                // Either way Skip or Fail declines and records the step; Quit
                // stops. `Pure` builtins just announce and run.
                let nature = self.executable_nature(executable);
                let kind = self.execute_kind(executable);
                // Pure builtins record nothing; only effectful calls are recorded.
                let effectful = if let Kind::Computable = kind {
                    false
                } else {
                    true
                };
                if effectful {
                    self.record(
                        &qualified,
                        State::Execute {
                            function: function.clone(),
                        },
                    )?;
                }
                let outcome = match kind {
                    // Nothing to vet, and cannot fail, so we skip the command
                    // prompt.
                    Kind::System if nature == Nature::Instant => {
                        let value = super::evaluator::dispatch(
                            &self.library,
                            &self.context,
                            env,
                            executable,
                            None,
                        )?;
                        Ok(Conclusion::Completed(Outcome::Done(value)))
                    }
                    Kind::System => {
                        let script = self.script_text(env, executable)?;
                        match self.lift(|driver| driver.command(&qualified, &script))? {
                            Answer::Done(chosen) => match super::evaluator::dispatch(
                                &self.library,
                                &self.context,
                                env,
                                executable,
                                Some(&[chosen]),
                            ) {
                                Ok(value) => Ok(Conclusion::Completed(Outcome::Done(value))),
                                // A non-zero exit throws to fail the step rather
                                // than aborting the run; the walk continues.
                                Err(RunnerError::CommandFailed(code)) => {
                                    Ok(Conclusion::Throwing(Failure::Aborted(format!(
                                        "External command exited with status {}",
                                        code
                                    ))))
                                }
                                Err(other) => Err(other),
                            },
                            Answer::Skip => Ok(Conclusion::Completed(Outcome::Skip(Value::Unitus))),
                            Answer::Fail(reason) => {
                                Ok(Conclusion::Throwing(Failure::Aborted(reason)))
                            }
                            Answer::Ended(conclusion) => Ok(conclusion),
                        }
                    }
                    Kind::Action => {
                        let (verb, value) = self.action_parts(env, executable)?;
                        match self
                            .lift(|driver| driver.action(&qualified, &function, &verb, &value))?
                        {
                            Answer::Done(_) => {
                                let value = super::evaluator::dispatch(
                                    &self.library,
                                    &self.context,
                                    env,
                                    executable,
                                    None,
                                )?;
                                Ok(Conclusion::Completed(Outcome::Done(value)))
                            }
                            Answer::Skip => Ok(Conclusion::Completed(Outcome::Skip(Value::Unitus))),
                            Answer::Fail(reason) => {
                                Ok(Conclusion::Throwing(Failure::Aborted(reason)))
                            }
                            Answer::Ended(conclusion) => Ok(conclusion),
                        }
                    }
                    Kind::Computable => {
                        self.driver
                            .announce(&describe_execute(&function));
                        let value = super::evaluator::dispatch(
                            &self.library,
                            &self.context,
                            env,
                            executable,
                            None,
                        )?;
                        Ok(Conclusion::Completed(Outcome::Done(value)))
                    }
                    _ => unreachable!(), // execute_kind yields only System/Action/Computable
                }?;
                // Pair the Execute with a Return carrying its value; a stopped
                // run leaves the enter unpaired.
                let unwinding = if let Conclusion::Stopping | Conclusion::Restarting = outcome {
                    true
                } else {
                    false
                };
                if effectful && !unwinding {
                    let returned = if let Conclusion::Completed(Outcome::Done(value)) = &outcome {
                        Some(value.clone())
                    } else {
                        None
                    };
                    self.record(&qualified, State::Return(returned))?;
                }
                Ok(outcome)
            }
            Operation::Bind {
                names,
                value,
                inferred,
                ..
            } => self.walk_bind(env, names, value, inferred.as_ref()),
            Operation::Variable(_, _)
            | Operation::Number(_, _)
            | Operation::Response(_, _)
            | Operation::String(_, _)
            | Operation::Verbatim(_, _)
            | Operation::Tablet(_, _)
            | Operation::List(_, _)
            | Operation::Tuple(_, _)
            | Operation::Prose(_, _)
            | Operation::Hole(_)
            | Operation::Unit(_) => {
                let value = super::evaluator::evaluate(&self.library, &self.context, env, op)?;
                Ok(Conclusion::Completed(Outcome::Done(value)))
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

    /// The shell script an `exec` will run, rendered for the user to see
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

    /// Echo a deferred external's arguments after its `<uri>` path in the
    /// `value ~ name` binding form. A bare variable shows its binding; any
    /// other expression shows its evaluated value. Not shown if ther eare no
    /// arguments.
    fn render_deferred_echo(
        &self,
        env: &mut Environment,
        arguments: &[Operation<'i>],
    ) -> Result<String, RunnerError> {
        if arguments.is_empty() {
            return Ok(String::new());
        }
        let mut parts = Vec::new();
        for arg in arguments {
            let value = super::evaluator::evaluate(&self.library, &self.context, env, arg)?;
            let part = if let Operation::Variable(id, _) = arg {
                format!("{} ~ {}", value, id.value)
            } else {
                value.to_string()
            };
            parts.push(part);
        }
        Ok(format!("({})", parts.join(", ")))
    }

    /// An action's parts for the user to confirm: its imperative verb (the
    /// library's `display` name, e.g. `Click`) and the value its single
    /// argument evaluates to.
    fn action_parts(
        &mut self,
        env: &mut Environment,
        executable: &'i Executable<'i>,
    ) -> Result<(String, Value), RunnerError> {
        let verb = match &executable.target {
            ExecutableRef::Resolved(id) => self
                .library
                .display(*id)
                .map(str::to_string)
                .unwrap_or_else(|| self.executable_name(&executable.target)),
            _ => self.executable_name(&executable.target),
        };
        let value = match executable
            .arguments
            .first()
        {
            Some(arg) => super::evaluator::evaluate(&self.library, &self.context, env, arg)?,
            None => Value::Unitus,
        };
        Ok((verb, value))
    }

    fn walk_invoke(
        &mut self,
        env: &mut Environment,
        invocable: &'i Invocable<'i>,
    ) -> Result<Conclusion, RunnerError> {
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
                    let formae = render_parameter_formae(subroutine.signature);

                    // A prior run's recorded arguments for this callee, in
                    // parameter order. A prompted argument (an elided call or a
                    // `?` hole) is restored from here rather than re-acquired;
                    // an argument the author supplied as a source expression is
                    // re-evaluated, so a loop variable still varies. Every
                    // argument, however it was arrived at, is what the callee's
                    // Begin states it started with.
                    // A revoked entry states the very argument being amended,
                    // so restoring from it would put the old value back and the
                    // amendment would silently do nothing. It is still what the
                    // prompt is seeded from: a default the user commits at the
                    // real prompt is an ordinary prompt.
                    let entry = self
                        .ledger
                        .look(self.serial, &lexical);
                    let revoked = match entry {
                        Some(entry) if entry.revoked => Some(
                            entry
                                .began
                                .clone(),
                        ),
                        _ => None,
                    };
                    let began = match entry {
                        Some(entry) if !entry.revoked => Some(
                            entry
                                .began
                                .clone(),
                        ),
                        _ => None,
                    };

                    // First pass: everything knowable without asking. A hole
                    // with nothing recorded stays open until the dispatch has
                    // been recorded, below.
                    let count = if invocable.elided {
                        subroutine.arity()
                    } else {
                        invocable
                            .arguments
                            .len()
                    };
                    let mut supplied: Vec<Option<Supplied>> = Vec::with_capacity(count);
                    for i in 0..count {
                        let bind = params
                            .get(i)
                            .map(|p| p.value);
                        let prompted = if invocable.elided {
                            true
                        } else if let Operation::Hole(_) = &invocable.arguments[i] {
                            true
                        } else {
                            false
                        };
                        let value = if prompted {
                            began
                                .as_ref()
                                .and_then(|r| r.get(i))
                                .map(|s| {
                                    s.value
                                        .clone()
                                })
                        } else {
                            Some(super::evaluator::evaluate(
                                &self.library,
                                &self.context,
                                env,
                                &invocable.arguments[i],
                            )?)
                        };
                        supplied.push(value.map(|value| Supplied {
                            value,
                            name: bind.map(|b| b.to_string()),
                        }));
                    }

                    // A procedure body reads nothing but its arguments, so
                    // those are a closed frontier: a recorded completion holds
                    // for everything beneath it while they agree.
                    let known: Option<Vec<Supplied>> = supplied
                        .iter()
                        .cloned()
                        .collect();
                    let recall = match &known {
                        Some(known) => self.recall(&lexical, known),
                        None => Recall::Nothing,
                    };
                    if let Recall::Stale = recall {
                        self.replaying = 0;
                    }
                    if let Recall::Valid(serial, outcome, _) = recall {
                        // Descend for display, in the callee's own environment
                        // rebuilt from its arguments. The callee's bindings are
                        // its own and do not escape.
                        self.enter_replayed(serial);
                        for item in known
                            .iter()
                            .flatten()
                        {
                            if let Some(name) = &item.name {
                                local.extend(
                                    name.clone(),
                                    item.value
                                        .clone(),
                                );
                            }
                        }
                        let saved = self
                            .path
                            .replace(lexical_segments);
                        let outer = self.replaying;
                        self.replaying = outer + 1;
                        self.announce_procedure(subroutine, name, &lexical, &local);
                        let result = self.walk(&mut local, &subroutine.body);
                        self.replaying = outer;
                        self.path
                            .replace(saved);
                        result?;
                        self.leave_replayed(serial, &lexical, "\u{2199}", &verdict_of(&outcome));
                        return Ok(Conclusion::Completed(Outcome::Done(value_of(&outcome))));
                    }

                    // A call the run being retraced never settled is passed
                    // over: replaying descends through what happened, and this
                    // did not.
                    if self.replaying() {
                        return Ok(Conclusion::Completed(Outcome::Done(Value::Unitus)));
                    }

                    // Acquire deferred arguments at the call site, in the
                    // invocation's `<name>` form.
                    let caller = self
                        .path
                        .render();
                    let invoked = format!("<{}>", name);

                    // Record the dispatch on arrival, before any argument is
                    // solicited, so that the time the user takes supplying one
                    // falls between this and the Begin that follows. Declining
                    // at the prompt settles Skip or Fail at the callee's path,
                    // which stands as the record of a call that was dispatched
                    // and turned down.
                    // The dispatch is written when the `Begin` it introduces
                    // is, so a re-entry records neither.
                    let introduces = match &known {
                        Some(known) => {
                            let serial = self
                                .ledger
                                .serial_for(self.serial, &lexical);
                            !self
                                .ledger
                                .standing(serial, &lexical, known)
                        }
                        None => true,
                    };
                    if introduces {
                        self.record(
                            &caller,
                            State::Invoke(InvokeTarget::Procedure(name.to_string())),
                        )?;
                    }

                    // Second pass: solicit whatever is still open.
                    let mut settled: Vec<Supplied> = Vec::with_capacity(count);
                    for (i, item) in supplied
                        .into_iter()
                        .enumerate()
                    {
                        let item = match item {
                            Some(item) => item,
                            None => {
                                let bind = params
                                    .get(i)
                                    .map(|p| p.value);
                                let forma = formae
                                    .get(i)
                                    .map(|s| s.as_str());
                                let seed = revoked
                                    .as_ref()
                                    .and_then(|r| r.get(i))
                                    .map(|s| &s.value);
                                let value =
                                    match self.solicit(&caller, &invoked, bind, forma, seed)? {
                                        Conclusion::Completed(Outcome::Done(value)) => value,
                                        other => {
                                            return self.abandon(&lexical, settled, other);
                                        }
                                    };
                                Supplied {
                                    value,
                                    name: bind.map(|b| b.to_string()),
                                }
                            }
                        };
                        if let Some(name) = &item.name {
                            local.extend(
                                name.clone(),
                                item.value
                                    .clone(),
                            );
                        }
                        settled.push(item);
                    }

                    self.begin_scope(&lexical, settled)?;

                    let saved = self
                        .path
                        .replace(lexical_segments);
                    self.announce_procedure(subroutine, name, &lexical, &local);

                    // Walk the callee's body in its own `local` environment,
                    // then close its scope; a Quit or error skips the close,
                    // leaving the procedure unfinished. The close is taken back
                    // at the caller's depth: it is the call, and review reaches
                    // the body through it.
                    let result = self.walk(&mut local, &subroutine.body);
                    let kind = self.kind_of_scope(&subroutine.body);
                    let sealed = match result {
                        Ok(Conclusion::Stopping) => Ok(Conclusion::Stopping),
                        Ok(Conclusion::Restarting) => Ok(Conclusion::Restarting),
                        Ok(Conclusion::Completed(outcome)) => {
                            self.seal_scope(&lexical, outcome, kind)
                        }
                        Ok(Conclusion::Throwing(failure)) => {
                            self.seal_scope(&lexical, Outcome::Fail(failure), kind)
                        }
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
                Ok(Conclusion::Completed(Outcome::Done(Value::Unitus)))
            }
            SubroutineRef::Deferred(ext) => {
                // An external target lives in another document or system, so
                // this run cannot descend into it. Record the call site, then
                // present the invocation as its own node for the user to
                // decide: Done if they performed (or recorded elsewhere) the
                // external procedure, otherwise Skip or Fail. An unattended
                // (automatic) run records Skip — nothing executed it and no one
                // is present to attest it, so it is not marked Done.
                let caller = self
                    .path
                    .render();

                self.path
                    .push(PathSegment::External(ext.value));
                let qualified = self
                    .path
                    .render();
                // An external target has no body in this document, so a
                // completed one shows its recorded verdict and nothing more.
                if let Recall::Valid(serial, outcome, _) = self.recall(&qualified, &[]) {
                    self.enter_replayed(serial);
                    self.leave_replayed(serial, &qualified, "⇐", &verdict_of(&outcome));
                    self.path
                        .pop();
                    return Ok(Conclusion::Completed(Outcome::Done(value_of(&outcome))));
                }
                if self.replaying() {
                    self.path
                        .pop();
                    return Ok(Conclusion::Completed(Outcome::Done(Value::Unitus)));
                }

                let serial = self
                    .ledger
                    .serial_for(self.serial, &qualified);
                if !self
                    .ledger
                    .standing(serial, &qualified, &[])
                {
                    self.record(
                        &caller,
                        State::Invoke(InvokeTarget::Uri(
                            ext.value
                                .to_string(),
                        )),
                    )?;
                }
                self.begin_scope(&qualified, Vec::new())?;
                // Prompt at the departure, echoing the arguments flowing into
                // the external Technque.
                let echo = self.render_deferred_echo(env, &invocable.arguments)?;
                let embarked = self
                    .lift(|driver| driver.depart(&qualified, &echo))?
                    .completed();
                let conclusion = match embarked {
                    Conclusion::Completed(Outcome::Done(_)) => self
                        .lift(|driver| driver.external(&qualified))?
                        .completed(),
                    declined => declined,
                };
                if let Conclusion::Completed(_) = &conclusion {
                    self.driver
                        .show_verdict("⇐", &qualified, &verdict_from(&conclusion));
                    self.record_outcome(&qualified, record_state(&conclusion))?;
                }
                self.path
                    .pop();
                Ok(conclusion)
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
    ) -> Result<Conclusion, RunnerError> {
        let descriptive = if let Operation::Sequence(ops, _) = value {
            ops.is_empty()
        } else {
            false
        };
        if descriptive {
            // On a replay the value was solicited once already and comes back
            // from the enclosing scope's recorded `Bind`, so the prompt is not
            // put to the user a second time.
            if self.replaying() {
                return Ok(Conclusion::Completed(Outcome::Done(Value::Unitus)));
            }
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
            let mut acquired = Vec::with_capacity(names.len());
            for name in names {
                let seed = self
                    .seeds
                    .iter()
                    .find(|item| match &item.name {
                        Some(bound) => bound == name.value,
                        None => false,
                    })
                    .map(|item| {
                        item.value
                            .clone()
                    });
                let value =
                    match self.solicit(&qualified, "", Some(name.value), forma, seed.as_ref())? {
                        Conclusion::Completed(Outcome::Done(value)) => value,
                        // A skipped binding still binds, so a later reference
                        // to the name resolves.
                        Conclusion::Completed(Outcome::Skip(value)) => {
                            for name in names {
                                super::evaluator::bind_names(
                                    env,
                                    std::slice::from_ref(name),
                                    Value::Unitus,
                                )?;
                            }
                            return Ok(Conclusion::Completed(Outcome::Skip(value)));
                        }
                        other => return Ok(other),
                    };
                acquired.push(value);
            }
            for (name, value) in names
                .iter()
                .zip(&acquired)
            {
                super::evaluator::bind_names(env, std::slice::from_ref(name), value.clone())?;
                self.note_binding(name.value, value);
            }
            Ok(Conclusion::Completed(Outcome::Done(Value::Unitus)))
        } else {
            // Walk rather than evaluate: the bound value may be an effectful
            // spine operation — an `Invoke` that must descend into its callee
            // interactively, an `Execute` that must be gated, a `Loop` — which
            // the evaluator would mishandle as Unit. Walking a pure value is
            // equivalent to evaluating it.
            match self.walk(env, value)? {
                Conclusion::Completed(Outcome::Done(value)) => {
                    super::evaluator::bind_names(env, names, value.clone())?;
                    for name in names {
                        self.note_binding(
                            name.value,
                            env.lookup(name.value)
                                .unwrap_or(&Value::Unitus),
                        );
                    }
                    Ok(Conclusion::Completed(Outcome::Done(Value::Unitus)))
                }
                Conclusion::Completed(Outcome::Skip(_)) => {
                    super::evaluator::bind_names(env, names, Value::Unitus)?;
                    Ok(Conclusion::Completed(Outcome::Skip(Value::Unitus)))
                }
                // A failure, a stop or a restart binds nothing and propagates
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
    ) -> Result<Conclusion, RunnerError> {
        // Iterations a prior walk recorded in this scope, to be reused as the
        // loop meets items matching them. Entries leave the pool as they are
        // claimed, so multiplicity is preserved: a list of three items runs
        // three times whatever was recorded, and three identical items against
        // two recorded matches runs one fresh.
        let mut pool: Vec<Iteration> = self
            .ledger
            .iterations(self.serial)
            .into_iter()
            .map(|(number, entry)| Iteration {
                number,
                began: entry
                    .began
                    .clone(),
                complete: entry
                    .outcome
                    .is_some(),
            })
            .collect();
        // Sibling loops in one scope share this numbering, so the second loop
        // continues where the first left off rather than colliding with it.
        let mut highest = pool
            .iter()
            .map(|item| item.number)
            .max()
            .unwrap_or(0);

        match over {
            None => {
                loop {
                    // `repeat` binds no item, so there is nothing to match on
                    // and reuse stays positional.
                    let number = match pool.is_empty() {
                        true => {
                            highest += 1;
                            highest
                        }
                        false => {
                            pool.remove(0)
                                .number
                        }
                    };
                    match self.walk_iteration(env, names, number, body)? {
                        Conclusion::Stopping => return Ok(Conclusion::Stopping),
                        Conclusion::Restarting => return Ok(Conclusion::Restarting),
                        _ => {}
                    }
                }
            }
            Some(expr) => {
                // A collection naming an as-yet-unbound variable — e.g. a list
                // a zero-iteration or skipped earlier loop never populated —
                // iterates nothing rather than aborting the run. The name is
                // statically in scope (resolution guarantees it); it simply has
                // no value yet at runtime.
                if let Operation::Variable(id, _) = expr {
                    if env
                        .lookup(id.value)
                        .is_none()
                    {
                        return Ok(Conclusion::Completed(Outcome::Done(Value::Unitus)));
                    }
                }
                let value = super::evaluator::evaluate(&self.library, &self.context, env, expr)?;
                let items = super::evaluator::coerce_to_list(value)?;
                let mut rollup = Rollup::new();
                for item in items {
                    super::evaluator::bind_names(env, names, item)?;
                    let number =
                        claim_iteration(&mut pool, &iteration_values(names, env), &mut highest);
                    match self.walk_iteration(env, names, number, body)? {
                        Conclusion::Stopping => return Ok(Conclusion::Stopping),
                        Conclusion::Restarting => return Ok(Conclusion::Restarting),
                        Conclusion::Throwing(f) => return Ok(Conclusion::Throwing(f)),
                        Conclusion::Completed(other) => rollup.absorb(other),
                    }
                }
                // A loop yields unit, so discard the rolled-up value while keeping its verdict.
                match rollup.settle() {
                    Outcome::Done(_) => Ok(Conclusion::Completed(Outcome::Done(Value::Unitus))),
                    Outcome::Skip(_) => Ok(Conclusion::Completed(Outcome::Skip(Value::Unitus))),
                    other => Ok(Conclusion::Completed(other)),
                }
            }
        }
    }

    /// Walk a `within` block's body once. The budget is evaluated up front
    /// and pushed onto `self.constraints` for the duration of the body walk,
    /// so every enclosed step's prompt line can announce it.
    fn walk_within(
        &mut self,
        env: &mut Environment,
        bound: &'i Operation<'i>,
        body: &'i Operation<'i>,
    ) -> Result<Conclusion, RunnerError> {
        let budget = super::evaluator::evaluate(&self.library, &self.context, env, bound)?;
        self.constraints
            .push(budget);
        let result = self.walk(env, body);
        self.constraints
            .pop();
        result
    }

    /// Walk one pass of a loop body within its `[number]` iteration scope.
    fn walk_iteration(
        &mut self,
        env: &mut Environment,
        names: &'i [language::Identifier<'i>],
        number: usize,
        body: &'i Operation<'i>,
    ) -> Result<Conclusion, RunnerError> {
        self.path
            .push(PathSegment::Iteration(number));
        let qualified = self
            .path
            .render();
        let outer = self.serial;
        let replaying = self.replaying;
        let result = self.perform_iteration(env, names, &qualified, body);
        self.serial = outer;
        self.replaying = replaying;
        self.path
            .pop();
        result
    }

    /// Bracket one iteration `Begin ( item ~ name )`…outcome and walk its
    /// body, with `↘`/`↙` chrome echoing the loop variable(s) bound for this
    /// pass in the same form a procedure call's arguments take. An iteration
    /// takes no prompt of its own: its outcome is what its body rolled up to.
    fn perform_iteration(
        &mut self,
        env: &mut Environment,
        names: &'i [language::Identifier<'i>],
        qualified: &str,
        body: &'i Operation<'i>,
    ) -> Result<Conclusion, RunnerError> {
        let supplied = iteration_values(names, env);
        let echo = render_iteration_echo(names, env);
        match self.recall(qualified, &supplied) {
            Recall::Stale => self.replaying = 0,
            Recall::Nothing if self.replaying() => {
                return Ok(Conclusion::Completed(Outcome::Done(Value::Unitus)));
            }
            Recall::Nothing => {}
            Recall::Valid(serial, outcome, bound) => {
                self.enter_replayed(serial);
                self.driver
                    .enter(qualified, &echo);
                self.replay(env, body, &bound)?;
                self.leave_replayed(serial, qualified, "\u{2199}", &verdict_of(&outcome));
                return Ok(Conclusion::Completed(Outcome::Done(value_of(&outcome))));
            }
        }

        self.begin_scope(qualified, supplied)?;
        self.driver
            .enter(qualified, &echo);
        let result = self.walk(env, body);
        // A stopped or errored pass leaves its Begin unpaired, which is what
        // marks the iteration to be redone.
        if let Ok(conclusion) = &result {
            if let Conclusion::Stopping | Conclusion::Restarting = conclusion {
            } else {
                self.record_outcome(qualified, record_state(conclusion))?;
                self.driver
                    .show_verdict("\u{2199}", qualified, &verdict_from(conclusion));
            }
        }
        result
    }

    fn walk_sequence(
        &mut self,
        env: &mut Environment,
        ops: &'i [Operation<'i>],
    ) -> Result<Conclusion, RunnerError> {
        let mut parallel_idx: usize = 0;
        let mut rollup = Rollup::new();
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
            // A prose child contributes its value but no verdict, so it cannot
            // make an otherwise-skipped sequence roll up as Done.
            if let Operation::Prose(_, _) = op {
                if let Conclusion::Completed(Outcome::Done(value)) = outcome {
                    rollup.observe(value);
                }
                continue;
            }
            match outcome {
                // Stopped and Throw abandon the sequence at once; a Fail rolls up.
                Conclusion::Stopping => return Ok(Conclusion::Stopping),
                Conclusion::Restarting => return Ok(Conclusion::Restarting),
                Conclusion::Throwing(failure) => return Ok(Conclusion::Throwing(failure)),
                Conclusion::Completed(other) => rollup.absorb(other),
            }
        }
        Ok(Conclusion::Completed(rollup.settle()))
    }

    fn walk_section(
        &mut self,
        env: &mut Environment,
        numeral: &'i str,
        title: Option<&'i Operation<'i>>,
        body: &'i Operation<'i>,
    ) -> Result<Conclusion, RunnerError> {
        self.path
            .push(PathSegment::Section(numeral));
        let qualified = self
            .path
            .render();
        // A Section's `Begin` is empty and `names_read` stops at one, so its
        // own guard says nothing; what is stale beneath it is caught by the
        // guards on the way down.
        if let Recall::Valid(serial, outcome, bound) = self.recall(&qualified, &[]) {
            // Descend rather than return: a completed Section's `Begin` is
            // empty, so its own guard can say nothing about the work nested
            // beneath it and only the walk can reach it.
            self.enter_replayed(serial);
            let outer = self.replaying;
            self.replaying = outer + 1;
            let result = self.perform_section(env, numeral, title, body);
            self.replaying = outer;
            result?;
            for item in &bound {
                if let Some(name) = &item.name {
                    env.extend(
                        name.clone(),
                        item.value
                            .clone(),
                    );
                }
            }
            self.leave_replayed(serial, &qualified, "\u{2199}", &verdict_of(&outcome));
            self.path
                .pop();
            return Ok(Conclusion::Completed(Outcome::Done(value_of(&outcome))));
        }
        self.begin_scope(&qualified, Vec::new())?;
        let result = self.perform_section(env, numeral, title, body);
        self.path
            .pop();
        let kind = self.kind_of_scope(body);
        // A section is a structural scope: the user signs it off at its
        // close before the next sibling runs. A Quit or error walk skips the
        // prompt — the section did not complete.
        match result {
            Ok(Conclusion::Stopping) => Ok(Conclusion::Stopping),
            Ok(Conclusion::Restarting) => Ok(Conclusion::Restarting),
            Ok(Conclusion::Completed(outcome)) => self.seal_scope(&qualified, outcome, kind),
            Ok(Conclusion::Throwing(failure)) => {
                self.seal_scope(&qualified, Outcome::Fail(failure), kind)
            }
            Err(error) => Err(error),
        }
    }

    fn perform_section(
        &mut self,
        env: &mut Environment,
        numeral: &'i str,
        title: Option<&'i Operation<'i>>,
        body: &'i Operation<'i>,
    ) -> Result<Conclusion, RunnerError> {
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
    ) -> Result<Conclusion, RunnerError> {
        let Operation::Step {
            ordinal,
            attributes,
            ..
        } = op
        else {
            unreachable!(); // walk_step called with non-Step operation
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

        let outer = self.serial;
        let replaying = self.replaying;
        let seeds = std::mem::take(&mut self.seeds);
        let result = self.perform_step(env, &qualified, op);
        self.serial = outer;
        self.replaying = replaying;
        self.seeds = seeds;

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
    // commands; unlike a step it takes no closing prompt of its own, folding its
    // outcome into the enclosing procedure's seal.
    fn perform_prologue(
        &mut self,
        env: &mut Environment,
        qualified: &str,
        ops: &'i [Operation<'i>],
    ) -> Result<Conclusion, RunnerError> {
        // A prologue reads only what its procedure was called with, and the
        // procedure's own guard covers that, so its `Begin` is empty.
        if let Recall::Valid(serial, outcome, bound) = self.recall(qualified, &[]) {
            self.serial = serial;
            let outer = self.replaying;
            self.replaying = outer + 1;
            let result = self.walk_sequence(env, ops);
            self.replaying = outer;
            result?;
            for item in &bound {
                if let Some(name) = &item.name {
                    env.extend(
                        name.clone(),
                        item.value
                            .clone(),
                    );
                }
            }
            return Ok(Conclusion::Completed(Outcome::Done(value_of(&outcome))));
        }

        self.begin_scope(qualified, Vec::new())?;
        let conclusion = self.walk_sequence(env, ops)?;
        if let Conclusion::Stopping | Conclusion::Restarting = conclusion {
            return Ok(conclusion);
        }
        // Translation emits a Prologue only when the description carries real
        // work (prose-only descriptions never become step 0)
        self.record_outcome(qualified, record_state(&conclusion))?;
        Ok(conclusion)
    }

    fn perform_step(
        &mut self,
        env: &mut Environment,
        qualified: &str,
        op: &'i Operation<'i>,
    ) -> Result<Conclusion, RunnerError> {
        let Operation::Step {
            source,
            body,
            responses,
            ..
        } = op
        else {
            // perform_step called with non-Step operation
            unreachable!();
        };
        let reads = read_values(body, env);
        match self.recall(qualified, &reads) {
            // An amended input reaches its consumers by re-execution, so a
            // stale step runs for real even inside a scope being replayed.
            // That is how an amendment reaches beneath a completed Section.
            Recall::Stale => self.replaying = 0,
            Recall::Nothing if self.replaying() => {
                // The run being retraced never settled this step — a sibling
                // below a failure, say. Pass over it rather than prompting for
                // work that is not being redone.
                return Ok(Conclusion::Completed(Outcome::Done(Value::Unitus)));
            }
            Recall::Nothing => {}
            Recall::Valid(serial, outcome, bound) => {
                // A replayed step shows itself and descends, so the user watching
                // sees the work being passed over and the guard reaches the scopes
                // nested within it — but nothing is prompted for or recorded, and
                // the bindings it made come from the journal rather than from walking
                // the body again.
                //
                // Standing at the recorded serial is what makes those descendants
                // reachable — they are keyed under it, not under the caller.
                self.enter_replayed(serial);
                self.display_step(env, source, qualified);
                self.replay(env, body, &bound)?;
                self.leave_replayed(serial, qualified, "→", &verdict_of(&outcome));
                return Ok(Conclusion::Completed(Outcome::Done(value_of(&outcome))));
            }
        }

        // A revoked step's recorded bindings, for its acquire prompts to open
        // on. Taken here because the `Begin` below rebuilds the entry.
        self.seeds = match self
            .ledger
            .look(self.serial, qualified)
        {
            Some(entry) if entry.revoked => entry
                .bound
                .clone(),
            _ => Vec::new(),
        };

        // Mark the start of work on this step before walking its body,
        // so any Invoke/Execute records emitted by the body land between
        // this Begin and the eventual outcome record.
        self.allocate(qualified);
        self.opening = "\u{2192}";
        self.record(qualified, State::Begin(reads))?;

        self.display_step(env, source, qualified);

        // A descriptive binding on a step with response choices takes its value
        // from the chosen response, not a separate acquire: skip the body walk
        // and bind the choice (taken below) to the step's name(s).
        let binding_via_response = !responses.is_empty() && binds_descriptively(body);

        let produced = if binding_via_response {
            Value::Unitus
        } else {
            match self.walk(env, body)? {
                Conclusion::Stopping => return Ok(Conclusion::Stopping),
                Conclusion::Restarting => return Ok(Conclusion::Restarting),
                Conclusion::Completed(Outcome::Done(value)) => value,
                // A rolled-up child failure signs off through `overrule`: the
                // failure stands and propagates by default, but an interactive
                // run may Override it to Done, severing the rollup.
                Conclusion::Completed(Outcome::Fail(_)) => {
                    let question = Question {
                        qualified,
                        marker: "→",
                        standing: Standing::Fail,
                        kind: Kind::Prose,
                        produced: Value::Unitus,
                        reviewable: self.reviewable(),
                    };
                    let conclusion = self.settle(question, &[], "→")?;
                    if let Conclusion::Completed(_) = &conclusion {
                        self.driver
                            .show_verdict("→", qualified, &verdict_from(&conclusion));
                        self.record_outcome(qualified, record_state(&conclusion))?;
                    }
                    return Ok(conclusion);
                }
                // The body recorded itself — a declined command beat (Skip) or a
                // thrown exec failure (caught here as a Fail). Record and show
                // its verdict without an acceptance prompt.
                settled => {
                    let outcome = match settled {
                        Conclusion::Throwing(failure) => Outcome::Fail(failure),
                        Conclusion::Completed(other) => other,
                        // Stopping and Restarting both returned above
                        Conclusion::Stopping | Conclusion::Restarting => unreachable!(),
                    };
                    let conclusion = Conclusion::Completed(outcome);
                    self.record_outcome(qualified, record_state(&conclusion))?;
                    let verdict = match &conclusion {
                        Conclusion::Completed(Outcome::Skip(_)) => UserInput::Skip,
                        Conclusion::Completed(Outcome::Fail(Failure::Aborted(reason))) => {
                            UserInput::Fail(reason.clone())
                        }
                        // only Skip and Fail reach the settled branch
                        _ => unreachable!(),
                    };
                    self.driver
                        .show_verdict("→", qualified, &verdict);
                    return Ok(conclusion);
                }
            }
        };

        // A descriptive binding already took the user's input at its acquire
        // prompt; that value (or a bare <Enter>) is the step's verdict, so
        // record Done without a redundant acceptance prompt.
        if responses.is_empty() && binds_descriptively(body) {
            let conclusion = Conclusion::Completed(Outcome::Done(produced));
            self.driver
                .show_verdict("→", qualified, &verdict_from(&conclusion));
            self.record_outcome(qualified, record_state(&conclusion))?;
            return Ok(conclusion);
        }

        let choices: Vec<&str> = responses
            .iter()
            .map(|r| r.value)
            .collect();
        let kind = self.kind_of_step(op);
        let question = Question {
            qualified,
            marker: "→",
            standing: Standing::Done,
            kind,
            produced,
            reviewable: self.reviewable(),
        };
        // Quit halts the walk; this step's Begin stands without a matching
        // outcome, so resume re-runs it.
        let conclusion = self.settle(question, &choices, "→")?;
        if let Conclusion::Completed(_) = &conclusion {
        } else {
            return Ok(conclusion);
        }

        self.driver
            .show_verdict("→", qualified, &verdict_from(&conclusion));
        // Bind the chosen response to the step's name(s); a skip binds Unitus
        // so a later reference resolves, mirroring a descriptive acquire.
        if binding_via_response {
            if let Some(names) = binding_names(body) {
                if let Conclusion::Completed(outcome) = &conclusion {
                    let bound = match outcome {
                        Outcome::Done(value) => Some(value.clone()),
                        Outcome::Skip(_) => Some(Value::Unitus),
                        _ => None,
                    };
                    if let Some(value) = bound {
                        super::evaluator::bind_names(env, names, value)?;
                        for name in names {
                            self.note_binding(
                                name.value,
                                env.lookup(name.value)
                                    .unwrap_or(&Value::Unitus),
                            );
                        }
                    }
                }
            }
        }
        self.record_outcome(qualified, record_state(&conclusion))?;
        Ok(conclusion)
    }

    /// Take a verdict at an Enter-site, entering the review loop and asking
    /// again as many times as the user asks for it.
    ///
    /// The site is remembered once it settles, so review reaches back over what
    /// the walk has passed but never over the node being prompted.
    fn settle(
        &mut self,
        question: Question<'_>,
        choices: &[&str],
        marker: &str,
    ) -> Result<Conclusion, RunnerError> {
        let qualified = question
            .qualified
            .to_string();
        let serial = self.serial;
        // A verdict already chosen for this position in review settles it
        // without asking: the answer was given there.
        let mut chosen = match &self.amending {
            Some((at, _)) if *at == serial => self
                .amending
                .take()
                .map(|(_, verdict)| verdict),
            _ => None,
        };
        // `ask` consumes the produced value, which a Skip propagates.
        let propagate = question
            .produced
            .clone();
        let mut standing = question.standing;
        let mut kind = question.kind;
        let mut produced = question.produced;
        let mut reviewable = question.reviewable;
        loop {
            let offers = self.offers(standing);
            let input = match chosen.take() {
                Some(verdict) => verdict,
                None => {
                    let question = Question {
                        qualified: &qualified,
                        marker,
                        standing,
                        kind,
                        produced: produced.clone(),
                        reviewable,
                    };
                    self.driver
                        .ask(question, choices, &offers)
                }
            };
            return Ok(match input {
                UserInput::Done(value) => Conclusion::Completed(Outcome::Done(value)),
                UserInput::Skip => Conclusion::Completed(Outcome::Skip(propagate.clone())),
                UserInput::Fail(reason) => {
                    Conclusion::Completed(Outcome::Fail(Failure::Aborted(reason)))
                }
                UserInput::Override => Conclusion::Completed(Outcome::Done(Value::Unitus)),
                UserInput::Quit => self.record_stop()?,
                UserInput::Review => match self.review()? {
                    Reviewed::Amended => Conclusion::Restarting,
                    Reviewed::Quit => self.record_stop()?,
                    // The same question again, its produced value having been
                    // consumed by the prompt just left.
                    Reviewed::Left => {
                        standing = offers_standing(&offers);
                        kind = Kind::Prose;
                        produced = Value::Unitus;
                        reviewable = self.reviewable();
                        continue;
                    }
                },
            });
        }
    }

    /// Take the driver's `UserInput` up into the runner's `Answer`, putting the
    /// prompt again for as long as the user is away in the review cursor.
    fn lift(&mut self, mut prompt: impl FnMut(&mut D) -> UserInput) -> Result<Answer, RunnerError> {
        loop {
            return Ok(match prompt(&mut self.driver) {
                UserInput::Done(value) => Answer::Done(value),
                UserInput::Skip => Answer::Skip,
                UserInput::Fail(reason) => Answer::Fail(reason),
                UserInput::Override => Answer::Done(Value::Unitus),
                UserInput::Quit => Answer::Ended(self.record_stop()?),
                UserInput::Review => match self.review()? {
                    Reviewed::Amended => Answer::Ended(Conclusion::Restarting),
                    Reviewed::Quit => Answer::Ended(self.record_stop()?),
                    Reviewed::Left => continue,
                },
            });
        }
    }

    fn solicit(
        &mut self,
        qualified: &str,
        text: &str,
        name: Option<&str>,
        forma: Option<&str>,
        seed: Option<&Value>,
    ) -> Result<Conclusion, RunnerError> {
        Ok(self
            .lift(|driver| driver.acquire(qualified, text, name, forma, seed))?
            .completed())
    }

    /// Run the review cursor over the Enter-sites this walk has passed, until
    /// the user leaves it or amends one. `true` means they amended: the
    /// `Revoke` is written and the walk is to restart.
    ///
    /// The walker's position does not move — it is a Rust call stack, which
    /// cannot be rewound — so this is a modal loop at the live prompt.
    fn review(&mut self) -> Result<Reviewed, RunnerError> {
        // The cursor reads the journal, and the loop below writes to it, so it
        // moves over a copy taken when review opens. Amending ends review, so
        // the copy cannot go stale underneath the cursor.
        let records = self
            .records
            .clone();
        let journal = Journal::new(&records);
        let mut at = match journal.last() {
            Some(at) => at,
            None => return Ok(Reviewed::Left),
        };
        loop {
            let record = match at {
                Position::At(i) => &records[i],
                Position::Live => return Ok(Reviewed::Left),
            };
            let qualified = record
                .path
                .clone();
            let serial = record.serial;
            let settled = settled_by(&record.state);
            let marker = marker_of(record);
            let offers = reviewing(settled.as_ref());
            let motion = match self
                .driver
                .review(marker, &qualified, settled.as_ref(), &offers)
            {
                Review::Move(motion) => motion,
                Review::Chose(offer) => match offer {
                    Offer::Quit => return Ok(Reviewed::Quit),
                    // The answer was given here, so the position is not asked
                    // again: the walk restarts and settles it on the way back
                    // through, and what depended on it is redone.
                    Offer::Skip => {
                        self.amend(serial, &qualified, UserInput::Skip)?;
                        return Ok(Reviewed::Amended);
                    }
                    Offer::Override => {
                        self.amend(serial, &qualified, UserInput::Override)?;
                        return Ok(Reviewed::Amended);
                    }
                    // Edit has no value and Fail no reason until someone types
                    // one, so these two withdraw and let the replay ask.
                    Offer::Edit | Offer::Fail => {
                        self.revoke(serial, &qualified)?;
                        return Ok(Reviewed::Amended);
                    }
                },
                Review::Leave => return Ok(Reviewed::Left),
                Review::Quit => return Ok(Reviewed::Quit),
            };
            // A refusal changes nothing; Down off the last record is the one
            // motion that ends review, and it is the way back to the prompt.
            match journal.step(at, motion) {
                Some(Position::Live) => return Ok(Reviewed::Left),
                Some(next) => at = next,
                None => {}
            }
        }
    }

    /// Withdraw a recorded value. The `Revoke` reaches the file before the
    /// restart, so a crash in between leaves a resumable state that redoes the
    /// step rather than one that has lost the amendment.
    ///
    /// Nothing is collected here: correcting a value is a `Revoke` plus
    /// ordinary re-execution. The walk restarts, replays what still stands,
    /// arrives at the step and prompts exactly as it did the first time.
    fn revoke(&mut self, serial: Serial, qualified: &str) -> Result<(), RunnerError> {
        self.stamp(serial, qualified, State::Revoke)
    }

    /// Withdraw a recorded value and say what replaces it. The verdict is held
    /// in memory across the restart and settles the position when the replay
    /// reaches it; a crash in between leaves the `Revoke` on disk, so the
    /// position is redone by asking rather than silently keeping the old value.
    fn amend(
        &mut self,
        serial: Serial,
        qualified: &str,
        verdict: UserInput,
    ) -> Result<(), RunnerError> {
        self.revoke(serial, qualified)?;
        self.amending = Some((serial, verdict));
        Ok(())
    }

    /// Stand at a position the walk is replaying. The records it wrote the
    /// first time are already in the journal, so review reaches it without the
    /// replay having to announce itself.
    fn enter_replayed(&mut self, serial: Serial) {
        self.serial = serial;
    }

    /// Show a replayed position's recorded verdict.
    fn leave_replayed(
        &mut self,
        serial: Serial,
        qualified: &str,
        marker: &'static str,
        verdict: &UserInput,
    ) {
        let _ = serial;
        self.driver
            .show_verdict(marker, qualified, verdict);
    }

    /// The actions legal where the walk is standing. `Override` only where a
    /// child failed; `Edit` is offered wherever there is a value, the UI greying
    /// it when the value has no editable shape.
    ///
    /// Invariant: the set always contains the standing default, so a driver
    /// that reads the standing rather than the offers can never be handed a
    /// position where its answer is not on offer.
    fn offers(&self, standing: Standing) -> Vec<Offer> {
        let mut offers = vec![Offer::Edit, Offer::Skip, Offer::Fail];
        if let Standing::Fail = standing {
            offers.push(Offer::Override);
        }
        offers.push(Offer::Quit);
        offers
    }

    /// Whether anything has settled behind the live prompt, so `<Up>` steps
    /// back into review rather than doing nothing.
    fn reviewable(&self) -> bool {
        !self
            .records
            .is_empty()
    }

    /// Show a step's text, interpolated against the bindings in scope and
    /// annotated with any `within` budget enclosing it.
    fn display_step(&mut self, env: &Environment, source: &language::Scope<'_>, qualified: &str) {
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
        let text = render_constraints(&self.constraints).unwrap_or_default();
        self.driver
            .step(qualified, &text, &step_text, depth);
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
                .enter(qualified, "");
        } else {
            let echo = render_argument_echo(params, env);
            self.driver
                .enter(qualified, &echo);
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

    /// Append one line to the journal, stamped with the moment it happened, the
    /// identifier of the run writing it, and the serial of the scope the walk
    /// is standing in. Every record the walk emits goes through here.
    fn record(&mut self, qualified: &str, state: State) -> Result<(), RunnerError> {
        let serial = self.serial;
        self.stamp(serial, qualified, state)
    }

    /// Append a record bracketing no scope — the run lifecycle events at the
    /// root path, which wear serial `000` however deep the walk had reached.
    fn record_lifecycle(&mut self, state: State) -> Result<(), RunnerError> {
        self.stamp(Serial::LIFECYCLE, "/", state)
    }

    fn stamp(&mut self, serial: Serial, qualified: &str, state: State) -> Result<(), RunnerError> {
        if self.replaying() {
            return Ok(());
        }
        let run_id = self
            .appender
            .run_id();
        let record = Record {
            recorded: now_iso8601(),
            run_id,
            serial,
            path: qualified.to_string(),
            state,
        };
        // A record the journal already states, still truly, stands as it is.
        if !self
            .ledger
            .carries(&record)
        {
            self.appender
                .append(&record)?;
            self.records
                .push(record.clone());
        }
        self.ledger
            .apply(&record);
        Ok(())
    }

    /// Close a scope: its `Bind` if it bound anything, then its outcome. At
    /// most one `Bind` per entry, so the fold assigns rather than accumulates.
    fn record_outcome(&mut self, qualified: &str, state: State) -> Result<(), RunnerError> {
        if !self
            .bound
            .is_empty()
        {
            let bound = std::mem::take(&mut self.bound);
            self.record(qualified, State::Bind(bound))?;
        }
        self.record(qualified, state)
    }

    /// Note a binding the current scope made, for its `Bind` record. A
    /// binding's own value is unit; this is where the value it captured is
    /// kept so a replay can restore it.
    fn note_binding(&mut self, name: &str, value: &Value) {
        if self.replaying() {
            return;
        }
        self.bound
            .push(Supplied {
                value: value.clone(),
                name: Some(name.to_string()),
            });
    }

    /// What a prior walk left at this position, as it bears on the walk
    /// arriving here now. `reads` is what the node reads at this moment.
    fn recall(&self, qualified: &str, reads: &[Supplied]) -> Recall {
        let entry = match self
            .ledger
            .look(self.serial, qualified)
        {
            Some(entry) => entry,
            None => return Recall::Nothing,
        };
        if self
            .entered
            .contains(&entry.serial)
        {
            return Recall::Nothing;
        }
        let outcome = match &entry.outcome {
            Some(outcome) => outcome,
            None => return Recall::Nothing,
        };
        if entry.began != reads {
            return Recall::Stale;
        }
        Recall::Valid(
            entry.serial,
            outcome.clone(),
            entry
                .bound
                .clone(),
        )
    }

    /// Whether the walk is replaying a completed scope. Guards every prompt
    /// and every append, so a replay shows what was done without doing it
    /// again.
    fn replaying(&self) -> bool {
        self.replaying > 0
    }

    /// Walk a completed scope's body for display alone. Recorded bindings are
    /// re-established afterwards, so what the journal says a scope bound wins
    /// over whatever the replay walk happened to arrive at.
    fn replay(
        &mut self,
        env: &mut Environment,
        body: &'i Operation<'i>,
        bound: &[Supplied],
    ) -> Result<(), RunnerError> {
        let outer = self.replaying;
        self.replaying = outer + 1;
        let result = self.walk(env, body);
        self.replaying = outer;
        result?;
        for item in bound {
            if let Some(name) = &item.name {
                env.extend(
                    name.clone(),
                    item.value
                        .clone(),
                );
            }
        }
        Ok(())
    }

    /// Enter the scope at `qualified` under the scope the walk is standing in,
    /// taking the serial a prior walk used here if it reached this position and
    /// a fresh one otherwise. Reuse is what keeps a resumed scope addressing
    /// its own recorded descendants. The caller restores the enclosing scope's
    /// serial on the way back out.
    fn allocate(&mut self, qualified: &str) {
        let serial = self
            .ledger
            .serial_for(self.serial, qualified);
        // A serial this walk has already entered is another execution of the
        // same address, not a return to the one recorded there.
        self.serial = if self
            .entered
            .contains(&serial)
        {
            self.ledger
                .next_serial()
        } else {
            serial
        };
        self.entered
            .insert(self.serial);
    }

    /// Open a structural scope — the entry procedure, a Section, or an invoked
    /// procedure — pairing with the `Done` its `seal_scope` records on close, so
    /// every scope's address is bracketed `Begin`…`Done` just as a step's is.
    fn begin_scope(&mut self, qualified: &str, supplied: Vec<Supplied>) -> Result<(), RunnerError> {
        self.allocate(qualified);
        self.opening = "\u{2198}";
        self.record(qualified, State::Begin(supplied))
    }

    /// At a procedure's entry, restore its parameter bindings from a prior
    /// run's recorded inputs if present (resume), otherwise gather the inputs
    /// it was called with (a fresh run). Used for the entry procedure, whose
    /// arguments come from the command line. The values returned are the ones
    /// its `Begin` records.
    fn restore_or_collect_inputs(
        &mut self,
        env: &mut Environment,
        qualified: &str,
        params: &[language::Identifier<'i>],
    ) -> Result<Vec<Supplied>, RunnerError> {
        if let Some(supplied) = self
            .ledger
            .look(self.serial, qualified)
            .filter(|entry| !entry.revoked)
            .map(|entry| {
                entry
                    .began
                    .clone()
            })
        {
            for item in supplied.iter() {
                if let Some(name) = &item.name {
                    env.extend(
                        name.clone(),
                        item.value
                            .clone(),
                    );
                }
            }
            return Ok(supplied);
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
        Ok(supplied)
    }

    /// Sign off a completed structural scope — a Section at its close, or the
    /// whole run at the entry procedure.
    fn seal_scope(
        &mut self,
        qualified: &str,
        outcome: Outcome,
        kind: Kind,
    ) -> Result<Conclusion, RunnerError> {
        if self.replaying() {
            return Ok(Conclusion::Completed(outcome));
        }
        let standing = match &outcome {
            Outcome::Fail(_) => Some(Standing::Fail),
            Outcome::Skip(_) => Some(Standing::Skip),
            _ => None,
        };
        if let Some(standing) = standing {
            let question = Question {
                qualified,
                marker: "↙",
                standing,
                kind,
                produced: Value::Unitus,
                reviewable: self.reviewable(),
            };
            let settled = self.settle(question, &[], "↙")?;
            if let Conclusion::Completed(_) = &settled {
                self.driver
                    .show_verdict("↙", qualified, &verdict_from(&settled));
                self.record_outcome(qualified, record_state(&settled))?;
            }
            return Ok(settled);
        }
        let produced = match outcome {
            Outcome::Done(value) | Outcome::Skip(value) => value,
            _ => Value::Unitus,
        };
        let question = Question {
            qualified,
            marker: "↙",
            standing: Standing::Done,
            kind,
            produced,
            reviewable: self.reviewable(),
        };
        let conclusion = self.settle(question, &[], "↙")?;
        if let Conclusion::Completed(_) = &conclusion {
            self.driver
                .show_verdict("↙", qualified, &verdict_from(&conclusion));
            self.record_outcome(qualified, record_state(&conclusion))?;
        }
        Ok(conclusion)
    }

    /// Record an invocation declined at its acquire prompt: Skip and Fail
    /// bracket the call at `qualified`, its `Begin` stating the arguments
    /// gathered before the decline; Quit stops the run.
    fn abandon(
        &mut self,
        qualified: &str,
        supplied: Vec<Supplied>,
        conclusion: Conclusion,
    ) -> Result<Conclusion, RunnerError> {
        if let Conclusion::Completed(_) = &conclusion {
        } else {
            return Ok(conclusion);
        }
        self.begin_scope(qualified, supplied)?;
        self.record_outcome(qualified, record_state(&conclusion))?;
        Ok(conclusion)
    }

    /// Record a `Finish` at the root path, closing a run that walked to its end.
    fn record_finish(&mut self) -> Result<(), RunnerError> {
        self.record_lifecycle(State::Finish)
    }

    /// Record a deliberate Stop at the root path and unwind the walk.
    fn record_stop(&mut self) -> Result<Conclusion, RunnerError> {
        self.record_lifecycle(State::Stop)?;
        Ok(Conclusion::Stopping)
    }

    /// The Nature of an Executable's resolved target; `Pure` if unresolved.
    fn executable_nature(&self, exec: &Executable) -> Nature {
        match &exec.target {
            ExecutableRef::Resolved(id) => self
                .library
                .nature(*id),
            _ => Nature::Pure,
        }
    }

    /// Classify an `Execute` by its builtin's `Nature`, resolving the target as
    /// the dispatch does.
    fn execute_kind(&self, exec: &Executable) -> Kind {
        match self.executable_nature(exec) {
            Nature::Pure => Kind::Computable,
            Nature::Command | Nature::Instant => Kind::System,
            Nature::Action => Kind::Action,
        }
    }

    /// Classify a step by its final member: a step offering response choices is
    /// a `Choice`; otherwise the value comes from the last operation, an ending
    /// in prose being `Prose` and everything else `Computable`.
    fn kind_of_step(&self, op: &Operation) -> Kind {
        match op {
            Operation::Step { responses, .. } if !responses.is_empty() => Kind::Choice,
            Operation::Step { body, .. } => self.kind_of_step(body),
            Operation::Sequence(ops, _) | Operation::Prologue(ops, _) => match ops.last() {
                Some(last) => self.kind_of_step(last),
                None => Kind::Prose,
            },
            Operation::Execute(exec, _) => self.execute_kind(exec),
            Operation::Prose(_, _) => Kind::Prose,
            _ => Kind::Computable,
        }
    }

    /// A scope's Kind: `Computable` if any member holds work, else `Prose`. A
    /// pure-prose scope is `Prose`, so an unattended run skips its close.
    fn kind_of_scope(&self, op: &Operation) -> Kind {
        match op {
            Operation::Sequence(ops, _) | Operation::Prologue(ops, _) => {
                if ops
                    .iter()
                    .any(|op| self.kind_of_scope(op) == Kind::Computable)
                {
                    Kind::Computable
                } else {
                    Kind::Prose
                }
            }
            Operation::Step { body, .. } | Operation::Section { body, .. } => {
                self.kind_of_scope(body)
            }
            Operation::Prose(_, _) => Kind::Prose,
            _ => Kind::Computable,
        }
    }
}

fn describe_execute(function: &str) -> String {
    format!("{}()", function)
}

/// Accumulates child outcomes into one worst-wins verdict: the rank climbs by
/// `Standing` precedence (Fail > Done > Skip) while the value tracks last-seen.
/// A `None` rank distinguishes an empty group (yields Done) from an all-skipped
/// one (yields Skip).
struct Rollup {
    rank: Option<Standing>,
    value: Value,
    failure: Option<Failure>,
}

impl Rollup {
    fn new() -> Self {
        Rollup {
            rank: None,
            value: Value::Unitus,
            failure: None,
        }
    }

    fn absorb(&mut self, outcome: Outcome) {
        let rank = match outcome {
            Outcome::Done(value) => {
                self.value = value;
                Standing::Done
            }
            Outcome::Skip(value) => {
                self.value = value;
                Standing::Skip
            }
            Outcome::Fail(failure) => {
                if self
                    .failure
                    .is_none()
                {
                    self.failure = Some(failure);
                }
                Standing::Fail
            }
        };
        self.rank = Some(match self.rank {
            Some(cur) => cur.max(rank),
            None => rank,
        });
    }

    /// Prose contributes its value but not a verdict, so surrounding prose never
    /// masks a skipped Invoke.
    fn observe(&mut self, value: Value) {
        self.value = value;
    }

    fn settle(self) -> Outcome {
        match self
            .rank
            .unwrap_or(Standing::Done)
        {
            Standing::Fail => Outcome::Fail(
                self.failure
                    .unwrap(),
            ),
            Standing::Skip => Outcome::Skip(self.value),
            Standing::Done => Outcome::Done(self.value),
        }
    }
}

/// The closing line's verdict, rolling the run's final `Conclusion` back into
/// the `UserInput` glyph the driver renders — mirroring the entry procedure's
/// close. A `Done` shows `✓`, a `Skip` `⊘`, a failure `✗`; the value and
/// reason are immaterial to the glyph.
fn verdict_from(conclusion: &Conclusion) -> UserInput {
    match conclusion {
        Conclusion::Completed(Outcome::Done(_)) => UserInput::Done(Value::Unitus),
        Conclusion::Completed(Outcome::Skip(_)) => UserInput::Skip,
        Conclusion::Completed(Outcome::Fail(_)) | Conclusion::Throwing(_) => {
            UserInput::Fail(String::new())
        }
        // An unwinding walk shows no closing verdict
        Conclusion::Stopping | Conclusion::Restarting => UserInput::Quit,
    }
}

/// Project a `Conclusion` into the on-disk `State` for the PFFTT file. A `Done`
/// records its full value (serialized by the state codec), so a value bound
/// with `~` rehydrates on resume. A thrown failure records Fail. Stopped is
/// unreachable here: the caller filters it out before recording.
fn record_state(conclusion: &Conclusion) -> State {
    match conclusion {
        Conclusion::Completed(Outcome::Done(value)) => State::Done(Some(value.clone())),
        Conclusion::Completed(Outcome::Skip(_)) => State::Skip,
        Conclusion::Completed(Outcome::Fail(Failure::Aborted(reason)))
        | Conclusion::Throwing(Failure::Aborted(reason)) => {
            if reason.is_empty() {
                // The user failed the step without giving a reason; record the
                // failure with no reason rather than an empty-string one.
                State::Fail(None)
            } else {
                State::Fail(Some(crate::engraving::fail_reason(reason)))
            }
        }
        // A stop is recorded as a lifecycle event, not a step result, and a
        // restart records nothing at all
        Conclusion::Stopping | Conclusion::Restarting => unreachable!(),
    }
}

/// A loop iteration a prior walk recorded, waiting to be claimed by an item
/// matching it.
struct Iteration {
    number: usize,
    began: Vec<Supplied>,
    complete: bool,
}

/// The index an item runs at: the first unclaimed recorded iteration whose
/// `Begin` states that item, preferring a completed one so a run interrupted
/// mid-iteration resumes without redoing its finished sibling. An item
/// matching nothing takes the next index, numeric maximum plus one — key
/// order would hand back `[9]` from a scope holding `[10]`, since `[10]`
/// sorts between `[1]` and `[2]`.
///
/// The claimed entry leaves the pool, which is what preserves multiplicity:
/// two identical items are two executions and cannot collapse onto one
/// recorded iteration. Reuse is otherwise best-effort — declining to match
/// merely costs redundant work — but collapsing would report work as done
/// that was never performed.
fn claim_iteration(pool: &mut Vec<Iteration>, wanted: &[Supplied], highest: &mut usize) -> usize {
    let states = |item: &Iteration| item.began == wanted;
    let chosen = pool
        .iter()
        .position(|item| item.complete && states(item))
        .or_else(|| {
            pool.iter()
                .position(states)
        });
    match chosen {
        Some(at) => {
            pool.remove(at)
                .number
        }
        None => {
            *highest += 1;
            *highest
        }
    }
}

/// The loop variables bound for this pass, in the form an iteration's `Begin`
/// states them. Empty for `repeat`, which binds nothing.
fn iteration_values(names: &[language::Identifier], env: &Environment) -> Vec<Supplied> {
    names
        .iter()
        .filter_map(|name| {
            env.lookup(name.value)
                .map(|value| Supplied {
                    value: value.clone(),
                    name: Some(
                        name.value
                            .to_string(),
                    ),
                })
        })
        .collect()
}

/// The actions on offer at a reviewed position: the ones it was answered with,
/// so changing an answer means giving a different one. A frame the walk never
/// asked about has no answer to change, leaving only Quit.
fn reviewing(settled: Option<&UserInput>) -> Vec<Offer> {
    let mut offers = Vec::new();
    if let Some(verdict) = settled {
        offers.push(Offer::Edit);
        offers.push(Offer::Skip);
        offers.push(Offer::Fail);
        if let UserInput::Fail(_) = verdict {
            offers.push(Offer::Override);
        }
    }
    offers.push(Offer::Quit);
    offers
}

/// A verdict given at a prompt, or `Ended` where the run stops or restarts
/// instead of yielding one.
enum Answer {
    Done(Value),
    Skip,
    Fail(String),
    Ended(Conclusion),
}

impl Answer {
    fn completed(self) -> Conclusion {
        match self {
            Answer::Done(value) => Conclusion::Completed(Outcome::Done(value)),
            Answer::Skip => Conclusion::Completed(Outcome::Skip(Value::Unitus)),
            Answer::Fail(reason) => Conclusion::Completed(Outcome::Fail(Failure::Aborted(reason))),
            Answer::Ended(conclusion) => conclusion,
        }
    }
}

/// How a spell in the review cursor ended.
enum Reviewed {
    /// Back to the live prompt, which the walker re-issues.
    Left,
    /// A recorded value was withdrawn; the walk restarts.
    Amended,
    /// Stop the run.
    Quit,
}

/// The standing an offer set was built for, read back so a re-issued prompt
/// keeps it. `Override` is offered only where a child failed.
fn offers_standing(offers: &[Offer]) -> Standing {
    if offers.contains(&Offer::Override) {
        Standing::Fail
    } else {
        Standing::Done
    }
}

/// What a prior walk left at a position the walk has arrived at again.
enum Recall {
    /// Nothing that applies: never reached, left unfinished, or a scope this
    /// walk entered itself.
    Nothing,
    /// Completed, and the values the node reads now are the ones its `Begin`
    /// recorded, so the work stands: its serial, outcome, and bindings.
    Valid(Serial, State, Vec<Supplied>),
    /// Completed, but an input has been amended since. This is the whole of
    /// staleness propagation — the node is redone, and redoing it invalidates
    /// its own consumers in turn, with nothing computed or stored beyond the
    /// amendment itself.
    Stale,
}

/// The value a recorded outcome settled on. Skip and Fail carry none.
fn value_of(state: &State) -> Value {
    match state {
        State::Done(Some(value)) => value.clone(),
        _ => Value::Unitus,
    }
}

/// The verdict a recorded outcome stands for, so a replayed scope can show
/// what it settled on where it took its prompt the first time.
/// The verdict a record states, where it states one. A record that is not an
/// outcome has none, and offers nothing to amend.
fn settled_by(state: &State) -> Option<UserInput> {
    match state {
        State::Done(value) => Some(UserInput::Done(match value {
            Some(value) => value.clone(),
            None => Value::Unitus,
        })),
        State::Skip => Some(UserInput::Skip),
        State::Fail(reason) => Some(UserInput::Fail(match reason {
            Some(value) => value.to_string(),
            None => String::new(),
        })),
        _ => None,
    }
}

/// The marker a record is drawn with, matching the one the live trail used.
/// The path says what kind of thing stands there — the grammar being read back
/// is `render_segment` in `path.rs` — and the state says whether this is the
/// way in or the way out. A dispatch is neither: it stands at the *caller's*
/// path, so nothing but its own state names it.
fn marker_of(record: &Record) -> &'static str {
    if let State::Invoke(target) = &record.state {
        return match target {
            InvokeTarget::Uri(_) => "\u{21d2}",
            InvokeTarget::Procedure(_) => "\u{2192}",
        };
    }
    let leaving = match record.state {
        State::Done(_) | State::Skip | State::Fail(_) | State::Finish | State::Stop => true,
        _ => false,
    };
    let edge = record
        .path
        .rsplit('/')
        .next()
        .unwrap_or("");
    // The run's own boundary, and a call that leaves the document for another
    // Technique, are the only crossings drawn with the double arrows.
    if record.path == "/" || (edge.starts_with('<') && edge.ends_with('>')) {
        return match leaving {
            true => "\u{21d0}",
            false => "\u{21d2}",
        };
    }
    // What encloses other work is entered and left: a procedure ends in a
    // colon, a section is an upper-case Roman numeral, an iteration is
    // bracketed. A step is an arrow throughout.
    let nests = edge.ends_with(':')
        || (edge.starts_with('[') && edge.ends_with(']'))
        || (!edge.is_empty()
            && edge
                .chars()
                .all(|c| "IVXLCDM".contains(c)));
    match (nests, leaving) {
        (true, true) => "\u{2199}",
        (true, false) => "\u{2198}",
        (false, _) => "\u{2192}",
    }
}

fn verdict_of(state: &State) -> UserInput {
    match state {
        State::Skip => UserInput::Skip,
        State::Fail(reason) => UserInput::Fail(match reason {
            Some(value) => value.to_string(),
            None => String::new(),
        }),
        _ => UserInput::Done(Value::Unitus),
    }
}

/// The values a step reads, in the order the names are met, so its `Begin`
/// states what it started with. A name not yet bound contributes nothing.
fn read_values(op: &Operation, env: &Environment) -> Vec<Supplied> {
    let mut names = Vec::new();
    names_read(op, &mut names);
    names
        .into_iter()
        .filter_map(|name| {
            env.lookup(name)
                .map(|value| Supplied {
                    value: value.clone(),
                    name: Some(name.to_string()),
                })
        })
        .collect()
}

/// Gather the variables a node reads directly.
fn names_read<'i>(op: &Operation<'i>, found: &mut Vec<&'i str>) {
    match op {
        Operation::Variable(id, _) => {
            if !found.contains(&id.value) {
                found.push(id.value);
            }
        }
        Operation::Loop { over, body, .. } => {
            if let Some(over) = over {
                names_read(over, found);
            }
            names_read(body, found);
        }
        Operation::Within { bound, body, .. } => {
            names_read(bound, found);
            names_read(body, found);
        }
        Operation::Bind { value, .. } => names_read(value, found),
        Operation::Cost(inner, _) => names_read(inner, found),
        Operation::Sequence(ops, _)
        | Operation::List(ops, _)
        | Operation::Tuple(ops, _)
        | Operation::Prologue(ops, _) => {
            for op in ops {
                names_read(op, found);
            }
        }
        Operation::Invoke(invocable, _) => {
            for arg in &invocable.arguments {
                names_read(arg, found);
            }
        }
        Operation::Execute(executable, _) => {
            for arg in &executable.arguments {
                names_read(arg, found);
            }
        }
        Operation::String(fragments, _) => {
            for fragment in fragments {
                if let Fragment::Interpolation(op) = fragment {
                    names_read(op, found);
                }
            }
        }
        Operation::Tablet(entries, _) => {
            for entry in entries {
                names_read(&entry.value, found);
            }
        }
        Operation::Step { .. }
        | Operation::Section { .. }
        | Operation::Number(_, _)
        | Operation::Response(_, _)
        | Operation::Verbatim(_, _)
        | Operation::Prose(_, _)
        | Operation::Hole(_)
        | Operation::Unit(_) => {}
    }
}

/// The names a step body binds, if any: the first `Bind` reached without
/// descending through a nested step, loop, or section. Used on resume to
/// rebind a replayed step's value into the environment.
fn binding_names<'i>(op: &Operation<'i>) -> Option<&'i [language::Identifier<'i>]> {
    match op {
        Operation::Bind { names, .. } => Some(names),
        Operation::Sequence(ops, _) => ops
            .iter()
            .find_map(binding_names),
        _ => None,
    }
}

/// Whether walking a step body amounts to nothing more than acquiring one or
/// more descriptive `~` bindings — prose interleaved with bindings that carry no
/// expression to compute their value. Such a step takes the user's input at its
/// acquire prompt(s); the last doubles as the step's completion, so there is no
/// separate verdict left to take.
fn binds_descriptively(op: &Operation) -> bool {
    match op {
        Operation::Bind { value, .. } => {
            if let Operation::Sequence(ops, _) = value.as_ref() {
                ops.is_empty()
            } else {
                false
            }
        }
        Operation::Sequence(ops, _) => {
            let mut bound = false;
            for op in ops {
                if let Operation::Prose(_, _) = op {
                    continue;
                }
                if binds_descriptively(op) {
                    bound = true;
                } else {
                    return false;
                }
            }
            bound
        }
        _ => false,
    }
}

/// Render a procedure's bound arguments in `value ~ name` form, e.g.
/// `([] ~ e, 0 ~ s)`, to announce alongside the qualified path.
fn render_argument_echo(params: &[language::Identifier], env: &Environment) -> String {
    format!("({})", render_bindings(params, env))
}

/// Render a loop iteration's bound variable(s) in `value ~ name` form, e.g.
/// `("i-1234" ~ instance)` — a string value shows quoted — to announce
/// alongside the iteration's path. Empty when `repeat` binds no name.
fn render_iteration_echo(names: &[language::Identifier], env: &Environment) -> String {
    if names.is_empty() {
        String::new()
    } else {
        format!("({})", render_bindings(names, env))
    }
}

/// Render the enclosing `within` budgets as a `$(...)`-annotated suffix for a
/// step's prompt line, set off from the path with a space like an acquire
/// prompt's `(name : forma)` — an announcement, not part of the addressable
/// path. `None` when no `within` block encloses the step.
fn render_constraints(constraints: &[Value]) -> Option<String> {
    if constraints.is_empty() {
        return None;
    }
    let rendered = constraints
        .iter()
        .map(|budget| format!("$({budget})"))
        .collect::<Vec<_>>()
        .join(" ");
    Some(rendered)
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
        let parameter = param
            .value
            .to_string();
        let value = super::evaluator::parse_value(argument).ok_or_else(|| {
            RunnerError::MalformedArgument {
                parameter: parameter.clone(),
                argument: argument.to_string(),
            }
        })?;
        env.extend(parameter, value);
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
