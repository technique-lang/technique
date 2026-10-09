//! A walk: one pass over a program from its entry procedure. What the journal
//! already records is replayed rather than asked or executed again, and only
//! what is new is appended.

use std::collections::HashMap;

use super::driver::{
    Answer, Driver, Event, Kind, Marker, Offer, Prompt, Question, Standing, Verdict,
};
use super::error::RunnerError;
use super::evaluator::{self, Environment};
use super::library::Nature;
use super::path::{PathSegment, QualifiedPath, render_path};
use super::session::{Reviewed, Runner};
use crate::engraving::{self, Activation, History, InvokeTarget, Serial, State, Supplied, edge};
use crate::formatting::{self, Identity};
use crate::language;
use crate::program::{
    Executable, ExecutableRef, Fragment, Invocable, Locale, Operation, Ordinal, Subroutine,
    SubroutineRef,
};
use crate::value::Value;

/// What a scope concluded.
#[derive(Debug, Clone, PartialEq)]
pub enum Outcome {
    Done(Value),
    /// Keeps the body's value for the walk; the journal records a bare
    /// `Skip`.
    Skip(Value),
    Fail(String),
}

/// A change chosen in review, handed to the next walk.
#[derive(Debug, Clone, PartialEq)]
pub struct Amendment {
    pub serial: Serial,
    pub change: Change,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Change {
    Redo,
    Skip,
    Fail(String),
    Override,
    /// Ask an invocation's prompted arguments again.
    Reask,
}

/// Why a walk ended before the entry procedure finished.
#[derive(Debug)]
pub(super) enum Halt {
    /// The user quit; records held back are dropped and `Stop` written.
    Stop,
    /// An amendment chosen in review; its `Revoke` is held back, not yet
    /// written.
    Restart(Amendment),
    Error(RunnerError),
}

impl From<RunnerError> for Halt {
    fn from(error: RunnerError) -> Self {
        Halt::Error(error)
    }
}

/// What walking one operation yields. `Throwing` carries the reason a command
/// or action failed up to the nearest enclosing scope, which records it.
enum Flow {
    Completed(Outcome),
    Throwing(String),
}

enum Reply {
    Done(Value),
    Skip,
    Fail(String),
    Override,
}

/// How the walk meets a slot, given what the journal records there.
#[derive(Clone, Copy)]
enum Stance<'h> {
    /// Nothing recorded.
    Fresh,
    /// Withdrawn, or begun with different inputs: begun again at the same
    /// serial, and nothing recorded beneath it stands.
    Again,
    /// Closed with nothing beneath it: its recorded outcome is replayed.
    Restore(&'h Activation),
    /// Still open, or enclosing children: entered, and what stands beneath it
    /// is replayed.
    Continue(&'h Activation),
}

/// How a scope closes: with its recorded outcome, with one already decided,
/// or by asking the user.
enum Closing {
    Restored(Outcome),
    Given(Outcome),
    Ask,
}

struct Slot<'h> {
    serial: Serial,
    /// The recorded activation that stands, which the walk may restore or
    /// continue.
    prior: Option<&'h Activation>,
    /// The last activation recorded here, even one revoked or superseded; its
    /// answers pre-fill prompts asked again.
    seed: Option<&'h Activation>,
}

struct Scope<'h> {
    serial: Serial,
    path: String,
    /// As in `Slot`.
    prior: Option<&'h Activation>,
    seed: Option<&'h Activation>,
    /// The standing children of `prior`; those this walk does not reach are
    /// revoked when the scope closes.
    children: &'h [Serial],
    /// The walk's `written` count on entry, to tell whether anything was
    /// written within the scope.
    mark: usize,
    bound: Vec<Supplied>,
    /// Commands and actions met so far, indexing those `prior` recorded.
    effects: usize,
    /// `Invoke`s met so far, indexing those `prior` recorded.
    invokes: usize,
    /// How many times each edge has been taken, to find a slot's occurrence.
    occurrences: HashMap<String, usize>,
    iterations: usize,
    /// The child slots this walk has taken.
    reached: Vec<Serial>,
    /// A child closed differently from before or was revoked, so this scope's
    /// close is asked again rather than reused.
    changed: bool,
}

impl<'h> Scope<'h> {
    fn new(serial: Serial, path: &str, children: &'h [Serial], mark: usize) -> Self {
        Scope {
            serial,
            path: path.to_string(),
            prior: None,
            seed: None,
            children,
            mark,
            bound: Vec::new(),
            effects: 0,
            invokes: 0,
            occurrences: HashMap::new(),
            iterations: 0,
            reached: Vec::new(),
            changed: false,
        }
    }
}

const CONFIRM: &[Offer] = &[Offer::Edit, Offer::Skip, Offer::Fail, Offer::Quit];
const OVERRULE: &[Offer] = &[
    Offer::Edit,
    Offer::Skip,
    Offer::Fail,
    Offer::Override,
    Offer::Quit,
];
const BOUNDARY: &[Offer] = &[Offer::Skip, Offer::Fail, Offer::Quit];
const DEPTH: usize = 100;

/// Walk the program once from the top. `arguments` supply the entry
/// procedure's `Begin` if the journal has none.
pub(super) fn walk<'i, D: Driver>(
    runner: &mut Runner<'i, D>,
    history: &History,
    amendment: Option<Amendment>,
    arguments: &[Supplied],
) -> Result<Outcome, Halt> {
    let mut walker = Walker {
        next: history.next_serial(),
        scopes: vec![Scope::new(Serial::ROOT, "", history.roots(), 0)],
        runner,
        history,
        amendment,
        path: QualifiedPath::new(),
        written: 0,
        depth: 0,
        constraints: Vec::new(),
        asked: Vec::new(),
    };
    walker.run(arguments)
}

struct Walker<'i, 'h, 'r, D: Driver> {
    runner: &'r mut Runner<'i, D>,
    history: &'h History,
    amendment: Option<Amendment>,
    path: QualifiedPath<'i>,
    scopes: Vec<Scope<'h>>,
    /// The serial a new slot is given.
    next: Serial,
    /// Records this walk has written.
    written: usize,
    /// How deeply procedure invocations are nested, against `DEPTH`.
    depth: usize,
    /// The `within` budgets in force, shown with each step.
    constraints: Vec<Value>,
    /// Invocations reached this walk whose arguments were prompted for, so
    /// review can offer to ask them again.
    asked: Vec<Serial>,
}

impl<'i, 'h, 'r, D: Driver> Walker<'i, 'h, 'r, D> {
    fn run(&mut self, arguments: &[Supplied]) -> Result<Outcome, Halt> {
        let program = self
            .runner
            .program;
        let entry = program
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
        let path = self
            .path
            .render();
        let slot = self.slot(&path);
        let inputs = match slot.prior {
            Some(a) => a
                .began
                .clone(),
            None => arguments.to_vec(),
        };
        let mut env = Environment::new();
        bind_supplied(&mut env, &inputs);
        let stance = stance(slot.prior, &inputs);
        if let Stance::Restore(a) = stance {
            return Ok(outcome_of(&self.restore(&mut env, a, Marker::Close)?));
        }
        let again = self.open(&slot, &path, inputs, stance)?;
        if name.is_some() {
            self.announce(entry, &path, &env);
        }
        let flow = self.perform(&mut env, &entry.body, again)?;
        self.seal(&mut env, &path, flow, kind_of_scope(&entry.body))
    }

    fn walk(&mut self, env: &mut Environment, op: &'i Operation<'i>) -> Result<Flow, Halt> {
        match op {
            Operation::Sequence(ops, _) => self.walk_sequence(env, ops),
            Operation::Prologue(ops, _) => self.walk_prologue(env, ops),
            Operation::Section {
                numeral,
                title,
                body,
                ..
            } => self.walk_section(env, numeral, title, body),
            Operation::Step { .. } => {
                unreachable!() // a Step is always walked as a Sequence member
            }
            Operation::Loop {
                names, over, body, ..
            } => self.walk_loop(env, names, over, body),
            Operation::Within { bound, body, .. } => {
                let budget = match self.value(env, bound)? {
                    Ok(value) => value,
                    Err(flow) => return Ok(flow),
                };
                self.constraints
                    .push(budget);
                let flow = self.walk(env, body)?;
                self.constraints
                    .pop();
                Ok(flow)
            }
            Operation::Cost(inner, _) => match self.value(env, inner)? {
                Ok(Value::Quanticle(numeric)) => Ok(done(Value::Intratempse(numeric))),
                Ok(_) => Err(RunnerError::InvalidCost.into()),
                Err(flow) => Ok(flow),
            },
            Operation::Invoke(invocable, _) => match &invocable.target {
                SubroutineRef::Resolved(id) => {
                    let program = self
                        .runner
                        .program;
                    self.walk_procedure(env, &program.subroutines[id.0], invocable)
                }
                SubroutineRef::Deferred(external) => {
                    self.walk_external(env, external.value, &invocable.arguments)
                }
                SubroutineRef::Unresolved(_) => {
                    unreachable!() // resolution resolves every procedure name
                }
            },
            Operation::Execute(executable, _) => self.walk_execute(env, executable),
            Operation::Bind {
                names,
                value,
                inferred,
                ..
            } => self.walk_bind(env, names, value, inferred.as_ref()),
            Operation::String(fragments, _) => match self.text(env, fragments)? {
                Ok(text) => Ok(done(Value::Literali(text))),
                Err(flow) => Ok(flow),
            },
            Operation::List(items, _) => match self.values(env, items)? {
                Ok(values) => Ok(done(Value::Arraeum(values))),
                Err(flow) => Ok(flow),
            },
            Operation::Tuple(items, _) => match self.values(env, items)? {
                Ok(values) => Ok(done(Value::Parametriq(values))),
                Err(flow) => Ok(flow),
            },
            Operation::Tablet(entries, _) => {
                let mut pairs = Vec::with_capacity(entries.len());
                for entry in entries {
                    let value = match self.value(env, &entry.value)? {
                        Ok(value) => value,
                        Err(flow) => return Ok(flow),
                    };
                    let label = match self.text(env, &entry.label)? {
                        Ok(label) => label,
                        Err(flow) => return Ok(flow),
                    };
                    pairs.push((label, value));
                }
                Ok(done(Value::Tabularum(pairs)))
            }
            Operation::Variable(_, _)
            | Operation::Number(_, _)
            | Operation::Response(_, _)
            | Operation::Verbatim(_, _)
            | Operation::Prose(_, _)
            | Operation::Hole(_)
            | Operation::Unit(_) => {
                let value = evaluator::evaluate(
                    &self
                        .runner
                        .library,
                    &self
                        .runner
                        .context,
                    env,
                    op,
                )?;
                Ok(done(value))
            }
        }
    }

    // Walk an operation for its value. Anything but Done is handed back for
    // the caller to propagate, a Skip with its value dropped.
    fn value(
        &mut self,
        env: &mut Environment,
        op: &'i Operation<'i>,
    ) -> Result<Result<Value, Flow>, Halt> {
        Ok(match self.walk(env, op)? {
            Flow::Completed(Outcome::Done(value)) => Ok(value),
            Flow::Completed(Outcome::Skip(_)) => Err(Flow::Completed(Outcome::Skip(Value::Unitus))),
            other => Err(other),
        })
    }

    fn values(
        &mut self,
        env: &mut Environment,
        ops: &'i [Operation<'i>],
    ) -> Result<Result<Vec<Value>, Flow>, Halt> {
        let mut values = Vec::with_capacity(ops.len());
        for op in ops {
            match self.value(env, op)? {
                Ok(value) => values.push(value),
                Err(flow) => return Ok(Err(flow)),
            }
        }
        Ok(Ok(values))
    }

    fn text(
        &mut self,
        env: &mut Environment,
        fragments: &'i [Fragment<'i>],
    ) -> Result<Result<String, Flow>, Halt> {
        let mut text = String::new();
        for fragment in fragments {
            match fragment {
                Fragment::Text(t) => text.push_str(t),
                Fragment::Escaped(c) => text.push(*c),
                Fragment::Interpolation(inner) => match self.value(env, inner)? {
                    Ok(Value::Literali(s)) => text.push_str(&s),
                    Ok(other) => text.push_str(&other.to_string()),
                    Err(flow) => return Ok(Err(flow)),
                },
            }
        }
        Ok(Ok(text))
    }

    fn walk_sequence(
        &mut self,
        env: &mut Environment,
        ops: &'i [Operation<'i>],
    ) -> Result<Flow, Halt> {
        let mut parallel = 0;
        let mut rollup = Rollup::new();
        for op in ops {
            let flow = match op {
                Operation::Step { ordinal, .. } => {
                    if let Ordinal::Parallel = ordinal {
                        parallel += 1;
                    }
                    self.walk_step(env, op, parallel)?
                }
                _ => self.walk(env, op)?,
            };
            // Prose contributes its value but no verdict.
            if let Operation::Prose(_, _) = op {
                if let Flow::Completed(Outcome::Done(value)) = flow {
                    rollup.observe(value);
                }
                continue;
            }
            match flow {
                Flow::Completed(outcome) => rollup.absorb(outcome),
                throwing => return Ok(throwing),
            }
        }
        Ok(Flow::Completed(rollup.outcome()))
    }

    fn walk_step(
        &mut self,
        env: &mut Environment,
        op: &'i Operation<'i>,
        parallel: usize,
    ) -> Result<Flow, Halt> {
        let Operation::Step {
            ordinal,
            attributes,
            ..
        } = op
        else {
            unreachable!() // walk_sequence passes only Steps
        };
        let frames: Vec<&'i [language::Attribute<'i>]> = attributes
            .iter()
            .copied()
            .filter(|frame| {
                !self
                    .path
                    .holds(frame)
            })
            .collect();
        for frame in &frames {
            self.path
                .push(PathSegment::Attributes(frame));
        }
        self.path
            .push(match ordinal {
                Ordinal::Dependent(s) => PathSegment::DependentStep(s),
                Ordinal::Parallel => PathSegment::ParallelStep(parallel),
            });
        let flow = self.perform_step(env, op)?;
        self.path
            .pop();
        for _ in &frames {
            self.path
                .pop();
        }
        Ok(flow)
    }

    fn perform_step(&mut self, env: &mut Environment, op: &'i Operation<'i>) -> Result<Flow, Halt> {
        let Operation::Step {
            source,
            body,
            responses,
            ..
        } = op
        else {
            unreachable!() // walk_step passes only Steps
        };
        let path = self
            .path
            .render();
        let reads = read_values([body.as_ref()], env);
        let slot = self.slot(&path);
        let stance = stance(slot.prior, &reads);
        if let Stance::Restore(a) = stance {
            return self.restore(env, a, Marker::Step);
        }
        let again = self.open(&slot, &path, reads, stance)?;
        self.display_step(env, source, &path);

        // A step that binds a name and offers responses binds the chosen one.
        let chooses = !responses.is_empty() && binds_descriptively(body);
        let flow = if chooses {
            done(Value::Unitus)
        } else {
            self.perform(env, body, again)?
        };
        let (outcome, restored) =
            self.resolve(env, |walker| walker.confirm(op, &path, flow, slot.seed))?;
        if !restored {
            if chooses {
                self.choose(env, body, &outcome)?;
            }
            self.vacate(env, std::slice::from_ref(body), &outcome)?;
        }
        self.finish(env, Marker::Step, &path, &outcome, restored)?;
        Ok(Flow::Completed(outcome))
    }

    // Ask the user to confirm a step's outcome, unless its body already
    // decided it: a value acquired, a command or action declined, a failure
    // thrown.
    fn confirm(
        &mut self,
        op: &'i Operation<'i>,
        path: &str,
        flow: Flow,
        seed: Option<&'h Activation>,
    ) -> Result<Outcome, Halt> {
        let Operation::Step {
            body, responses, ..
        } = op
        else {
            unreachable!() // perform_step passes only Steps
        };
        let acquired = responses.is_empty() && binds_descriptively(body);
        let choices: Vec<&str> = responses
            .iter()
            .map(|r| r.value)
            .collect();
        let kind = kind_of_step(
            &self
                .runner
                .library,
            op,
        );
        let former = match seed.and_then(|a| {
            a.outcome
                .as_ref()
                .or(a
                    .former_outcome
                    .as_ref())
        }) {
            Some(State::Done(Some(Value::Literali(text)))) if !choices.is_empty() => {
                Some(text.clone())
            }
            _ => None,
        };
        match flow {
            Flow::Completed(Outcome::Done(produced)) if !acquired => {
                let reply = self.ask_from(
                    Marker::Step,
                    path,
                    Prompt::Confirm {
                        standing: Standing::Done,
                        kind,
                        produced: &produced,
                        choices: &choices,
                    },
                    CONFIRM,
                    former,
                )?;
                Ok(answered(reply, produced))
            }
            Flow::Completed(Outcome::Fail(reason)) if !acquired => {
                let reply = self.ask(
                    Marker::Step,
                    path,
                    Prompt::Confirm {
                        standing: Standing::Fail,
                        kind: Kind::Prose,
                        produced: &Value::Unitus,
                        choices: &[],
                    },
                    OVERRULE,
                )?;
                // Accepting a failure without a reason keeps the original
                // one.
                Ok(match answered(reply, Value::Unitus) {
                    Outcome::Fail(given) if given.is_empty() => Outcome::Fail(reason),
                    outcome => outcome,
                })
            }
            other => Ok(outcome_of(&other)),
        }
    }

    // Bind a choice step's names to what was chosen.
    fn choose(
        &mut self,
        env: &mut Environment,
        body: &'i Operation<'i>,
        outcome: &Outcome,
    ) -> Result<(), Halt> {
        let Some(names) = binding_names(body) else {
            return Ok(());
        };
        let value = match outcome {
            Outcome::Done(value) => value.clone(),
            Outcome::Skip(_) => Value::Unitus,
            Outcome::Fail(_) => return Ok(()),
        };
        evaluator::bind_names(env, names, value)?;
        self.note(env, names)
    }

    fn walk_prologue(
        &mut self,
        env: &mut Environment,
        ops: &'i [Operation<'i>],
    ) -> Result<Flow, Halt> {
        self.path
            .push(PathSegment::Prologue);
        let path = self
            .path
            .render();
        let slot = self.slot(&path);
        let stance = stance(slot.prior, &[]);
        let flow = match stance {
            Stance::Restore(a) => self.restore_quietly(env, a),
            _ => {
                let again = self.open(&slot, &path, Vec::new(), stance)?;
                let flow = if again && self.pending() {
                    self.withhold(env, ops)?
                } else {
                    self.walk_sequence(env, ops)?
                };
                let (outcome, restored) = self.resolve(env, |_| Ok(outcome_of(&flow)))?;
                if !restored {
                    self.vacate(env, ops, &outcome)?;
                }
                self.record(env, &path, &outcome, restored)?;
                Flow::Completed(outcome)
            }
        };
        self.path
            .pop();
        Ok(flow)
    }

    fn walk_section(
        &mut self,
        env: &mut Environment,
        numeral: &'i str,
        title: &'i Option<Box<Operation<'i>>>,
        body: &'i Operation<'i>,
    ) -> Result<Flow, Halt> {
        self.path
            .push(PathSegment::Section(numeral));
        let path = self
            .path
            .render();
        // A title's expressions are evaluated with the section, so what they
        // read counts too.
        let reads = read_values(
            title
                .iter()
                .map(Box::as_ref)
                .chain([body]),
            env,
        );
        let slot = self.slot(&path);
        let stance = stance(slot.prior, &reads);
        let flow = match stance {
            Stance::Restore(a) => self.restore(env, a, Marker::Close)?,
            _ => {
                let again = self.open(&slot, &path, reads, stance)?;
                let heading = match title {
                    Some(title) => match self.value(env, title)? {
                        Ok(Value::Literali(text)) => text,
                        Ok(other) => other.to_string(),
                        Err(_) => String::new(),
                    },
                    None => String::new(),
                };
                self.runner
                    .driver
                    .show(Event::Section {
                        path: &path,
                        numeral,
                        title: &heading,
                    });
                let flow = self.perform(env, body, again)?;
                let outcome = self.seal(env, &path, flow, kind_of_scope(body))?;
                Flow::Completed(outcome)
            }
        };
        self.path
            .pop();
        Ok(flow)
    }

    fn walk_loop(
        &mut self,
        env: &mut Environment,
        names: &'i [language::Identifier<'i>],
        over: &'i Option<Box<Operation<'i>>>,
        body: &'i Operation<'i>,
    ) -> Result<Flow, Halt> {
        let Some(over) = over else {
            loop {
                let number = self.number();
                self.walk_iteration(env, names, number, body)?;
            }
        };
        // Resolution guarantees the name is in scope, not that it holds a
        // value.
        if let Operation::Variable(id, _) = over.as_ref() {
            if env
                .lookup(id.value)
                .is_none()
            {
                return Ok(done(Value::Unitus));
            }
        }
        let items = match self.value(env, over)? {
            Ok(value) => evaluator::coerce_to_list(value)?,
            Err(flow) => return Ok(flow),
        };
        let mut rollup = Rollup::new();
        for item in items {
            evaluator::bind_names(env, names, item)?;
            let number = self.number();
            match self.walk_iteration(env, names, number, body)? {
                Flow::Completed(outcome) => rollup.absorb(outcome),
                throwing => return Ok(throwing),
            }
        }
        // A loop yields unit, keeping only its verdict.
        Ok(Flow::Completed(match rollup.outcome() {
            Outcome::Done(_) => Outcome::Done(Value::Unitus),
            Outcome::Skip(_) => Outcome::Skip(Value::Unitus),
            failed => failed,
        }))
    }

    // Number the next iteration; sibling loops in one scope share one count.
    fn number(&mut self) -> usize {
        let scope = self.top();
        scope.iterations += 1;
        scope.iterations
    }

    fn walk_iteration(
        &mut self,
        env: &mut Environment,
        names: &'i [language::Identifier<'i>],
        number: usize,
        body: &'i Operation<'i>,
    ) -> Result<Flow, Halt> {
        self.path
            .push(PathSegment::Iteration(number));
        let path = self
            .path
            .render();
        let inputs = iteration_values(names, env);
        let mut slot = self.slot(&path);
        // Former answers pre-fill only an iteration over the same item.
        if slot
            .seed
            .is_some_and(|a| a.began != inputs)
        {
            slot.seed = None;
        }
        let stance = stance(slot.prior, &inputs);
        let flow = match stance {
            Stance::Restore(a) => self.restore(env, a, Marker::Close)?,
            _ => {
                let again = self.open(&slot, &path, inputs, stance)?;
                let echo = render_iteration_echo(names, env);
                self.runner
                    .driver
                    .show(Event::Enter {
                        path: &path,
                        echo: &echo,
                    });
                let flow = self.perform(env, body, again)?;
                // An iteration asks for confirmation only if it closed
                // before.
                let reclosing = self
                    .top()
                    .prior
                    .and_then(concluded)
                    .is_some();
                let (outcome, restored) = self.resolve(env, |walker| {
                    if reclosing {
                        walker.close(&path, &flow, kind_of_scope(body))
                    } else {
                        Ok(outcome_of(&flow))
                    }
                })?;
                if !restored {
                    self.vacate(env, std::slice::from_ref(body), &outcome)?;
                }
                self.finish(env, Marker::Close, &path, &outcome, restored)?;
                Flow::Completed(outcome)
            }
        };
        self.path
            .pop();
        Ok(flow)
    }

    fn walk_procedure(
        &mut self,
        env: &mut Environment,
        subroutine: &'i Subroutine<'i>,
        invocable: &'i Invocable<'i>,
    ) -> Result<Flow, Halt> {
        let Some(name) = subroutine
            .name
            .as_ref()
            .map(|n| n.value)
        else {
            unreachable!() // only the entry is anonymous
        };
        if self.depth >= DEPTH {
            return Err(RunnerError::RecursionLimit {
                procedure: name.to_string(),
                depth: DEPTH,
            }
            .into());
        }
        let caller = self
            .path
            .render();
        let given = match self.given(env, subroutine, invocable)? {
            Ok(given) => given,
            Err(flow) => return Ok(flow),
        };
        let segments: Vec<PathSegment<'i>> = subroutine
            .locale
            .iter()
            .map(|locale| match *locale {
                Locale::Procedure(n) => PathSegment::Procedure(n),
                Locale::Section(n) => PathSegment::Section(n),
            })
            .collect();
        let lexical = render_path(&segments);
        let slot = self.slot(&lexical);
        if given
            .iter()
            .any(Option::is_none)
        {
            self.asked
                .push(slot.serial);
        }
        let reask = self.reasking(slot.serial);
        // Arguments prompted for stand only while the evaluated ones are
        // unchanged.
        let current = match slot.prior {
            Some(a) if !reask && agrees(&a.began, &given) => Some(a),
            _ => None,
        };
        self.invoke(&caller, InvokeTarget::Procedure(name.to_string()))?;
        // A call the user declined at an argument prompt stays declined.
        if let Some(a) = current {
            if a.standing == engraving::Standing::Closed
                && a.children
                    .is_empty()
                && a.began
                    .len()
                    < given.len()
            {
                return self.restore(&mut Environment::new(), a, Marker::Close);
            }
        }
        let recorded = match current {
            Some(a) if a.standing != engraving::Standing::Withdrawn => Some(&a.began[..]),
            _ => None,
        };
        let supplied = match self.supply(subroutine, name, &lexical, &slot, given, recorded)? {
            Ok(supplied) => supplied,
            Err(flow) => return Ok(flow),
        };

        // The callee sees only its parameters.
        let mut local = Environment::new();
        bind_supplied(&mut local, &supplied);
        let stance = stance(slot.prior, &supplied);
        if let Stance::Restore(a) = stance {
            self.announce(subroutine, &lexical, &local);
            return self.restore(&mut local, a, Marker::Close);
        }
        let again = self.open(&slot, &lexical, supplied, stance)?;
        let saved = self
            .path
            .replace(segments);
        self.announce(subroutine, &lexical, &local);
        self.depth += 1;
        let flow = self.perform(&mut local, &subroutine.body, again)?;
        self.depth -= 1;
        let outcome = self.seal(&mut local, &lexical, flow, kind_of_scope(&subroutine.body))?;
        self.path
            .replace(saved);
        Ok(Flow::Completed(outcome))
    }

    // An invocation's arguments evaluated, `None` for each to be prompted
    // for.
    fn given(
        &mut self,
        env: &mut Environment,
        subroutine: &'i Subroutine<'i>,
        invocable: &'i Invocable<'i>,
    ) -> Result<Result<Vec<Option<Value>>, Flow>, Halt> {
        if invocable.elided {
            return Ok(Ok(vec![None; subroutine.arity()]));
        }
        let mut given = Vec::new();
        for argument in &invocable.arguments {
            if is_hole(argument) {
                given.push(None);
                continue;
            }
            match self.value(env, argument)? {
                Ok(value) => given.push(Some(value)),
                Err(flow) => return Ok(Err(flow)),
            }
        }
        Ok(Ok(given))
    }

    // Fill each argument to be prompted for from `recorded`, else ask the
    // user.
    fn supply(
        &mut self,
        subroutine: &'i Subroutine<'i>,
        name: &str,
        lexical: &str,
        slot: &Slot<'h>,
        given: Vec<Option<Value>>,
        recorded: Option<&[Supplied]>,
    ) -> Result<Result<Vec<Supplied>, Flow>, Halt> {
        let formae = render_parameter_formae(subroutine.signature);
        let caller = self
            .path
            .render();
        let label = format!("<{}>", name);
        let mut supplied = Vec::new();
        for (i, value) in given
            .into_iter()
            .enumerate()
        {
            let bind = subroutine
                .parameters
                .get(i)
                .cloned()
                .flatten();
            let value = match (value, recorded.and_then(|began| began.get(i))) {
                (Some(value), _) => value,
                (None, Some(item)) => item
                    .value
                    .clone(),
                (None, None) => {
                    // A verdict chosen in review stands in for the prompt.
                    if let Some(outcome) = self.verdict(slot.serial) {
                        return self.abandon(slot, lexical, supplied, outcome);
                    }
                    let seed = slot
                        .seed
                        .and_then(|a| {
                            a.began
                                .get(i)
                        })
                        .map(|item| &item.value);
                    let forma = formae
                        .get(i)
                        .map(|f| f.as_str());
                    let named = match &bind {
                        Some(bind) => Some(bind.as_str()),
                        None => None,
                    };
                    match self.acquire(&caller, &label, named, forma, seed)? {
                        Reply::Done(value) => value,
                        Reply::Override => Value::Unitus,
                        Reply::Skip => {
                            return self.abandon(
                                slot,
                                lexical,
                                supplied,
                                Outcome::Skip(Value::Unitus),
                            );
                        }
                        Reply::Fail(reason) => {
                            return self.abandon(slot, lexical, supplied, Outcome::Fail(reason));
                        }
                    }
                }
            };
            supplied.push(Supplied { value, name: bind });
        }
        Ok(Ok(supplied))
    }

    // Record an invocation declined at an argument prompt: a `Begin` with the
    // arguments supplied so far, then its outcome, at the callee's path.
    fn abandon(
        &mut self,
        slot: &Slot<'h>,
        path: &str,
        supplied: Vec<Supplied>,
        outcome: Outcome,
    ) -> Result<Result<Vec<Supplied>, Flow>, Halt> {
        let state = state_of(&outcome);
        self.compare(slot.seed, &state);
        self.write(slot.serial, path, State::Begin(supplied))?;
        self.write(slot.serial, path, state)?;
        Ok(Err(Flow::Completed(outcome)))
    }

    // Mark the innermost scope changed if a child closed differently from
    // before.
    fn compare(&mut self, seed: Option<&'h Activation>, state: &State) {
        let former = seed.and_then(|a| {
            a.outcome
                .as_ref()
                .or(a
                    .former_outcome
                    .as_ref())
        });
        if former != Some(state) {
            self.top()
                .changed = true;
        }
    }

    fn walk_external(
        &mut self,
        env: &mut Environment,
        uri: &'i str,
        arguments: &'i [Operation<'i>],
    ) -> Result<Flow, Halt> {
        let caller = self
            .path
            .render();
        let echo = match self.echo(env, arguments)? {
            Ok(echo) => echo,
            Err(flow) => return Ok(flow),
        };
        self.path
            .push(PathSegment::External(uri));
        let path = self
            .path
            .render();
        let slot = self.slot(&path);
        self.invoke(&caller, InvokeTarget::Uri(uri.to_string()))?;
        let stance = stance(slot.prior, &[]);
        let flow = match stance {
            Stance::Restore(a) => self.restore(env, a, Marker::Return)?,
            _ => {
                self.open(&slot, &path, Vec::new(), stance)?;
                let (outcome, restored) = self.resolve(env, |walker| {
                    match walker.ask(
                        Marker::Depart,
                        &path,
                        Prompt::Depart { echo: &echo },
                        BOUNDARY,
                    )? {
                        Reply::Skip => return Ok(Outcome::Skip(Value::Unitus)),
                        Reply::Fail(reason) => return Ok(Outcome::Fail(reason)),
                        Reply::Done(_) | Reply::Override => {}
                    }
                    walker
                        .runner
                        .driver
                        .show(Event::Depart {
                            path: &path,
                            echo: &echo,
                        });
                    let reply = walker.ask(Marker::Return, &path, Prompt::External, BOUNDARY)?;
                    Ok(answered(reply, Value::Unitus))
                })?;
                self.finish(env, Marker::Return, &path, &outcome, restored)?;
                Flow::Completed(outcome)
            }
        };
        self.path
            .pop();
        Ok(flow)
    }

    // An external call's arguments for display, `value ~ name` for a
    // variable.
    fn echo(
        &mut self,
        env: &mut Environment,
        arguments: &'i [Operation<'i>],
    ) -> Result<Result<String, Flow>, Halt> {
        if arguments.is_empty() {
            return Ok(Ok(String::new()));
        }
        let mut parts = Vec::with_capacity(arguments.len());
        for argument in arguments {
            let value = match self.value(env, argument)? {
                Ok(value) => value,
                Err(flow) => return Ok(Err(flow)),
            };
            parts.push(match argument {
                Operation::Variable(id, _) => {
                    format!("{} ~ {}", engraving::serialize_value(&value), id.value)
                }
                _ => engraving::serialize_value(&value),
            });
        }
        Ok(Ok(format!("({})", parts.join(", "))))
    }

    fn walk_execute(
        &mut self,
        env: &mut Environment,
        executable: &'i Executable<'i>,
    ) -> Result<Flow, Halt> {
        let values = match self.values(env, &executable.arguments)? {
            Ok(values) => values,
            Err(flow) => return Ok(flow),
        };
        let id = match &executable.target {
            ExecutableRef::Resolved(id) => *id,
            ExecutableRef::Unresolved(target) => {
                return Err(RunnerError::UnknownFunction(
                    target
                        .value
                        .to_string(),
                )
                .into());
            }
        };
        let function = self
            .runner
            .library
            .name(id);
        let described = format!("{}()", function);
        let nature = self
            .runner
            .library
            .nature(id);
        if let Nature::Pure = nature {
            self.runner
                .driver
                .show(Event::Announce(&described));
            return Ok(done(self.call(id, env, &values)?));
        }

        let scope = self.top();
        let k = scope.effects;
        scope.effects += 1;
        let serial = scope.serial;
        let prior = scope.prior;
        let recorded = prior.and_then(|a| {
            a.effects
                .get(k)
        });
        if let Some(flow) = reused(self.history, prior, k) {
            self.runner
                .driver
                .show(Event::Announce(&described));
            return Ok(flow);
        }
        let path = self
            .path
            .render();
        if recorded.is_none() {
            self.write(
                serial,
                &path,
                State::Execute {
                    function: function.to_string(),
                },
            )?;
        }
        let flow = match nature {
            Nature::Command => self.command(env, id, &path, &values)?,
            Nature::Action => self.action(env, id, &path, &values)?,
            Nature::Instant => done(self.call(id, env, &values)?),
            Nature::Pure => unreachable!(), // announced and returned above
        };
        let returned = match &flow {
            Flow::Completed(Outcome::Done(value)) => Some(value.clone()),
            _ => None,
        };
        self.write(serial, &path, State::Return(returned))?;
        Ok(flow)
    }

    // Run a gated command with the script the user chose, and only that.
    fn command(
        &mut self,
        env: &Environment,
        id: crate::program::ExecutableId,
        path: &str,
        values: &[Value],
    ) -> Result<Flow, Halt> {
        let script = match values.first() {
            Some(Value::Literali(text)) => text.clone(),
            Some(other) => other.to_string(),
            None => String::new(),
        };
        let chosen = match self.ask(
            Marker::Step,
            path,
            Prompt::Command { script: &script },
            BOUNDARY,
        )? {
            Reply::Done(chosen) => chosen,
            Reply::Override => Value::Literali(script),
            Reply::Skip => return Ok(Flow::Completed(Outcome::Skip(Value::Unitus))),
            Reply::Fail(reason) => return Ok(Flow::Throwing(reason)),
        };
        let script = match &chosen {
            Value::Literali(text) => text.clone(),
            other => other.to_string(),
        };
        self.runner
            .driver
            .show(Event::Command {
                path,
                script: &script,
            });
        match self.call(id, env, &[chosen]) {
            Ok(value) => Ok(done(value)),
            Err(RunnerError::CommandFailed(code)) => Ok(Flow::Throwing(format!(
                "External command exited with status {}",
                code
            ))),
            Err(error) => Err(error.into()),
        }
    }

    fn action(
        &mut self,
        env: &Environment,
        id: crate::program::ExecutableId,
        path: &str,
        values: &[Value],
    ) -> Result<Flow, Halt> {
        let library = &self
            .runner
            .library;
        let function = library.name(id);
        let verb = library
            .display(id)
            .unwrap_or(function);
        let shown = match values.first() {
            Some(value) => value.clone(),
            None => Value::Unitus,
        };
        match self.ask(
            Marker::Action,
            path,
            Prompt::Action {
                function,
                verb,
                value: &shown,
            },
            BOUNDARY,
        )? {
            Reply::Done(_) | Reply::Override => {
                self.runner
                    .driver
                    .show(Event::Action { path, function });
                Ok(done(self.call(id, env, values)?))
            }
            Reply::Skip => Ok(Flow::Completed(Outcome::Skip(Value::Unitus))),
            Reply::Fail(reason) => Ok(Flow::Throwing(reason)),
        }
    }

    fn call(
        &self,
        id: crate::program::ExecutableId,
        env: &Environment,
        values: &[Value],
    ) -> Result<Value, RunnerError> {
        self.runner
            .library
            .call(
                id,
                &self
                    .runner
                    .context,
                env,
                values,
            )
    }

    fn walk_bind(
        &mut self,
        env: &mut Environment,
        names: &'i [language::Identifier<'i>],
        value: &'i Operation<'i>,
        inferred: Option<&'i language::Genus<'i>>,
    ) -> Result<Flow, Halt> {
        if !is_empty_sequence(value) {
            return match self.walk(env, value)? {
                Flow::Completed(Outcome::Done(value)) => {
                    evaluator::bind_names(env, names, value)?;
                    self.note(env, names)?;
                    Ok(done(Value::Unitus))
                }
                Flow::Completed(Outcome::Skip(_)) => {
                    self.unbind(env, names)?;
                    Ok(Flow::Completed(Outcome::Skip(Value::Unitus)))
                }
                other => Ok(other),
            };
        }

        // Descriptive: each name is acquired from the user, unless the scope
        // being continued bound it already.
        let path = self
            .path
            .render();
        let forma = inferred.map(|genus| formatting::render_genus(genus, &Identity));
        let scope = self.top();
        let prior = scope.prior;
        let seed = scope.seed;
        let mut acquired = Vec::with_capacity(names.len());
        for name in names {
            let value = match prior.and_then(|a| lookup(&a.bound, name.value)) {
                Some(value) => value.clone(),
                None => {
                    let seed = seed.and_then(|a| lookup(kept(a), name.value));
                    let forma = match &forma {
                        Some(text) => Some(text.as_str()),
                        None => None,
                    };
                    match self.acquire(&path, "", Some(name.value), forma, seed)? {
                        Reply::Done(value) => value,
                        Reply::Override => Value::Unitus,
                        Reply::Skip => {
                            self.unbind(env, names)?;
                            return Ok(Flow::Completed(Outcome::Skip(Value::Unitus)));
                        }
                        Reply::Fail(reason) => {
                            self.unbind(env, names)?;
                            return Ok(Flow::Completed(Outcome::Fail(reason)));
                        }
                    }
                }
            };
            acquired.push(value);
        }
        for (name, value) in names
            .iter()
            .zip(acquired)
        {
            env.extend(
                name.value
                    .to_string(),
                value,
            );
        }
        self.note(env, names)?;
        Ok(done(Value::Unitus))
    }

    // A skipped binding binds each name to unit, and records that it did.
    fn unbind(
        &mut self,
        env: &mut Environment,
        names: &[language::Identifier<'_>],
    ) -> Result<(), Halt> {
        for name in names {
            env.extend(
                name.value
                    .to_string(),
                Value::Unitus,
            );
        }
        self.note(env, names)
    }

    // A failed scope binds unit to each name its body would have bound, as a
    // Skip does.
    fn vacate(
        &mut self,
        env: &mut Environment,
        ops: &'i [Operation<'i>],
        outcome: &Outcome,
    ) -> Result<(), Halt> {
        let Outcome::Fail(_) = outcome else {
            return Ok(());
        };
        let mut names = Vec::new();
        for op in ops {
            bindings(op, &mut names);
        }
        let bound = &self
            .top()
            .bound;
        let names: Vec<language::Identifier> = names
            .into_iter()
            .filter(|name| lookup(bound, name.value).is_none())
            .copied()
            .collect();
        self.unbind(env, &names)
    }

    // Note what the innermost scope binds, writing a `Bind` for any value not
    // already recorded.
    fn note(&mut self, env: &Environment, names: &[language::Identifier<'_>]) -> Result<(), Halt> {
        let scope = self.top();
        let mut fresh = Vec::new();
        for name in names {
            let item = Supplied {
                value: match env.lookup(name.value) {
                    Some(value) => value.clone(),
                    None => Value::Unitus,
                },
                name: Some(
                    name.value
                        .to_string(),
                ),
            };
            let held = scope
                .prior
                .and_then(|a| lookup(&a.bound, name.value));
            if held != Some(&item.value) {
                fresh.push(item.clone());
            }
            scope
                .bound
                .retain(|bound| bound.name != item.name);
            scope
                .bound
                .push(item);
        }
        if fresh.is_empty() {
            return Ok(());
        }
        let (serial, path) = (
            scope.serial,
            scope
                .path
                .clone(),
        );
        self.write(serial, &path, State::Bind(fresh))
    }

    // Walk a scope's body, unless it was begun again only to take a verdict
    // chosen in review; its names are then bound to unit.
    fn perform(
        &mut self,
        env: &mut Environment,
        body: &'i Operation<'i>,
        again: bool,
    ) -> Result<Flow, Halt> {
        if again && self.pending() {
            return self.withhold(env, std::slice::from_ref(body));
        }
        self.walk(env, body)
    }

    // A verdict chosen in review stands in for the body, which binds unit to
    // each name it would have bound.
    fn withhold(&mut self, env: &mut Environment, ops: &'i [Operation<'i>]) -> Result<Flow, Halt> {
        let mut names = Vec::new();
        for op in ops {
            bindings(op, &mut names);
        }
        let names: Vec<language::Identifier> = names
            .into_iter()
            .copied()
            .collect();
        self.unbind(env, &names)?;
        Ok(done(Value::Unitus))
    }

    // Close a scope that encloses steps: the entry procedure, a section, or
    // an invoked procedure.
    fn seal(
        &mut self,
        env: &mut Environment,
        path: &str,
        flow: Flow,
        kind: Kind,
    ) -> Result<Outcome, Halt> {
        let (outcome, restored) = self.resolve(env, |walker| walker.close(path, &flow, kind))?;
        self.finish(env, Marker::Close, path, &outcome, restored)?;
        Ok(outcome)
    }

    fn close(&mut self, path: &str, flow: &Flow, kind: Kind) -> Result<Outcome, Halt> {
        let (standing, produced, offers) = match outcome_of(flow) {
            Outcome::Fail(_) => (Standing::Fail, Value::Unitus, OVERRULE),
            Outcome::Skip(_) => (Standing::Skip, Value::Unitus, CONFIRM),
            Outcome::Done(value) => (Standing::Done, value, CONFIRM),
        };
        let reply = self.ask(
            Marker::Close,
            path,
            Prompt::Confirm {
                standing,
                kind,
                produced: &produced,
                choices: &[],
            },
            offers,
        )?;
        // Accepting a failure without a reason keeps the original one.
        Ok(match (answered(reply, produced), outcome_of(flow)) {
            (Outcome::Fail(given), Outcome::Fail(reason)) if given.is_empty() => {
                Outcome::Fail(reason)
            }
            (outcome, _) => outcome,
        })
    }

    // Decide how the innermost scope closes: restored, given by review, or
    // else by `ordinary`. Also answers whether the outcome was restored.
    fn resolve(
        &mut self,
        env: &mut Environment,
        ordinary: impl FnOnce(&mut Self) -> Result<Outcome, Halt>,
    ) -> Result<(Outcome, bool), Halt> {
        match self.conclude()? {
            Closing::Restored(outcome) => {
                if let Some(a) = self
                    .top()
                    .prior
                {
                    bind_supplied(env, &a.bound);
                }
                Ok((outcome, true))
            }
            Closing::Given(outcome) => Ok((outcome, false)),
            Closing::Ask => Ok((ordinary(self)?, false)),
        }
    }

    fn conclude(&mut self) -> Result<Closing, Halt> {
        let history = self.history;
        let scope = self.top();
        let serial = scope.serial;
        let prior = scope.prior;
        let mark = scope.mark;
        if let Some(a) = prior {
            let unreached: Vec<&Activation> = a
                .children
                .iter()
                .filter(|child| {
                    !scope
                        .reached
                        .contains(child)
                })
                .filter_map(|child| history.get(*child))
                .filter(|child| child.standing != engraving::Standing::Withdrawn)
                .collect();
            if !unreached.is_empty() {
                self.top()
                    .changed = true;
            }
            for child in unreached {
                self.write(child.serial, &child.path, State::Revoke)?;
            }
        }
        if let Some(outcome) = self.verdict(serial) {
            return Ok(Closing::Given(outcome));
        }
        let Some(a) = prior else {
            return Ok(Closing::Ask);
        };
        if self.written == mark && a.standing == engraving::Standing::Closed {
            return Ok(Closing::Restored(recorded(a)));
        }
        if a.revoked
            || self
                .top()
                .changed
        {
            return Ok(Closing::Ask);
        }
        match concluded(a) {
            Some((state, _)) => Ok(Closing::Given(outcome_from(state))),
            None => Ok(Closing::Ask),
        }
    }

    fn finish(
        &mut self,
        env: &mut Environment,
        marker: Marker,
        path: &str,
        outcome: &Outcome,
        restored: bool,
    ) -> Result<(), Halt> {
        self.record(env, path, outcome, restored)?;
        self.show_verdict(marker, path, outcome, restored);
        Ok(())
    }

    // Close the innermost scope, writing its outcome unless its recorded one
    // was restored.
    fn record(
        &mut self,
        env: &mut Environment,
        path: &str,
        outcome: &Outcome,
        restored: bool,
    ) -> Result<(), Halt> {
        let Some(scope) = self
            .scopes
            .pop()
        else {
            unreachable!() // the root scope is never closed
        };
        if restored {
            return Ok(());
        }
        let state = state_of(outcome);
        self.compare(scope.seed, &state);
        // A continued scope that bound nothing this walk keeps what it bound
        // before.
        if scope
            .bound
            .is_empty()
        {
            if let Some(a) = scope.prior {
                bind_supplied(env, &a.bound);
            }
        }
        self.write(scope.serial, path, state)
    }

    // A leaf whose recorded outcome stands: bind what it bound, yield what it
    // concluded.
    fn restore(
        &mut self,
        env: &mut Environment,
        a: &'h Activation,
        marker: Marker,
    ) -> Result<Flow, Halt> {
        let flow = self.restore_quietly(env, a);
        let outcome = recorded(a);
        self.show_verdict(marker, &a.path, &outcome, true);
        Ok(flow)
    }

    fn restore_quietly(&mut self, env: &mut Environment, a: &'h Activation) -> Flow {
        bind_supplied(env, &a.bound);
        Flow::Completed(recorded(a))
    }

    // Find the slot `path` takes beneath the innermost scope, giving it a new
    // serial if nothing was recorded there.
    fn slot(&mut self, path: &str) -> Slot<'h> {
        let history = self.history;
        let n = self
            .scopes
            .len()
            - 1;
        let scope = &mut self.scopes[n];
        let edge = edge(&scope.path, path).to_string();
        let count = scope
            .occurrences
            .entry(edge.clone())
            .or_insert(0);
        let occurrence = *count;
        *count += 1;
        let found = history.slot(scope.serial, &edge, occurrence);
        let serial = match found {
            Some(serial) => serial,
            None => {
                let serial = self.next;
                self.next = Serial(serial.0 + 1);
                serial
            }
        };
        scope
            .reached
            .push(serial);
        let prior = found
            .filter(|s| {
                scope
                    .children
                    .contains(s)
            })
            .and_then(|s| history.get(s));
        // Nothing beneath an unseeded scope is seeded.
        let seed = found
            .filter(|_| {
                scope
                    .seed
                    .is_some()
                    || scope.serial == Serial::ROOT
            })
            .and_then(|s| {
                history
                    .get(s)
                    .or_else(|| history.retired(s))
            });
        Slot {
            serial,
            prior,
            seed,
        }
    }

    // Enter a slot that is not restored, returning whether it was begun
    // again.
    fn open(
        &mut self,
        slot: &Slot<'h>,
        path: &str,
        inputs: Vec<Supplied>,
        stance: Stance<'h>,
    ) -> Result<bool, Halt> {
        let (prior, children, again): (Option<&'h Activation>, &'h [Serial], bool) = match stance {
            Stance::Continue(a) => (Some(a), &a.children, false),
            Stance::Fresh => (None, &[], false),
            Stance::Again => (None, &[], true),
            Stance::Restore(_) => unreachable!(), // restored slots are not entered
        };
        if prior.is_none() {
            // While records are held back, a `Begin` is held back with them
            // unless the user was prompted for its arguments.
            if self
                .runner
                .pending
                > 0
                && !self
                    .asked
                    .contains(&slot.serial)
            {
                self.runner
                    .defer(slot.serial, path, State::Begin(inputs));
            } else {
                self.write(slot.serial, path, State::Begin(inputs))?;
            }
        }
        let mut scope = Scope::new(slot.serial, path, children, self.written);
        scope.prior = prior;
        scope.seed = slot.seed;
        self.scopes
            .push(scope);
        Ok(again)
    }

    // Write an `Invoke`, unless the activation being continued recorded it.
    fn invoke(&mut self, path: &str, target: InvokeTarget) -> Result<(), Halt> {
        let scope = self.top();
        let k = scope.invokes;
        scope.invokes += 1;
        let serial = scope.serial;
        if let Some(a) = scope.prior {
            if a.invoked
                .len()
                > k
            {
                return Ok(());
            }
        }
        self.write(serial, path, State::Invoke(target))
    }

    // Whether the innermost scope has a verdict waiting from review.
    fn pending(&self) -> bool {
        let serial = self.scopes[self
            .scopes
            .len()
            - 1]
        .serial;
        match &self.amendment {
            Some(Amendment {
                serial: at,
                change: Change::Skip | Change::Fail(_) | Change::Override,
            }) => *at == serial,
            _ => false,
        }
    }

    // Take the verdict chosen in review for this serial, if one waits.
    fn verdict(&mut self, serial: Serial) -> Option<Outcome> {
        let outcome = match &self.amendment {
            Some(Amendment { serial: at, change }) if *at == serial => match change {
                Change::Skip => Outcome::Skip(Value::Unitus),
                Change::Fail(reason) => Outcome::Fail(reason.clone()),
                Change::Override => Outcome::Done(Value::Unitus),
                Change::Redo | Change::Reask => return None,
            },
            _ => return None,
        };
        self.amendment = None;
        Some(outcome)
    }

    fn reasking(&mut self, serial: Serial) -> bool {
        if let Some(Amendment {
            serial: at,
            change: Change::Reask,
        }) = &self.amendment
        {
            if *at == serial {
                self.amendment = None;
                return true;
            }
        }
        false
    }

    // Put a question, taking the user through review for as long as they are
    // there. Quit stops the session; an amendment unwinds the walk.
    fn ask(
        &mut self,
        marker: Marker,
        path: &str,
        prompt: Prompt<'_>,
        offers: &[Offer],
    ) -> Result<Reply, Halt> {
        self.ask_from(marker, path, prompt, offers, None)
    }

    // As `ask`, with the answer field opening on `draft`.
    fn ask_from(
        &mut self,
        marker: Marker,
        path: &str,
        prompt: Prompt<'_>,
        offers: &[Offer],
        mut draft: Option<String>,
    ) -> Result<Reply, Halt> {
        loop {
            let question = Question {
                marker,
                path,
                prompt: prompt.clone(),
                offers,
                reviewable: !self
                    .runner
                    .records
                    .is_empty(),
                draft: match &draft {
                    Some(text) => Some(text.as_str()),
                    None => None,
                },
            };
            match self
                .runner
                .driver
                .ask(question)
            {
                Answer::Done(value) => return Ok(Reply::Done(value)),
                Answer::Skip => return Ok(Reply::Skip),
                Answer::Fail(reason) => return Ok(Reply::Fail(reason)),
                Answer::Override => return Ok(Reply::Override),
                Answer::Quit => return Err(self.stop()),
                Answer::Review(typed) => {
                    let serial = self.scopes[self
                        .scopes
                        .len()
                        - 1]
                    .serial;
                    match self
                        .runner
                        .review(Some(serial), &self.asked)?
                    {
                        Reviewed::Leave => {
                            if typed.is_some() {
                                draft = typed;
                            }
                        }
                        Reviewed::Quit => return Err(self.stop()),
                        Reviewed::Amend(amendment) => return Err(Halt::Restart(amendment)),
                    }
                }
            }
        }
    }

    fn acquire(
        &mut self,
        path: &str,
        text: &str,
        name: Option<&str>,
        forma: Option<&str>,
        seed: Option<&Value>,
    ) -> Result<Reply, Halt> {
        self.ask(
            Marker::Enter,
            path,
            Prompt::Acquire {
                text,
                name,
                forma,
                seed,
            },
            BOUNDARY,
        )
    }

    fn stop(&mut self) -> Halt {
        match self
            .runner
            .stop()
        {
            Ok(()) => Halt::Stop,
            Err(error) => Halt::Error(error),
        }
    }

    fn write(&mut self, serial: Serial, path: &str, state: State) -> Result<(), Halt> {
        self.runner
            .append(serial, path, state)?;
        self.written += 1;
        Ok(())
    }

    fn top(&mut self) -> &mut Scope<'h> {
        let n = self
            .scopes
            .len()
            - 1;
        &mut self.scopes[n]
    }

    fn show_verdict(&mut self, marker: Marker, path: &str, outcome: &Outcome, restored: bool) {
        let verdict = verdict_of(outcome);
        self.runner
            .driver
            .show(Event::Verdict {
                marker,
                path,
                verdict: &verdict,
                restored,
            });
    }

    fn display_step(&mut self, env: &Environment, source: &'i language::Scope<'i>, path: &str) {
        let subs = env.substitutions();
        let text = formatting::render_step(
            source,
            &subs,
            self.runner
                .driver
                .renderer(),
        );
        let constraints = render_constraints(&self.constraints);
        let depth = self
            .path
            .depth();
        self.runner
            .driver
            .show(Event::Step {
                path,
                constraints: &constraints,
                text: &text,
                depth,
            });
    }

    // A named procedure's heading: its entry line, declaration, title, and
    // description.
    fn announce(&mut self, subroutine: &'i Subroutine<'i>, path: &str, env: &Environment) {
        let echo = if subroutine
            .parameters
            .is_empty()
        {
            String::new()
        } else {
            render_argument_echo(&subroutine.parameters, env)
        };
        let renderer = self
            .runner
            .driver
            .renderer();
        let driver = &mut self
            .runner
            .driver;
        driver.show(Event::Enter { path, echo: &echo });
        if let Some(source) = subroutine.source {
            driver.show(Event::Display(&formatting::render_procedure_declaration(
                source, renderer,
            )));
        }
        if let Some(title) = subroutine.title {
            driver.show(Event::Display(&formatting::render_title(title, renderer)));
        }
        if !subroutine
            .description
            .is_empty()
        {
            driver.show(Event::Display(&formatting::render_description(
                subroutine.description,
                renderer,
            )));
        }
    }
}

fn stance<'h>(prior: Option<&'h Activation>, inputs: &[Supplied]) -> Stance<'h> {
    let Some(a) = prior else {
        return Stance::Fresh;
    };
    if a.standing == engraving::Standing::Withdrawn || a.began != inputs {
        return Stance::Again;
    }
    if a.children
        .is_empty()
        && a.standing == engraving::Standing::Closed
    {
        return Stance::Restore(a);
    }
    Stance::Continue(a)
}

// The k-th effect of a continued activation, from its recorded `Return`. A
// bare `Return` last in an activation still open is run again, as it may have
// failed; if the activation closed failing there, the failure is thrown
// again.
fn reused(history: &History, prior: Option<&Activation>, k: usize) -> Option<Flow> {
    let a = prior?;
    let last = k + 1
        == a.effects
            .len();
    match a
        .effects
        .get(k)?
        .returned
        .as_ref()?
    {
        Some(value) => Some(done(value.clone())),
        None if last && a.standing != engraving::Standing::Closed => None,
        None => match &a.outcome {
            Some(State::Fail(reason)) if last && threw(history, a) => {
                Some(Flow::Throwing(reason_of(reason)))
            }
            _ => Some(Flow::Completed(Outcome::Skip(Value::Unitus))),
        },
    }
}

// Whether a closed activation failed at its last effect: after it, nothing
// but `Bind`s and the outcome were recorded, and no child was begun.
fn threw(history: &History, a: &Activation) -> bool {
    let Some(&end) = a
        .records
        .iter()
        .rev()
        .find(|i| {
            Some(**i) != a.closed_at
                && !a
                    .bound_at
                    .contains(i)
        })
    else {
        return false;
    };
    a.children
        .iter()
        .filter_map(|child| history.get(*child))
        .all(|child| child.begun_at < end)
}

fn done(value: Value) -> Flow {
    Flow::Completed(Outcome::Done(value))
}

fn outcome_of(flow: &Flow) -> Outcome {
    match flow {
        Flow::Completed(outcome) => outcome.clone(),
        Flow::Throwing(reason) => Outcome::Fail(reason.clone()),
    }
}

fn answered(reply: Reply, produced: Value) -> Outcome {
    match reply {
        Reply::Done(value) => Outcome::Done(value),
        Reply::Skip => Outcome::Skip(produced),
        Reply::Fail(reason) => Outcome::Fail(reason),
        Reply::Override => Outcome::Done(Value::Unitus),
    }
}

/// The outcome an activation recorded.
pub(super) fn recorded(a: &Activation) -> Outcome {
    match &a.outcome {
        Some(state) => outcome_from(state),
        None => Outcome::Done(Value::Unitus),
    }
}

fn outcome_from(state: &State) -> Outcome {
    match state {
        State::Skip => Outcome::Skip(Value::Unitus),
        State::Fail(reason) => Outcome::Fail(reason_of(reason)),
        State::Done(Some(value)) => Outcome::Done(value.clone()),
        _ => Outcome::Done(Value::Unitus),
    }
}

/// The text of a recorded `Fail [ "reason" = … ]`.
pub(super) fn reason_of(reason: &Option<Value>) -> String {
    match reason {
        Some(Value::Tabularum(pairs)) => match pairs.first() {
            Some((_, Value::Literali(text))) => text.clone(),
            Some((_, other)) => other.to_string(),
            None => String::new(),
        },
        Some(other) => other.to_string(),
        None => String::new(),
    }
}

fn state_of(outcome: &Outcome) -> State {
    match outcome {
        Outcome::Done(value) => State::Done(Some(value.clone())),
        Outcome::Skip(_) => State::Skip,
        Outcome::Fail(reason) if reason.is_empty() => State::Fail(None),
        Outcome::Fail(reason) => State::Fail(Some(engraving::fail_reason(reason))),
    }
}

pub(super) fn verdict_of(outcome: &Outcome) -> Verdict {
    match outcome {
        Outcome::Done(value) => Verdict::Done(value.clone()),
        Outcome::Skip(_) => Verdict::Skip,
        Outcome::Fail(reason) => Verdict::Fail(reason.clone()),
    }
}

// What a closed activation concluded and bound, or a reopened one did before
// its Revoke.
fn concluded(a: &Activation) -> Option<(&State, &[Supplied])> {
    match a.standing {
        engraving::Standing::Closed => a
            .outcome
            .as_ref()
            .map(|state| {
                (
                    state,
                    a.bound
                        .as_slice(),
                )
            }),
        engraving::Standing::Reopened => a
            .former_outcome
            .as_ref()
            .map(|state| {
                (
                    state,
                    a.former_bound
                        .as_slice(),
                )
            }),
        _ => None,
    }
}

// What an activation bound, or last bound before it was withdrawn.
fn kept(a: &Activation) -> &[Supplied] {
    if a.bound
        .is_empty()
    {
        &a.former_bound
    } else {
        &a.bound
    }
}

fn lookup<'a>(bound: &'a [Supplied], name: &str) -> Option<&'a Value> {
    bound
        .iter()
        .find(|item| match &item.name {
            Some(bound) => bound == name,
            None => false,
        })
        .map(|item| &item.value)
}

fn bind_supplied(env: &mut Environment, supplied: &[Supplied]) {
    for item in supplied {
        if let Some(name) = &item.name {
            env.extend(
                name.clone(),
                item.value
                    .clone(),
            );
        }
    }
}

fn agrees(began: &[Supplied], given: &[Option<Value>]) -> bool {
    given
        .iter()
        .zip(began)
        .all(|(value, item)| match value {
            Some(value) => *value == item.value,
            None => true,
        })
}

fn is_hole(op: &Operation) -> bool {
    if let Operation::Hole(_) = op {
        true
    } else {
        false
    }
}

fn is_empty_sequence(op: &Operation) -> bool {
    if let Operation::Sequence(ops, _) = op {
        ops.is_empty()
    } else {
        false
    }
}

/// Children's outcomes combined, worst first: Fail over Done over Skip. Keeps
/// the last value seen and the first failure's reason; Done if there were
/// none.
struct Rollup {
    rank: Option<Standing>,
    value: Value,
    failure: Option<String>,
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
            Outcome::Fail(reason) => {
                if self
                    .failure
                    .is_none()
                {
                    self.failure = Some(reason);
                }
                Standing::Fail
            }
        };
        self.rank = Some(match self.rank {
            Some(current) => current.max(rank),
            None => rank,
        });
    }

    fn observe(&mut self, value: Value) {
        self.value = value;
    }

    fn outcome(self) -> Outcome {
        match self
            .rank
            .unwrap_or(Standing::Done)
        {
            Standing::Fail => Outcome::Fail(
                self.failure
                    .unwrap_or_default(),
            ),
            Standing::Skip => Outcome::Skip(self.value),
            Standing::Done => Outcome::Done(self.value),
        }
    }
}

/// Classify a step by its final member: `Choice` if it offers responses,
/// otherwise from the last operation of its body.
fn kind_of_step(library: &super::library::Library, op: &Operation) -> Kind {
    match op {
        Operation::Step { responses, .. } if !responses.is_empty() => Kind::Choice,
        Operation::Step { body, .. } => kind_of_step(library, body),
        Operation::Sequence(ops, _) | Operation::Prologue(ops, _) => match ops.last() {
            Some(last) => kind_of_step(library, last),
            None => Kind::Prose,
        },
        Operation::Execute(executable, _) => match &executable.target {
            ExecutableRef::Resolved(id) => match library.nature(*id) {
                Nature::Pure => Kind::Computable,
                Nature::Command | Nature::Instant => Kind::System,
                Nature::Action => Kind::Action,
            },
            ExecutableRef::Unresolved(_) => Kind::Computable,
        },
        Operation::Prose(_, _) => Kind::Prose,
        _ => Kind::Computable,
    }
}

/// `Computable` if any member holds work, else `Prose`.
fn kind_of_scope(op: &Operation) -> Kind {
    match op {
        Operation::Sequence(ops, _) | Operation::Prologue(ops, _) => {
            if ops
                .iter()
                .any(|op| kind_of_scope(op) == Kind::Computable)
            {
                Kind::Computable
            } else {
                Kind::Prose
            }
        }
        Operation::Step { body, .. } | Operation::Section { body, .. } => kind_of_scope(body),
        Operation::Prose(_, _) => Kind::Prose,
        _ => Kind::Computable,
    }
}

/// The values a scope reads directly, in the order first met. A name not yet
/// bound contributes nothing.
fn read_values<'a, 'i: 'a>(
    ops: impl IntoIterator<Item = &'a Operation<'i>>,
    env: &Environment,
) -> Vec<Supplied> {
    let mut names = Vec::new();
    for op in ops {
        names_read(op, &mut names);
    }
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
            for argument in &invocable.arguments {
                names_read(argument, found);
            }
        }
        Operation::Execute(executable, _) => {
            for argument in &executable.arguments {
                names_read(argument, found);
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

/// The names of the first `Bind` in a step body.
fn binding_names<'i>(op: &Operation<'i>) -> Option<&'i [language::Identifier<'i>]> {
    match op {
        Operation::Bind { names, .. } => Some(names),
        Operation::Sequence(ops, _) => ops
            .iter()
            .find_map(binding_names),
        _ => None,
    }
}

/// Every name a body binds directly, not descending into nested steps.
fn bindings<'i>(op: &Operation<'i>, found: &mut Vec<&'i language::Identifier<'i>>) {
    match op {
        Operation::Bind { names, .. } => found.extend(names.iter()),
        Operation::Sequence(ops, _) | Operation::Prologue(ops, _) => {
            for op in ops {
                bindings(op, found);
            }
        }
        _ => {}
    }
}

/// Whether a step body is nothing but prose and descriptive `~` bindings.
fn binds_descriptively(op: &Operation) -> bool {
    match op {
        Operation::Bind { value, .. } => is_empty_sequence(value),
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

/// The loop variables bound for this pass, as an iteration's `Begin` states
/// them.
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

fn render_argument_echo(params: &[Option<String>], env: &Environment) -> String {
    let names: Vec<&str> = params
        .iter()
        .flatten()
        .map(String::as_str)
        .collect();
    format!("({})", render_bindings(&names, env))
}

fn render_iteration_echo(names: &[language::Identifier], env: &Environment) -> String {
    if names.is_empty() {
        return String::new();
    }
    let names: Vec<&str> = names
        .iter()
        .map(|n| n.value)
        .collect();
    format!("({})", render_bindings(&names, env))
}

fn render_bindings(names: &[&str], env: &Environment) -> String {
    names
        .iter()
        .map(|name| match env.lookup(name) {
            Some(value) => format!("{} ~ {}", engraving::serialize_value(value), name),
            None => format!(" ~ {}", name),
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_constraints(constraints: &[Value]) -> String {
    constraints
        .iter()
        .map(|budget| format!("$({budget})"))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Each parameter's forma as a prompt shows it. A single list parameter
/// (`[Region]`) renders bracketed so the prompt takes a list.
pub(super) fn render_parameter_formae(signature: Option<&language::Signature>) -> Vec<String> {
    match signature.map(|s| &s.requires) {
        Some(genus @ language::Genus::List(_)) => vec![formatting::render_genus(genus, &Identity)],
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

#[cfg(test)]
#[path = "checks/walker.rs"]
mod check;
