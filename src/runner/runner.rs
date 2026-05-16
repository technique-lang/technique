//! Interactive walker over a translated Program.

use std::collections::HashSet;
use std::io;
use std::path::PathBuf;

use super::evaluator::Environment;
use super::path::{PathSegment, QualifiedPath};
use super::prompt::{Prompt, UserInput};
use super::state::{Appender, Outcome as RecordOutcome, Record, RecordError, RunId};
use crate::program::{Operation, Ordinal, Program};
use crate::value::Value;

/// What executing an Operation (or evaluating a Step at any scale)
/// produced. `Done(Value)` is the natural success — for a leaf Step
/// the operator's recorded value, for a Sequence / Section / procedure
/// body the unit value once the whole subtree is finished. `Skipped` and
/// `Failed` are operator verdicts on individual Steps. `Quit` is a
/// control signal that propagates immediately up the call stack:
/// nothing is recorded, a `technique resume` would pick up where
/// this run paused.
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Outcome {
    Done(Value),
    Skipped,
    Failed(Failure),
    Quit,
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
    StoreError { path: PathBuf, error: io::Error },
    MalformedRecord { run: RunId, error: RecordError },
    ManifestMissing(RunId),
    InvalidRunId(String),
    MissingEntryProcedure,
    UnboundVariable(String),
    BindArityMismatch { expected: usize, actual: usize },
    UserQuit,
}

/// Execute a Technique interactively by walking the `Program` tree. Tracks
/// the position in the document via a `QaulifiedPath` stack, carries an
/// `Environment` with known result values. Maintains a set of
/// already-completed step FQNs, an append handle to write results, and the
/// prompt the operator interacts through.
#[allow(dead_code)]
pub struct Runner<'i, P: Prompt> {
    program: &'i Program<'i>,
    appender: Appender,
    completed: HashSet<String>,
    prompt: P,
    env: Environment,
    path: QualifiedPath<'i>,
}

#[allow(dead_code)]
impl<'i, P: Prompt> Runner<'i, P> {
    pub fn with_pieces(
        program: &'i Program<'i>,
        appender: Appender,
        completed: HashSet<String>,
        prompt: P,
    ) -> Self {
        Runner {
            program,
            appender,
            completed,
            prompt,
            env: Environment::new(),
            path: QualifiedPath::new(),
        }
    }

    /// Consume the runner and return the inner prompt. Tests use this
    /// to assert on the Mock's event log after a run completes.
    pub fn into_prompt(self) -> P {
        self.prompt
    }

    /// Walk the entry procedure top to bottom. Entry-procedure
    /// selection here is `program.subroutines[0]` — the synthetic
    /// anonymous wrapper if the document is top-level Steps, otherwise
    /// the first declared procedure.
    pub fn run(&mut self) -> Result<Outcome, RunnerError> {
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
        let result = self.walk(&entry.body);
        if name.is_some() {
            self.path
                .pop();
        }
        result
    }

    fn walk(&mut self, op: &'i Operation<'i>) -> Result<Outcome, RunnerError> {
        match op {
            Operation::Sequence(ops) => self.walk_sequence(ops),
            Operation::Section {
                numeral,
                title,
                body,
                ..
            } => self.walk_section(numeral, title.as_deref(), body),
            Operation::Step { .. } => {
                // Dependent vs Parallel ordinal index needs the
                // surrounding Sequence's parallel counter; a Step
                // encountered outside a Sequence (i.e. as the entire
                // body of a procedure) is treated as Dependent.
                self.walk_step(op, 0)
            }
            // TODO
            Operation::Loop { body, .. } => self.walk(body),
            Operation::Invoke(_) | Operation::Execute(_) => Ok(Outcome::Done(Value::Unitus)),
            Operation::Bind { .. }
            | Operation::Variable(_)
            | Operation::Number(_)
            | Operation::String(_)
            | Operation::Multiline(_, _)
            | Operation::Tablet(_) => {
                let value = super::evaluator::evaluate(&mut self.env, op)?;
                Ok(Outcome::Done(value))
            }
        }
    }

    fn walk_sequence(&mut self, ops: &'i [Operation<'i>]) -> Result<Outcome, RunnerError> {
        let mut parallel_idx: usize = 0;
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
                    self.walk_step(op, index)?
                }
                _ => self.walk(op)?,
            };
            if let Outcome::Quit = outcome {
                return Ok(Outcome::Quit);
            }
        }
        Ok(Outcome::Done(Value::Unitus))
    }

    fn walk_section(
        &mut self,
        numeral: &'i str,
        title: Option<&'i Operation<'i>>,
        body: &'i Operation<'i>,
    ) -> Result<Outcome, RunnerError> {
        self.path
            .push(PathSegment::Section(numeral));
        let result = self.execute_section(title, body);
        self.path
            .pop();
        result
    }

    fn execute_section(
        &mut self,
        title: Option<&'i Operation<'i>>,
        body: &'i Operation<'i>,
    ) -> Result<Outcome, RunnerError> {
        let qualified = self
            .path
            .render();
        let title_text = match title {
            Some(op) => match super::evaluator::evaluate(&mut self.env, op)? {
                Value::Literali(s) => s,
                other => other.to_string(),
            },
            None => String::new(),
        };
        self.prompt
            .section(&qualified, &title_text);
        self.walk(body)
    }

    fn walk_step(
        &mut self,
        op: &'i Operation<'i>,
        parallel_index: usize,
    ) -> Result<Outcome, RunnerError> {
        let Operation::Step {
            ordinal,
            attributes,
            description,
            body,
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

        let result = self.execute_step(&qualified, body, description);

        self.path
            .pop();
        for _ in attributes {
            self.path
                .pop();
        }

        result
    }

    fn execute_step(
        &mut self,
        qualified: &str,
        body: &'i Operation<'i>,
        description: &'i [Operation<'i>],
    ) -> Result<Outcome, RunnerError> {
        if self
            .completed
            .contains(qualified)
        {
            return Ok(Outcome::Done(Value::Unitus));
        }
        if let Outcome::Quit = self.walk(body)? {
            return Ok(Outcome::Quit);
        }

        let mut description_text = String::new();
        for op in description {
            if !description_text.is_empty() {
                description_text.push('\n');
            }
            match super::evaluator::evaluate(&mut self.env, op)? {
                Value::Literali(s) => description_text.push_str(&s),
                other => description_text.push_str(&other.to_string()),
            }
        }

        self.prompt
            .step(qualified, &description_text);
        let outcome = outcome_from(
            self.prompt
                .ask(),
        );
        if let Outcome::Quit = outcome {
            return Ok(Outcome::Quit);
        }

        let record = Record {
            recorded: now_iso8601(),
            path: qualified.to_string(),
            outcome: record_outcome(&outcome),
        };
        self.appender
            .append(&record)?;
        self.completed
            .insert(qualified.to_string());
        Ok(outcome)
    }
}

/// Lift a `UserInput` from the prompt into the runner's `Outcome`.
fn outcome_from(input: UserInput) -> Outcome {
    match input {
        UserInput::Done(value) => Outcome::Done(value),
        UserInput::Skip => Outcome::Skipped,
        UserInput::Fail => Outcome::Failed(Failure::Aborted("Failed".to_string())),
        UserInput::Quit => Outcome::Quit,
    }
}

/// Project the runner's in-memory `Outcome` into the on-disk shape
/// the PFFTT writer expects. Done always serializes as `result = ()`
/// for now — rendered-value persistence is future work. Quit is
/// unreachable here: the caller filters it out before recording.
fn record_outcome(outcome: &Outcome) -> RecordOutcome {
    match outcome {
        Outcome::Done(_) => RecordOutcome::Done(Some("()".to_string())),
        Outcome::Skipped => RecordOutcome::Skipped,
        Outcome::Failed(Failure::Aborted(reason)) => {
            RecordOutcome::Failed(Some(format!("\"{}\"", reason)))
        }
        Outcome::Quit => unreachable!("Quit is not recorded"),
    }
}

/// Current UTC time as an RFC3339 second-precision string, used for
/// the `recorded` field of every Result tablet.
fn now_iso8601() -> String {
    use time::format_description::well_known::Rfc3339;
    time::OffsetDateTime::now_utc()
        .replace_nanosecond(0)
        .unwrap() // Zero nanoseconds is always valid
        .format(&Rfc3339)
        .unwrap() // Rfc3339 formatting is infallible for a valid OffsetDateTime
}

#[cfg(test)]
#[path = "checks/runner.rs"]
mod check;
