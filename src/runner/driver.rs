//! Driver trait, console and automatic implementations, and test mock.
//!
//! A driver is what walks the operator (or no one) through a run; the
//! run's `Mode` selects which. The walker tells the driver what to show
//! (`step`, `section`, `announce`) and then asks for the step's outcome
//! (`ask`). `Console` renders to stdout and reads the operator's verdict
//! from stdin; `Automatic` takes the body's computed value with no human;
//! `Mock` records what the walker tried to show and returns canned answers.

use std::io::{self, BufRead, Write};

use crate::value::Value;

/// Which driver walks a run: `Interactive` prompts the user, `Automatic` runs
/// to completion, taking each step's body value as the result.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mode {
    Interactive,
    Automatic,
}

/// The person executing each step indicates a verdict on each prompt as
/// follows:
#[derive(Debug, Clone, PartialEq)]
pub enum UserInput {
    Done(Value),
    Skip,
    Fail,
    Quit,
}

/// What the walker uses to drive a run. Implementations are the interactive
/// console `Console`, the no-operator `Automatic`, and the test `Mock`.
pub trait Driver {
    /// Show the step's Qualified Name and rendered description.
    /// The implementation displays them; it does not block waiting for
    /// input — the walker calls `ask` for that separately.
    fn step(&mut self, qualified: &str, description: &str);

    /// Announce entry to a Section, with its Qualified Name and title text.
    fn section(&mut self, qualified: &str, title: &str);

    /// Surface an informational line — Loop body announcements,
    /// Execute / Unresolved Invoke announce-only, resume diagnostics.
    fn announce(&mut self, message: &str);

    /// Answer the most recent `step` prompt. `produced` is the value the
    /// step body computed, offered as the step's value: `Console` presents it
    /// for the operator to accept, `Automatic` takes it directly. When
    /// `choices` is non-empty the operator instead selects one of those
    /// response values, yielding `Done(Literali(choice))`. Skip, fail, and
    /// quit are available either way.
    fn ask(&mut self, choices: &[&str], produced: &Value) -> UserInput;
}

/// Interactive console prompt. Writes to stdout, reads line-buffered
/// stdin. `d` → Done, `s` → Skip, `f` → Fail, `q` → Quit; matched
/// case-insensitively on the first non-whitespace character. Anything
/// else re-prompts.
pub struct Console<R: BufRead, W: Write> {
    input: R,
    output: W,
}

impl Console<io::BufReader<io::Stdin>, io::Stdout> {
    /// Default console wired to process stdin (line-buffered) and
    /// stdout.
    pub fn new() -> Self {
        Console {
            input: io::BufReader::new(io::stdin()),
            output: io::stdout(),
        }
    }
}

#[cfg(test)]
impl<R: BufRead, W: Write> Console<R, W> {
    /// Construct a console over arbitrary input and output handles.
    /// Useful for tests that exercise the actual rendering and parsing
    /// paths without needing a TTY.
    pub fn with_handles(input: R, output: W) -> Self {
        Console { input, output }
    }
}

impl<R: BufRead, W: Write> Driver for Console<R, W> {
    fn step(&mut self, fqn: &str, description: &str) {
        let _ = writeln!(self.output, "  {}", fqn);
        let _ = writeln!(self.output, "{}", description);
    }

    fn section(&mut self, qualified: &str, title: &str) {
        let _ = writeln!(self.output);
        let _ = writeln!(self.output, "=== {} ===", qualified);
        if !title.is_empty() {
            let _ = writeln!(self.output, "{}", title);
        }
    }

    fn announce(&mut self, message: &str) {
        let _ = writeln!(self.output, "{}", message);
    }

    fn ask(&mut self, choices: &[&str], produced: &Value) -> UserInput {
        loop {
            if choices.is_empty() {
                let _ = write!(self.output, "[d]one / [s]kip / [f]ail / [q]uit ? ");
            } else {
                for (i, choice) in choices
                    .iter()
                    .enumerate()
                {
                    let _ = writeln!(self.output, "  {}) {}", i + 1, choice);
                }
                let _ = write!(
                    self.output,
                    "[1-{}] / [s]kip / [f]ail / [q]uit ? ",
                    choices.len()
                );
            }
            let _ = self
                .output
                .flush();
            let mut line = String::new();
            match self
                .input
                .read_line(&mut line)
            {
                Ok(0) => return UserInput::Quit,
                Ok(_) => {}
                Err(_) => return UserInput::Quit,
            }
            let trimmed = line.trim();
            // A numbered selection picks the corresponding response value.
            if !choices.is_empty() {
                if let Ok(n) = trimmed.parse::<usize>() {
                    if (1..=choices.len()).contains(&n) {
                        return UserInput::Done(Value::Literali(choices[n - 1].to_string()));
                    }
                }
            }
            match trimmed
                .chars()
                .next()
                .map(|c| c.to_ascii_lowercase())
            {
                Some('d') if choices.is_empty() => return UserInput::Done(produced.clone()),
                Some('s') => return UserInput::Skip,
                Some('f') => return UserInput::Fail,
                Some('q') => return UserInput::Quit,
                _ => continue,
            }
        }
    }
}

/// No-operator driver: writes a trace of each step to its output and takes
/// the body's computed value as the step's outcome, running to completion
/// or first failure. A pure-prose step (empty body value) records ().
pub struct Automatic<W: Write> {
    output: W,
}

impl Automatic<io::Stdout> {
    pub fn new() -> Self {
        Automatic {
            output: io::stdout(),
        }
    }
}

#[cfg(test)]
impl<W: Write> Automatic<W> {
    pub fn with_handle(output: W) -> Self {
        Automatic { output }
    }
}

impl<W: Write> Driver for Automatic<W> {
    fn step(&mut self, fqn: &str, description: &str) {
        let _ = writeln!(self.output, "  {}", fqn);
        if !description.is_empty() {
            let _ = writeln!(self.output, "{}", description);
        }
    }

    fn section(&mut self, qualified: &str, title: &str) {
        let _ = writeln!(self.output, "=== {} ===", qualified);
        if !title.is_empty() {
            let _ = writeln!(self.output, "{}", title);
        }
    }

    fn announce(&mut self, message: &str) {
        let _ = writeln!(self.output, "{}", message);
    }

    fn ask(&mut self, _choices: &[&str], produced: &Value) -> UserInput {
        UserInput::Done(produced.clone())
    }
}

/// Simulated prompt responses for test cases. Returns answers from a
/// pre-loaded queue and records every announcement / step / section call so a
/// test can assert what the walker tried to show.
#[cfg(test)]
#[derive(Debug, Default)]
pub struct Mock {
    answers: std::collections::VecDeque<UserInput>,
    events: Vec<Event>,
}

/// One thing the walker showed (or attempted to show). Tests use this
/// to inspect ordering and content of the walker's user-facing output.
#[cfg(test)]
#[derive(Debug, Clone, PartialEq)]
pub enum Event {
    Step {
        qualified: String,
        description: String,
    },
    Section {
        qualified: String,
        title: String,
    },
    Announce(String),
    Ask {
        choices: Vec<String>,
    },
}

#[cfg(test)]
impl Mock {
    /// Construct a Mock with no canned answers — useful for tests that
    /// only inspect announcements and never reach an `ask` call.
    pub fn new() -> Self {
        Mock::default()
    }

    /// Construct a Mock pre-loaded with the answers `ask()` will pop
    /// in order. Tests that walk past `n` Step prompts need at least
    /// `n` answers.
    pub fn with_answers<I: IntoIterator<Item = UserInput>>(answers: I) -> Self {
        Mock {
            answers: answers
                .into_iter()
                .collect(),
            events: Vec::new(),
        }
    }

    /// Snapshot the event log so far. Cheap clone for assertion in tests.
    pub fn events(&self) -> &[Event] {
        &self.events
    }
}

#[cfg(test)]
impl Driver for Mock {
    fn step(&mut self, fqn: &str, description: &str) {
        self.events
            .push(Event::Step {
                qualified: fqn.to_string(),
                description: description.to_string(),
            });
    }

    fn section(&mut self, fqn: &str, title: &str) {
        self.events
            .push(Event::Section {
                qualified: fqn.to_string(),
                title: title.to_string(),
            });
    }

    fn announce(&mut self, message: &str) {
        self.events
            .push(Event::Announce(message.to_string()));
    }

    fn ask(&mut self, choices: &[&str], _produced: &Value) -> UserInput {
        self.events
            .push(Event::Ask {
                choices: choices
                    .iter()
                    .map(|c| c.to_string())
                    .collect(),
            });
        self.answers
            .pop_front()
            .expect("Mock::ask called with no canned answers remaining")
    }
}

#[cfg(test)]
#[path = "checks/driver.rs"]
mod check;
