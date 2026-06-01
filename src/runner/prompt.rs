//! Prompt trait, console implementation, and test mock.
//!
//! The walker tells the prompt what to show (`step`, `section`,
//! `announce`) and then asks for the operator's verdict (`ask`). The
//! split lets a console implementation render output to stdout, then
//! read a line from stdin, while a test mock simply records what the
//! walker tried to show and returns canned responses.

use std::io::{self, BufRead, Write};

use crate::value::Value;

/// The person executing each step indicates a verdict on each prompt as
/// follows:
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum UserInput {
    Done(Value),
    Skip,
    Fail,
    Quit,
}

/// What the walker uses to talk to the user executing the Technique. The two
/// initial implementations are for an interactive console, `Console`, and for
/// test cases, `Mock`.
#[allow(dead_code)]
pub trait Prompt {
    /// Show the step's Qualified Name and rendered description.
    /// The implementation displays them; it does not block waiting for
    /// input — the walker calls `ask` for that separately.
    fn step(&mut self, qualified: &str, description: &str);

    /// Announce entry to a Section, with its Qualified Name and title text.
    fn section(&mut self, qualified: &str, title: &str);

    /// Surface an informational line — Loop body announcements,
    /// Execute / Unresolved Invoke announce-only, resume diagnostics.
    fn announce(&mut self, message: &str);

    /// Block until the operator answers the most recent `step` prompt.
    /// When `choices` is non-empty the operator selects one of those
    /// response values, yielding `Done(Literali(choice))`; an empty slice
    /// presents the plain done/skip/fail/quit verdict, yielding
    /// `Done(Unitus)`. Skip, fail, and quit remain available either way.
    fn ask(&mut self, choices: &[&str]) -> UserInput;
}

/// Interactive console prompt. Writes to stdout, reads line-buffered
/// stdin. `d` → Done, `s` → Skip, `f` → Fail, `q` → Quit; matched
/// case-insensitively on the first non-whitespace character. Anything
/// else re-prompts.
#[allow(dead_code)]
pub struct Console<R: BufRead, W: Write> {
    input: R,
    output: W,
}

#[allow(dead_code)]
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

#[allow(dead_code)]
impl<R: BufRead, W: Write> Console<R, W> {
    /// Construct a console over arbitrary input and output handles.
    /// Useful for end-to-end tests that exercise the actual rendering
    /// and parsing paths without needing a TTY.
    pub fn with_handles(input: R, output: W) -> Self {
        Console { input, output }
    }
}

impl<R: BufRead, W: Write> Prompt for Console<R, W> {
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

    fn ask(&mut self, choices: &[&str]) -> UserInput {
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
                Some('d') if choices.is_empty() => return UserInput::Done(Value::Unitus),
                Some('s') => return UserInput::Skip,
                Some('f') => return UserInput::Fail,
                Some('q') => return UserInput::Quit,
                _ => continue,
            }
        }
    }
}

/// Simulated prompt responses for test cases. Returns answers from a
/// pre-loaded queue and records every announcement / step / section call so a
/// test can assert what the walker tried to show.
#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct Mock {
    answers: std::collections::VecDeque<UserInput>,
    events: Vec<Event>,
}

/// One thing the walker showed (or attempted to show). Tests use this
/// to inspect ordering and content of the walker's user-facing output.
#[allow(dead_code)]
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

#[allow(dead_code)]
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

impl Prompt for Mock {
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

    fn ask(&mut self, choices: &[&str]) -> UserInput {
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
#[path = "checks/prompt.rs"]
mod check;
