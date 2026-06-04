//! Driver trait, console and automatic implementations, and a test mock.
//!
//! A driver is what walks through a run; the run's `Mode` selects whether
//! this is a human user or a program running non-interactively.

//! The walker tells the driver what to show and then asks for the step's
//! outcome. `Console` drives a raw-mode terminal UI, presenting a step or
//! scope's returned Value as an editable candidate and reading the operator's
//! keystrokes; `Automatic` takes the body's returned Value with no human
//! intervention; `Mock` is for testing, recording what the walker tried to
//! show and returning canned answers.

use std::io::{self, Write};

use crossterm::event::{self, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use crossterm::style::{Attribute, SetAttribute};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode, Clear, ClearType};
use crossterm::{cursor, queue};

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
    /// quit are available either way. `produced` is consumed: the driver
    /// either moves it into the returned `Done` or discards it.
    fn ask(&mut self, choices: &[&str], produced: Value) -> UserInput;
}

/// Interactive console prompt in a terminal. `step` / `section` / `announce`
/// print in "cooked mode" (to use the old terminfo slang term for it); `ask`
/// switches to "raw mode" to read keystrokes, presenting the step's value as
/// an editable candidate (if a scalar) or a read-only display (a complex
/// value) and offering an `<Esc>` menu for Skip / Fail / Quit. The keystroke
/// logic lives in `Interaction`; this is the terminal shell around it.
pub struct Console<W: Write> {
    output: W,
}

impl Console<io::Stdout> {
    pub fn new() -> Self {
        Console {
            output: io::stdout(),
        }
    }
}

#[cfg(test)]
impl<W: Write> Console<W> {
    pub fn with_output(output: W) -> Self {
        Console { output }
    }
}

impl<W: Write> Driver for Console<W> {
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

    fn ask(&mut self, choices: &[&str], produced: Value) -> UserInput {
        let mut interaction = Interaction::begin(choices, produced);
        // The interactive path is guarded on stdout being a terminal before
        // the walk begins, so a raw-mode failure here is an unexpected
        // terminal fault rather than a redirect; bail by quitting.
        if enable_raw_mode().is_err() {
            let _ = writeln!(self.output, "(could not enter raw mode)");
            return UserInput::Quit;
        }
        let result = loop {
            if draw(&mut self.output, &interaction).is_err() {
                break UserInput::Quit;
            }
            match event::read() {
                Ok(event::Event::Key(key)) if key.kind != KeyEventKind::Release => {
                    if let Some(input) = interaction.handle(key) {
                        break input;
                    }
                }
                Ok(_) => {}
                Err(_) => break UserInput::Quit,
            }
        };
        let _ = disable_raw_mode();
        let _ = writeln!(self.output);
        result
    }
}

/// Esc-menu options, in navigation order.
const MENU: [&str; 3] = ["skip", "fail", "quit"];

/// Prompt prefix shown before an editable / read-only candidate, and its
/// width in terminal columns (for placing the edit cursor). Later we will
/// toggle between ▶ and ■
const PROMPT_TEXT: &str = "▶ ";
const PROMPT_WIDTH: u16 = 2;

/// Longest scalar offered as an inline-editable candidate. A longer (or
/// multi-line) value can't be edited on one line, so it falls back to the
/// read-only display.
const INLINE_MAX: usize = 78;

/// The candidate a step prompt presents: an editable scalar, a read-only
/// complex value, or a response selection.
enum Field {
    Edit {
        buffer: String,
        cursor: usize,
        edited: bool,
        produced: Value,
    },
    Frozen {
        produced: Value,
    },
    Choose {
        choices: Vec<String>,
        active: usize,
    },
}

/// Our state machine behind the "raw-mode" Console. `handle`
/// folds one key into the state, returning `Some(UserInput)` once the
/// operator has settled on an outcome.
struct Interaction {
    field: Field,
    menu: Option<usize>,
}

impl Interaction {
    /// Seed an interaction with the supplied choices and a Value. There is
    /// some complex UI logic encoded here:
    /// 
    /// - a non-empty `choices` array indicates we want to do selection
    /// between these choices;
    /// - a scalar Value becomes an editable buffer; and
    /// - a complex value a read-only display.
    fn begin(choices: &[&str], produced: Value) -> Self {
        let field = if choices.is_empty() {
            match produced {
                Value::Unitus => edit(String::new(), Value::Unitus),
                quantity @ Value::Quanticle(_) => {
                    let buffer = quantity.to_string();
                    edit(buffer, quantity)
                }
                Value::Literali(text) if is_inline(&text) => {
                    edit(text.clone(), Value::Literali(text))
                }
                other => Field::Frozen { produced: other },
            }
        } else {
            Field::Choose {
                choices: choices
                    .iter()
                    .map(|c| c.to_string())
                    .collect(),
                active: 0,
            }
        };
        Interaction { field, menu: None }
    }

    fn handle(&mut self, key: KeyEvent) -> Option<UserInput> {
        if key
            .modifiers
            .contains(KeyModifiers::CONTROL)
        {
            if let KeyCode::Char('c') = key.code {
                // Believe it or not we actually have to handle <Ctrl>+<c>
                // explicitly when in raw mode!
                return Some(UserInput::Quit);
            }
        }
        if self
            .menu
            .is_some()
        {
            self.menu_key(key.code)
        } else {
            self.field_key(key.code)
        }
    }

    fn menu_key(&mut self, code: KeyCode) -> Option<UserInput> {
        let active = self
            .menu
            .as_mut()?;
        match code {
            KeyCode::Left => {
                if *active > 0 {
                    *active -= 1;
                }
                None
            }
            KeyCode::Right => {
                if *active + 1 < MENU.len() {
                    *active += 1;
                }
                None
            }
            KeyCode::Enter => Some(menu_input(*active)),
            KeyCode::Esc => {
                self.menu = None;
                None
            }
            _ => None,
        }
    }

    fn field_key(&mut self, code: KeyCode) -> Option<UserInput> {
        match &mut self.field {
            Field::Edit {
                buffer,
                cursor,
                edited,
                produced,
            } => match code {
                KeyCode::Enter => {
                    if *edited {
                        Some(UserInput::Done(Value::Literali(std::mem::take(buffer))))
                    } else {
                        Some(UserInput::Done(std::mem::replace(produced, Value::Unitus)))
                    }
                }
                KeyCode::Char(c) => {
                    buffer.insert(*cursor, c);
                    *cursor += c.len_utf8();
                    *edited = true;
                    None
                }
                KeyCode::Backspace => {
                    if *cursor > 0 {
                        let start = prev_boundary(buffer, *cursor);
                        buffer.replace_range(start..*cursor, "");
                        *cursor = start;
                        *edited = true;
                    }
                    None
                }
                KeyCode::Left => {
                    *cursor = prev_boundary(buffer, *cursor);
                    None
                }
                KeyCode::Right => {
                    *cursor = next_boundary(buffer, *cursor);
                    None
                }
                KeyCode::Esc => {
                    self.menu = Some(0);
                    None
                }
                _ => None,
            },
            Field::Frozen { produced } => match code {
                KeyCode::Enter => Some(UserInput::Done(std::mem::replace(produced, Value::Unitus))),
                KeyCode::Esc => {
                    self.menu = Some(0);
                    None
                }
                _ => None,
            },
            Field::Choose { choices, active } => match code {
                KeyCode::Left | KeyCode::Up => {
                    if *active > 0 {
                        *active -= 1;
                    }
                    None
                }
                KeyCode::Right | KeyCode::Down => {
                    if *active + 1 < choices.len() {
                        *active += 1;
                    }
                    None
                }
                KeyCode::Enter => Some(UserInput::Done(Value::Literali(std::mem::take(
                    &mut choices[*active],
                )))),
                KeyCode::Esc => {
                    self.menu = Some(0);
                    None
                }
                _ => None,
            },
        }
    }
}

/// Draw the current interaction state onto a single, repeatedly-cleared
/// terminal line, leaving the edit cursor at the right column.
fn draw<W: Write>(out: &mut W, interaction: &Interaction) -> io::Result<()> {
    queue!(out, cursor::MoveToColumn(0), Clear(ClearType::CurrentLine))?;
    match interaction.menu {
        Some(active) => {
            write!(out, "esc: ")?;
            render_choices(out, &MENU, active)?;
        }
        None => match &interaction.field {
            Field::Edit { buffer, cursor, .. } => {
                write!(out, "{}{}", PROMPT_TEXT, buffer)?;
                let col = PROMPT_WIDTH
                    + buffer[..*cursor]
                        .chars()
                        .count() as u16;
                queue!(out, cursor::MoveToColumn(col))?;
            }
            Field::Frozen { .. } => {
                // The value was already shown above; the prompt carries only
                // the affordances, not a re-truncation of it.
                write!(out, "{}[enter] done   [esc] skip / fail / quit", PROMPT_TEXT)?;
            }
            Field::Choose { choices, active } => {
                let refs: Vec<&str> = choices
                    .iter()
                    .map(String::as_str)
                    .collect();
                render_choices(out, &refs, *active)?;
            }
        },
    }
    out.flush()
}

/// Render a horizontal row of options with the active one in reverse video.
fn render_choices<W: Write>(out: &mut W, choices: &[&str], active: usize) -> io::Result<()> {
    for (i, choice) in choices
        .iter()
        .enumerate()
    {
        if i > 0 {
            write!(out, "  ")?;
        }
        if i == active {
            queue!(out, SetAttribute(Attribute::Reverse))?;
            write!(out, " {} ", choice)?;
            queue!(out, SetAttribute(Attribute::Reset))?;
        } else {
            write!(out, " {} ", choice)?;
        }
    }
    Ok(())
}

/// Map an Esc-menu index to its outcome.
fn menu_input(index: usize) -> UserInput {
    match index {
        0 => UserInput::Skip,
        1 => UserInput::Fail,
        _ => UserInput::Quit,
    }
}

/// Build an editable field from a scalar's text, cursor at the end.
fn edit(buffer: String, produced: Value) -> Field {
    let cursor = buffer.len();
    Field::Edit {
        buffer,
        cursor,
        edited: false,
        produced,
    }
}

/// Whether a scalar's text fits on the single editable candidate line.
fn is_inline(text: &str) -> bool {
    !text.contains('\n')
        && text
            .chars()
            .count()
            <= INLINE_MAX
}

/// Byte index of the char boundary one character before `i`.
fn prev_boundary(s: &str, i: usize) -> usize {
    s[..i]
        .chars()
        .next_back()
        .map_or(i, |c| i - c.len_utf8())
}

/// Byte index of the char boundary one character after `i`.
fn next_boundary(s: &str, i: usize) -> usize {
    s[i..]
        .chars()
        .next()
        .map_or(i, |c| i + c.len_utf8())
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

    fn ask(&mut self, _choices: &[&str], produced: Value) -> UserInput {
        UserInput::Done(produced)
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

    fn ask(&mut self, choices: &[&str], _produced: Value) -> UserInput {
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
