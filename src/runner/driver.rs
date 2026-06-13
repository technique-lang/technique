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
use crossterm::style::{Attribute, SetAttribute, Stylize};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode, Clear, ClearType};
use crossterm::{cursor, queue};

use crate::formatting::{Identity, Render};
use crate::highlighting::Terminal;
use crate::value::Value;

/// Which driver walks a run: `Interactive` prompts the user, `Automatic` runs
/// to completion, taking each step's body value as the result.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mode {
    Interactive,
    Automatic,
}

/// The person executing each step indicates a verdict on each prompt as
/// follows. `Quit` stops the run (also Ctrl-C); the run stays resumable and is
/// recorded as a `Stop` lifecycle event, so a deliberate stop is distinguishable
/// from a crash.
#[derive(Debug, Clone, PartialEq)]
pub enum UserInput {
    Done(Value),
    Skip,
    Fail(String),
    Quit,
}

/// What the walker uses to drive a run. Implementations are the interactive
/// console `Console`, the no-operator `Automatic`, and the test `Mock`.
pub trait Driver {
    /// Show the step's Qualified Name and rendered description.
    /// The implementation displays them; it does not block waiting for
    /// input — the walker calls `ask` for that separately.
    fn step(&mut self, qualified: &str, description: &str);

    /// Announce descent into a named scope — a Section or an invoked
    /// subroutine — with its Qualified Name and title text (the `↘` marker).
    fn enter(&mut self, qualified: &str, title: &str);

    /// Surface an informational line — Loop body announcements,
    /// Execute / Unresolved Invoke announce-only, resume diagnostics.
    fn announce(&mut self, message: &str);

    /// Answer the most recent `step` prompt. `produced` is the value the
    /// step body computed, offered as the step's value: `Console` presents it
    /// for the operator to accept, `Automatic` takes it directly. When
    /// `choices` is non-empty the operator instead selects one of those
    /// response values, yielding `Done(Literali(choice))`. Skip, fail, and
    /// quit are available either way. `produced` is consumed: the driver
    /// either moves it into the returned `Done` or discards it. `qualified` is
    /// the step's Qualified Name, repeated on the live prompt line.
    fn ask(&mut self, qualified: &str, choices: &[&str], produced: Value) -> UserInput;

    /// Gate a shell `exec` on the user's command: show the `script` to be run
    /// and settle on the user's verdict. `Done` means run it now; Skip / Fail
    /// decline the run and settle the step; Quit stops. `Automatic` runs
    /// unconditionally, returning `Done` without prompting.
    fn command(&mut self, qualified: &str, script: &str) -> UserInput;

    /// Open a Section: the grey `↘ /fqp` descent bracket (matching the `↙`
    /// the section's sign-off closes with) followed by its prose heading —
    /// numeral and title, e.g. `II. Check internet connectivity`.
    fn section(&mut self, qualified: &str, numeral: &str, title: &str);

    /// Prompt the operator to sign off a completed structural scope — a Section
    /// at its close, or the whole run at the entry procedure's close. Like
    /// `ask` but with no response choices, settling to the `↙` close marker;
    /// `produced` is the scope's value, offered for acceptance.
    fn seal(&mut self, qualified: &str, produced: Value) -> UserInput;

    /// The syntax renderer for highlighting source fragments shown to the
    /// user. `Console` returns the ANSI `Terminal` renderer; non-interactive
    /// drivers return `Identity` (no markup).
    fn renderer(&self) -> &'static dyn Render;
}

/// Interactive console prompt in a terminal. `step` / `section` / `announce`
/// print in "cooked mode" (to use the old terminfo slang term for it); `ask`
/// switches to "raw mode" to read keystrokes. The default is confirmation:
/// `<Enter>` completes the step, accepting the body's value intact. The
/// `<Esc>` menu offers Skip / Fail / Quit and, for an editable scalar, Edit —
/// the one path to reshape the value. The keystroke logic lives in
/// `Interaction`; this is the terminal shell around it.
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
        let _ = writeln!(self.output, "{}", format!("→ {}", fqn).dark_grey());
        let _ = writeln!(self.output);
        write_indented(&mut self.output, description);
        let _ = writeln!(self.output);
    }

    fn enter(&mut self, qualified: &str, title: &str) {
        let _ = writeln!(self.output, "{}", format!("↘ {}", qualified).dark_grey());
        if !title.is_empty() {
            let _ = writeln!(self.output);
            let _ = writeln!(self.output, "{}", title);
        }
        let _ = writeln!(self.output);
    }

    fn section(&mut self, qualified: &str, numeral: &str, title: &str) {
        let renderer = self.renderer();
        let _ = writeln!(self.output, "{}", format!("↘ {}", qualified).dark_grey());
        let _ = writeln!(self.output);
        render_section(&mut self.output, numeral, title, renderer);
        let _ = writeln!(self.output);
    }

    fn announce(&mut self, message: &str) {
        write_indented(&mut self.output, message);
    }

    fn ask(&mut self, qualified: &str, choices: &[&str], produced: Value) -> UserInput {
        prompt(&mut self.output, qualified, "→", choices, produced)
    }

    fn command(&mut self, qualified: &str, script: &str) -> UserInput {
        prompt_command(&mut self.output, qualified, script)
    }

    fn seal(&mut self, qualified: &str, produced: Value) -> UserInput {
        prompt(&mut self.output, qualified, "↙", &[], produced)
    }

    fn renderer(&self) -> &'static dyn Render {
        &Terminal
    }
}

/// Run one interactive prompt and settle it. The live `▶` line is drawn and
/// re-drawn as keys arrive; on settle it is replaced by a `settle` verdict line
/// (`→` for a step, `↙` for a scope close) in dark grey with a trailing verdict
/// glyph (`✓` done, `⊘` skip, `✗` fail), which scrolls up into the record —
/// except Quit, which leaves the scope unfinished (resume re-runs it) and just
/// clears the row.
fn prompt<W: Write>(
    out: &mut W,
    qualified: &str,
    settle: &str,
    choices: &[&str],
    produced: Value,
) -> UserInput {
    let result = interact(out, qualified, settle, Interaction::begin(choices, produced));
    let col = settle
        .chars()
        .count() as u16
        + 1
        + qualified
            .chars()
            .count() as u16
        + 1;
    match &result {
        UserInput::Done(_) => {
            let _ = queue!(out, cursor::MoveToColumn(col));
            let _ = write!(out, "{}", "✓".green());
            let _ = queue!(out, Clear(ClearType::UntilNewLine));
            let _ = writeln!(out);
        }
        UserInput::Skip => {
            let _ = queue!(out, cursor::MoveToColumn(col));
            let _ = write!(out, "{}", "⊘".yellow());
            let _ = queue!(out, Clear(ClearType::UntilNewLine));
            let _ = writeln!(out);
        }
        UserInput::Fail(_) => {
            let _ = queue!(out, cursor::MoveToColumn(col));
            let _ = write!(out, "{}", "✗".red());
            let _ = queue!(out, Clear(ClearType::UntilNewLine));
            let _ = writeln!(out);
        }
        UserInput::Quit => {
            let _ = writeln!(out);
        }
    }
    let _ = out.flush();
    result
}

/// Solicit the user's approval to run a shell command. The script appears
/// pre-filled on the `▶` prompt line as if already typed — Enter runs it,
/// typing edits it in place, Esc opens the menu (Skip / Fail / Quit). On
/// `Done` no settle line is printed — the command's output follows immediately
/// and the step's own verdict prompt judges the result.
fn prompt_command<W: Write>(out: &mut W, qualified: &str, script: &str) -> UserInput {
    let field = edit(script.to_string(), Value::Literali(script.to_string()));
    let result = interact(
        out,
        qualified,
        "→",
        Interaction { field, menu: None, reason: None },
    );
    let col = "→"
        .chars()
        .count() as u16
        + 1
        + qualified
            .chars()
            .count() as u16
        + 1;
    match &result {
        UserInput::Done(_) => {}
        UserInput::Quit => {
            let _ = writeln!(out);
        }
        UserInput::Skip => {
            let _ = queue!(out, cursor::MoveToColumn(col));
            let _ = write!(out, "{}", "⊘".yellow());
            let _ = queue!(out, Clear(ClearType::UntilNewLine));
            let _ = writeln!(out);
        }
        UserInput::Fail(_) => {
            let _ = queue!(out, cursor::MoveToColumn(col));
            let _ = write!(out, "{}", "✗".red());
            let _ = queue!(out, Clear(ClearType::UntilNewLine));
            let _ = writeln!(out);
        }
    }
    let _ = out.flush();
    result
}

/// Drive one raw-mode interaction to a settled `UserInput`, leaving the prompt
/// row cleared. Shared by the step/scope prompt and the exec command gate; the
/// caller writes whatever record line it wants afterward.
fn interact<W: Write>(out: &mut W, qualified: &str, settle: &str, mut interaction: Interaction) -> UserInput {
    // The interactive path is guarded on stdout being a terminal before the
    // walk begins, so a raw-mode failure here is an unexpected terminal fault
    // rather than a redirect; bail by quitting.
    if enable_raw_mode().is_err() {
        let _ = writeln!(out, "(could not enter raw mode)");
        return UserInput::Quit;
    }
    let result = loop {
        if draw(out, qualified, settle, &interaction).is_err() {
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
    let _ = queue!(out, cursor::Show);
    let _ = out.flush();
    result
}

/// Write text indented by four spaces, replicating the canonical source
/// layout the code formatter emits.
fn write_indented<W: Write>(out: &mut W, text: &str) {
    for line in text.lines() {
        let _ = writeln!(out, "    {}", line);
    }
}

/// Render a step's `→` line and description.
fn render_step<W: Write>(out: &mut W, fqn: &str, description: &str) {
    let _ = writeln!(out, "→ {}", fqn);
    let _ = writeln!(out);
    write_indented(out, description);
    let _ = writeln!(out);
}

/// Render a named scope's `↘` descent line and title.
fn render_enter<W: Write>(out: &mut W, qualified: &str, title: &str) {
    let _ = writeln!(out, "↘ {}", qualified);
    if !title.is_empty() {
        let _ = writeln!(out);
        let _ = writeln!(out, "{}", title);
    }
    let _ = writeln!(out);
}

/// Render a Section heading: its numeral and title.
fn render_section<W: Write>(out: &mut W, numeral: &str, title: &str, renderer: &dyn Render) {
    let styled_numeral = renderer.style(crate::formatting::Syntax::StepItem, numeral);
    if title.is_empty() {
        let _ = writeln!(out, "{}.", styled_numeral);
    } else {
        let _ = writeln!(out, "{}. {}", styled_numeral, title);
    }
}

/// One Esc-menu option. `Edit` reshapes the produced value in place; the rest
/// are the step exits. Navigation order is the slice order in `MENU`.
#[derive(Debug, Clone, Copy, PartialEq)]
enum MenuItem {
    Edit,
    Skip,
    Fail,
    Quit,
}

impl MenuItem {
    fn label(self) -> &'static str {
        match self {
            MenuItem::Edit => "Edit",
            MenuItem::Skip => "Skip",
            MenuItem::Fail => "Fail",
            MenuItem::Quit => "Quit",
        }
    }
}

/// The Esc-menu, always shown in full. `Edit` leads but is greyed and
/// unselectable unless the produced value is an editable scalar, so the menu
/// teaches that some steps can be edited while most cannot.
const MENU: [MenuItem; 4] = [
    MenuItem::Edit,
    MenuItem::Skip,
    MenuItem::Fail,
    MenuItem::Quit,
];

/// Shell-prompt glyph placed after the path, before the cursor — the
/// equivalent of `$` or `>` in a shell.
const PROMPT_SYMBOL: &str = "▶";

/// Number of columns spanned by the prompt prefix:
/// `{settle} {path} {PROMPT_SYMBOL} `
fn prompt_prefix_width(qualified: &str, settle: &str) -> u16 {
    settle
        .chars()
        .count() as u16
        + 1 // space after settle
        + qualified
            .chars()
            .count() as u16
        + 1 // space before prompt symbol
        + PROMPT_SYMBOL
            .chars()
            .count() as u16
        + 1 // space after prompt symbol
}

/// Shown on the prompt line (after the `▶` prefix) when soliciting a reason for
/// e.g. failure, replacing the menu; the typed buffer follows it.
const REASON_PREFIX: &str = "Reason? ";

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
        original: Value,
    },
    Frozen {
        produced: Value,
    },
    Choose {
        choices: Vec<String>,
        active: usize,
    },
}

/// The inline buffer soliciting a fail reason, shown on the menu line while
/// Fail is highlighted. A submenu of the menu, not a Field: the underlying
/// `Frozen` value is left untouched so backing out restores it.
struct Reason {
    buffer: String,
    cursor: usize,
}

/// Our state machine behind the "raw-mode" Console. `handle`
/// folds one key into the state, returning `Some(UserInput)` once the
/// operator has settled on an outcome. `reason` is the Fail submenu, open only
/// while `menu` rests on Fail.
struct Interaction {
    field: Field,
    menu: Option<usize>,
    reason: Option<Reason>,
}

impl Interaction {
    /// Seed an interaction with the supplied choices and a Value. The
    /// behaviour on pressing <Enter> is confirmation that the step is done,
    /// not data entry: `<Enter>` completes the step and the step's value is
    /// whatever its body produced.
    ///
    /// - a non-empty `choices` array selects between Responses; otherwise
    ///
    /// - the Value originally produced by the step is shown `Frozen`, and
    /// `<Enter>` accepts it intact.
    ///
    /// Editing is opt-in via the `<Esc>` menu (see `menu_items`), offered only
    /// when the value is an editable scalar; `ask()` will later present that
    /// same Edit field up-front.
    fn begin(choices: &[&str], produced: Value) -> Self {
        let field = if choices.is_empty() {
            Field::Frozen { produced }
        } else {
            Field::Choose {
                choices: choices
                    .iter()
                    .map(|c| c.to_string())
                    .collect(),
                active: 0,
            }
        };
        Interaction {
            field,
            menu: None,
            reason: None,
        }
    }

    /// Whether a menu item is currently selectable. Only `Edit` is ever
    /// disabled — offered when the produced value is an editable scalar, greyed
    /// otherwise; the exits are always available. Navigation skips a disabled
    /// item and the menu never opens onto one.
    fn enabled(&self, item: MenuItem) -> bool {
        match item {
            MenuItem::Edit => match &self.field {
                Field::Frozen { produced } => editable_seed(produced).is_some(),
                _ => false,
            },
            _ => true,
        }
    }

    /// First selectable item, where the menu opens. The exits are always
    /// enabled, so the fallback never fires.
    fn first_enabled(&self) -> usize {
        (0..MENU.len())
            .find(|&i| self.enabled(MENU[i]))
            .unwrap_or(0)
    }

    /// Nearest selectable item after / before `from`, or `None` at the edge —
    /// so navigation stops rather than wrapping, and steps over a greyed item.
    fn next_enabled(&self, from: usize) -> Option<usize> {
        ((from + 1)..MENU.len()).find(|&i| self.enabled(MENU[i]))
    }

    fn prev_enabled(&self, from: usize) -> Option<usize> {
        (0..from)
            .rev()
            .find(|&i| self.enabled(MENU[i]))
    }

    /// Transition a `Frozen` scalar into an editable buffer seeded from it.
    /// Reached only via the `Edit` menu item, which is offered only when the
    /// seed exists, so the fallback restore never fires in practice.
    fn enter_edit(&mut self) {
        if let Field::Frozen { produced } = &mut self.field {
            let taken = std::mem::replace(produced, Value::Unitus);
            match editable_seed(&taken) {
                Some(seed) => self.field = edit(seed, taken),
                None => self.field = Field::Frozen { produced: taken },
            }
        }
        self.menu = None;
    }

    fn handle(&mut self, key: KeyEvent) -> Option<UserInput> {
        if key
            .modifiers
            .contains(KeyModifiers::CONTROL)
        {
            if let KeyCode::Char('c') = key.code {
                // Believe it or not we actually have to handle <Ctrl>+<c>
                // explicitly when in raw mode! It reads as a blunt Quit.
                return Some(UserInput::Quit);
            }
        }
        if self
            .reason
            .is_some()
        {
            self.reason_key(key.code)
        } else if self
            .menu
            .is_some()
        {
            self.menu_key(key.code)
        } else {
            self.field_key(key.code)
        }
    }

    fn menu_key(&mut self, code: KeyCode) -> Option<UserInput> {
        let active = (*self
            .menu
            .as_ref()?)
        .min(MENU.len() - 1);
        match code {
            KeyCode::Left => {
                if let Some(prev) = self.prev_enabled(active) {
                    self.menu = Some(prev);
                }
                None
            }
            KeyCode::Right => {
                if let Some(next) = self.next_enabled(active) {
                    self.menu = Some(next);
                }
                None
            }
            KeyCode::Enter => match MENU[active] {
                MenuItem::Edit => {
                    self.enter_edit();
                    None
                }
                MenuItem::Skip => Some(UserInput::Skip),
                MenuItem::Fail => {
                    // Open the reason submenu, leaving Fail highlighted and the
                    // underlying value untouched so Esc can back out cleanly.
                    self.reason = Some(Reason {
                        buffer: String::new(),
                        cursor: 0,
                    });
                    None
                }
                MenuItem::Quit => Some(UserInput::Quit),
            },
            KeyCode::Esc => {
                self.menu = None;
                None
            }
            _ => None,
        }
    }

    /// Fold a key into the fail-reason submenu. `<Enter>` settles as
    /// `Fail(reason)` (an empty reason is taken as given); `<Esc>` closes the
    /// submenu back to the menu with Fail still highlighted; the rest edit the
    /// inline buffer.
    fn reason_key(&mut self, code: KeyCode) -> Option<UserInput> {
        let reason = self
            .reason
            .as_mut()?;
        match code {
            KeyCode::Enter => Some(UserInput::Fail(std::mem::take(&mut reason.buffer))),
            KeyCode::Esc => {
                self.reason = None;
                None
            }
            other => {
                text_key(&mut reason.buffer, &mut reason.cursor, other);
                None
            }
        }
    }

    fn field_key(&mut self, code: KeyCode) -> Option<UserInput> {
        if let KeyCode::Esc = code {
            self.menu = Some(self.first_enabled());
            return None;
        }
        match &mut self.field {
            Field::Edit {
                buffer,
                cursor,
                edited,
                original,
            } => match code {
                KeyCode::Enter => {
                    if *edited {
                        Some(UserInput::Done(Value::Literali(std::mem::take(buffer))))
                    } else {
                        Some(UserInput::Done(std::mem::replace(original, Value::Unitus)))
                    }
                }
                other => {
                    if text_key(buffer, cursor, other) {
                        *edited = true;
                    }
                    None
                }
            },
            Field::Frozen { produced } => match code {
                KeyCode::Enter => Some(UserInput::Done(std::mem::replace(produced, Value::Unitus))),
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
                _ => None,
            },
        }
    }
}

/// Draw the current interaction state onto the repeatedly-cleared prompt line.
/// Layout: `{settle} {path} ▶ {content}` — the settle arrow and path are dark
/// grey (matching the trace lines above), and ▶ is the shell-prompt character
/// before the cursor/content area.
fn draw<W: Write>(out: &mut W, qualified: &str, settle: &str, interaction: &Interaction) -> io::Result<()> {
    queue!(out, cursor::MoveToColumn(0), Clear(ClearType::CurrentLine))?;

    let prefix = prompt_prefix_width(qualified, settle);
    write!(
        out,
        "{} {} ",
        format!("{} {}", settle, qualified).dark_grey(),
        PROMPT_SYMBOL.blue(),
    )?;

    // Where the cursor lands; a value of `None` hides it.
    let mut cursor_col: Option<u16> = None;
    match interaction.menu {
        Some(active) => match &interaction.reason {
            Some(reason) => {
                write!(out, "{}{}", REASON_PREFIX, reason.buffer)?;
                cursor_col = Some(
                    prefix
                        + REASON_PREFIX
                            .chars()
                            .count() as u16
                        + reason.buffer[..reason.cursor]
                            .chars()
                            .count() as u16,
                );
            }
            None => render_menu(out, interaction, active)?,
        },
        None => match &interaction.field {
            Field::Edit { buffer, cursor, .. } => {
                write!(out, "{}", buffer)?;
                cursor_col = Some(
                    prefix
                        + buffer[..*cursor]
                            .chars()
                            .count() as u16,
                );
            }
            Field::Frozen { .. } => {
                cursor_col = Some(prefix);
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

    match cursor_col {
        Some(col) => queue!(out, cursor::Show, cursor::MoveToColumn(col))?,
        None => queue!(out, cursor::Hide)?,
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

/// Render the Esc-menu: the active item in reverse video, a disabled item (a
/// greyed `Edit`) dimmed, the rest plain. The active item is always enabled, so
/// reverse and dim never apply to the same item.
fn render_menu<W: Write>(out: &mut W, interaction: &Interaction, active: usize) -> io::Result<()> {
    for (i, item) in MENU
        .iter()
        .enumerate()
    {
        if i > 0 {
            write!(out, "  ")?;
        }
        let label = item.label();
        if i == active {
            queue!(out, SetAttribute(Attribute::Reverse))?;
            write!(out, "{}", label)?;
            queue!(out, SetAttribute(Attribute::Reset))?;
        } else if !interaction.enabled(*item) {
            queue!(out, SetAttribute(Attribute::Dim))?;
            write!(out, "{}", label)?;
            queue!(out, SetAttribute(Attribute::Reset))?;
        } else {
            write!(out, "{}", label)?;
        }
    }
    Ok(())
}

/// The buffer an editable scalar seeds its Edit field with: the rendered
/// number for a Quanticle, the raw text for an inline Literali. `None` marks
/// a value with nothing to edit, coming from a step returning Unit and thus
/// is onlt pure confirmation.
fn editable_seed(produced: &Value) -> Option<String> {
    match produced {
        Value::Unitus => None,
        Value::Quanticle(_) => Some(produced.to_string()),
        Value::Literali(text) if is_inline(text) => Some(text.clone()),
        // Complex or multi-line values are not editable inline.
        _ => None,
    }
}

/// Build an editable field from a scalar's text, cursor at the end. The
/// `original` value is returned verbatim if the buffer is accepted unedited.
fn edit(buffer: String, original: Value) -> Field {
    let cursor = buffer.len();
    Field::Edit {
        buffer,
        cursor,
        edited: false,
        original,
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

/// Apply one text-editing key to a buffer and cursor, returning whether the
/// buffer's content changed (an insertion or deletion) as opposed to a cursor
/// move or an unhandled key. Callers tracking an `edited` flag set it on a
/// `true` return; `<Enter>` / `<Esc>` are the caller's, not handled here.
fn text_key(buffer: &mut String, cursor: &mut usize, code: KeyCode) -> bool {
    match code {
        KeyCode::Char(c) => {
            buffer.insert(*cursor, c);
            *cursor += c.len_utf8();
            true
        }
        KeyCode::Backspace => {
            if *cursor > 0 {
                let start = prev_boundary(buffer, *cursor);
                buffer.replace_range(start..*cursor, "");
                *cursor = start;
                true
            } else {
                false
            }
        }
        KeyCode::Left => {
            *cursor = prev_boundary(buffer, *cursor);
            false
        }
        KeyCode::Right => {
            *cursor = next_boundary(buffer, *cursor);
            false
        }
        _ => false,
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
        render_step(&mut self.output, fqn, description);
    }

    fn enter(&mut self, qualified: &str, title: &str) {
        render_enter(&mut self.output, qualified, title);
    }

    fn section(&mut self, qualified: &str, numeral: &str, title: &str) {
        let renderer = self.renderer();
        let _ = writeln!(self.output, "↘ {}", qualified);
        let _ = writeln!(self.output);
        render_section(&mut self.output, numeral, title, renderer);
        let _ = writeln!(self.output);
    }

    fn announce(&mut self, message: &str) {
        write_indented(&mut self.output, message);
    }

    fn ask(&mut self, _qualified: &str, _choices: &[&str], produced: Value) -> UserInput {
        UserInput::Done(produced)
    }

    fn command(&mut self, _qualified: &str, script: &str) -> UserInput {
        write_indented(&mut self.output, script);
        UserInput::Done(Value::Unitus)
    }

    fn seal(&mut self, _qualified: &str, produced: Value) -> UserInput {
        UserInput::Done(produced)
    }

    fn renderer(&self) -> &'static dyn Render {
        &Identity
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
    Enter {
        qualified: String,
        title: String,
    },
    Section {
        qualified: String,
        numeral: String,
        title: String,
    },
    Announce(String),
    Command {
        qualified: String,
        script: String,
    },
    Ask {
        qualified: String,
        choices: Vec<String>,
    },
    Seal {
        qualified: String,
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

    fn enter(&mut self, fqn: &str, title: &str) {
        self.events
            .push(Event::Enter {
                qualified: fqn.to_string(),
                title: title.to_string(),
            });
    }

    fn section(&mut self, qualified: &str, numeral: &str, title: &str) {
        self.events
            .push(Event::Section {
                qualified: qualified.to_string(),
                numeral: numeral.to_string(),
                title: title.to_string(),
            });
    }

    fn announce(&mut self, message: &str) {
        self.events
            .push(Event::Announce(message.to_string()));
    }

    fn ask(&mut self, qualified: &str, choices: &[&str], _produced: Value) -> UserInput {
        self.events
            .push(Event::Ask {
                qualified: qualified.to_string(),
                choices: choices
                    .iter()
                    .map(|c| c.to_string())
                    .collect(),
            });
        self.answers
            .pop_front()
            .expect("Mock::ask called with no canned answers remaining")
    }

    /// Records the command beat and auto-commands the run (`Done`) without
    /// draining the answer queue — the exec gate is orthogonal to the step
    /// verdicts a test drives. A test asserting gate behaviour inspects the
    /// recorded `Command` event.
    fn command(&mut self, qualified: &str, script: &str) -> UserInput {
        self.events
            .push(Event::Command {
                qualified: qualified.to_string(),
                script: script.to_string(),
            });
        UserInput::Done(Value::Unitus)
    }

    /// A scope sign-off auto-accepts (records `Done`) and records the event,
    /// rather than draining the `ask` answer queue — the structural-scope
    /// close is orthogonal to the step verdicts a test drives. A test
    /// asserting sign-off behaviour inspects the recorded `Seal` event.
    fn seal(&mut self, qualified: &str, _produced: Value) -> UserInput {
        self.events
            .push(Event::Seal {
                qualified: qualified.to_string(),
            });
        UserInput::Done(Value::Unitus)
    }

    fn renderer(&self) -> &'static dyn Render {
        &Identity
    }
}

#[cfg(test)]
#[path = "checks/driver.rs"]
mod check;
