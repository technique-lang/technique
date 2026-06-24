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
use crossterm::style::{
    Attribute, Color, ResetColor, SetAttribute, SetBackgroundColor, SetForegroundColor, Stylize,
};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode, Clear, ClearType};
use crossterm::{cursor, queue};

use super::path::display_path;
use crate::formatting::{Identity, Render, Syntax};
use crate::highlighting::Terminal;
use crate::value::Value;

/// Which driver walks a run: `Interactive` prompts the user, `Automatic` runs
/// to completion taking each step's body value as the result, `Quiet` does the
/// same but with the no-output `Headless` driver, leaving only executed
/// commands' output on the terminal.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mode {
    Interactive,
    Automatic,
    Quiet,
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
/// console `Console`, the no-operator `Automatic`, the no-output `Headless`,
/// the debugging `Transcript`, and the test `Mock`.
pub trait Driver {
    /// Show the step's Qualified Name and rendered description. `depth` is the
    /// step's document nesting level (one-origin), indenting the description to
    /// match while the `→` marker stays at the left margin. The implementation
    /// displays them; it does not block waiting for input — the walker calls
    /// `ask` for that separately.
    fn step(&mut self, qualified: &str, description: &str, depth: usize);

    /// Announce descent into a named scope — a Section or an invoked
    /// subroutine — with its Qualified Name (the `↘` marker).
    fn enter(&mut self, qualified: &str);

    /// Cross into this document on the way in: the `⇒` boundary line carrying
    /// the document name, version, and run identifier (`/ NetworkProbe,1 000096`).
    fn commence(&mut self, label: &str);

    /// Cross out of this document on the way out: the `⇐` boundary line carrying
    /// the document name, version, and run identifier (`/ NetworkProbe,1 000096`),
    /// closed by the run's rolled-up verdict glyph just as a scope's `↙` sign-off
    /// is. Quit renders no glyph.
    fn conclude(&mut self, label: &str, verdict: &UserInput);

    /// Display a line of formatted content at the left margin.
    fn display(&mut self, content: &str);

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
    /// the step's Qualified Name, repeated on the live prompt line. `computable`
    /// is whether the step has a body; an unattended driver `Done`s it, else `Skip`.
    fn ask(
        &mut self,
        qualified: &str,
        choices: &[&str],
        produced: Value,
        computable: bool,
    ) -> UserInput;

    /// Cross out into an external `<uri>` procedure: prompt at the `⇒` departure
    /// — the place arguments would be solicited — then leave the glyph-less `⇒`
    /// depart line, paired with the `⇐` return that `external` prompts and
    /// `settle` closes with the verdict. The same document-boundary crossing the
    /// run's own `commence`/`conclude` makes, one level down. `Quit` abandons
    /// before departing; the unattended drivers proceed with `Done`.
    fn depart(&mut self, qualified: &str) -> UserInput;

    /// Settle an external invocation this run cannot perform (a `<uri>` call
    /// into another document or system). `Console` prompts the operator to
    /// attest it — `Done` if it was performed or recorded elsewhere, otherwise
    /// `Skip` / `Fail` / `Quit`. The unattended drivers return `Skip`: nothing
    /// executed it and no one is present to vouch that it was done, so the run
    /// records that this execution did not do it rather than fabricating a Done.
    fn external(&mut self, qualified: &str) -> UserInput;

    /// Gate a shell `exec` on the user's command: show the `script` to be run
    /// and settle on the user's verdict. `Done` means run it now; Skip / Fail
    /// decline the run and settle the step; Quit stops. `Automatic` runs
    /// unconditionally, returning `Done` without prompting.
    fn command(&mut self, qualified: &str, script: &str) -> UserInput;

    /// Present a physical `Action` (a `browser`-library call like
    /// `click("Actions")`) the user performs themselves: show the imperative
    /// `verb` and its `label` read-only on the prompt line and settle on the
    /// user's verdict. `Done` means they did it, leaving a compact `{name}()`
    /// trace; Skip / Fail decline and settle the step; Quit stops. Unlike
    /// `command` there is no edit buffer. `Automatic` returns `Done` without
    /// prompting.
    fn action(&mut self, qualified: &str, name: &str, verb: &str, label: &str) -> UserInput;

    /// Open a Section: the grey `↘ /fqp` descent bracket (matching the `↙`
    /// the section's sign-off closes with) followed by its prose heading —
    /// numeral and title, e.g. `II. Check internet connectivity`.
    fn section(&mut self, qualified: &str, numeral: &str, title: &str);

    /// Prompt the operator to sign off a completed structural scope — a Section
    /// at its close, or the whole run at the entry procedure's close. Like
    /// `ask` but with no response choices, settling to the `↙` close marker;
    /// `produced` is the scope's value, offered for acceptance; `computable`
    /// drives unattended `Done`/`Skip` as in `ask`.
    fn seal(&mut self, qualified: &str, produced: Value, computable: bool) -> UserInput;

    /// Render the settled verdict line for a step or scope close: `marker`
    /// (`→` step, `↙` scope close), Qualified Name, and the verdict's glyph.
    /// Quit renders nothing.
    fn settle(&mut self, marker: &str, qualified: &str, verdict: &UserInput);

    /// Obtain a value for a deferred input: `Done` supplies it, Skip / Fail
    /// abandon the call, Quit stops the run.
    fn acquire(&mut self, qualified: &str, name: Option<&str>, forma: Option<&str>) -> UserInput;

    /// The syntax renderer for highlighting source fragments shown to the
    /// user — the ANSI `Terminal` when colouring, otherwise `Identity`.
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
    fn step(&mut self, fqn: &str, description: &str, depth: usize) {
        let renderer = self.renderer();
        render_step(
            &mut self.output,
            &display_path(fqn),
            description,
            depth,
            renderer,
        );
    }

    fn enter(&mut self, qualified: &str) {
        let renderer = self.renderer();
        render_enter(&mut self.output, &display_path(qualified), renderer);
        let _ = writeln!(self.output);
    }

    fn commence(&mut self, label: &str) {
        let renderer = self.renderer();
        write_marker_line(&mut self.output, &format!("⇒ {}", label), renderer);
        let _ = writeln!(self.output);
    }

    fn conclude(&mut self, label: &str, verdict: &UserInput) {
        let renderer = self.renderer();
        render_conclude(&mut self.output, label, verdict, renderer);
    }

    fn display(&mut self, content: &str) {
        let _ = writeln!(self.output, "{}", content);
        let _ = writeln!(self.output);
    }

    fn section(&mut self, qualified: &str, numeral: &str, title: &str) {
        let qualified = display_path(qualified);
        let renderer = self.renderer();
        write_marker_line(&mut self.output, &format!("↘ {}", qualified), renderer);
        let _ = writeln!(self.output);
        render_section(&mut self.output, numeral, title, renderer);
        let _ = writeln!(self.output);
    }

    fn announce(&mut self, message: &str) {
        write_indented(&mut self.output, message);
    }

    fn ask(
        &mut self,
        qualified: &str,
        choices: &[&str],
        produced: Value,
        _computable: bool,
    ) -> UserInput {
        prompt(&mut self.output, qualified, "→", choices, produced)
    }

    fn depart(&mut self, qualified: &str) -> UserInput {
        let input = prompt(&mut self.output, qualified, "⇒", &[], Value::Unitus);
        if let UserInput::Quit = input {
        } else {
            let renderer = self.renderer();
            write_marker_line(&mut self.output, &format!("⇒ {}", display_path(qualified)), renderer);
        }
        input
    }

    fn external(&mut self, qualified: &str) -> UserInput {
        prompt(&mut self.output, qualified, "⇐", &[], Value::Unitus)
    }

    fn command(&mut self, qualified: &str, script: &str) -> UserInput {
        prompt_command(&mut self.output, qualified, script)
    }

    fn action(&mut self, qualified: &str, name: &str, verb: &str, label: &str) -> UserInput {
        prompt_action(&mut self.output, qualified, name, verb, label)
    }

    fn seal(&mut self, qualified: &str, produced: Value, _computable: bool) -> UserInput {
        prompt(&mut self.output, qualified, "↙", &[], produced)
    }

    fn settle(&mut self, marker: &str, qualified: &str, verdict: &UserInput) {
        let qualified = display_path(qualified);
        let renderer = self.renderer();
        render_settle(&mut self.output, marker, &qualified, verdict, renderer);
        let _ = self
            .output
            .flush();
    }

    fn acquire(&mut self, qualified: &str, name: Option<&str>, forma: Option<&str>) -> UserInput {
        let label = format!(
            "{}({} : {})",
            display_path(qualified),
            name.unwrap_or("?"),
            forma.unwrap_or("?")
        );
        prompt_acquire(&mut self.output, &label, is_list_forma(forma))
    }

    fn renderer(&self) -> &'static dyn Render {
        &Terminal
    }
}

/// Run one interactive prompt and return the user's verdict, clearing the
/// live `▶` row on settle.
fn prompt<W: Write>(
    out: &mut W,
    qualified: &str,
    settle: &str,
    choices: &[&str],
    produced: Value,
) -> UserInput {
    let qualified = display_path(qualified);
    let result = interact(out, Interaction::begin(choices, produced), |o, i| {
        draw(o, &qualified, settle, i)
    });
    let _ = queue!(out, cursor::MoveToColumn(0), Clear(ClearType::CurrentLine));
    let _ = out.flush();
    result
}

/// Present a read-only action on the live prompt line — `» {path} {verb} {label} ▶`
/// — and settle on the user's verdict. On Done it leaves a compact dark-grey
/// trace `» {path} {name}()`; Skip / Fail / Quit clear the line for the step's
/// own settle to follow.
fn prompt_action<W: Write>(
    out: &mut W,
    qualified: &str,
    name: &str,
    verb: &str,
    label: &str,
) -> UserInput {
    let qualified = display_path(qualified);
    let result = interact(out, Interaction::begin(&[], Value::Unitus), |o, i| {
        draw_action(o, &qualified, verb, label, i)
    });
    let _ = queue!(out, cursor::MoveToColumn(0), Clear(ClearType::CurrentLine));
    if let UserInput::Done(_) = &result {
        let _ = writeln!(out, "{}", format!("» {} {}()", qualified, name).dark_grey());
    }
    let _ = out.flush();
    result
}

/// Solicit the user's approval to run a shell command. The script appears
/// pre-filled on the '▶' prompt line as if already typed — Enter runs it,
/// typing edits it in place, Esc opens the menu (Skip / Fail / Quit). On
/// `Done` the live line is redrawn in grey with the interactive prompt marker
/// becoming the '$', reminiscent of a shell.
fn prompt_command<W: Write>(out: &mut W, qualified: &str, script: &str) -> UserInput {
    let qualified = display_path(qualified);
    let field = edit(
        script.to_string(),
        Value::Literali(script.to_string()),
        false,
    );
    let result = interact(
        out,
        Interaction {
            field,
            menu: None,
            reason: None,
        },
        |o, i| draw(o, &qualified, "→", i),
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
        UserInput::Done(produced) => {
            let ran = if let Value::Literali(text) = produced {
                text.trim_end()
            } else {
                script.trim_end()
            };
            let _ = queue!(out, cursor::MoveToColumn(0), Clear(ClearType::CurrentLine));
            let _ = writeln!(out, "{}", format!("→ {} $ {}", qualified, ran).dark_grey());
        }
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

/// Solicit a deferred input on the `▶` prompt line: `<Enter>` accepts the
/// empty default, typing overrides it; the `<Esc>` menu and `<Ctrl-C>` abandon
/// the call.
fn prompt_acquire<W: Write>(out: &mut W, label: &str, list: bool) -> UserInput {
    let field = edit(String::new(), Value::Literali(String::new()), list);
    let result = interact(
        out,
        Interaction {
            field,
            menu: None,
            reason: None,
        },
        |o, i| draw(o, label, "↘", i),
    );
    let _ = queue!(out, cursor::MoveToColumn(0), Clear(ClearType::CurrentLine));
    let _ = out.flush();
    result
}

/// Drive one raw-mode interaction to a settled `UserInput`, leaving the prompt
/// row cleared. Shared by the step/scope prompt and the exec command gate; the
/// caller writes whatever record line it wants afterward.
fn interact<W: Write>(
    out: &mut W,
    mut interaction: Interaction,
    mut render: impl FnMut(&mut W, &Interaction) -> io::Result<()>,
) -> UserInput {
    // The interactive path is guarded on stdout being a terminal before the
    // walk begins, so a raw-mode failure here is an unexpected terminal fault
    // rather than a redirect; bail by quitting.
    if enable_raw_mode().is_err() {
        let _ = writeln!(out, "(could not enter raw mode)");
        return UserInput::Quit;
    }
    let result = loop {
        if render(out, &interaction).is_err() {
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
    write_indented_by(out, text, 1);
}

/// Write text indented by four spaces per `depth` level, so a step's prose
/// sits at the same nesting it has in the source document. A `depth` of zero
/// is treated as one — every step is indented at least one level.
fn write_indented_by<W: Write>(out: &mut W, text: &str, depth: usize) {
    let pad = " ".repeat(4 * depth.max(1));
    for line in text.lines() {
        let _ = writeln!(out, "{}{}", pad, line);
    }
}

fn write_marker_line<W: Write>(out: &mut W, text: &str, renderer: &dyn Render) {
    let _ = writeln!(out, "{}", renderer.style(Syntax::Marker, text));
}

/// Render a step's `→` line and description. The marker stays at the left
/// margin; the description is indented to its document nesting `depth`.
fn render_step<W: Write>(
    out: &mut W,
    fqn: &str,
    description: &str,
    depth: usize,
    renderer: &dyn Render,
) {
    write_marker_line(out, &format!("→ {}", fqn), renderer);
    let _ = writeln!(out);
    write_indented_by(out, description, depth);
    let _ = writeln!(out);
}

/// Render a named scope's `↘` descent line.
fn render_enter<W: Write>(out: &mut W, qualified: &str, renderer: &dyn Render) {
    write_marker_line(out, &format!("↘ {}", qualified), renderer);
}

/// The glyph and styling for a settled verdict, or `None` for Quit (which
/// renders no glyph).
fn verdict_glyph(verdict: &UserInput) -> Option<(&'static str, Syntax)> {
    match verdict {
        UserInput::Done(_) => Some(("✓", Syntax::Done)),
        UserInput::Skip => Some(("⊘", Syntax::Skip)),
        UserInput::Fail(_) => Some(("✗", Syntax::Fail)),
        UserInput::Quit => None,
    }
}

fn render_settle<W: Write>(
    out: &mut W,
    marker: &str,
    qualified: &str,
    verdict: &UserInput,
    renderer: &dyn Render,
) {
    let (glyph, syntax) = match verdict_glyph(verdict) {
        Some(pair) => pair,
        None => return,
    };
    let path = renderer.style(Syntax::Marker, &format!("{} {}", marker, qualified));
    let _ = writeln!(out, "{} {}", path, renderer.style(syntax, glyph));
}

/// Render the run's closing `⇐` boundary line, the rolled-up verdict glyph
/// following the label just as a scope's `↙` sign-off carries its own. Quit
/// renders nothing, as in `render_settle`.
fn render_conclude<W: Write>(
    out: &mut W,
    label: &str,
    verdict: &UserInput,
    renderer: &dyn Render,
) {
    let (glyph, syntax) = match verdict_glyph(verdict) {
        Some(pair) => pair,
        None => return,
    };
    let line = renderer.style(Syntax::Marker, &format!("⇐ {}", label));
    let _ = writeln!(out, "{} {}", line, renderer.style(syntax, glyph));
}

/// Render an automatically-run shell command on one line: `{path} $ {script}`,
/// the whole line dark grey like the trace chrome so it reads as announce
/// rather than as the command's own output that follows. The `$` stands in for
/// a shell prompt (distinct from the operator's `▶` edit prompt). Trailing
/// whitespace is trimmed so a code block's blank tail line is not echoed.
fn render_command<W: Write>(out: &mut W, qualified: &str, script: &str, renderer: &dyn Render) {
    let line = renderer.style(Syntax::Marker, &format!("{} $ {}", qualified, script.trim_end()));
    let _ = writeln!(out, "{}", line);
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

/// Columns spanned by the action prompt prefix: `» {path} {verb} {label} ▶ `.
fn action_prefix_width(qualified: &str, verb: &str, label: &str) -> u16 {
    let mut width = 1 // »
        + 1 // space
        + qualified
            .chars()
            .count() as u16
        + 1 // space
        + verb
            .chars()
            .count() as u16;
    if !label.is_empty() {
        width += 1 // space
            + label
                .chars()
                .count() as u16;
    }
    width
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
        /// A list field renders its buffer between `[` and `]` and submits the
        /// buffer wrapped as `[buffer]`, so an empty answer yields `[]`.
        bracketed: bool,
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
                Some(seed) => self.field = edit(seed, taken, false),
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

    /// Carry out a menu item, as `<Enter>` does on the highlighted one. Fail
    /// opens the reason submenu, leaving the underlying value untouched so Esc
    /// can back out cleanly.
    fn activate(&mut self, item: MenuItem) -> Option<UserInput> {
        match item {
            MenuItem::Edit => {
                self.enter_edit();
                None
            }
            MenuItem::Skip => Some(UserInput::Skip),
            MenuItem::Fail => {
                self.reason = Some(Reason {
                    buffer: String::new(),
                    cursor: 0,
                });
                None
            }
            MenuItem::Quit => Some(UserInput::Quit),
        }
    }

    /// A letter shortcut: rest the highlight on `item`, then activate it.
    fn choose(&mut self, item: MenuItem) -> Option<UserInput> {
        self.menu = MENU
            .iter()
            .position(|m| *m == item);
        self.activate(item)
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
            KeyCode::Enter => self.activate(MENU[active]),
            KeyCode::Char('s') | KeyCode::Char('S') => self.choose(MenuItem::Skip),
            KeyCode::Char('f') | KeyCode::Char('F') => self.choose(MenuItem::Fail),
            KeyCode::Char('q') | KeyCode::Char('Q') => self.choose(MenuItem::Quit),
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
                bracketed,
            } => match code {
                KeyCode::Enter => {
                    if *bracketed {
                        // A list field always submits its buffer wrapped, so an
                        // empty answer is `[]` and `coerce_to_list` iterates it
                        // zero times.
                        Some(UserInput::Done(Value::Literali(format!("[{}]", buffer))))
                    } else if !*edited {
                        // Unchanged: return the original value verbatim, with
                        // its type and exact value intact.
                        Some(UserInput::Done(std::mem::replace(original, Value::Unitus)))
                    } else if let Value::Quanticle(_) = original {
                        // An edited numeric value stays numeric: re-parse the
                        // buffer with the language's own number grammar. A
                        // buffer that is not a valid number is not accepted —
                        // the edit stays open for correction.
                        match crate::parsing::parse_numeric(buffer) {
                            Some(numeric) => Some(UserInput::Done(Value::Quanticle(
                                crate::value::Numeric::from(&numeric),
                            ))),
                            None => None,
                        }
                    } else {
                        Some(UserInput::Done(Value::Literali(std::mem::take(buffer))))
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
fn draw<W: Write>(
    out: &mut W,
    qualified: &str,
    settle: &str,
    interaction: &Interaction,
) -> io::Result<()> {
    queue!(out, cursor::MoveToColumn(0), Clear(ClearType::CurrentLine))?;

    let prefix = prompt_prefix_width(qualified, settle);
    write!(
        out,
        "{} {} ",
        format!("{} {}", settle, qualified).dark_grey(),
        PROMPT_SYMBOL.blue(),
    )?;
    let cursor_col = draw_tail(out, interaction, prefix)?;
    place_cursor(out, cursor_col)
}

/// Draw the read-only action prompt line: `» {path} {verb} {label} ▶`, the verb
/// in light brown, the marker and path dark grey like the trace lines above.
fn draw_action<W: Write>(
    out: &mut W,
    qualified: &str,
    verb: &str,
    label: &str,
    interaction: &Interaction,
) -> io::Result<()> {
    queue!(out, cursor::MoveToColumn(0), Clear(ClearType::CurrentLine))?;
    write!(out, "{} ", format!("» {}", qualified).dark_grey())?;
    queue!(out, SetForegroundColor(LIGHT_BROWN))?;
    write!(out, "{}", verb)?;
    queue!(out, ResetColor)?;
    if !label.is_empty() {
        write!(out, " {}", label)?;
    }
    write!(out, " {} ", PROMPT_SYMBOL.blue())?;
    let prefix = action_prefix_width(qualified, verb, label);
    let cursor_col = draw_tail(out, interaction, prefix)?;
    place_cursor(out, cursor_col)
}

/// Render the interaction state after the prompt prefix — the Esc menu, the
/// fail-reason buffer, or the field's content — returning the cursor column
/// (`None` hides it). Shared by the step/command prompt and the action prompt.
fn draw_tail<W: Write>(
    out: &mut W,
    interaction: &Interaction,
    prefix: u16,
) -> io::Result<Option<u16>> {
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
            Field::Edit {
                buffer,
                cursor,
                bracketed,
                ..
            } => {
                let lead = if *bracketed {
                    write!(out, "[{}]", buffer)?;
                    1
                } else {
                    write!(out, "{}", buffer)?;
                    0
                };
                cursor_col = Some(
                    prefix
                        + lead
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
    Ok(cursor_col)
}

/// Position (or hide) the cursor after a prompt line is drawn, then flush.
fn place_cursor<W: Write>(out: &mut W, cursor_col: Option<u16>) -> io::Result<()> {
    match cursor_col {
        Some(col) => queue!(out, cursor::Show, cursor::MoveToColumn(col))?,
        None => queue!(out, cursor::Hide)?,
    }
    out.flush()
}

/// The orange that highlights Responses in the formatter, mirrored here so the
/// menu carries the same identity.
const RESPONSE: Color = Color::Rgb {
    r: 0xf5,
    g: 0x79,
    b: 0x00,
};

/// A darker brown for the active Response, legible on the white highlight bar.
const RESPONSE_ACTIVE: Color = Color::Rgb {
    r: 0x8f,
    g: 0x59,
    b: 0x02,
};

/// Light brown for an Action's verb, softer than the Response orange.
const LIGHT_BROWN: Color = Color::Rgb {
    r: 0xc8,
    g: 0x96,
    b: 0x4b,
};

/// Render a horizontal row of Response options in the formatter's orange, the
/// active one in reverse video.
fn render_choices<W: Write>(out: &mut W, choices: &[&str], active: usize) -> io::Result<()> {
    for (i, choice) in choices
        .iter()
        .enumerate()
    {
        if i > 0 {
            write!(out, "  ")?;
        }
        queue!(out, SetAttribute(Attribute::Bold))?;
        if i == active {
            queue!(
                out,
                SetBackgroundColor(Color::White),
                SetForegroundColor(RESPONSE_ACTIVE)
            )?;
            write!(out, " {} ", choice)?;
        } else {
            queue!(out, SetForegroundColor(RESPONSE))?;
            write!(out, " {} ", choice)?;
        }
        queue!(out, ResetColor, SetAttribute(Attribute::Reset))?;
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
            write!(out, " {} ", label)?;
            queue!(out, SetAttribute(Attribute::Reset))?;
        } else if !interaction.enabled(*item) {
            queue!(out, SetAttribute(Attribute::Dim))?;
            write!(out, " {} ", label)?;
            queue!(out, SetAttribute(Attribute::Reset))?;
        } else {
            write!(out, " {} ", label)?;
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
fn edit(buffer: String, original: Value, bracketed: bool) -> Field {
    let cursor = buffer.len();
    Field::Edit {
        buffer,
        cursor,
        edited: false,
        original,
        bracketed,
    }
}

/// Whether a forma display string reads as a list (`[…]`), the cue for the
/// bracketed list-entry prompt.
fn is_list_forma(forma: Option<&str>) -> bool {
    match forma {
        Some(text) => text.starts_with('[') && text.ends_with(']'),
        None => false,
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
    renderer: &'static dyn Render,
}

impl Automatic<io::Stdout> {
    pub fn new(colour: bool) -> Self {
        Automatic {
            output: io::stdout(),
            renderer: if colour { &Terminal } else { &Identity },
        }
    }
}

#[cfg(test)]
impl<W: Write> Automatic<W> {
    pub fn with_handle(output: W) -> Self {
        Automatic {
            output,
            renderer: &Identity,
        }
    }

    pub fn into_output(self) -> W {
        self.output
    }
}

impl<W: Write> Driver for Automatic<W> {
    fn step(&mut self, fqn: &str, description: &str, depth: usize) {
        render_step(
            &mut self.output,
            &display_path(fqn),
            description,
            depth,
            self.renderer,
        );
    }

    fn enter(&mut self, qualified: &str) {
        render_enter(&mut self.output, &display_path(qualified), self.renderer);
        let _ = writeln!(self.output);
    }

    fn commence(&mut self, label: &str) {
        write_marker_line(&mut self.output, &format!("⇒ {}", label), self.renderer);
        let _ = writeln!(self.output);
    }

    fn conclude(&mut self, label: &str, verdict: &UserInput) {
        render_conclude(&mut self.output, label, verdict, self.renderer);
    }

    fn display(&mut self, content: &str) {
        let _ = writeln!(self.output, "{}", content);
        let _ = writeln!(self.output);
    }

    fn section(&mut self, qualified: &str, numeral: &str, title: &str) {
        let qualified = display_path(qualified);
        write_marker_line(&mut self.output, &format!("↘ {}", qualified), self.renderer);
        let _ = writeln!(self.output);
        render_section(&mut self.output, numeral, title, self.renderer);
        let _ = writeln!(self.output);
    }

    fn announce(&mut self, message: &str) {
        write_indented(&mut self.output, message);
    }

    fn ask(
        &mut self,
        _qualified: &str,
        _choices: &[&str],
        produced: Value,
        computable: bool,
    ) -> UserInput {
        if computable {
            UserInput::Done(produced)
        } else {
            UserInput::Skip
        }
    }

    fn depart(&mut self, qualified: &str) -> UserInput {
        write_marker_line(&mut self.output, &format!("⇒ {}", display_path(qualified)), self.renderer);
        UserInput::Done(Value::Unitus)
    }

    fn external(&mut self, _qualified: &str) -> UserInput {
        UserInput::Skip
    }

    fn command(&mut self, qualified: &str, script: &str) -> UserInput {
        render_command(&mut self.output, &display_path(qualified), script, self.renderer);
        UserInput::Done(Value::Literali(script.to_string()))
    }

    fn action(&mut self, _qualified: &str, _name: &str, verb: &str, label: &str) -> UserInput {
        let text = if label.is_empty() {
            format!("» {}", verb)
        } else {
            format!("» {} {}", verb, label)
        };
        write_indented(&mut self.output, &text);
        UserInput::Done(Value::Unitus)
    }

    fn seal(&mut self, _qualified: &str, produced: Value, computable: bool) -> UserInput {
        if computable {
            UserInput::Done(produced)
        } else {
            UserInput::Skip
        }
    }

    fn settle(&mut self, marker: &str, qualified: &str, verdict: &UserInput) {
        let qualified = display_path(qualified);
        render_settle(&mut self.output, marker, &qualified, verdict, self.renderer);
    }

    fn acquire(
        &mut self,
        _qualified: &str,
        _name: Option<&str>,
        _forma: Option<&str>,
    ) -> UserInput {
        UserInput::Done(Value::Unitus)
    }

    fn renderer(&self) -> &'static dyn Render {
        self.renderer
    }
}

#[derive(Debug)]
#[allow(dead_code)] // fields are read only via Debug
enum Trace {
    Enter {
        path: String,
    },
    Leave {
        path: String,
        outcome: Disposition,
        result: Value,
    },
    Execute {
        path: String,
        script: String,
    },
    Acquire {
        path: String,
        name: Option<String>,
        forma: Option<String>,
        supplied: Value,
    },
    External {
        path: String,
    },
}

#[derive(Debug)]
#[allow(dead_code)] // read only via Debug
enum Disposition {
    Done,
    Skip,
    Fail(String),
    Stop,
}

/// A driver for debugging that prints each value-bearing callback as a
/// `Trace`, delegating decisions about outcomes to the inner wrapped driver.
pub struct Transcript<D, W> {
    inner: D,
    output: W,
}

impl<D> Transcript<D, io::Stdout> {
    pub fn new(inner: D) -> Self {
        Transcript {
            inner,
            output: io::stdout(),
        }
    }
}

impl<D, W: Write> Transcript<D, W> {
    fn emit(&mut self, trace: Trace) {
        let _ = writeln!(self.output, "{:#?}", trace);
    }

    fn trace_outcome(&mut self, path: &str, produced: Value, outcome: &UserInput) {
        let outcome = match outcome {
            UserInput::Done(_) => Disposition::Done,
            UserInput::Skip => Disposition::Skip,
            UserInput::Fail(reason) => Disposition::Fail(reason.clone()),
            UserInput::Quit => Disposition::Stop,
        };
        self.emit(Trace::Leave {
            path: path.to_string(),
            outcome,
            result: produced,
        });
    }
}

impl<D: Driver, W: Write> Driver for Transcript<D, W> {
    fn step(&mut self, qualified: &str, description: &str, depth: usize) {
        self.emit(Trace::Enter {
            path: qualified.to_string(),
        });
        self.inner
            .step(qualified, description, depth);
    }

    fn enter(&mut self, qualified: &str) {
        self.emit(Trace::Enter {
            path: qualified.to_string(),
        });
        self.inner
            .enter(qualified);
    }

    fn commence(&mut self, label: &str) {
        self.inner
            .commence(label);
    }

    fn conclude(&mut self, label: &str, verdict: &UserInput) {
        self.inner
            .conclude(label, verdict);
    }

    fn display(&mut self, content: &str) {
        self.inner
            .display(content);
    }

    fn announce(&mut self, message: &str) {
        self.inner
            .announce(message);
    }

    fn ask(
        &mut self,
        qualified: &str,
        choices: &[&str],
        produced: Value,
        computable: bool,
    ) -> UserInput {
        let outcome = self
            .inner
            .ask(qualified, choices, produced.clone(), computable);
        self.trace_outcome(qualified, produced, &outcome);
        outcome
    }

    fn depart(&mut self, qualified: &str) -> UserInput {
        self.inner
            .depart(qualified)
    }

    fn external(&mut self, qualified: &str) -> UserInput {
        self.emit(Trace::External {
            path: qualified.to_string(),
        });
        self.inner
            .external(qualified)
    }

    fn command(&mut self, qualified: &str, script: &str) -> UserInput {
        self.emit(Trace::Execute {
            path: qualified.to_string(),
            script: script.to_string(),
        });
        self.inner
            .command(qualified, script)
    }

    fn action(&mut self, qualified: &str, name: &str, verb: &str, label: &str) -> UserInput {
        self.emit(Trace::Execute {
            path: qualified.to_string(),
            script: format!("{} {}", verb, label)
                .trim_end()
                .to_string(),
        });
        self.inner
            .action(qualified, name, verb, label)
    }

    fn section(&mut self, qualified: &str, numeral: &str, title: &str) {
        self.emit(Trace::Enter {
            path: qualified.to_string(),
        });
        self.inner
            .section(qualified, numeral, title);
    }

    fn seal(&mut self, qualified: &str, produced: Value, computable: bool) -> UserInput {
        let outcome = self
            .inner
            .seal(qualified, produced.clone(), computable);
        self.trace_outcome(qualified, produced, &outcome);
        outcome
    }

    fn settle(&mut self, marker: &str, qualified: &str, verdict: &UserInput) {
        self.inner
            .settle(marker, qualified, verdict);
    }

    fn acquire(&mut self, qualified: &str, name: Option<&str>, forma: Option<&str>) -> UserInput {
        let outcome = self
            .inner
            .acquire(qualified, name, forma);
        let supplied = if let UserInput::Done(value) = &outcome {
            value.clone()
        } else {
            Value::Unitus
        };
        self.emit(Trace::Acquire {
            path: qualified.to_string(),
            name: name.map(|n| n.to_string()),
            forma: forma.map(|f| f.to_string()),
            supplied,
        });
        outcome
    }

    fn renderer(&self) -> &'static dyn Render {
        self.inner
            .renderer()
    }
}

/// No-operator, no-output driver: takes each step and scope's computed value as
/// its result, emitting nothing, and counts the results it settles — one per
/// step and per structural-scope close. Lets a Technique be run without a
/// terminal, the result count read back from `results()`.
pub struct Headless {
    results: usize,
}

impl Headless {
    pub fn new() -> Self {
        Headless { results: 0 }
    }

    pub fn results(&self) -> usize {
        self.results
    }
}

impl Driver for Headless {
    fn step(&mut self, _qualified: &str, _description: &str, _depth: usize) {}

    fn enter(&mut self, _qualified: &str) {}

    fn commence(&mut self, _label: &str) {}

    fn conclude(&mut self, _label: &str, _verdict: &UserInput) {}

    fn display(&mut self, _content: &str) {}

    fn announce(&mut self, _message: &str) {}

    fn ask(
        &mut self,
        _qualified: &str,
        _choices: &[&str],
        produced: Value,
        _computable: bool,
    ) -> UserInput {
        self.results += 1;
        UserInput::Done(produced)
    }

    fn depart(&mut self, _qualified: &str) -> UserInput {
        UserInput::Done(Value::Unitus)
    }

    fn external(&mut self, _qualified: &str) -> UserInput {
        self.results += 1;
        UserInput::Skip
    }

    fn command(&mut self, _qualified: &str, script: &str) -> UserInput {
        UserInput::Done(Value::Literali(script.to_string()))
    }

    fn action(&mut self, _qualified: &str, _name: &str, _verb: &str, _label: &str) -> UserInput {
        UserInput::Done(Value::Unitus)
    }

    fn section(&mut self, _qualified: &str, _numeral: &str, _title: &str) {}

    fn seal(&mut self, _qualified: &str, produced: Value, _computable: bool) -> UserInput {
        self.results += 1;
        UserInput::Done(produced)
    }

    fn settle(&mut self, _marker: &str, _qualified: &str, _verdict: &UserInput) {}

    fn acquire(
        &mut self,
        _qualified: &str,
        _name: Option<&str>,
        _forma: Option<&str>,
    ) -> UserInput {
        UserInput::Done(Value::Unitus)
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

/// What the walker showed (or attempted to show). Tests use this to inspect
/// ordering and content of the walker's user-facing output.
#[cfg(test)]
#[derive(Debug, Clone, PartialEq)]
pub enum Event {
    Step {
        qualified: String,
        description: String,
    },
    Enter {
        qualified: String,
    },
    Display(String),
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
    Action {
        qualified: String,
        name: String,
        verb: String,
        label: String,
    },
    Ask {
        qualified: String,
        choices: Vec<String>,
    },
    External {
        qualified: String,
    },
    Seal {
        qualified: String,
    },
    Acquire {
        name: Option<String>,
        forma: Option<String>,
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
    fn step(&mut self, fqn: &str, description: &str, _depth: usize) {
        self.events
            .push(Event::Step {
                qualified: fqn.to_string(),
                description: description.to_string(),
            });
    }

    fn enter(&mut self, fqn: &str) {
        self.events
            .push(Event::Enter {
                qualified: fqn.to_string(),
            });
    }

    fn commence(&mut self, _label: &str) {}

    fn conclude(&mut self, _label: &str, _verdict: &UserInput) {}

    fn display(&mut self, content: &str) {
        self.events
            .push(Event::Display(content.to_string()));
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

    fn ask(
        &mut self,
        qualified: &str,
        choices: &[&str],
        _produced: Value,
        _computable: bool,
    ) -> UserInput {
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

    fn depart(&mut self, _qualified: &str) -> UserInput {
        self.answers
            .pop_front()
            .expect("Mock::depart called with no canned answers remaining")
    }

    fn external(&mut self, qualified: &str) -> UserInput {
        self.events
            .push(Event::External {
                qualified: qualified.to_string(),
            });
        self.answers
            .pop_front()
            .expect("Mock::external called with no canned answers remaining")
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

    /// Records the action beat and auto-confirms the run (`Done`) without
    /// draining the answer queue — like `command`, the action gate is
    /// orthogonal to the step verdicts a test drives.
    fn action(&mut self, qualified: &str, name: &str, verb: &str, label: &str) -> UserInput {
        self.events
            .push(Event::Action {
                qualified: qualified.to_string(),
                name: name.to_string(),
                verb: verb.to_string(),
                label: label.to_string(),
            });
        UserInput::Done(Value::Unitus)
    }

    /// A scope sign-off auto-accepts (records `Done`) and records the event,
    /// rather than draining the `ask` answer queue — the structural-scope
    /// close is orthogonal to the step verdicts a test drives. A test
    /// asserting sign-off behaviour inspects the recorded `Seal` event.
    fn seal(&mut self, qualified: &str, _produced: Value, _computable: bool) -> UserInput {
        self.events
            .push(Event::Seal {
                qualified: qualified.to_string(),
            });
        UserInput::Done(Value::Unitus)
    }

    fn settle(&mut self, _marker: &str, _qualified: &str, _verdict: &UserInput) {}

    fn acquire(&mut self, _qualified: &str, name: Option<&str>, forma: Option<&str>) -> UserInput {
        self.events
            .push(Event::Acquire {
                name: name.map(|n| n.to_string()),
                forma: forma.map(|f| f.to_string()),
            });
        self.answers
            .pop_front()
            .unwrap_or(UserInput::Done(Value::Unitus))
    }

    fn renderer(&self) -> &'static dyn Render {
        &Identity
    }
}

#[cfg(test)]
#[path = "checks/driver.rs"]
mod check;
