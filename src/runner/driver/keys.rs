//! Where keystrokes come from, and what each one means.

use std::collections::VecDeque;

use crossterm::event::{self, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode, size};
use crossterm::{cursor, execute};

use crate::engraving::Motion;

/// What a keystroke means. A key whose meaning depends on its holder stays
/// physical, to be reinterpreted by it.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Intent {
    Accept,
    Decline,
    Move(Motion),
    Typed(char),
    Erase,
    End,
}

/// The program's key bindings, whole. `None` is a key that carries no meaning.
pub fn intent(key: KeyEvent) -> Option<Intent> {
    match key.code {
        KeyCode::Enter => Some(Intent::Accept),
        KeyCode::Esc => Some(Intent::Decline),
        KeyCode::Up => Some(Intent::Move(Motion::Up)),
        KeyCode::Down => Some(Intent::Move(Motion::Down)),
        KeyCode::Left => Some(Intent::Move(Motion::Left)),
        KeyCode::Right => Some(Intent::Move(Motion::Right)),
        KeyCode::PageUp => Some(Intent::Move(Motion::PageUp)),
        KeyCode::PageDown => Some(Intent::Move(Motion::PageDown)),
        KeyCode::Backspace => Some(Intent::Erase),
        KeyCode::End => Some(Intent::End),
        // Shift is how a capital arrives, not a modifier on one.
        KeyCode::Char(c) => {
            if key
                .modifiers
                .intersects(KeyModifiers::CONTROL | KeyModifiers::ALT)
            {
                None
            } else {
                Some(Intent::Typed(c))
            }
        }
        _ => None,
    }
}

/// Where an interactive prompt takes its keystrokes from.
pub trait Keys {
    type Held;

    /// None where the terminal will not go raw.
    fn hold(&mut self) -> Option<Self::Held>;

    fn read(&mut self) -> Option<KeyEvent>;

    /// Columns the prompt line wraps at.
    fn width(&self) -> u16;

    /// None means interrupted: `<Ctrl>+<c>`, a read error, or no more keys.
    fn next(&mut self) -> Option<Intent> {
        loop {
            let key = self.read()?;
            if key.kind == KeyEventKind::Release {
                continue;
            }
            if key
                .modifiers
                .contains(KeyModifiers::CONTROL)
            {
                // Raw mode delivers <Ctrl>+<c> as a key rather than a signal.
                if let KeyCode::Char('c') = key.code {
                    return None;
                }
            }
            if let Some(intent) = intent(key) {
                return Some(intent);
            }
        }
    }
}

/// Raw mode, restored when this drops, including on an unwind.
pub struct Raw;

impl Drop for Raw {
    fn drop(&mut self) {
        let _ = execute!(std::io::stdout(), cursor::Show);
        let _ = disable_raw_mode();
    }
}

/// The user's terminal.
pub struct RealKeyboard;

impl Keys for RealKeyboard {
    type Held = Raw;

    fn hold(&mut self) -> Option<Raw> {
        match enable_raw_mode() {
            Ok(()) => Some(Raw),
            Err(_) => None,
        }
    }

    fn read(&mut self) -> Option<KeyEvent> {
        loop {
            match event::read() {
                Ok(event::Event::Key(key)) => return Some(key),
                Ok(_) => continue,
                Err(_) => return None,
            }
        }
    }

    fn width(&self) -> u16 {
        size()
            .map(|(cols, _)| cols)
            .unwrap_or(80)
            .max(1)
    }
}

/// A queue of keystrokes standing in for the terminal; run dry, it reads as
/// interrupted.
pub struct MockKeyboard {
    keys: VecDeque<KeyEvent>,
}

impl MockKeyboard {
    pub fn new<I: IntoIterator<Item = KeyEvent>>(keys: I) -> Self {
        MockKeyboard {
            keys: keys
                .into_iter()
                .collect(),
        }
    }
}

impl Keys for MockKeyboard {
    type Held = ();

    fn hold(&mut self) -> Option<()> {
        Some(())
    }

    fn read(&mut self) -> Option<KeyEvent> {
        self.keys
            .pop_front()
    }

    fn width(&self) -> u16 {
        80
    }
}

#[cfg(test)]
#[path = "checks/keys.rs"]
mod check;
