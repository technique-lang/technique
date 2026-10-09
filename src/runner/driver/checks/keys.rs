use crossterm::event::{KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers};

use super::*;

fn pressed(code: KeyCode) -> Option<Intent> {
    intent(KeyEvent::new(code, KeyModifiers::NONE))
}

fn modified(code: KeyCode, modifiers: KeyModifiers) -> Option<Intent> {
    intent(KeyEvent::new(code, modifiers))
}

#[test]
fn the_key_table() {
    assert_eq!(pressed(KeyCode::Enter), Some(Intent::Accept));
    assert_eq!(pressed(KeyCode::Esc), Some(Intent::Decline));
    assert_eq!(pressed(KeyCode::Up), Some(Intent::Move(Motion::Up)));
    assert_eq!(pressed(KeyCode::Down), Some(Intent::Move(Motion::Down)));
    assert_eq!(pressed(KeyCode::Left), Some(Intent::Move(Motion::Left)));
    assert_eq!(pressed(KeyCode::Right), Some(Intent::Move(Motion::Right)));
    assert_eq!(pressed(KeyCode::PageUp), Some(Intent::Move(Motion::PageUp)));
    assert_eq!(
        pressed(KeyCode::PageDown),
        Some(Intent::Move(Motion::PageDown))
    );
    assert_eq!(pressed(KeyCode::Backspace), Some(Intent::Erase));
    assert_eq!(pressed(KeyCode::End), Some(Intent::End));
    assert_eq!(pressed(KeyCode::Char('e')), Some(Intent::Typed('e')));
    assert_eq!(
        modified(KeyCode::Char('E'), KeyModifiers::SHIFT),
        Some(Intent::Typed('E'))
    );
    assert_eq!(modified(KeyCode::Char('a'), KeyModifiers::CONTROL), None);
    assert_eq!(modified(KeyCode::Char('a'), KeyModifiers::ALT), None);
    assert_eq!(pressed(KeyCode::Delete), None);
    assert_eq!(pressed(KeyCode::Home), None);
    assert_eq!(pressed(KeyCode::Tab), None);
}

#[test]
fn releases_and_meaningless_keys_are_read_past() {
    let release = KeyEvent {
        code: KeyCode::Char('x'),
        modifiers: KeyModifiers::NONE,
        kind: KeyEventKind::Release,
        state: KeyEventState::NONE,
    };
    let mut keys = MockKeyboard::new([
        release,
        KeyEvent::new(KeyCode::Home, KeyModifiers::NONE),
        KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE),
    ]);
    assert_eq!(keys.next(), Some(Intent::Accept));
    assert_eq!(keys.next(), None);
}

#[test]
fn ctrl_c_reads_as_interrupted() {
    let mut keys = MockKeyboard::new([
        KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL),
        KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE),
    ]);
    assert_eq!(keys.next(), None);

    let mut keys = MockKeyboard::new([]);
    assert_eq!(keys.next(), None);
}
