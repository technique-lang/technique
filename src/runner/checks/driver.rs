use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::runner::driver::{draw, Console, Driver, Event, Interaction, Mock, UserInput};
use crate::value::Value;

#[test]
fn mock_returns_canned_answers_in_order() {
    let mut p = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Skip,
        UserInput::Quit,
    ]);
    assert_eq!(p.ask(&[], Value::Unitus), UserInput::Done(Value::Unitus));
    assert_eq!(p.ask(&[], Value::Unitus), UserInput::Skip);
    assert_eq!(p.ask(&[], Value::Unitus), UserInput::Quit);
}

#[test]
fn mock_records_step_and_ask_events() {
    let mut p = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    p.step("local_network:I/1", "Check the cable.");
    let _ = p.ask(&[], Value::Unitus);
    assert_eq!(
        p.events(),
        &[
            Event::Step {
                qualified: "local_network:I/1".to_string(),
                description: "Check the cable.".to_string(),
            },
            Event::Ask { choices: vec![] },
        ]
    );
}

#[test]
fn mock_records_offered_choices() {
    let mut p = Mock::with_answers([UserInput::Done(Value::Literali("Yes".to_string()))]);
    let _ = p.ask(&["Yes", "No"], Value::Unitus);
    assert_eq!(
        p.events(),
        &[Event::Ask {
            choices: vec!["Yes".to_string(), "No".to_string()],
        }]
    );
}

#[test]
fn mock_records_enter_leave_and_announce() {
    let mut p = Mock::new();
    p.enter("I", "Setup");
    p.announce("Calling helper");
    p.leave("I");
    assert_eq!(
        p.events(),
        &[
            Event::Enter {
                qualified: "I".to_string(),
                title: "Setup".to_string(),
            },
            Event::Announce("Calling helper".to_string()),
            Event::Leave {
                qualified: "I".to_string(),
            },
        ]
    );
}

#[test]
#[should_panic(expected = "Mock::ask called with no canned answers remaining")]
fn mock_ask_without_answers_panics() {
    let mut p = Mock::new();
    let _ = p.ask(&[], Value::Unitus);
}

#[test]
fn console_step_writes_fqn_and_description() {
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_output(&mut output);
    p.step("local_network:I/1", "Check the cable.");
    let written = String::from_utf8(output).expect("utf8");
    assert!(written.contains("→ local_network:I/1"));
    assert!(written.contains("  Check the cable."));
}

#[test]
fn console_enter_writes_fqn_and_title() {
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_output(&mut output);
    p.enter("I", "Setup");
    let written = String::from_utf8(output).expect("utf8");
    assert!(written.contains("↘ I"));
    assert!(written.contains("  Setup"));
}

#[test]
fn default_enter_completes_with_produced() {
    // The default is confirmation: Enter accepts the body's value intact, so
    // an untouched Unitus stays Unitus, not Literali("").
    let mut it = Interaction::begin(&[], Value::Unitus);
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Unitus))
    );
}

#[test]
fn esc_edit_typed_enter_returns_literali() {
    // Editing is opt-in: Esc -> Edit (the first menu item) opens the buffer
    // seeded from the value, which the operator can then extend.
    let mut it = Interaction::begin(&[], Value::Literali("eth".to_string()));
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        None
    );
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Char('0'), KeyModifiers::NONE)),
        None
    );
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Literali("eth0".to_string())))
    );
}

#[test]
fn esc_edit_seeds_buffer_and_backspace_trims() {
    // Edit seeds the buffer from the produced value, cursor at the end.
    let mut it = Interaction::begin(&[], Value::Literali("abc".to_string()));
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE)),
        None
    );
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Literali("ab".to_string())))
    );
}

#[test]
fn esc_menu_navigates_edit_skip_fail_quit() {
    // For an editable scalar the menu is edit, skip, fail, quit in order.
    let editable = || Value::Literali("eth0".to_string());

    let mut it = Interaction::begin(&[], editable());
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Skip)
    );

    let mut it = Interaction::begin(&[], editable());
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Fail)
    );

    let mut it = Interaction::begin(&[], editable());
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    // Right past the end clamps on quit.
    for _ in 0..5 {
        it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    }
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Quit)
    );
}

#[test]
fn esc_menu_disables_edit_for_unit_and_complex() {
    // Neither a Unit step (pure confirmation) nor a complex value is
    // inline-editable, so Edit is greyed and the menu opens on Skip.
    let mut it = Interaction::begin(&[], Value::Unitus);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Skip)
    );

    let tablet = Value::Tabularum(vec![("k".to_string(), Value::Unitus)]);
    let mut it = Interaction::begin(&[], tablet);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Skip)
    );
}

#[test]
fn menu_shows_greyed_edit_when_unavailable() {
    // Edit is always listed so it stays discoverable; for a non-editable value
    // it is drawn (greyed) alongside the exits, and the menu opens on Skip.
    let mut it = Interaction::begin(&[], Value::Unitus);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(written.contains("Edit"));
    assert!(written.contains("Skip"));
}

#[test]
fn esc_menu_backs_out_to_field() {
    let mut it = Interaction::begin(&[], Value::Literali("x".to_string()));
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    // Esc out of the menu returns to the frozen value; Enter accepts it intact.
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE)),
        None
    );
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Literali("x".to_string())))
    );
}

#[test]
fn choices_navigate_and_accept() {
    let mut it = Interaction::begin(&["Yes", "No"], Value::Unitus);
    // First choice is the default.
    let mut first = Interaction::begin(&["Yes", "No"], Value::Unitus);
    assert_eq!(
        first.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Literali("Yes".to_string())))
    );
    // Right moves to the second.
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE)),
        None
    );
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Literali("No".to_string())))
    );
}

#[test]
fn choices_esc_opens_menu() {
    let mut it = Interaction::begin(&["Yes", "No"], Value::Unitus);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Skip)
    );
}

#[test]
fn complex_value_is_read_only() {
    let tablet = Value::Tabularum(vec![(
        "name".to_string(),
        Value::Literali("eth0".to_string()),
    )]);
    let mut it = Interaction::begin(&[], tablet.clone());
    // Typing into a frozen value does nothing; Enter accepts it intact.
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Char('x'), KeyModifiers::NONE)),
        None
    );
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(tablet))
    );
}

#[test]
fn multiline_scalar_is_read_only() {
    // A step whose body computed multi-line text (e.g. captured exec output)
    // is not editable inline; it is accepted intact, like a complex value.
    let dump = Value::Literali("1: lo\n2: eth0\n3: wlan0".to_string());
    let mut it = Interaction::begin(&[], dump.clone());
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Char('x'), KeyModifiers::NONE)),
        None
    );
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(dump))
    );
}

#[test]
fn render_frozen_shows_only_triangle() {
    // A read-only value (already streamed above) is not re-echoed on the
    // prompt line, and the menu options are not advertised — the normal prompt
    // is just the "play" triangle.
    let dump = Value::Literali("1: lo\n2: eth0\n3: wlan0".to_string());
    let it = Interaction::begin(&[], dump);
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(!written.contains('\n'));
    assert!(!written.contains("eth0"));
    assert!(written.contains('▶'));
    assert!(!written.contains("[enter]"));
}

#[test]
fn ctrl_c_quits_from_any_field() {
    let mut it = Interaction::begin(&[], Value::Unitus);
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL)),
        Some(UserInput::Quit)
    );
}

#[test]
fn render_edit_shows_candidate_text() {
    let mut it = Interaction::begin(&[], Value::Literali("hello".to_string()));
    // Frozen by default; once edited, the candidate text is shown for editing.
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(written.contains("hello"));
}

#[test]
fn render_choices_lists_options() {
    let it = Interaction::begin(&["Yes", "No"], Value::Unitus);
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(written.contains('▶'));
    assert!(written.contains("Yes"));
    assert!(written.contains("No"));
}
