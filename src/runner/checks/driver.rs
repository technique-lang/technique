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
fn mock_records_section_and_announce() {
    let mut p = Mock::new();
    p.section("I", "Setup");
    p.announce("Calling helper");
    assert_eq!(
        p.events(),
        &[
            Event::Section {
                qualified: "I".to_string(),
                title: "Setup".to_string(),
            },
            Event::Announce("Calling helper".to_string()),
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
    assert!(written.contains("local_network:I/1"));
    assert!(written.contains("Check the cable."));
}

#[test]
fn console_section_writes_fqn_and_title() {
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_output(&mut output);
    p.section("I", "Setup");
    let written = String::from_utf8(output).expect("utf8");
    assert!(written.contains("I"));
    assert!(written.contains("Setup"));
}

#[test]
fn edit_unedited_enter_preserves_produced() {
    // An untouched Unitus stays Unitus, not Literali("").
    let mut it = Interaction::begin(&[], Value::Unitus);
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Unitus))
    );
}

#[test]
fn edit_typed_enter_returns_literali() {
    let mut it = Interaction::begin(&[], Value::Unitus);
    for c in "eth0".chars() {
        assert_eq!(
            it.handle(KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE)),
            None
        );
    }
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Literali("eth0".to_string())))
    );
}

#[test]
fn edit_backspace_trims_candidate() {
    // The Literali candidate starts with the cursor at the end.
    let mut it = Interaction::begin(&[], Value::Literali("abc".to_string()));
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
fn esc_menu_navigates_skip_fail_quit() {
    let mut it = Interaction::begin(&[], Value::Unitus);
    // Esc opens the menu on the first item, skip.
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE)),
        None
    );
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Skip)
    );

    let mut it = Interaction::begin(&[], Value::Unitus);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Fail)
    );

    let mut it = Interaction::begin(&[], Value::Unitus);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    // Right past the end clamps on quit.
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Quit)
    );
}

#[test]
fn esc_menu_backs_out_to_field() {
    let mut it = Interaction::begin(&[], Value::Literali("x".to_string()));
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
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
fn render_frozen_shows_only_affordances() {
    // A read-only value (already streamed above) is not re-echoed on the
    // prompt line; the line carries no raw newline and only the key options.
    let dump = Value::Literali("1: lo\n2: eth0\n3: wlan0".to_string());
    let it = Interaction::begin(&[], dump);
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(!written.contains('\n'));
    assert!(!written.contains("eth0"));
    assert!(written.contains("[enter]"));
    assert!(written.contains("[esc]"));
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
    let it = Interaction::begin(&[], Value::Literali("hello".to_string()));
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
    assert!(written.contains("Yes"));
    assert!(written.contains("No"));
}
