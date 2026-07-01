use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::runner::driver::{
    draw, edit, is_list_forma, Automatic, Console, Driver, Event, Kind, Mock, Prompt, Standing,
    UserInput,
};
use crate::value::{Numeric, Value};

#[test]
fn mock_returns_canned_answers_in_order() {
    let mut p = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Skip,
        UserInput::Quit,
    ]);
    assert_eq!(
        p.ask("/I/1", &[], Value::Unitus, Kind::Computable),
        UserInput::Done(Value::Unitus)
    );
    assert_eq!(
        p.ask("/I/1", &[], Value::Unitus, Kind::Computable),
        UserInput::Skip
    );
    assert_eq!(
        p.ask("/I/1", &[], Value::Unitus, Kind::Computable),
        UserInput::Quit
    );
}

#[test]
fn mock_records_step_and_ask_events() {
    let mut p = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    p.step("/local_network:I/1", "Check the cable.", 1);
    let _ = p.ask("/local_network:I/1", &[], Value::Unitus, Kind::Computable);
    assert_eq!(
        p.events(),
        &[
            Event::Step {
                qualified: "/local_network:I/1".to_string(),
                description: "Check the cable.".to_string(),
            },
            Event::Ask {
                qualified: "/local_network:I/1".to_string(),
                choices: vec![],
            },
        ]
    );
}

#[test]
fn mock_records_offered_choices() {
    let mut p = Mock::with_answers([UserInput::Done(Value::Literali("Yes".to_string()))]);
    let _ = p.ask("I/1", &["Yes", "No"], Value::Unitus, Kind::Computable);
    assert_eq!(
        p.events(),
        &[Event::Ask {
            qualified: "I/1".to_string(),
            choices: vec!["Yes".to_string(), "No".to_string()],
        }]
    );
}

#[test]
fn mock_records_enter_and_announce() {
    let mut p = Mock::new();
    p.enter("I");
    p.announce("Calling helper");
    assert_eq!(
        p.events(),
        &[
            Event::Enter {
                qualified: "I".to_string(),
            },
            Event::Announce("Calling helper".to_string()),
        ]
    );
}

#[test]
#[should_panic(expected = "Mock::ask called with no canned answers remaining")]
fn mock_ask_without_answers_panics() {
    let mut p = Mock::new();
    let _ = p.ask("I/1", &[], Value::Unitus, Kind::Computable);
}

#[test]
fn console_step_writes_fqn_and_description() {
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_output(&mut output);
    p.step("local_network:I/1", "Check the cable.", 1);
    let written = String::from_utf8(output).expect("utf8");
    assert!(written.contains("→ local_network:I/1"));
    assert!(written.contains("    Check the cable."));
}

#[test]
fn console_step_indents_description_to_depth() {
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_output(&mut output);
    // A depth-2 substep indents its prose by eight spaces while the marker
    // stays at the left margin.
    p.step("local_network:I/1/a", "Inspect the connector.", 2);
    let written = String::from_utf8(output).expect("utf8");
    assert!(written.contains("→ local_network:I/1/a"));
    assert!(written.contains("\n        Inspect the connector."));
    assert!(!written.contains("\n    Inspect the connector."));
}

#[test]
fn automatic_settles_done_when_computable_skip_otherwise() {
    let mut p = Automatic::with_handle(Vec::new());
    assert_eq!(
        p.ask(
            "/I/1",
            &[],
            Value::Literali("ran".to_string()),
            Kind::Computable
        ),
        UserInput::Done(Value::Literali("ran".to_string()))
    );
    assert_eq!(
        p.ask("/I/2", &[], Value::Unitus, Kind::Prose),
        UserInput::Skip
    );
    assert_eq!(
        p.seal("/I", Value::Unitus, Kind::Computable),
        UserInput::Done(Value::Unitus)
    );
    assert_eq!(p.seal("/II", Value::Unitus, Kind::Prose), UserInput::Skip);
}

#[test]
fn automatic_declines_action_and_choice_as_skip() {
    // Unattended, a physical Action cannot be attested and a response Choice
    // cannot be made, so both decline to Skip; a System command's output is
    // taken as Done just like a plain Computable.
    let mut p = Automatic::with_handle(Vec::new());
    assert_eq!(
        p.action("/I/1", "click", "Click", "Actions"),
        UserInput::Skip
    );
    assert_eq!(
        p.ask("/I/2", &["Yes", "No"], Value::Unitus, Kind::Choice),
        UserInput::Skip
    );
    assert_eq!(
        p.ask("/I/3", &[], Value::Unitus, Kind::Action),
        UserInput::Skip
    );
    assert_eq!(
        p.ask(
            "/I/4",
            &[],
            Value::Literali("out".to_string()),
            Kind::System
        ),
        UserInput::Done(Value::Literali("out".to_string()))
    );
}

#[test]
fn automatic_settle_renders_verdict_glyph() {
    let mut output: Vec<u8> = Vec::new();
    let mut p = Automatic::with_handle(&mut output);
    p.show_verdict("→", "/I/1", &UserInput::Done(Value::Unitus));
    p.show_verdict("→", "/I/2", &UserInput::Skip);
    p.show_verdict("↙", "/I", &UserInput::Done(Value::Unitus));
    let written = String::from_utf8(output).expect("utf8");
    assert!(written.contains("→ I/1 ✓"));
    assert!(written.contains("→ I/2 ⊘"));
    assert!(written.contains("↙ I ✓"));
}

#[test]
fn console_settle_writes_verdict_line() {
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_output(&mut output);
    p.show_verdict("→", "/I/1", &UserInput::Done(Value::Unitus));
    p.show_verdict("↙", "/I", &UserInput::Skip);
    let written = String::from_utf8(output).expect("utf8");
    assert!(written.contains("→ I/1"));
    assert!(written.contains("✓"));
    assert!(written.contains("↙ I"));
    assert!(written.contains("⊘"));
}

#[test]
fn console_enter_writes_fqn() {
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_output(&mut output);
    p.enter("I");
    let written = String::from_utf8(output).expect("utf8");
    assert!(written.contains("↘ I"));
}

#[test]
fn default_enter_completes_with_produced() {
    // The default is confirmation: Enter accepts the body's value intact, so
    // an untouched Unitus stays Unitus, not Literali("").
    let mut it = Prompt::begin(&[], Value::Unitus);
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Unitus))
    );
}

#[test]
fn overrule_fail_enter_propagates() {
    // At a failed sign-off the default is the failure itself: a bare Enter
    // settles Fail and never silently lifts it.
    let mut it = Prompt::overrule(Standing::Fail);
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Fail(String::new()))
    );
}

#[test]
fn overrule_fail_menu_o_overrides() {
    // Override is reachable only deliberately — from the menu — and settles as
    // Override, which the runner lifts to a rollup-severing Done.
    let mut it = Prompt::overrule(Standing::Fail);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Char('o'), KeyModifiers::NONE)),
        Some(UserInput::Override)
    );
}

#[test]
fn overrule_skip_enter_propagates() {
    // An all-skipped scope defaults to Skip, not Done.
    let mut it = Prompt::overrule(Standing::Skip);
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Skip)
    );
}

#[test]
fn override_inert_without_a_failure() {
    // A Skip standing has nothing to override, so the menu's `o` is greyed and
    // does nothing.
    let mut it = Prompt::overrule(Standing::Skip);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Char('o'), KeyModifiers::NONE)),
        None
    );
}

#[test]
fn esc_edit_typed_enter_returns_literali() {
    // Editing is opt-in: Esc -> Edit (the first menu item) opens the buffer
    // seeded from the value, which the user can then extend.
    let mut it = Prompt::begin(&[], Value::Literali("eth".to_string()));
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
    let mut it = Prompt::begin(&[], Value::Literali("abc".to_string()));
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
fn quanticle_edit_roundtrips() {
    let quanticle = || Value::Quanticle(Numeric::Integral(42));

    // Editing a numeric value and changing it keeps it numeric: 42 -> 43 is
    // re-parsed back to a Quanticle, not flattened to text.
    let mut it = Prompt::begin(&[], quanticle());
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Char('3'), KeyModifiers::NONE)),
        None
    );
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Quanticle(Numeric::Integral(43))))
    );

    // Entering and leaving the edit without a change returns the original
    // numeric value untouched.
    let mut it = Prompt::begin(&[], quanticle());
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(quanticle()))
    );

    // A numeric value edited into something that is not a number is not
    // accepted: Enter stays in the edit so it can be corrected.
    let mut it = Prompt::begin(&[], quanticle());
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Char('x'), KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        None
    );
}

#[test]
fn esc_menu_navigates_edit_skip_fail_quit() {
    // For an editable scalar the menu is edit, skip, fail, quit in order.
    let editable = || Value::Literali("eth0".to_string());

    let mut it = Prompt::begin(&[], editable());
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Skip)
    );

    let mut it = Prompt::begin(&[], editable());
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    // Fail opens a reason buffer; it settles once the reason is entered.
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        None
    );
    it.handle(KeyEvent::new(KeyCode::Char('n'), KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Char('o'), KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Fail("no".to_string()))
    );

    let mut it = Prompt::begin(&[], editable());
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
    let mut it = Prompt::begin(&[], Value::Unitus);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Skip)
    );

    let tablet = Value::Tabularum(vec![("k".to_string(), Value::Unitus)]);
    let mut it = Prompt::begin(&[], tablet);
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
    let mut it = Prompt::begin(&[], Value::Unitus);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, "I/1", "→", &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(written.contains("Edit"));
    assert!(written.contains("Skip"));
}

#[test]
fn fail_reason_backs_out_through_menu_to_field() {
    // Fail opens the reason submenu; Esc closes it back to the menu (Fail still
    // selectable), and a second Esc returns to the untouched frozen value, so
    // Enter still completes the step with its produced value intact.
    let mut it = Prompt::begin(&[], Value::Literali("eth0".to_string()));
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
    // Type into the reason, then abandon it.
    it.handle(KeyEvent::new(KeyCode::Char('x'), KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE)),
        None
    );
    // Back at the menu, Fail is still selectable.
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE)),
        None
    );
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Literali("eth0".to_string())))
    );
}

#[test]
fn fail_reason_reopens_empty_after_abandon() {
    // Abandoning a reason discards its text; reopening Fail starts fresh.
    let mut it = Prompt::begin(&[], Value::Unitus);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Char('o'), KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Char('p'), KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Char('y'), KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Fail("y".to_string()))
    );
}

#[test]
fn esc_menu_backs_out_to_field() {
    let mut it = Prompt::begin(&[], Value::Literali("x".to_string()));
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
    let mut it = Prompt::begin(&["Yes", "No"], Value::Unitus);
    // First choice is the default.
    let mut first = Prompt::begin(&["Yes", "No"], Value::Unitus);
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
    let mut it = Prompt::begin(&["Yes", "No"], Value::Unitus);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Skip)
    );
}

#[test]
fn read_only_values_accept_intact() {
    // A tablet and multi-line text are both read-only: typing is ignored.
    let tablet = Value::Tabularum(vec![(
        "name".to_string(),
        Value::Literali("eth0".to_string()),
    )]);
    let mut it = Prompt::begin(&[], tablet.clone());
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Char('x'), KeyModifiers::NONE)),
        None
    );
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(tablet))
    );

    let dump = Value::Literali("1: lo\n2: eth0\n3: wlan0".to_string());
    let mut it = Prompt::begin(&[], dump.clone());
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
    let it = Prompt::begin(&[], dump);
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, "I/1", "→", &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(!written.contains('\n'));
    assert!(!written.contains("eth0"));
    assert!(written.contains('▶'));
}

#[test]
fn ctrl_c_quits_from_any_field() {
    let mut it = Prompt::begin(&[], Value::Unitus);
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL)),
        Some(UserInput::Quit)
    );
}

#[test]
fn render_edit_shows_candidate_text() {
    let mut it = Prompt::begin(&[], Value::Literali("hello".to_string()));
    // Frozen by default; once edited, the candidate text is shown for editing.
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, "I/1", "→", &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(written.contains("hello"));
}

#[test]
fn render_reason_replaces_menu() {
    // Choosing Fail replaces the menu with the reason prompt on the same line,
    // keeping the ▶ prefix; the menu items are gone, and it stays one line.
    let mut it = Prompt::begin(&[], Value::Unitus);
    it.handle(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Right, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Char('o'), KeyModifiers::NONE));
    it.handle(KeyEvent::new(KeyCode::Char('k'), KeyModifiers::NONE));
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, "I/1", "→", &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(!written.contains('\n'));
    assert!(written.contains('▶'));
    assert!(written.contains("Reason? ok"));
    assert!(!written.contains("Skip"));
}

#[test]
fn render_choices_lists_options() {
    let it = Prompt::begin(&["Yes", "No"], Value::Unitus);
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, "I/1", "→", &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(written.contains('▶'));
    assert!(written.contains("Yes"));
    assert!(written.contains("No"));
}

#[test]
fn is_list_forma_recognises_brackets() {
    assert!(is_list_forma(Some("[*]")));
    assert!(is_list_forma(Some("[Region]")));
    assert!(!is_list_forma(Some("Region")));
    assert!(!is_list_forma(Some("()")));
    assert!(!is_list_forma(None));
}

// A bracketed list field, as prompt_acquire seeds it for an iterated binding.
fn list_prompt() -> Prompt {
    Prompt {
        field: edit(String::new(), Value::Literali(String::new()), true),
        menu: None,
        reason: None,
        standing: Standing::Done,
    }
}

#[test]
fn list_prompt_empty_submits_empty_list() {
    // Enter on an untouched list field yields `[]`, which coerce_to_list reads
    // as zero iterations.
    let mut it = list_prompt();
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Literali("[]".to_string())))
    );
}

#[test]
fn list_prompt_wraps_typed_buffer() {
    // Whatever the user types is wrapped in brackets on submit, so the
    // result parses through the existing list-literal path.
    let mut it = list_prompt();
    for c in "east, west".chars() {
        it.handle(KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE));
    }
    assert_eq!(
        it.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE)),
        Some(UserInput::Done(Value::Literali("[east, west]".to_string())))
    );
}

#[test]
fn list_prompt_draws_bracketed_buffer() {
    let mut it = list_prompt();
    for c in "east".chars() {
        it.handle(KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE));
    }
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, "I/1", "↘", &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(written.contains("[east]"));
}
