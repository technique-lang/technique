use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::engraving::Motion;
use crate::runner::driver::{
    Automatic, Console, Driver, Event, Intent, Keys, Kind, Mock, MockKeyboard, Offer, Prompt,
    Question, Review, Reviewing, Standing, UserInput, draw, draw_action, edit, intent,
    is_list_forma,
};
use crate::value::{Numeric, Value};

fn pressed(code: KeyCode) -> Option<Intent> {
    intent(KeyEvent::new(code, KeyModifiers::NONE))
}

fn modified(code: KeyCode, modifiers: KeyModifiers) -> Option<Intent> {
    intent(KeyEvent::new(code, modifiers))
}

// The offer set a step's verdict prompt carries: everything but Override,
// which only a rolled-up failure lights up.
const STEP: [Offer; 4] = [Offer::Edit, Offer::Skip, Offer::Fail, Offer::Quit];

// The offer set at a node whose body rolled up to a failure.
const FAILED: [Offer; 5] = [
    Offer::Edit,
    Offer::Skip,
    Offer::Fail,
    Offer::Override,
    Offer::Quit,
];

// A step's verdict prompt, standing at Done.
fn asking(qualified: &str, kind: Kind, produced: Value) -> Question<'_> {
    Question {
        qualified,
        marker: "→",
        standing: Standing::Done,
        kind,
        produced,
        reviewable: false,
    }
}

// A scope's close, standing at Done.
fn sealing(qualified: &str, kind: Kind) -> Question<'_> {
    Question {
        qualified,
        marker: "↙",
        standing: Standing::Done,
        kind,
        produced: Value::Unitus,
        reviewable: false,
    }
}

#[test]
fn mock_returns_canned_answers_in_order() {
    let mut p = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Skip,
        UserInput::Quit,
    ]);
    assert_eq!(
        p.ask(asking("/I/1", Kind::Computable, Value::Unitus), &[], &STEP),
        UserInput::Done(Value::Unitus)
    );
    assert_eq!(
        p.ask(asking("/I/1", Kind::Computable, Value::Unitus), &[], &STEP),
        UserInput::Skip
    );
    assert_eq!(
        p.ask(asking("/I/1", Kind::Computable, Value::Unitus), &[], &STEP),
        UserInput::Quit
    );
}

#[test]
fn mock_records_step_and_ask_events() {
    let mut p = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    p.step("/local_network:I/1", "", "Check the cable.", 1);
    let _ = p.ask(
        asking("/local_network:I/1", Kind::Computable, Value::Unitus),
        &[],
        &STEP,
    );
    assert_eq!(
        p.events(),
        &[
            Event::Step {
                qualified: "/local_network:I/1".to_string(),
                description: "Check the cable.".to_string(),
            },
            Event::Ask {
                qualified: "/local_network:I/1".to_string(),
                marker: "→".to_string(),
                choices: vec![],
            },
        ]
    );
}

#[test]
fn mock_records_offered_choices() {
    let mut p = Mock::with_answers([UserInput::Done(Value::Literali("Yes".to_string()))]);
    let _ = p.ask(
        asking("I/1", Kind::Computable, Value::Unitus),
        &["Yes", "No"],
        &STEP,
    );
    assert_eq!(
        p.events(),
        &[Event::Ask {
            qualified: "I/1".to_string(),
            marker: "→".to_string(),
            choices: vec!["Yes".to_string(), "No".to_string()],
        }]
    );
}

#[test]
fn mock_records_enter_and_announce() {
    let mut p = Mock::new();
    p.enter("I", "");
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
    let _ = p.ask(asking("I/1", Kind::Computable, Value::Unitus), &[], &STEP);
}

#[test]
fn console_step_writes_fqn_and_description() {
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_output(&mut output);
    p.step("local_network:I/1", "", "Check the cable.", 1);
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
    p.step("local_network:I/1/a", "", "Inspect the connector.", 2);
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
            asking("/I/1", Kind::Computable, Value::Literali("ran".to_string())),
            &[],
            &STEP
        ),
        UserInput::Done(Value::Literali("ran".to_string()))
    );
    assert_eq!(
        p.ask(asking("/I/2", Kind::Prose, Value::Unitus), &[], &STEP),
        UserInput::Skip
    );
    assert_eq!(
        p.ask(sealing("/I", Kind::Computable), &[], &STEP),
        UserInput::Done(Value::Unitus)
    );
    assert_eq!(
        p.ask(sealing("/II", Kind::Prose), &[], &STEP),
        UserInput::Skip
    );
}

#[test]
fn automatic_declines_action_and_choice_as_skip() {
    // Unattended, a physical Action cannot be attested and a response Choice
    // cannot be made, so both decline to Skip; a System command's output is
    // taken as Done just like a plain Computable.
    let mut p = Automatic::with_handle(Vec::new());
    assert_eq!(
        p.action(
            "/I/1",
            "click",
            "Click",
            &Value::Literali("Actions".to_string())
        ),
        UserInput::Skip
    );
    assert_eq!(
        p.ask(
            asking("/I/2", Kind::Choice, Value::Unitus),
            &["Yes", "No"],
            &STEP
        ),
        UserInput::Skip
    );
    assert_eq!(
        p.ask(asking("/I/3", Kind::Action, Value::Unitus), &[], &STEP),
        UserInput::Skip
    );
    assert_eq!(
        p.ask(
            asking("/I/4", Kind::System, Value::Literali("out".to_string())),
            &[],
            &STEP
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
    p.enter("I", "");
    let written = String::from_utf8(output).expect("utf8");
    assert!(written.contains("↘ I"));
}

#[test]
fn default_enter_completes_with_produced() {
    // The default is confirmation: Enter accepts the body's value intact, so
    // an untouched Unitus stays Unitus, not Literali("").
    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Done, &STEP);
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Done(Value::Unitus))
    );
}

#[test]
fn overrule_fail_enter_propagates() {
    // At a failed sign-off the default is the failure itself: a bare Enter
    // settles Fail and never silently lifts it.
    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Fail, &FAILED);
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Fail(String::new()))
    );
}

#[test]
fn overrule_fail_menu_o_overrides() {
    // Override is reachable only deliberately — from the menu — and settles as
    // Override, which the runner lifts to a rollup-severing Done.
    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Fail, &FAILED);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Typed('o')), Some(UserInput::Override));
}

#[test]
fn overrule_skip_enter_propagates() {
    // An all-skipped scope defaults to Skip, not Done.
    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Skip, &STEP);
    assert_eq!(it.handle(Intent::Accept), Some(UserInput::Skip));
}

#[test]
fn override_inert_without_a_failure() {
    // A Skip standing has nothing to override, so the menu's `o` is greyed and
    // does nothing.
    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Skip, &STEP);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Typed('o')), None);
}

#[test]
fn esc_edit_typed_enter_returns_literali() {
    // Editing is opt-in: Esc -> Edit (the first menu item) opens the buffer
    // seeded from the value, which the user can then extend.
    let mut it = Prompt::begin(
        &[],
        Value::Literali("eth".to_string()),
        Standing::Done,
        &STEP,
    );
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Accept), None);
    assert_eq!(it.handle(Intent::Typed('0')), None);
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Done(Value::Literali("eth0".to_string())))
    );
}

#[test]
fn esc_edit_seeds_buffer_and_backspace_trims() {
    // Edit seeds the buffer from the produced value, cursor at the end.
    let mut it = Prompt::begin(
        &[],
        Value::Literali("abc".to_string()),
        Standing::Done,
        &STEP,
    );
    it.handle(Intent::Decline);
    it.handle(Intent::Accept);
    assert_eq!(it.handle(Intent::Erase), None);
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Done(Value::Literali("ab".to_string())))
    );
}

#[test]
fn quanticle_edit_roundtrips() {
    let quanticle = || Value::Quanticle(Numeric::Integral(42));

    // Editing a numeric value and changing it keeps it numeric: 42 -> 43 is
    // re-parsed back to a Quanticle, not flattened to text.
    let mut it = Prompt::begin(&[], quanticle(), Standing::Done, &STEP);
    it.handle(Intent::Decline);
    it.handle(Intent::Accept);
    it.handle(Intent::Erase);
    assert_eq!(it.handle(Intent::Typed('3')), None);
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Done(Value::Quanticle(Numeric::Integral(43))))
    );

    // Entering and leaving the edit without a change returns the original
    // numeric value untouched.
    let mut it = Prompt::begin(&[], quanticle(), Standing::Done, &STEP);
    it.handle(Intent::Decline);
    it.handle(Intent::Accept);
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Done(quanticle()))
    );

    // A numeric value edited into something that is not a number is not
    // accepted: Enter stays in the edit so it can be corrected.
    let mut it = Prompt::begin(&[], quanticle(), Standing::Done, &STEP);
    it.handle(Intent::Decline);
    it.handle(Intent::Accept);
    it.handle(Intent::Typed('x'));
    assert_eq!(it.handle(Intent::Accept), None);
}

#[test]
fn esc_menu_navigates_edit_skip_fail_quit() {
    // For an editable scalar the menu is edit, skip, fail, quit in order.
    let editable = || Value::Literali("eth0".to_string());

    let mut it = Prompt::begin(&[], editable(), Standing::Done, &STEP);
    it.handle(Intent::Decline);
    it.handle(Intent::Move(Motion::Right));
    assert_eq!(it.handle(Intent::Accept), Some(UserInput::Skip));

    let mut it = Prompt::begin(&[], editable(), Standing::Done, &STEP);
    it.handle(Intent::Decline);
    it.handle(Intent::Move(Motion::Right));
    it.handle(Intent::Move(Motion::Right));
    // Fail opens a reason buffer; it settles once the reason is entered.
    assert_eq!(it.handle(Intent::Accept), None);
    it.handle(Intent::Typed('n'));
    it.handle(Intent::Typed('o'));
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Fail("no".to_string()))
    );

    let mut it = Prompt::begin(&[], editable(), Standing::Done, &STEP);
    it.handle(Intent::Decline);
    // Right past the end clamps on quit.
    for _ in 0..5 {
        it.handle(Intent::Move(Motion::Right));
    }
    assert_eq!(it.handle(Intent::Accept), Some(UserInput::Quit));
}

#[test]
fn esc_menu_disables_edit_for_unit_and_complex() {
    // Neither a Unit step (pure confirmation) nor a complex value is
    // inline-editable, so Edit is greyed and the menu opens on Skip.
    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Done, &STEP);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Accept), Some(UserInput::Skip));

    let tablet = Value::Tabularum(vec![("k".to_string(), Value::Unitus)]);
    let mut it = Prompt::begin(&[], tablet, Standing::Done, &STEP);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Accept), Some(UserInput::Skip));
}

#[test]
fn action_response_label_is_coloured() {
    // A response literal argument (e.g. `scroll('BOTTOM')`) shows on the action
    // line in the Response orange (0xf5, 0x79, 0x00), not the default white a
    // plain string label gets.
    let it = Prompt::begin(&[], Value::Unitus, Standing::Done, &STEP);

    let mut out: Vec<u8> = Vec::new();
    draw_action(
        &mut out,
        "I/1",
        "Scroll to",
        &Value::Enumerati("BOTTOM".to_string()),
        &it,
    )
    .expect("draw");
    let coloured = String::from_utf8(out).expect("utf8");
    assert!(coloured.contains("BOTTOM"));
    assert!(coloured.contains("245;121;0"));

    let mut out: Vec<u8> = Vec::new();
    draw_action(
        &mut out,
        "I/1",
        "Type",
        &Value::Literali("hello".to_string()),
        &it,
    )
    .expect("draw");
    let plain = String::from_utf8(out).expect("utf8");
    assert!(plain.contains("hello"));
    assert!(!plain.contains("245;121;0"));
}

#[test]
fn menu_shows_greyed_edit_when_unavailable() {
    // Edit is always listed so it stays discoverable; for a non-editable value
    // it is drawn (greyed) alongside the exits, and the menu opens on Skip.
    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Done, &STEP);
    it.handle(Intent::Decline);
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
    let mut it = Prompt::begin(
        &[],
        Value::Literali("eth0".to_string()),
        Standing::Done,
        &STEP,
    );
    it.handle(Intent::Decline);
    it.handle(Intent::Move(Motion::Right));
    it.handle(Intent::Move(Motion::Right));
    it.handle(Intent::Accept);
    // Type into the reason, then abandon it.
    it.handle(Intent::Typed('x'));
    assert_eq!(it.handle(Intent::Decline), None);
    // Back at the menu, Fail is still selectable.
    assert_eq!(it.handle(Intent::Decline), None);
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Done(Value::Literali("eth0".to_string())))
    );
}

#[test]
fn fail_reason_reopens_empty_after_abandon() {
    // Abandoning a reason discards its text; reopening Fail starts fresh.
    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Done, &STEP);
    it.handle(Intent::Decline);
    it.handle(Intent::Move(Motion::Right));
    it.handle(Intent::Accept);
    it.handle(Intent::Typed('o'));
    it.handle(Intent::Typed('p'));
    it.handle(Intent::Decline);
    it.handle(Intent::Accept);
    it.handle(Intent::Typed('y'));
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Fail("y".to_string()))
    );
}

#[test]
fn esc_menu_backs_out_to_field() {
    let mut it = Prompt::begin(&[], Value::Literali("x".to_string()), Standing::Done, &STEP);
    it.handle(Intent::Decline);
    // Esc out of the menu returns to the frozen value; Enter accepts it intact.
    assert_eq!(it.handle(Intent::Decline), None);
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Done(Value::Literali("x".to_string())))
    );
}

#[test]
fn choices_navigate_and_accept() {
    let mut it = Prompt::begin(&["Yes", "No"], Value::Unitus, Standing::Done, &STEP);
    // First choice is the default.
    let mut first = Prompt::begin(&["Yes", "No"], Value::Unitus, Standing::Done, &STEP);
    assert_eq!(
        first.handle(Intent::Accept),
        Some(UserInput::Done(Value::Literali("Yes".to_string())))
    );
    // Right moves to the second.
    assert_eq!(it.handle(Intent::Move(Motion::Right)), None);
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Done(Value::Literali("No".to_string())))
    );
}

#[test]
fn choices_esc_opens_menu() {
    let mut it = Prompt::begin(&["Yes", "No"], Value::Unitus, Standing::Done, &STEP);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Accept), Some(UserInput::Skip));
}

#[test]
fn read_only_values_accept_intact() {
    // A tablet and multi-line text are both read-only: typing is ignored.
    let tablet = Value::Tabularum(vec![(
        "name".to_string(),
        Value::Literali("eth0".to_string()),
    )]);
    let mut it = Prompt::begin(&[], tablet.clone(), Standing::Done, &STEP);
    assert_eq!(it.handle(Intent::Typed('x')), None);
    assert_eq!(it.handle(Intent::Accept), Some(UserInput::Done(tablet)));

    let dump = Value::Literali("1: lo\n2: eth0\n3: wlan0".to_string());
    let mut it = Prompt::begin(&[], dump.clone(), Standing::Done, &STEP);
    assert_eq!(it.handle(Intent::Typed('x')), None);
    assert_eq!(it.handle(Intent::Accept), Some(UserInput::Done(dump)));
}

#[test]
fn render_frozen_shows_only_triangle() {
    // A read-only value (already streamed above) is not re-echoed on the
    // prompt line, and the menu options are not advertised — the normal prompt
    // is just the "play" triangle.
    let dump = Value::Literali("1: lo\n2: eth0\n3: wlan0".to_string());
    let it = Prompt::begin(&[], dump, Standing::Done, &STEP);
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, "I/1", "→", &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(!written.contains('\n'));
    assert!(!written.contains("eth0"));
    assert!(written.contains('▶'));
}

#[test]
fn the_key_table() {
    // The program's whole binding, asserted here rather than incidentally in
    // every test that presses a key.
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

    // A character arrives as struck, Shift being how a capital reaches us
    // rather than a modifier on one; Edit and Reason are text, where `A` is
    // not `a`, and a menu lowercases for itself.
    assert_eq!(
        modified(KeyCode::Char('E'), KeyModifiers::SHIFT),
        Some(Intent::Typed('E'))
    );

    // Control and Alt carry no text: <Ctrl>+<a> is not a literal `a`.
    assert_eq!(modified(KeyCode::Char('a'), KeyModifiers::CONTROL), None);
    assert_eq!(modified(KeyCode::Char('a'), KeyModifiers::ALT), None);

    // Delete and Home have no producer and no consumer.
    assert_eq!(pressed(KeyCode::Delete), None);
    assert_eq!(pressed(KeyCode::Home), None);
    assert_eq!(pressed(KeyCode::Tab), None);
}

#[test]
fn the_source_swallows_what_carries_no_intent() {
    // A release, then a key that means nothing, then one that does: the holder
    // above sees only the last of the three.
    let mut keys = MockKeyboard::new([
        KeyEvent::new(KeyCode::Home, KeyModifiers::NONE),
        KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE),
    ]);
    assert_eq!(keys.next(), Some(Intent::Accept));
}

#[test]
fn ctrl_c_reads_as_interrupted() {
    // <Ctrl>+<c> is refusable nowhere, so it never reaches a holder as an
    // intent; the source short-circuits and each site answers its own quit.
    let mut keys = MockKeyboard::new([KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL)]);
    assert_eq!(keys.next(), None);

    // A queue run dry is the user having walked away from the terminal, which
    // is the same answer.
    let mut keys = MockKeyboard::new([]);
    assert_eq!(keys.next(), None);
}

#[test]
fn up_steps_back_only_where_something_has_settled() {
    // The gate the runner tests could not reach: <Up> raises a Review at a
    // prompt with a journal behind it, and nothing at one without.
    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Done, &STEP);
    it.reviewable = true;
    assert_eq!(it.handle(Intent::Move(Motion::Up)), Some(UserInput::Review));

    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Done, &STEP);
    it.reviewable = false;
    assert_eq!(it.handle(Intent::Move(Motion::Up)), None);

    // And not while the menu stands in front of it: the innermost holder reads
    // the key first.
    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Done, &STEP);
    it.reviewable = true;
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Move(Motion::Up)), None);
}

#[test]
fn menu_letters_reach_every_offer() {
    // `e` selected Edit in review and did nothing at the live prompt, the menu
    // there having been a hardcoded s/f/o/q. One table, so every offer's own
    // letter reaches it.
    let editable = || Value::Literali("eth0".to_string());

    let mut it = Prompt::begin(&[], editable(), Standing::Done, &STEP);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Typed('e')), None);
    // Edit opened the buffer, so the value can now be extended.
    assert_eq!(it.handle(Intent::Typed('1')), None);
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Done(Value::Literali("eth01".to_string())))
    );

    let mut it = Prompt::begin(&[], editable(), Standing::Done, &STEP);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Typed('S')), Some(UserInput::Skip));

    let mut it = Prompt::begin(&[], editable(), Standing::Done, &STEP);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Typed('q')), Some(UserInput::Quit));
}

#[test]
fn menu_letters_stop_at_the_offer_set() {
    // A letter naming something greyed does not reach it: `e` at a complex
    // value would otherwise open an edit whose restore path closes the menu
    // behind it.
    let tablet = Value::Tabularum(vec![("k".to_string(), Value::Unitus)]);
    let mut it = Prompt::begin(&[], tablet, Standing::Done, &STEP);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Typed('e')), None);
    // The menu is still open on Skip, where it landed.
    assert_eq!(it.handle(Intent::Accept), Some(UserInput::Skip));

    // And a letter naming something the walker did not offer does not answer
    // for it. BOUNDARY carries no Override.
    let boundary: [Offer; 3] = [Offer::Skip, Offer::Fail, Offer::Quit];
    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Done, &boundary);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Typed('o')), None);
}

// The offers at a reviewed position the walk answered, as `reviewing` builds
// them.
const ANSWERED: [Offer; 4] = [Offer::Edit, Offer::Skip, Offer::Fail, Offer::Quit];

// The offers at a frame the walk entered but never asked about.
const UNANSWERED: [Offer; 1] = [Offer::Quit];

#[test]
fn review_moves_the_cursor() {
    // Every motion is the cursor's, and the runner steps the journal with it.
    let mut it = Reviewing::begin(&ANSWERED);
    assert_eq!(
        it.handle(Intent::Move(Motion::Up)),
        Some(Review::Move(Motion::Up))
    );
    assert_eq!(
        it.handle(Intent::Move(Motion::PageDown)),
        Some(Review::Move(Motion::PageDown))
    );
}

#[test]
fn review_enter_does_nothing_until_the_offers_are_open() {
    // Nothing is standing at a reviewed position, and at an unanswered frame
    // the only offer is Quit — so binding <Enter> to open the offers would put
    // the run's one destructive action two taps of the affirmative key away.
    let mut it = Reviewing::begin(&UNANSWERED);
    assert_eq!(it.handle(Intent::Accept), None);

    // <Esc> opens them, as it does at the live prompt, and then <Enter> takes
    // the highlighted one.
    assert_eq!(it.handle(Intent::Decline), None);
    assert_eq!(it.handle(Intent::Accept), Some(Review::Chose(Offer::Quit)));
}

#[test]
fn review_offers_open_on_the_first_and_step_without_skipping() {
    // A reviewed position greys nothing — `reviewing` withholds an offer rather
    // than showing it disabled — so the menu opens on the first offer and walks
    // them all.
    let mut it = Reviewing::begin(&ANSWERED);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Accept), Some(Review::Chose(Offer::Edit)));

    let mut it = Reviewing::begin(&ANSWERED);
    it.handle(Intent::Decline);
    it.handle(Intent::Move(Motion::Right));
    assert_eq!(it.handle(Intent::Accept), Some(Review::Chose(Offer::Skip)));

    // A letter reaches its offer here too.
    let mut it = Reviewing::begin(&ANSWERED);
    it.handle(Intent::Decline);
    assert_eq!(
        it.handle(Intent::Typed('f')),
        Some(Review::Chose(Offer::Fail))
    );

    // And one naming nothing on the row does not answer.
    let mut it = Reviewing::begin(&UNANSWERED);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Typed('e')), None);
}

#[test]
fn review_esc_pops_one_level_and_pushes_at_the_bottom() {
    // Decline pops the stack one level per press and never further; at the
    // bottom there is nothing to pop, so it surfaces the offers instead.
    let mut it = Reviewing::begin(&ANSWERED);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::Decline), None);
    // Back at the position, where a motion is a motion again.
    assert_eq!(
        it.handle(Intent::Move(Motion::Up)),
        Some(Review::Move(Motion::Up))
    );
}

#[test]
fn end_leaves_review_from_anywhere() {
    // Six levels deep costs six presses of <Left> otherwise, and a journal that
    // reached its Finish refuses the <Down> that would otherwise be the way
    // out. An escape hatch a menu can stand in front of is not one.
    let mut it = Reviewing::begin(&ANSWERED);
    assert_eq!(it.handle(Intent::End), Some(Review::Leave));

    let mut it = Reviewing::begin(&ANSWERED);
    it.handle(Intent::Decline);
    assert_eq!(it.handle(Intent::End), Some(Review::Leave));
}

#[test]
fn end_is_inert_at_a_text_field() {
    // <End> is not part of the editing tail; adding it there is a feature to be
    // asked for, not part of one key table.
    let mut it = Prompt::begin(
        &[],
        Value::Literali("eth0".to_string()),
        Standing::Done,
        &STEP,
    );
    it.handle(Intent::Decline);
    it.handle(Intent::Typed('e'));
    assert_eq!(it.handle(Intent::End), None);
    assert_eq!(
        it.handle(Intent::Accept),
        Some(UserInput::Done(Value::Literali("eth0".to_string())))
    );
}

#[test]
fn render_edit_shows_candidate_text() {
    let mut it = Prompt::begin(
        &[],
        Value::Literali("hello".to_string()),
        Standing::Done,
        &STEP,
    );
    // Frozen by default; once edited, the candidate text is shown for editing.
    it.handle(Intent::Decline);
    it.handle(Intent::Accept);
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, "I/1", "→", &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(written.contains("hello"));
}

#[test]
fn render_reason_replaces_menu() {
    // Choosing Fail replaces the menu with the reason prompt on the same line,
    // keeping the ▶ prefix; the menu items are gone, and it stays one line.
    let mut it = Prompt::begin(&[], Value::Unitus, Standing::Done, &STEP);
    it.handle(Intent::Decline);
    it.handle(Intent::Move(Motion::Right));
    it.handle(Intent::Accept);
    it.handle(Intent::Typed('o'));
    it.handle(Intent::Typed('k'));
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
    let it = Prompt::begin(&["Yes", "No"], Value::Unitus, Standing::Done, &STEP);
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
        offers: STEP.to_vec(),
        reviewable: false,
    }
}

// Type `text` into a list field and submit it, returning what the prompt
// settles on — None if it refused the buffer, leaving the edit open.
fn submit_list(text: &str) -> Option<UserInput> {
    let mut it = list_prompt();
    for c in text.chars() {
        it.handle(Intent::Typed(c));
    }
    it.handle(Intent::Accept)
}

// As above, for the cases that are expected to settle.
fn gather_list(text: &str) -> Value {
    match submit_list(text) {
        Some(UserInput::Done(value)) => value,
        other => panic!("expected Done, got {:?}", other),
    }
}

#[test]
fn list_prompt_empty_submits_empty_list() {
    // Enter on an untouched list field yields the empty list, which
    // coerce_to_list reads as zero iterations.
    assert_eq!(gather_list(""), Value::Arraeum(Vec::new()));
}

#[test]
fn list_prompt_gathers_elements() {
    // What the user types is gathered as a list of its elements, not as one
    // string: the structure survives into the binding and onto the record.
    // Each element takes its natural type, the same as an argument would.
    assert_eq!(
        gather_list("east, 5"),
        Value::Arraeum(vec![
            Value::Literali("east".to_string()),
            Value::Quanticle(Numeric::Integral(5)),
        ])
    );
}

#[test]
fn list_prompt_refuses_malformed_buffer() {
    // A buffer that doesn't parse leaves the edit open rather than settling
    // on a mangled list.
    assert_eq!(submit_list(r#""east, west"#), None);
}

#[test]
fn list_prompt_draws_bracketed_buffer() {
    let mut it = list_prompt();
    for c in "east".chars() {
        it.handle(Intent::Typed(c));
    }
    let mut out: Vec<u8> = Vec::new();
    draw(&mut out, "I/1", "↘", &it).expect("draw");
    let written = String::from_utf8(out).expect("utf8");
    assert!(written.contains("[east]"));
}
