use std::io::Cursor;

use crate::runner::driver::{Console, Driver, Event, Mock, UserInput};
use crate::value::Value;

#[test]
fn mock_returns_canned_answers_in_order() {
    let mut p = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Skip,
        UserInput::Quit,
    ]);
    assert_eq!(p.ask(&[], &Value::Unitus), UserInput::Done(Value::Unitus));
    assert_eq!(p.ask(&[], &Value::Unitus), UserInput::Skip);
    assert_eq!(p.ask(&[], &Value::Unitus), UserInput::Quit);
}

#[test]
fn mock_records_step_and_ask_events() {
    let mut p = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    p.step("local_network:I/1", "Check the cable.");
    let _ = p.ask(&[], &Value::Unitus);
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
    let _ = p.ask(&["Yes", "No"], &Value::Unitus);
    assert_eq!(
        p.events(),
        &[Event::Ask {
            choices: vec!["Yes".to_string(), "No".to_string()],
        }]
    );
}

#[test]
fn console_response_choices() {
    // A numbered selection returns the chosen response value, and the
    // choices are listed in the output.
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(Cursor::new(b"2\n"), &mut output);
    assert_eq!(
        p.ask(&["Yes", "No"], &Value::Unitus),
        UserInput::Done(Value::Literali("No".to_string()))
    );
    let written = String::from_utf8(output).expect("utf8");
    assert!(written.contains("1) Yes"));
    assert!(written.contains("2) No"));

    // Skip / fail / quit stay available when choices are offered.
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(Cursor::new(b"s\n"), &mut output);
    assert_eq!(p.ask(&["Yes", "No"], &Value::Unitus), UserInput::Skip);

    // An out-of-range number and a bare `d` both re-prompt; the valid
    // pick that follows is accepted.
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(Cursor::new(b"9\nd\n1\n"), &mut output);
    assert_eq!(
        p.ask(&["Yes", "No"], &Value::Unitus),
        UserInput::Done(Value::Literali("Yes".to_string()))
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
    let _ = p.ask(&[], &Value::Unitus);
}

#[test]
fn console_input() {
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(Cursor::new(b"d\n"), &mut output);
    assert_eq!(p.ask(&[], &Value::Unitus), UserInput::Done(Value::Unitus));

    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(Cursor::new(b"s\n"), &mut output);
    assert_eq!(p.ask(&[], &Value::Unitus), UserInput::Skip);

    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(Cursor::new(b"f\n"), &mut output);
    assert_eq!(p.ask(&[], &Value::Unitus), UserInput::Fail);

    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(Cursor::new(b"q\n"), &mut output);
    assert_eq!(p.ask(&[], &Value::Unitus), UserInput::Quit);

    // Case-insensitive on the first character.
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(Cursor::new(b"DONE\n"), &mut output);
    assert_eq!(p.ask(&[], &Value::Unitus), UserInput::Done(Value::Unitus));

    // Leading whitespace is tolerated.
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(Cursor::new(b"   q\n"), &mut output);
    assert_eq!(p.ask(&[], &Value::Unitus), UserInput::Quit);
}

#[test]
fn console_done_accepts_presented_value() {
    // Pressing done accepts the body's computed value, presented on the input
    // line, as the step's value.
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(Cursor::new(b"d\n"), &mut output);
    assert_eq!(
        p.ask(&[], &Value::Literali("probe output".to_string())),
        UserInput::Done(Value::Literali("probe output".to_string()))
    );
}

#[test]
fn console_unrecognized_input_reprompts() {
    let input = Cursor::new(b"x\nd\n");
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(input, &mut output);
    assert_eq!(p.ask(&[], &Value::Unitus), UserInput::Done(Value::Unitus));
    // Two prompts written: one for the rejected `x`, one for the
    // accepted `d`. The prompt text contains "[d]one".
    let written = String::from_utf8(output).expect("utf8");
    assert!(
        written
            .matches("[d]one")
            .count()
            >= 2
    );
}

#[test]
fn console_eof_returns_quit() {
    let input = Cursor::new(b"");
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(input, &mut output);
    assert_eq!(p.ask(&[], &Value::Unitus), UserInput::Quit);
}

#[test]
fn console_step_writes_fqn_and_description() {
    let input = Cursor::new(b"");
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(input, &mut output);
    p.step("local_network:I/1", "Check the cable.");
    let written = String::from_utf8(output).expect("utf8");
    assert!(written.contains("local_network:I/1"));
    assert!(written.contains("Check the cable."));
}

#[test]
fn console_section_writes_fqn_and_title() {
    let input = Cursor::new(b"");
    let mut output: Vec<u8> = Vec::new();
    let mut p = Console::with_handles(input, &mut output);
    p.section("I", "Setup");
    let written = String::from_utf8(output).expect("utf8");
    assert!(written.contains("I"));
    assert!(written.contains("Setup"));
}
