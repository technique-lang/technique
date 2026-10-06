use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::engraving::Motion;
use crate::runner::driver::{Console, Driver, Kind, Marker, MockKeyboard, Offer, Verdict};
use crate::value::Numeric;

use super::*;

const STEP: [Offer; 4] = [Offer::Edit, Offer::Skip, Offer::Fail, Offer::Quit];

const FAILED: [Offer; 5] = [
    Offer::Edit,
    Offer::Skip,
    Offer::Fail,
    Offer::Override,
    Offer::Quit,
];

const BOUNDARY: [Offer; 3] = [Offer::Skip, Offer::Fail, Offer::Quit];

fn key(code: KeyCode) -> KeyEvent {
    KeyEvent::new(code, KeyModifiers::NONE)
}

fn typed(text: &str) -> Vec<KeyEvent> {
    text.chars()
        .map(|c| key(KeyCode::Char(c)))
        .collect()
}

const ENTER: KeyCode = KeyCode::Enter;
const ESC: KeyCode = KeyCode::Esc;
const UP: KeyCode = KeyCode::Up;
const RIGHT: KeyCode = KeyCode::Right;

fn keys(codes: &[KeyCode]) -> Vec<KeyEvent> {
    codes
        .iter()
        .map(|c| key(*c))
        .collect()
}

// Each redraw of the prompt line, escape sequences dropped: a clear down
// starts the next frame.
fn frames(bytes: &[u8]) -> Vec<String> {
    let text = String::from_utf8(bytes.to_vec()).expect("utf8");
    let mut frames = Vec::new();
    let mut frame = String::new();
    let mut chars = text.chars();
    while let Some(c) = chars.next() {
        if c != '\x1b' {
            frame.push(c);
            continue;
        }
        let mut sequence = String::new();
        for c in chars.by_ref() {
            sequence.push(c);
            if c.is_ascii_alphabetic() && sequence.len() > 1 {
                break;
            }
        }
        if sequence == "[J" && !frame.is_empty() {
            frames.push(std::mem::take(&mut frame));
        }
    }
    if !frame.is_empty() {
        frames.push(frame);
    }
    frames
}

fn put(question: Question<'_>, pressed: Vec<KeyEvent>) -> (Answer, Vec<String>) {
    let mut console = Console::with_keys(Vec::new(), MockKeyboard::new(pressed));
    let answer = console.ask(question);
    (answer, frames(&console.into_output()))
}

fn raw(question: Question<'_>, pressed: Vec<KeyEvent>) -> String {
    let mut console = Console::with_keys(Vec::new(), MockKeyboard::new(pressed));
    let _ = console.ask(question);
    String::from_utf8(console.into_output()).expect("utf8")
}

fn confirm<'a>(standing: Standing, produced: &'a Value, offers: &'a [Offer]) -> Question<'a> {
    Question {
        marker: Marker::Step,
        path: "/probe:/1",
        prompt: Prompt::Confirm {
            standing,
            kind: Kind::Prose,
            produced,
            choices: &[],
        },
        offers,
        reviewable: true,
        draft: None,
    }
}

fn asked<'a>(
    marker: Marker,
    path: &'a str,
    prompt: Prompt<'a>,
    offers: &'a [Offer],
) -> Question<'a> {
    Question {
        marker,
        path,
        prompt,
        offers,
        reviewable: true,
        draft: None,
    }
}

#[test]
fn enter_accepts_what_was_produced() {
    let value = Value::Quanticle(Numeric::Integral(42));
    let (answer, drawn) = put(confirm(Standing::Done, &value, &STEP), keys(&[ENTER]));
    assert_eq!(answer, Answer::Done(value.clone()));
    assert_eq!(drawn, ["→ probe:/1 ▶ "]);
}

#[test]
fn the_triangle_takes_the_standing_colour() {
    let unit = Value::Unitus;
    assert!(raw(confirm(Standing::Done, &unit, &STEP), keys(&[ENTER])).contains("\x1b[38;5;12m▶"));
    assert!(raw(confirm(Standing::Fail, &unit, &FAILED), keys(&[ENTER])).contains("204;0;0m▶"));
    assert!(raw(confirm(Standing::Skip, &unit, &STEP), keys(&[ENTER])).contains("196;160;0m▶"));
    assert!(
        raw(confirm(Standing::Done, &unit, &STEP), keys(&[ENTER])).contains("85;87;83m→ probe:/1")
    );
}

#[test]
fn a_standing_failure_is_what_enter_gives() {
    let unit = Value::Unitus;
    let (answer, _) = put(confirm(Standing::Fail, &unit, &FAILED), keys(&[ENTER]));
    assert_eq!(answer, Answer::Fail(String::new()));

    // The menu lands on Fail, so Enter there opens the reason.
    let (answer, drawn) = put(
        confirm(Standing::Fail, &unit, &FAILED),
        keys(&[ESC, ENTER, ENTER]),
    );
    assert_eq!(answer, Answer::Fail(String::new()));
    assert_eq!(
        drawn,
        [
            "→ probe:/1 ▶ ",
            "→ probe:/1 ▶  Edit    Skip    Fail    Override    Quit ",
            "→ probe:/1 ▶ Reason? ",
        ]
    );

    let mut pressed = keys(&[ESC]);
    pressed.extend(typed("o"));
    let (answer, _) = put(confirm(Standing::Fail, &unit, &FAILED), pressed);
    assert_eq!(answer, Answer::Override);
}

#[test]
fn a_standing_skip_is_what_enter_gives() {
    let unit = Value::Unitus;
    let (answer, _) = put(confirm(Standing::Skip, &unit, &STEP), keys(&[ENTER]));
    assert_eq!(answer, Answer::Skip);

    let (answer, _) = put(confirm(Standing::Skip, &unit, &STEP), keys(&[ESC, ENTER]));
    assert_eq!(answer, Answer::Skip);

    // Override was not offered, so its letter reaches nothing.
    let mut pressed = keys(&[ESC]);
    pressed.extend(typed("o"));
    pressed.extend(keys(&[ENTER]));
    let (answer, _) = put(confirm(Standing::Skip, &unit, &STEP), pressed);
    assert_eq!(answer, Answer::Skip);
}

#[test]
fn edit_is_dim_and_passed_over_where_nothing_is_editable() {
    let unit = Value::Unitus;
    let (answer, _) = put(
        confirm(Standing::Done, &unit, &STEP),
        keys(&[ESC, RIGHT, ENTER]),
    );
    assert_eq!(answer, Answer::Skip);
    assert!(
        raw(
            confirm(Standing::Done, &unit, &STEP),
            keys(&[ESC, RIGHT, ENTER])
        )
        .contains("\x1b[2m Edit ")
    );

    let mut pressed = keys(&[ESC]);
    pressed.extend(typed("eS"));
    let (answer, _) = put(confirm(Standing::Done, &unit, &STEP), pressed);
    assert_eq!(answer, Answer::Skip);
}

#[test]
fn the_menu_clamps_and_esc_backs_out() {
    let value = Value::Literali("eth0".to_string());
    let (answer, _) = put(
        confirm(Standing::Done, &value, &STEP),
        keys(&[ESC, RIGHT, RIGHT, RIGHT, RIGHT, RIGHT, ENTER]),
    );
    assert_eq!(answer, Answer::Quit);

    let (answer, _) = put(
        confirm(Standing::Done, &value, &STEP),
        keys(&[ESC, ESC, ENTER]),
    );
    assert_eq!(answer, Answer::Done(value.clone()));
}

#[test]
fn edit_opens_the_value_for_typing() {
    let value = Value::Literali("eth".to_string());
    let mut pressed = keys(&[ESC, ENTER]);
    pressed.extend(typed("0"));
    pressed.extend(keys(&[ENTER]));
    let (answer, drawn) = put(confirm(Standing::Done, &value, &STEP), pressed);
    assert_eq!(answer, Answer::Done(Value::Literali("eth0".to_string())));
    assert_eq!(
        drawn,
        [
            "→ probe:/1 ▶ ",
            "→ probe:/1 ▶  Edit    Skip    Fail    Quit ",
            "→ probe:/1 ▶ eth",
            "→ probe:/1 ▶ eth0",
        ]
    );
}

#[test]
fn esc_then_enter_at_a_text_field_does_not_skip() {
    let value = Value::Literali("eth".to_string());
    let mut pressed = keys(&[ESC, ENTER, ESC, ENTER]);
    pressed.extend(typed("0"));
    pressed.extend(keys(&[ENTER]));
    let (answer, drawn) = put(confirm(Standing::Done, &value, &STEP), pressed);
    assert_eq!(answer, Answer::Done(Value::Literali("eth0".to_string())));
    assert_eq!(
        drawn
            .last()
            .map(String::as_str),
        Some("→ probe:/1 ▶ eth0")
    );
}

#[test]
fn an_edited_number_stays_a_number() {
    let value = Value::Quanticle(Numeric::Integral(42));
    let mut pressed = keys(&[ESC, ENTER, KeyCode::Backspace]);
    pressed.extend(typed("3"));
    pressed.extend(keys(&[ENTER]));
    let (answer, _) = put(confirm(Standing::Done, &value, &STEP), pressed);
    assert_eq!(
        answer,
        Answer::Done(Value::Quanticle(Numeric::Integral(43)))
    );

    let (answer, _) = put(
        confirm(Standing::Done, &value, &STEP),
        keys(&[ESC, ENTER, ENTER]),
    );
    assert_eq!(answer, Answer::Done(value.clone()));

    // Not a number: Enter is refused, and the keys run dry.
    let mut pressed = keys(&[ESC, ENTER]);
    pressed.extend(typed("x"));
    pressed.extend(keys(&[ENTER]));
    let (answer, drawn) = put(confirm(Standing::Done, &value, &STEP), pressed);
    assert_eq!(answer, Answer::Quit);
    assert_eq!(
        drawn
            .last()
            .map(String::as_str),
        Some("→ probe:/1 ▶ 42x")
    );
}

#[test]
fn a_fail_reason_is_typed_in_place() {
    let unit = Value::Unitus;
    let mut pressed = keys(&[ESC]);
    pressed.extend(typed("fwhy"));
    pressed.extend(keys(&[ENTER]));
    let (answer, drawn) = put(confirm(Standing::Done, &unit, &STEP), pressed);
    assert_eq!(answer, Answer::Fail("why".to_string()));
    assert_eq!(
        drawn
            .last()
            .map(String::as_str),
        Some("→ probe:/1 ▶ Reason? why")
    );

    // Esc goes back to the menu on Fail; reopening starts empty.
    let mut pressed = keys(&[ESC]);
    pressed.extend(typed("fx"));
    pressed.extend(keys(&[ESC, ENTER, ENTER]));
    let (answer, drawn) = put(confirm(Standing::Done, &unit, &STEP), pressed);
    assert_eq!(answer, Answer::Fail(String::new()));
    assert_eq!(
        drawn[drawn.len() - 2..],
        [
            "→ probe:/1 ▶  Edit    Skip    Fail    Quit ",
            "→ probe:/1 ▶ Reason? ",
        ]
    );
}

#[test]
fn a_choice_row_moves_and_accepts() {
    let unit = Value::Unitus;
    let question = || Question {
        marker: Marker::Step,
        path: "/probe:/2",
        prompt: Prompt::Confirm {
            standing: Standing::Done,
            kind: Kind::Choice,
            produced: &unit,
            choices: &["Open", "Closed"],
        },
        offers: &STEP,
        reviewable: true,
        draft: None,
    };
    let (answer, drawn) = put(question(), keys(&[RIGHT, RIGHT, ENTER]));
    assert_eq!(answer, Answer::Done(Value::Literali("Closed".to_string())));
    assert_eq!(drawn[0], "→ probe:/2 ▶  Open    Closed ");

    let (answer, _) = put(question(), keys(&[KeyCode::Left, ENTER]));
    assert_eq!(answer, Answer::Done(Value::Literali("Open".to_string())));

    let (answer, _) = put(question(), keys(&[ESC, ENTER, ENTER]));
    assert_eq!(answer, Answer::Done(Value::Literali("Open".to_string())));

    let drawn = raw(question(), keys(&[ENTER]));
    assert!(drawn.contains("\x1b[48;5;15m\x1b[38;2;143;89;2m Open "));
    assert!(drawn.contains("\x1b[38;2;245;121;0m Closed "));
}

#[test]
fn a_choice_left_for_review_comes_back_highlighted() {
    let unit = Value::Unitus;
    let question = |draft| Question {
        marker: Marker::Step,
        path: "/probe:/2",
        prompt: Prompt::Confirm {
            standing: Standing::Done,
            kind: Kind::Choice,
            produced: &unit,
            choices: &["Open", "Closed"],
        },
        offers: &STEP,
        reviewable: true,
        draft,
    };
    let (answer, _) = put(question(None), keys(&[RIGHT, UP]));
    assert_eq!(answer, Answer::Review(Some("Closed".to_string())));

    let (answer, _) = put(question(Some("Closed")), keys(&[ENTER]));
    assert_eq!(answer, Answer::Done(Value::Literali("Closed".to_string())));
    let drawn = raw(question(Some("Closed")), keys(&[ENTER]));
    assert!(drawn.contains("\x1b[48;5;15m\x1b[38;2;143;89;2m Closed "));

    let (answer, _) = put(question(Some("Ajar")), keys(&[ENTER]));
    assert_eq!(answer, Answer::Done(Value::Literali("Open".to_string())));
}

#[test]
fn an_acquire_is_typed() {
    let prompt = Prompt::Acquire {
        text: "",
        name: Some("colour"),
        forma: None,
        seed: None,
    };
    let mut pressed = typed("blXue");
    pressed.extend(keys(&[
        KeyCode::Left,
        KeyCode::Left,
        KeyCode::Backspace,
        ENTER,
    ]));
    let (answer, drawn) = put(
        asked(Marker::Enter, "/probe:/3", prompt, &BOUNDARY),
        pressed,
    );
    assert_eq!(answer, Answer::Done(Value::Literali("blue".to_string())));
    assert_eq!(drawn[0], "↘ probe:/3 (colour : ?) ▶ ");
    assert_eq!(
        drawn
            .last()
            .map(String::as_str),
        Some("↘ probe:/3 (colour : ?) ▶ blue")
    );

    let seed = Value::Literali("red".to_string());
    let prompt = Prompt::Acquire {
        text: "<greet>",
        name: Some("name"),
        forma: Some("Name"),
        seed: Some(&seed),
    };
    let (answer, drawn) = put(
        asked(Marker::Enter, "/probe:/4", prompt, &BOUNDARY),
        keys(&[ENTER]),
    );
    assert_eq!(answer, Answer::Done(seed.clone()));
    assert_eq!(drawn, ["↘ probe:/4 <greet>(name : Name) ▶ red"]);
}

#[test]
fn esc_then_enter_at_an_acquire_keeps_the_draft() {
    let prompt = || Prompt::Acquire {
        text: "",
        name: Some("name"),
        forma: None,
        seed: None,
    };
    let mut pressed = typed("Ford");
    pressed.extend(keys(&[ESC, ENTER]));
    let drawn = raw(
        asked(Marker::Enter, "/probe:/3", prompt(), &BOUNDARY),
        pressed.clone(),
    );
    assert!(!drawn.contains("\x1b[7m"));

    pressed.extend(keys(&[ENTER]));
    let (answer, drawn) = put(
        asked(Marker::Enter, "/probe:/3", prompt(), &BOUNDARY),
        pressed,
    );
    assert_eq!(answer, Answer::Done(Value::Literali("Ford".to_string())));
    assert_eq!(
        drawn[drawn.len() - 3..],
        [
            "↘ probe:/3 (name : ?) ▶ Ford",
            "↘ probe:/3 (name : ?) ▶  Skip    Fail    Quit ",
            "↘ probe:/3 (name : ?) ▶ Ford",
        ]
    );

    let mut pressed = typed("Ford");
    pressed.extend(keys(&[ESC, RIGHT, ENTER]));
    let (answer, _) = put(
        asked(Marker::Enter, "/probe:/3", prompt(), &BOUNDARY),
        pressed,
    );
    assert_eq!(answer, Answer::Skip);
}

#[test]
fn an_empty_acquire_is_refused() {
    let prompt = Prompt::Acquire {
        text: "",
        name: Some("colour"),
        forma: None,
        seed: None,
    };
    let mut pressed = keys(&[ENTER]);
    pressed.extend(typed("blue"));
    pressed.push(key(ENTER));
    let (answer, _) = put(
        asked(Marker::Enter, "/probe:/3", prompt, &BOUNDARY),
        pressed,
    );
    assert_eq!(answer, Answer::Done(Value::Literali("blue".to_string())));
}

#[test]
fn a_list_acquire_is_bracketed_and_opens_on_its_seed() {
    let seed = Value::Arraeum(vec![
        Value::Literali("a".to_string()),
        Value::Literali("b".to_string()),
    ]);
    let prompt = Prompt::Acquire {
        text: "<tally>",
        name: Some("xs"),
        forma: Some("[Text]"),
        seed: Some(&seed),
    };
    let (answer, drawn) = put(
        asked(Marker::Enter, "/list_probe:/1", prompt, &BOUNDARY),
        keys(&[ENTER]),
    );
    assert_eq!(answer, Answer::Done(seed.clone()));
    assert_eq!(
        drawn,
        [r#"↘ list_probe:/1 <tally>(xs : [Text]) ▶ ["a", "b"]"#]
    );

    let prompt = Prompt::Acquire {
        text: "<tally>",
        name: Some("xs"),
        forma: Some("[Number]"),
        seed: None,
    };
    let mut pressed = typed("1,2");
    pressed.extend(keys(&[ENTER]));
    let (answer, drawn) = put(
        asked(Marker::Enter, "/list_probe:/1", prompt.clone(), &BOUNDARY),
        pressed,
    );
    assert_eq!(
        answer,
        Answer::Done(Value::Arraeum(vec![
            Value::Quanticle(Numeric::Integral(1)),
            Value::Quanticle(Numeric::Integral(2)),
        ]))
    );
    assert_eq!(drawn[0], "↘ list_probe:/1 <tally>(xs : [Number]) ▶ []");

    // Malformed: refused, and the keys run dry.
    let mut pressed = typed("\"east");
    pressed.extend(keys(&[ENTER]));
    let (answer, _) = put(
        asked(Marker::Enter, "/list_probe:/1", prompt, &BOUNDARY),
        pressed,
    );
    assert_eq!(answer, Answer::Quit);

    let (answer, _) = put(
        asked(
            Marker::Enter,
            "/list_probe:/1",
            Prompt::Acquire {
                text: "",
                name: None,
                forma: Some("[Text]"),
                seed: None,
            },
            &BOUNDARY,
        ),
        keys(&[ENTER]),
    );
    assert_eq!(answer, Answer::Done(Value::Arraeum(Vec::new())));
}

#[test]
fn a_command_is_offered_as_typed() {
    let prompt = Prompt::Command {
        script: "echo hello world\n",
    };
    let (answer, drawn) = put(
        asked(Marker::Step, "/probe:/6", prompt.clone(), &BOUNDARY),
        keys(&[ENTER]),
    );
    assert_eq!(
        answer,
        Answer::Done(Value::Literali("echo hello world".to_string()))
    );
    assert_eq!(drawn, ["→ probe:/6 ▶ echo hello world"]);

    let (answer, _) = put(
        asked(Marker::Step, "/probe:/6", prompt.clone(), &BOUNDARY),
        keys(&[KeyCode::Backspace, ENTER]),
    );
    assert_eq!(
        answer,
        Answer::Done(Value::Literali("echo hello worl".to_string()))
    );

    let mut pressed = keys(&[ESC]);
    pressed.extend(typed("s"));
    let (answer, drawn) = put(asked(Marker::Step, "/probe:/6", prompt, &BOUNDARY), pressed);
    assert_eq!(answer, Answer::Skip);
    assert_eq!(drawn[1], "→ probe:/6 ▶  Skip    Fail    Quit ");
}

#[test]
fn an_action_shows_its_verb_and_label() {
    let label = Value::Literali("Submit".to_string());
    let prompt = Prompt::Action {
        function: "click",
        verb: "Click",
        value: &label,
    };
    let (answer, drawn) = put(
        asked(Marker::Action, "/probe:/5", prompt.clone(), &BOUNDARY),
        keys(&[ENTER]),
    );
    assert_eq!(answer, Answer::Done(Value::Unitus));
    assert_eq!(drawn, ["» probe:/5 Click Submit ▶ "]);
    let bytes = raw(
        asked(Marker::Action, "/probe:/5", prompt, &BOUNDARY),
        keys(&[ENTER]),
    );
    assert!(bytes.contains("\x1b[38;2;200;150;75mClick"));
    assert!(!bytes.contains("245;121;0"));

    let response = Value::Enumerati("BOTTOM".to_string());
    let prompt = Prompt::Action {
        function: "scroll",
        verb: "Scroll to",
        value: &response,
    };
    let bytes = raw(
        asked(Marker::Action, "/probe:/5", prompt, &BOUNDARY),
        keys(&[ENTER]),
    );
    assert!(bytes.contains("\x1b[38;2;245;121;0m BOTTOM"));
}

#[test]
fn boundaries_are_confirmed() {
    let (answer, drawn) = put(
        asked(
            Marker::Depart,
            "/probe:/7/<https://example.com/Helper>",
            Prompt::Depart {
                echo: "(\"x\" ~ y)",
            },
            &BOUNDARY,
        ),
        keys(&[ENTER]),
    );
    assert_eq!(answer, Answer::Done(Value::Unitus));
    assert_eq!(
        drawn,
        ["⇒ probe:/7/<https://example.com/Helper> (\"x\" ~ y) ▶ "]
    );

    let mut pressed = keys(&[ESC]);
    pressed.extend(typed("f"));
    pressed.extend(keys(&[ENTER]));
    let (answer, drawn) = put(
        asked(
            Marker::Return,
            "/probe:/7/<https://example.com/Helper>",
            Prompt::External,
            &BOUNDARY,
        ),
        pressed,
    );
    assert_eq!(answer, Answer::Fail(String::new()));
    assert_eq!(drawn[0], "⇐ probe:/7/<https://example.com/Helper> ▶ ");
}

#[test]
fn up_leaves_for_review_only_where_there_is_something() {
    let unit = Value::Unitus;
    let (answer, _) = put(confirm(Standing::Done, &unit, &STEP), keys(&[UP]));
    assert_eq!(answer, Answer::Review(None));

    let mut question = confirm(Standing::Done, &unit, &STEP);
    question.reviewable = false;
    let (answer, _) = put(question, keys(&[UP, ENTER]));
    assert_eq!(answer, Answer::Done(Value::Unitus));

    // Not while the menu stands in front of it.
    let (answer, _) = put(
        confirm(Standing::Done, &unit, &STEP),
        keys(&[ESC, UP, ENTER, ENTER]),
    );
    assert_eq!(answer, Answer::Done(Value::Unitus));
}

#[test]
fn what_was_typed_goes_to_review_and_comes_back() {
    let acquire = Prompt::Acquire {
        text: "",
        name: Some("colour"),
        forma: None,
        seed: None,
    };
    let mut pressed = typed("blu");
    pressed.extend(keys(&[UP]));
    let (answer, _) = put(
        asked(Marker::Enter, "/probe:/3", acquire.clone(), &BOUNDARY),
        pressed,
    );
    assert_eq!(answer, Answer::Review(Some("blu".to_string())));

    let mut question = asked(Marker::Enter, "/probe:/3", acquire, &BOUNDARY);
    question.draft = Some("blu");
    let mut pressed = typed("e");
    pressed.extend(keys(&[ENTER]));
    let (answer, drawn) = put(question, pressed);
    assert_eq!(answer, Answer::Done(Value::Literali("blue".to_string())));
    assert_eq!(drawn[0], "↘ probe:/3 (colour : ?) ▶ blu");

    let command = Prompt::Command { script: "ls" };
    let mut pressed = typed(" -l");
    pressed.extend(keys(&[UP]));
    let (answer, _) = put(
        asked(Marker::Step, "/probe:/6", command.clone(), &BOUNDARY),
        pressed,
    );
    assert_eq!(answer, Answer::Review(Some("ls -l".to_string())));

    // Nothing typed, nothing handed back.
    let (answer, _) = put(
        asked(Marker::Step, "/probe:/6", command.clone(), &BOUNDARY),
        keys(&[UP]),
    );
    assert_eq!(answer, Answer::Review(None));

    let mut question = asked(Marker::Step, "/probe:/6", command, &BOUNDARY);
    question.draft = Some("ls -l");
    let (answer, _) = put(question, keys(&[ENTER]));
    assert_eq!(answer, Answer::Done(Value::Literali("ls -l".to_string())));
}

#[test]
fn an_edited_value_comes_back_from_review() {
    let value = Value::Quanticle(Numeric::Integral(42));
    let mut pressed = keys(&[ESC, ENTER]);
    pressed.extend(typed("0"));
    pressed.extend(keys(&[UP]));
    let (answer, _) = put(confirm(Standing::Done, &value, &STEP), pressed);
    assert_eq!(answer, Answer::Review(Some("420".to_string())));

    let mut question = confirm(Standing::Done, &value, &STEP);
    question.draft = Some("420");
    let (answer, drawn) = put(question, keys(&[ENTER]));
    assert_eq!(
        answer,
        Answer::Done(Value::Quanticle(Numeric::Integral(420)))
    );
    assert_eq!(drawn, ["→ probe:/1 ▶ 420"]);

    // Left as it was, the same question behaves as before.
    let (answer, _) = put(confirm(Standing::Done, &value, &STEP), keys(&[UP]));
    assert_eq!(answer, Answer::Review(None));
    let (answer, _) = put(confirm(Standing::Done, &value, &STEP), keys(&[ENTER]));
    assert_eq!(answer, Answer::Done(value.clone()));
}

#[test]
fn ctrl_c_quits_a_prompt() {
    let unit = Value::Unitus;
    let (answer, _) = put(
        confirm(Standing::Done, &unit, &STEP),
        vec![KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL)],
    );
    assert_eq!(answer, Answer::Quit);

    let mut pressed = keys(&[ESC]);
    pressed.extend(typed("fwh"));
    pressed.push(KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL));
    let (answer, _) = put(confirm(Standing::Done, &unit, &STEP), pressed);
    assert_eq!(answer, Answer::Quit);
}

#[test]
fn the_prompt_line_is_cleared_once_answered() {
    let unit = Value::Unitus;
    let drawn = raw(confirm(Standing::Done, &unit, &STEP), keys(&[ENTER]));
    assert!(drawn.ends_with("\x1b[1G\x1b[J\x1b[?25h"));
    assert!(!drawn.contains('\n'));
}

#[test]
fn a_wrapped_line_is_cleared_from_its_first_row() {
    // 13 columns of prefix and 90 of script wrap onto a second row at 80.
    let script = "x".repeat(90);
    let prompt = Prompt::Command { script: &script };
    let drawn = raw(
        asked(Marker::Step, "/probe:/6", prompt.clone(), &BOUNDARY),
        keys(&[KeyCode::Backspace, ENTER]),
    );
    let redraw = format!("{}\x1b[?25h\x1b[1A\x1b[1G\x1b[J", script);
    assert!(drawn.contains(&redraw));
    assert!(drawn.ends_with("\x1b[1A\x1b[1G\x1b[J\x1b[?25h"));

    // Stepping back across the wrap takes the cursor up to the first row.
    let drawn = raw(
        asked(Marker::Step, "/probe:/6", prompt, &BOUNDARY),
        keys(&[KeyCode::Left; 24]),
    );
    assert!(drawn.contains("\x1b[1A\x1b[80G"));
}

#[test]
fn a_script_over_several_lines_is_cleared_whole() {
    let prompt = Prompt::Command {
        script: "echo one\necho two\n",
    };
    let drawn = raw(
        asked(Marker::Step, "/probe:/6", prompt, &BOUNDARY),
        keys(&[ENTER]),
    );
    assert!(drawn.contains("echo one\r\necho two"));
    assert!(drawn.ends_with("\x1b[1A\x1b[1G\x1b[J\x1b[?25h"));
}

// Review.

const ANSWERED: [Offer; 4] = [Offer::Edit, Offer::Skip, Offer::Fail, Offer::Quit];

fn frame<'a>(verdict: Option<&'a Verdict>, bound: &'a str, offers: &'a [Offer]) -> Frame<'a> {
    Frame {
        marker: Marker::Step,
        path: "/probe:/3",
        bound,
        verdict,
        offers,
    }
}

fn reviewed(frame: Frame<'_>, pressed: Vec<KeyEvent>) -> (Review, Vec<String>, String) {
    let mut console = Console::with_keys(Vec::new(), MockKeyboard::new(pressed));
    let answer = console.review(frame);
    let bytes = console.into_output();
    let text = String::from_utf8(bytes.clone()).expect("utf8");
    (answer, frames(&bytes), text)
}

#[test]
fn a_motion_moves_and_leaves_the_line_standing() {
    let done = Verdict::Done(Value::Unitus);
    let (answer, drawn, bytes) = reviewed(frame(Some(&done), "~ colour", &ANSWERED), keys(&[UP]));
    assert_eq!(answer, Review::Move(Motion::Up));
    assert_eq!(drawn, ["→ probe:/3 ~ colour ✓   "]);
    assert!(!bytes.ends_with("\x1b[J\x1b[?25h"));

    let (answer, drawn, _) = reviewed(frame(None, "", &[Offer::Quit]), keys(&[KeyCode::PageDown]));
    assert_eq!(answer, Review::Move(Motion::PageDown));
    assert_eq!(drawn, ["→ probe:/3    "]);

    // A frame wrapped onto a second row leaves the cursor at its first.
    let path = format!("/probe:{}", "/1".repeat(40));
    let wrapped = Frame {
        path: &path,
        ..frame(None, "", &[Offer::Quit])
    };
    let (_, _, bytes) = reviewed(wrapped, keys(&[UP]));
    assert!(bytes.ends_with("\x1b[1A"));
}

#[test]
fn boundary_frames_draw_their_arrows() {
    let done = Verdict::Done(Value::Unitus);
    let path = "/probe:/7/<https://example.com/Helper>";
    let depart = Frame {
        marker: Marker::Depart,
        path,
        ..frame(None, "", &ANSWERED)
    };
    let (_, drawn, _) = reviewed(depart, keys(&[UP]));
    assert_eq!(drawn, ["⇒ probe:/7/<https://example.com/Helper>    "]);

    let back = Frame {
        marker: Marker::Return,
        path,
        ..frame(Some(&done), "", &ANSWERED)
    };
    let (_, drawn, _) = reviewed(back, keys(&[UP]));
    assert_eq!(drawn, ["⇐ probe:/7/<https://example.com/Helper> ✓   "]);
}

#[test]
fn review_enter_does_nothing_until_the_menu_is_open() {
    let (answer, drawn, bytes) =
        reviewed(frame(None, "", &[Offer::Quit]), keys(&[ENTER, ESC, ENTER]));
    assert_eq!(answer, Review::Chose(Offer::Quit));
    assert_eq!(drawn[2], "→ probe:/3     Quit ");
    assert!(bytes.ends_with("\x1b[1G\x1b[J\x1b[?25h"));
}

#[test]
fn the_review_menu_lands_on_the_verdict() {
    let done = Verdict::Done(Value::Unitus);
    let (answer, _, _) = reviewed(frame(Some(&done), "", &ANSWERED), keys(&[ESC, ENTER]));
    assert_eq!(answer, Review::Chose(Offer::Edit));

    let (answer, _, _) = reviewed(
        frame(Some(&Verdict::Skip), "", &ANSWERED),
        keys(&[ESC, ENTER]),
    );
    assert_eq!(answer, Review::Chose(Offer::Skip));

    let (answer, _, _) = reviewed(
        frame(Some(&done), "", &ANSWERED),
        keys(&[ESC, RIGHT, ENTER]),
    );
    assert_eq!(answer, Review::Chose(Offer::Skip));

    let mut pressed = keys(&[ESC]);
    pressed.extend(typed("Q"));
    let (answer, _, _) = reviewed(frame(Some(&done), "", &ANSWERED), pressed);
    assert_eq!(answer, Review::Chose(Offer::Quit));
}

#[test]
fn failing_in_review_asks_the_reason_there() {
    let failed = Verdict::Fail("no".to_string());
    let offers = [
        Offer::Edit,
        Offer::Skip,
        Offer::Fail,
        Offer::Override,
        Offer::Quit,
    ];
    let mut pressed = keys(&[ESC, ENTER]);
    pressed.extend(typed("why"));
    pressed.extend(keys(&[ENTER]));
    let (answer, drawn, _) = reviewed(frame(Some(&failed), "", &offers), pressed);
    assert_eq!(answer, Review::Reason("why".to_string()));
    assert_eq!(
        drawn[1],
        "→ probe:/3 ✗    Edit    Skip    Fail    Override    Quit "
    );
    assert_eq!(
        drawn
            .last()
            .map(String::as_str),
        Some("→ probe:/3 ▶ Reason? why")
    );

    // Esc from the reason goes back to the cursor with the menu closed.
    let done = Verdict::Done(Value::Unitus);
    let mut pressed = keys(&[ESC]);
    pressed.extend(typed("fx"));
    pressed.extend(keys(&[ESC, UP]));
    let (answer, drawn, _) = reviewed(frame(Some(&done), "", &ANSWERED), pressed);
    assert_eq!(answer, Review::Move(Motion::Up));
    assert_eq!(
        drawn
            .last()
            .map(String::as_str),
        Some("→ probe:/3 ✓   ")
    );
}

#[test]
fn end_leaves_review_and_ctrl_c_quits_it() {
    let done = Verdict::Done(Value::Unitus);
    let (answer, _, _) = reviewed(frame(Some(&done), "", &ANSWERED), keys(&[KeyCode::End]));
    assert_eq!(answer, Review::Leave);

    let (answer, _, _) = reviewed(
        frame(Some(&done), "", &ANSWERED),
        keys(&[ESC, KeyCode::End]),
    );
    assert_eq!(answer, Review::Leave);

    let (answer, _, _) = reviewed(
        frame(Some(&done), "", &ANSWERED),
        vec![KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL)],
    );
    assert_eq!(answer, Review::Quit);

    let mut pressed = keys(&[ESC]);
    pressed.extend(typed("f"));
    pressed.push(KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL));
    let (answer, _, _) = reviewed(frame(Some(&done), "", &ANSWERED), pressed);
    assert_eq!(answer, Review::Quit);
}
