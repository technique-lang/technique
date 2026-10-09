use std::fs;
use std::path::Path;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use technique::engraving::{Appender, Record, RunId, Serial, State, parse_records};
use technique::parsing;
use technique::reporting::render_pfftt;
use technique::runner::driver::{Console, MockKeyboard};
use technique::runner::{Context, Library, Runner};
use technique::translation;

use crate::common::list_technique_documents;

// A `.keys` file is the keystrokes of one or more sessions. Tokens are key
// names (`Enter Esc Up Down Left Right PageUp PageDown End Backspace`), each
// optionally repeated as `Up*14`, or a quoted string typed a character at a
// time. `#` starts a comment. A `--- resume` line begins the next session on
// the same journal.
fn sessions(text: &str) -> Vec<Vec<KeyEvent>> {
    let mut sessions = vec![Vec::new()];
    for line in text.lines() {
        if line.trim() == "--- resume" {
            sessions.push(Vec::new());
            continue;
        }
        let keys = sessions
            .last_mut()
            .unwrap();
        let mut rest = line.trim_start();
        while !rest.is_empty() && !rest.starts_with('#') {
            if let Some(quoted) = rest.strip_prefix('"') {
                let end = quoted
                    .find('"')
                    .expect("closing quote");
                for c in quoted[..end].chars() {
                    keys.push(key(KeyCode::Char(c)));
                }
                rest = quoted[end + 1..].trim_start();
                continue;
            }
            let end = rest
                .find(char::is_whitespace)
                .unwrap_or(rest.len());
            let (name, count) = match rest[..end].split_once('*') {
                Some((name, count)) => (
                    name,
                    count
                        .parse()
                        .expect("repeat count"),
                ),
                None => (&rest[..end], 1),
            };
            let code = match name {
                "Enter" => KeyCode::Enter,
                "Esc" => KeyCode::Esc,
                "Up" => KeyCode::Up,
                "Down" => KeyCode::Down,
                "Left" => KeyCode::Left,
                "Right" => KeyCode::Right,
                "PageUp" => KeyCode::PageUp,
                "PageDown" => KeyCode::PageDown,
                "End" => KeyCode::End,
                "Backspace" => KeyCode::Backspace,
                other => panic!("unknown key {:?}", other),
            };
            for _ in 0..count {
                keys.push(key(code));
            }
            rest = rest[end..].trim_start();
        }
    }
    sessions
}

fn key(code: KeyCode) -> KeyEvent {
    KeyEvent::new(code, KeyModifiers::NONE)
}

// `serial path state`, dropping the timestamp and run identifier.
fn tails(journal: &str) -> Vec<String> {
    journal
        .lines()
        .map(|line| {
            line.splitn(3, ' ')
                .nth(2)
                .unwrap_or(line)
                .to_string()
        })
        .collect()
}

fn library() -> Library {
    let mut library = Library::core();
    library.extend(Library::system());
    library
}

/// Drive every document in `tests/sessions/` through the real Console with
/// the keystrokes beside it, one session per `--- resume`, each walking the
/// journal the sessions before it wrote. The journal they leave between
/// them must match the `.pfftt` beside the document.
#[test]
fn ensure_sessions() {
    let dir = Path::new("tests/sessions/");
    let mut failures = Vec::new();

    for file in list_technique_documents(dir) {
        let content = parsing::load(&file).expect("load");
        let document = parsing::parse(&file, &content).expect("parse");
        let mut program = translation::translate(&document).expect("translate");
        technique::resolution::resolve(&mut program).expect("resolve");
        technique::linking::link(&mut program, &library()).expect("link");

        let keys = fs::read_to_string(file.with_extension("keys")).expect("keys beside document");
        let mut records = vec![Record {
            recorded: "2026-09-28T00:00:00.000Z".to_string(),
            run_id: RunId(0),
            serial: Serial::ROOT,
            path: "/".to_string(),
            state: State::Start {
                uri: format!("file://{}", file.display()),
            },
        }];
        for session in sessions(&keys) {
            let driver = Console::with_keys(Vec::new(), MockKeyboard::new(session));
            let mut runner = Runner::new(
                &program,
                Appender::memory(),
                records.clone(),
                driver,
                library(),
            )
            .with_context(Context::capture());
            if let Err(error) = runner.run(Vec::new()) {
                println!("{:?} did not run cleanly: {:?}", file, error);
                failures.push(file.clone());
            }
            let written = parse_records(
                runner
                    .into_appender()
                    .contents(),
            )
            .expect("records");
            records.extend(written);
        }
        let captured = render_pfftt(&records);

        let expected = file.with_extension("pfftt");
        if std::env::var("TECHNIQUE_REGENERATE").is_ok() {
            fs::write(&expected, &captured).expect("rewrite expected journal");
            println!("regenerated {:?}", expected);
            continue;
        }
        let expected = fs::read_to_string(&expected).expect("expected journal beside document");
        let (want, got) = (tails(&expected), tails(&captured));
        if want != got {
            println!("\nJournal mismatch for {:?}", file);
            for i in 0..want
                .len()
                .max(got.len())
            {
                let w = want
                    .get(i)
                    .map(String::as_str)
                    .unwrap_or("");
                let g = got
                    .get(i)
                    .map(String::as_str)
                    .unwrap_or("");
                if w != g {
                    println!("@@ line {} @@\n- {}\n+ {}", i + 1, w, g);
                }
            }
            failures.push(file.clone());
        }
    }

    if !failures.is_empty() {
        panic!("{} session journals did not match", failures.len());
    }
}
