// Sessions over a journal: review at a prompt and over a finished run.

use std::path::Path;

use crate::engraving::{Appender, Record, RunId, Serial, State, format_record};
use crate::linking::link;
use crate::parsing;
use crate::resolution::resolve;
use crate::runner::context::Context;
use crate::runner::driver::{
    Answer, Driver, Headless, Marker, Mock, Offer, Review, Scripted, Verdict,
};
use crate::runner::library::Library;
use crate::runner::session::{Conclusion, Runner};
use crate::runner::walker::Outcome;
use crate::translation::translate;
use crate::value::Value;

const SURVEY: &str = r#"
% technique v1

survey :

    1.  Take a reading ~ reading
    2.  Sign the log
"#;

fn session<D: Driver>(records: Vec<Record>, driver: D) -> (Vec<Record>, Conclusion, D) {
    let source = SURVEY.trim_ascii();
    let document = parsing::parse(Path::new("Survey.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");
    let library = Library::core();
    link(&mut program, &library).expect("link");
    let mut runner = Runner::new(&program, Appender::memory(), records, driver, library)
        .with_context(Context::capture());
    let conclusion = runner
        .run(Vec::new())
        .expect("run");
    let records = runner
        .records
        .clone();
    (records, conclusion, runner.into_driver())
}

fn lines(records: &[Record]) -> Vec<String> {
    records
        .iter()
        .map(|record| {
            format_record(record)
                .trim_end()
                .splitn(3, ' ')
                .nth(2)
                .unwrap_or_default()
                .to_string()
        })
        .collect()
}

#[test]
fn finished_run_leaving_review_writes_nothing() {
    let (records, _, _) = session(Vec::new(), Headless::new());
    let finished = records.len();
    let (records, conclusion, _) = session(records, Headless::new());
    assert_eq!(records.len(), finished);
    assert_eq!(
        conclusion,
        Conclusion::Completed(Outcome::Skip(Value::Unitus))
    );
}

#[test]
fn finished_run_replays_its_trail_before_review() {
    let (records, _, _) = session(Vec::new(), Headless::new());
    let finished = records.len();
    let (records, _, driver) = session(records, Mock::new());
    assert_eq!(records.len(), finished);
    let log = driver.log();
    let replayed = log
        .iter()
        .position(|entry| {
            entry.contains(r#"path: "/survey:/1""#) && entry.contains("restored: true")
        })
        .expect("replayed step");
    let reviewed = log
        .iter()
        .position(|entry| entry.starts_with("Frame"))
        .expect("review");
    assert!(replayed < reviewed);
}

#[test]
fn amending_a_finished_run_resumes_it() {
    let (records, _, _) = session(Vec::new(), Headless::new());
    let finished = records.len();
    let driver = Scripted::reviewing([], [Review::Chose(Offer::Override)]);
    let (records, conclusion, _) = session(records, driver);
    assert_eq!(
        lines(&records[finished..]),
        vec![
            "000 / Resume",
            "003 /survey:/2 Revoke",
            "003 /survey:/2 Begin ()",
            "003 /survey:/2 Done ()",
            "001 /survey: Done ()",
            "000 / Finish",
        ]
    );
    assert_eq!(
        conclusion,
        Conclusion::Completed(Outcome::Done(Value::Unitus))
    );
}

#[test]
fn leaving_review_asks_again_with_the_draft() {
    let driver = Mock::with_answers([
        Answer::Review(Some("4".to_string())),
        Answer::Done(Value::Literali("42".to_string())),
        Answer::Skip,
    ]);
    let (records, _, driver) = session(Vec::new(), driver);
    assert!(
        driver
            .log()
            .iter()
            .any(|entry| entry.contains(r#"draft: Some("4")"#))
    );
    assert!(lines(&records).contains(&r#"002 /survey:/1 Bind ( "42" ~ reading )"#.to_string()));
}

#[test]
fn edit_is_offered_at_steps_not_scope_closes() {
    let record = |path: &str| Record {
        recorded: String::new(),
        run_id: RunId(0),
        serial: Serial(3),
        path: path.to_string(),
        state: State::Done(None),
    };
    let done = Verdict::Done(Value::Unitus);
    assert_eq!(
        super::offers_at(&record("/survey:/1"), Some(&done), false, &[]),
        vec![Offer::Edit, Offer::Skip, Offer::Fail, Offer::Quit]
    );
    assert_eq!(
        super::offers_at(&record("/survey:/1/[1]"), Some(&done), false, &[]),
        vec![Offer::Skip, Offer::Fail, Offer::Quit]
    );
    assert_eq!(
        super::offers_at(
            &record("/survey:/1/<https://example.com>"),
            Some(&done),
            false,
            &[]
        ),
        vec![Offer::Skip, Offer::Fail, Offer::Quit]
    );
}

#[test]
fn external_with_slashes_in_its_uri_is_marked_as_one() {
    let record = |state: State| Record {
        recorded: String::new(),
        run_id: RunId(0),
        serial: Serial(8),
        path: "/probe:/7/<https://example.com/Helper>".to_string(),
        state,
    };
    assert_eq!(
        super::marker_of(&record(State::Begin(Vec::new()))),
        Marker::Depart
    );
    assert_eq!(super::marker_of(&record(State::Done(None))), Marker::Return);
}
