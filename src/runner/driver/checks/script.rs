use crate::engraving::Motion;
use crate::runner::driver::{Kind, Offer, Scripted};

use super::*;

const STEP: [Offer; 4] = [Offer::Edit, Offer::Skip, Offer::Fail, Offer::Quit];

fn confirm<'a>(
    path: &'a str,
    marker: Marker,
    standing: Standing,
    produced: &'a Value,
) -> Question<'a> {
    Question {
        marker,
        path,
        prompt: Prompt::Confirm {
            standing,
            kind: Kind::Computable,
            produced,
            choices: &[],
        },
        offers: &STEP,
        reviewable: true,
        draft: None,
    }
}

fn frame(path: &str) -> Frame<'_> {
    Frame {
        marker: Marker::Step,
        path,
        bound: "",
        verdict: None,
        offers: &[Offer::Quit],
    }
}

#[test]
fn scripted_answers_a_path_once_then_unattended() {
    let value = Value::Literali("ran".to_string());
    let mut driver = Scripted::new([("/probe:/1".to_string(), Answer::Quit)]);
    assert_eq!(
        driver.ask(confirm("/probe:/1", Marker::Step, Standing::Done, &value)),
        Answer::Quit
    );
    assert_eq!(
        driver.ask(confirm("/probe:/1", Marker::Step, Standing::Done, &value)),
        Answer::Done(value.clone())
    );
    assert_eq!(
        driver.ask(confirm("/probe:/2", Marker::Step, Standing::Fail, &value)),
        Answer::Fail(String::new())
    );
}

#[test]
fn scripted_reviews_in_order_then_leaves() {
    let mut driver = Scripted::reviewing(
        [],
        [Review::Move(Motion::Up), Review::Reason("why".to_string())],
    );
    assert_eq!(driver.review(frame("/probe:/1")), Review::Move(Motion::Up));
    assert_eq!(
        driver.review(frame("/probe:/1")),
        Review::Reason("why".to_string())
    );
    assert_eq!(driver.review(frame("/probe:/1")), Review::Leave);
}

#[test]
fn mock_takes_answers_where_a_test_drives_them() {
    let unit = Value::Unitus;
    let mut mock = Mock::with_answers([Answer::Skip, Answer::Override]);
    assert_eq!(
        mock.ask(confirm("/probe:", Marker::Close, Standing::Done, &unit)),
        Answer::Done(Value::Unitus)
    );
    assert_eq!(
        mock.ask(confirm("/probe:/1", Marker::Step, Standing::Done, &unit)),
        Answer::Skip
    );
    assert_eq!(
        mock.ask(confirm("/probe:/2", Marker::Step, Standing::Fail, &unit)),
        Answer::Override
    );
    assert_eq!(
        mock.ask(confirm("/probe:/3", Marker::Step, Standing::Fail, &unit)),
        Answer::Fail(String::new())
    );
    assert_eq!(
        mock.ask(Question {
            marker: Marker::Step,
            path: "/probe:/4",
            prompt: Prompt::Command { script: "true" },
            offers: &STEP,
            reviewable: true,
            draft: None,
        }),
        Answer::Done(Value::Literali("true".to_string()))
    );
}

#[test]
#[should_panic(expected = "Mock::ask called with no canned answers remaining")]
fn mock_step_without_answers_panics() {
    let unit = Value::Unitus;
    let mut mock = Mock::new();
    let _ = mock.ask(confirm("/probe:/1", Marker::Step, Standing::Done, &unit));
}

#[test]
fn mock_logs_what_it_is_shown_and_asked() {
    let unit = Value::Unitus;
    let mut mock = Mock::new().reviewing([Review::Leave]);
    mock.show(Event::Announce("exec()"));
    let _ = mock.ask(confirm("/probe:", Marker::Close, Standing::Done, &unit));
    let _ = mock.review(frame("/probe:/1"));
    assert_eq!(
        mock.log(),
        &[
            format!("{:?}", Event::Announce("exec()")),
            format!(
                "{:?}",
                confirm("/probe:", Marker::Close, Standing::Done, &unit)
            ),
            format!("{:?}", frame("/probe:/1")),
        ]
    );
}
