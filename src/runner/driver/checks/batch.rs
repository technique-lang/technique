use crate::runner::driver::{Automatic, Driver, Marker, Offer};
use crate::value::Value;

use super::*;

const STEP: [Offer; 4] = [Offer::Edit, Offer::Skip, Offer::Fail, Offer::Quit];

fn confirm<'a>(standing: Standing, kind: Kind, produced: &'a Value) -> Question<'a> {
    Question {
        marker: Marker::Step,
        path: "/probe:/1",
        prompt: Prompt::Confirm {
            standing,
            kind,
            produced,
            choices: &[],
        },
        offers: &STEP,
        reviewable: true,
        draft: None,
    }
}

fn other(prompt: Prompt<'_>) -> Question<'_> {
    Question {
        marker: Marker::Step,
        path: "/probe:/1",
        prompt,
        offers: &STEP,
        reviewable: true,
        draft: None,
    }
}

#[test]
fn a_standing_fail_or_skip_is_kept() {
    let value = Value::Literali("ran".to_string());
    assert_eq!(
        unattended(&confirm(Standing::Fail, Kind::Computable, &value)),
        Answer::Fail(String::new())
    );
    assert_eq!(
        unattended(&confirm(Standing::Skip, Kind::System, &value)),
        Answer::Skip
    );
}

#[test]
fn a_standing_done_is_taken_only_from_a_program() {
    let value = Value::Literali("ran".to_string());
    assert_eq!(
        unattended(&confirm(Standing::Done, Kind::Computable, &value)),
        Answer::Done(value.clone())
    );
    assert_eq!(
        unattended(&confirm(Standing::Done, Kind::System, &value)),
        Answer::Done(value.clone())
    );
    assert_eq!(
        unattended(&confirm(Standing::Done, Kind::Prose, &value)),
        Answer::Skip
    );
    assert_eq!(
        unattended(&confirm(Standing::Done, Kind::Action, &value)),
        Answer::Skip
    );
    assert_eq!(
        unattended(&confirm(Standing::Done, Kind::Choice, &value)),
        Answer::Skip
    );
}

#[test]
fn the_other_prompts() {
    let label = Value::Literali("Submit".to_string());
    assert_eq!(
        unattended(&other(Prompt::Acquire {
            text: "",
            name: Some("colour"),
            forma: None,
            seed: None
        })),
        Answer::Skip
    );
    assert_eq!(
        unattended(&other(Prompt::Command {
            script: "echo hello\n"
        })),
        Answer::Done(Value::Literali("echo hello\n".to_string()))
    );
    assert_eq!(
        unattended(&other(Prompt::Action {
            function: "click",
            verb: "Click",
            value: &label
        })),
        Answer::Skip
    );
    assert_eq!(
        unattended(&other(Prompt::Depart { echo: "" })),
        Answer::Done(Value::Unitus)
    );
    assert_eq!(unattended(&other(Prompt::External)), Answer::Skip);
}

#[test]
fn automatic_draws_an_action_it_declines() {
    let label = Value::Literali("Submit".to_string());
    let mut driver = Automatic::with_handle(Vec::new());
    let answer = driver.ask(other(Prompt::Action {
        function: "click",
        verb: "Click",
        value: &label,
    }));
    assert_eq!(answer, Answer::Skip);
    let answer = driver.ask(other(Prompt::Command {
        script: "echo hello",
    }));
    assert_eq!(
        answer,
        Answer::Done(Value::Literali("echo hello".to_string()))
    );
    assert_eq!(
        driver.review(Frame {
            marker: Marker::Step,
            path: "/probe:/1",
            bound: "",
            verdict: None,
            offers: &[Offer::Quit]
        }),
        Review::Leave
    );
    assert_eq!(driver.into_output(), "    » Click Submit\n".as_bytes());
}
