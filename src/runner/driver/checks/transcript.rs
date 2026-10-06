use crate::runner::driver::{Headless, Kind, Marker, Offer};

use super::*;

const STEP: [Offer; 4] = [Offer::Edit, Offer::Skip, Offer::Fail, Offer::Quit];

#[test]
fn the_trail_carries_entries_values_and_executions() {
    let produced = Value::Literali("ran".to_string());
    let mut driver = Transcript::with_output(Headless::new(), Vec::new());
    driver.show(Event::Step {
        path: "/probe:/1",
        constraints: "",
        text: "Run it",
        depth: 1,
    });
    let command = driver.ask(Question {
        marker: Marker::Step,
        path: "/probe:/1",
        prompt: Prompt::Command { script: "true" },
        offers: &STEP,
        reviewable: true,
        draft: None,
    });
    let verdict = driver.ask(Question {
        marker: Marker::Step,
        path: "/probe:/1",
        prompt: Prompt::Confirm {
            standing: Standing::Done,
            kind: Kind::System,
            produced: &produced,
            choices: &[],
        },
        offers: &STEP,
        reviewable: true,
        draft: None,
    });
    assert_eq!(command, Answer::Done(Value::Literali("true".to_string())));
    assert_eq!(verdict, Answer::Done(produced.clone()));

    let (_, out) = driver.into_inner();
    let expected = [
        Trail::Enter {
            path: "/probe:/1".to_string(),
        },
        Trail::Execute {
            path: "/probe:/1".to_string(),
            script: "true".to_string(),
        },
        Trail::Leave {
            path: "/probe:/1".to_string(),
            outcome: Some(Standing::Done),
            result: produced,
        },
    ]
    .iter()
    .map(|t| format!("{:#?}\n", t))
    .collect::<String>();
    assert_eq!(String::from_utf8(out).expect("utf8"), expected);
}
