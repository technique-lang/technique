use std::fs;
use std::path::Path;

use super::*;
use crate::engraving::parse_records;
use crate::value::Value;

// Journal lines without their timestamp and run id.
fn journal(text: &str) -> Vec<Record> {
    let lines: String = text
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(|line| format!("2026-09-28T00:00:00.000Z 000001 {}\n", line))
        .collect();
    parse_records(&lines).expect("journal parses")
}

fn fold(text: &str) -> History {
    History::new(&journal(text))
}

fn named(value: &str, name: &str) -> Supplied {
    Supplied {
        value: Value::Literali(value.to_string()),
        name: Some(name.to_string()),
    }
}

fn done(value: &str) -> Option<State> {
    Some(State::Done(Some(Value::Literali(value.to_string()))))
}

#[test]
fn fresh_walk_folds_into_the_tree_it_took() {
    let history = fold(
        r#"
        000 / Start file://Survey.tq
        001 /survey: Begin ()
        002 /survey:/1 Begin ()
        002 /survey:/1 Bind ( "42" ~ reading )
        002 /survey:/1 Done "42"
        003 /survey:/2 Begin ( "42" ~ reading )
        003 /survey:/2 Invoke report:
        004 /report: Begin ( "42" ~ r )
        004 /report: Done ()
        003 /survey:/2 Done ()
        001 /survey: Done ()
        000 / Finish
        "#,
    );

    assert_eq!(history.roots(), &[Serial(1)]);
    assert!(history.finished());
    assert_eq!(history.next_serial(), Serial(5));

    let survey = history
        .get(Serial(1))
        .unwrap();
    assert_eq!(survey.parent, Serial::LIFECYCLE);
    assert_eq!(survey.edge, "/survey:");
    assert_eq!(survey.children, vec![Serial(2), Serial(3)]);
    assert_eq!(survey.standing, Standing::Closed);
    assert_eq!(survey.records, vec![1, 10]);

    let first = history
        .get(Serial(2))
        .unwrap();
    assert_eq!(first.edge, "/1");
    assert_eq!(first.bound, vec![named("42", "reading")]);
    assert_eq!(first.outcome, done("42"));
    assert_eq!(first.begun_at, 2);
    assert_eq!(first.bound_at, vec![3]);
    assert_eq!(first.closed_at, Some(4));
    assert_eq!(first.records, vec![2, 3, 4]);

    let second = history
        .get(Serial(3))
        .unwrap();
    assert_eq!(second.began, vec![named("42", "reading")]);
    assert_eq!(
        second.invoked,
        vec![InvokeTarget::Procedure("report".to_string())]
    );
    assert_eq!(second.children, vec![Serial(4)]);
    assert_eq!(second.records, vec![5, 6, 9]);

    let report = history
        .get(Serial(4))
        .unwrap();
    assert_eq!(report.parent, Serial(3));
    assert_eq!(report.edge, "/report:");
    assert_eq!(report.occurrence, 0);

    assert_eq!(history.slot(Serial(1), "/2", 0), Some(Serial(3)));
    assert_eq!(history.slot(Serial(3), "/report:", 0), Some(Serial(4)));
    assert_eq!(history.slot(Serial(3), "/report:", 1), None);
}

#[test]
fn a_callee_beneath_a_section_takes_the_suffix_as_its_edge() {
    let history = fold(
        r#"
        001 /check: Begin ()
        002 /check:/I Begin ()
        002 /check:/I Invoke local:
        003 /check:/I/local: Begin ()
        "#,
    );

    assert_eq!(
        history
            .get(Serial(3))
            .unwrap()
            .edge,
        "/local:"
    );
    assert_eq!(history.slot(Serial(2), "/local:", 0), Some(Serial(3)));
}

#[test]
fn stop_and_resume_leave_the_open_scopes_alone() {
    let history = fold(
        r#"
        000 / Start file://Survey.tq
        001 /survey: Begin ()
        002 /survey:/1 Begin ()
        003 /survey:/1/a Begin ()
        000 / Stop
        000 / Resume
        003 /survey:/1/a Done ()
        004 /survey:/1/b Begin ()
        000 / Stop
        "#,
    );

    assert!(!history.finished());
    let first = history
        .get(Serial(2))
        .unwrap();
    assert_eq!(first.children, vec![Serial(3), Serial(4)]);
    assert_eq!(first.standing, Standing::Open);
    assert_eq!(
        history
            .get(Serial(3))
            .unwrap()
            .records,
        vec![3, 6]
    );
    assert_eq!(
        history
            .get(Serial(4))
            .unwrap()
            .parent,
        Serial(2)
    );
}

#[test]
fn revoking_a_leaf_withdraws_it_and_reopens_its_ancestors() {
    let records = journal(
        r#"
        001 /survey: Begin ()
        002 /survey:/1 Begin ()
        002 /survey:/1 Bind ( "42" ~ reading )
        002 /survey:/1 Done "42"
        003 /survey:/2 Begin ()
        002 /survey:/1 Revoke
        "#,
    );
    let history = History::new(&records);

    let first = history
        .get(Serial(2))
        .unwrap();
    assert_eq!(first.standing, Standing::Withdrawn);
    assert!(first.revoked);
    assert_eq!(first.outcome, None);
    assert_eq!(first.bound, Vec::new());
    assert_eq!(first.former_bound, vec![named("42", "reading")]);
    assert_eq!(first.former_outcome, done("42"));
    assert_eq!(first.records, vec![1]);

    let survey = history
        .get(Serial(1))
        .unwrap();
    assert_eq!(survey.standing, Standing::Reopened);
    assert!(!survey.revoked);

    // Re-activated, it keeps what it had before the Revoke to seed from, and
    // the stack is back beneath the procedure.
    let mut records = records;
    records.extend(journal(
        r#"
        002 /survey:/1 Begin ()
        002 /survey:/1 Bind ( "73" ~ reading )
        002 /survey:/1 Done "73"
        004 /survey:/3 Begin ()
        "#,
    ));
    let history = History::new(&records);

    let first = history
        .get(Serial(2))
        .unwrap();
    assert_eq!(first.standing, Standing::Closed);
    assert!(!first.revoked);
    assert_eq!(first.bound, vec![named("73", "reading")]);
    assert_eq!(first.former_bound, vec![named("42", "reading")]);
    assert_eq!(first.former_outcome, done("42"));
    assert_eq!(first.records, vec![6, 7, 8]);
    assert_eq!(
        history
            .get(Serial(4))
            .unwrap()
            .parent,
        Serial(1)
    );
    assert_eq!(
        history
            .get(Serial(1))
            .unwrap()
            .children,
        vec![Serial(2), Serial(3), Serial(4)]
    );
}

#[test]
fn revoking_a_scope_reopens_it_with_its_children_standing() {
    let history = fold(
        r#"
        001 /task: Begin ()
        002 /task:/1 Begin ()
        003 /task:/1/[1] Begin ( "a" ~ s )
        003 /task:/1/[1] Done ()
        002 /task:/1 Done ()
        001 /task: Done ()
        002 /task:/1 Revoke
        004 /task:/1/[2] Begin ( "b" ~ s )
        "#,
    );

    let scope = history
        .get(Serial(2))
        .unwrap();
    assert_eq!(scope.standing, Standing::Reopened);
    assert!(scope.revoked);
    assert_eq!(scope.former_outcome, Some(State::Done(Some(Value::Unitus))));
    assert_eq!(scope.children, vec![Serial(3), Serial(4)]);
    assert_eq!(
        history
            .get(Serial(3))
            .unwrap()
            .standing,
        Standing::Closed
    );
    assert_eq!(
        history
            .get(Serial(4))
            .unwrap()
            .edge,
        "/[2]"
    );

    let task = history
        .get(Serial(1))
        .unwrap();
    assert_eq!(task.standing, Standing::Reopened);
    assert_eq!(task.outcome, None);
    assert_eq!(task.records, vec![0]);
}

#[test]
fn a_nested_revoke_withdraws_every_verdict_it_rolled_up_into() {
    let history = fold(
        r#"
        001 /survey: Begin ()
        002 /survey:/I Begin ()
        003 /survey:/I/1 Begin ()
        003 /survey:/I/1 Skip
        004 /survey:/I/2 Begin ()
        004 /survey:/I/2 Done ()
        002 /survey:/I Done ()
        005 /survey:/II Begin ()
        005 /survey:/II Done ()
        001 /survey: Done ()
        003 /survey:/I/1 Revoke
        "#,
    );

    let leaf = history
        .get(Serial(3))
        .unwrap();
    assert_eq!(leaf.standing, Standing::Withdrawn);
    assert_eq!(leaf.former_outcome, Some(State::Skip));
    for serial in [Serial(1), Serial(2)] {
        let ancestor = history
            .get(serial)
            .unwrap();
        assert_eq!(ancestor.standing, Standing::Reopened);
        assert_eq!(ancestor.outcome, None);
        assert_eq!(ancestor.closed_at, None);
    }
    for serial in [Serial(4), Serial(5)] {
        assert_eq!(
            history
                .get(serial)
                .unwrap()
                .standing,
            Standing::Closed
        );
    }
}

#[test]
fn beginning_again_supersedes_everything_beneath() {
    let history = fold(
        r#"
        001 /survey: Begin ()
        002 /survey:/1 Begin ( "42" ~ reading )
        002 /survey:/1 Invoke report:
        003 /report: Begin ( "42" ~ r )
        004 /report:/1 Begin ( "42" ~ r )
        004 /report:/1 Done ()
        003 /report: Done ()
        002 /survey:/1 Done ()
        002 /survey:/1 Begin ( "73" ~ reading )
        002 /survey:/1 Invoke report:
        003 /report: Begin ( "73" ~ r )
        "#,
    );

    let step = history
        .get(Serial(2))
        .unwrap();
    assert_eq!(step.began, vec![named("73", "reading")]);
    assert_eq!(step.standing, Standing::Open);
    assert_eq!(step.former_outcome, Some(State::Done(Some(Value::Unitus))));
    assert_eq!(step.records, vec![8, 9]);
    assert_eq!(
        step.invoked
            .len(),
        1
    );
    assert_eq!(step.children, vec![Serial(3)]);

    let report = history
        .get(Serial(3))
        .unwrap();
    assert_eq!(report.began, vec![named("73", "r")]);
    assert_eq!(
        report.former_outcome,
        Some(State::Done(Some(Value::Unitus)))
    );
    assert_eq!(report.children, Vec::new());

    // Its step has no current activation, but keeps its slot and what it was.
    assert_eq!(history.get(Serial(4)), None);
    assert_eq!(history.slot(Serial(3), "/1", 0), Some(Serial(4)));
    assert_eq!(
        history
            .retired(Serial(4))
            .unwrap()
            .outcome,
        Some(State::Done(Some(Value::Unitus)))
    );
    assert_eq!(history.retired(Serial(3)), None);
    assert_eq!(history.next_serial(), Serial(5));
}

#[test]
fn a_retired_slot_begun_again_returns_to_its_parent() {
    let history = fold(
        r#"
        001 /survey: Begin ()
        002 /survey:/1 Begin ( "42" ~ reading )
        003 /survey:/1/a Begin ()
        003 /survey:/1/a Bind ( "x" ~ mark )
        003 /survey:/1/a Done ()
        002 /survey:/1 Done ()
        002 /survey:/1 Begin ( "73" ~ reading )
        003 /survey:/1/a Begin ()
        "#,
    );

    let leaf = history
        .get(Serial(3))
        .unwrap();
    assert_eq!(leaf.parent, Serial(2));
    assert_eq!(leaf.former_bound, vec![named("x", "mark")]);
    assert_eq!(history.retired(Serial(3)), None);
    assert_eq!(
        history
            .get(Serial(2))
            .unwrap()
            .children,
        vec![Serial(3)]
    );
}

#[test]
fn two_calls_from_one_step_are_two_occurrences() {
    let history = fold(
        r#"
        001 /survey: Begin ()
        002 /survey:/3 Begin ()
        002 /survey:/3 Invoke report:
        003 /report: Begin ( "1" ~ r )
        003 /report: Done ()
        002 /survey:/3 Invoke report:
        004 /report: Begin ( "fixed" ~ r )
        004 /report: Done ()
        002 /survey:/3 Done ()
        "#,
    );

    assert_eq!(history.slot(Serial(2), "/report:", 0), Some(Serial(3)));
    assert_eq!(history.slot(Serial(2), "/report:", 1), Some(Serial(4)));
    assert_eq!(
        history
            .get(Serial(4))
            .unwrap()
            .occurrence,
        1
    );
    let step = history
        .get(Serial(2))
        .unwrap();
    assert_eq!(step.children, vec![Serial(3), Serial(4)]);
    assert_eq!(
        step.invoked
            .len(),
        2
    );
    assert_eq!(step.records, vec![1, 2, 5, 8]);
}

#[test]
fn sibling_iterations_are_slots_of_their_own() {
    let history = fold(
        r#"
        001 /survey: Begin ()
        002 /survey:/5 Begin ()
        003 /survey:/5/[1] Begin ( "p" ~ s )
        004 /survey:/5/[1]/-1 Begin ( "p" ~ s )
        004 /survey:/5/[1]/-1 Done ()
        003 /survey:/5/[1] Done ()
        005 /survey:/5/[2] Begin ( "q" ~ s )
        006 /survey:/5/[2]/-1 Begin ( "q" ~ s )
        "#,
    );

    assert_eq!(history.slot(Serial(2), "/[1]", 0), Some(Serial(3)));
    assert_eq!(history.slot(Serial(2), "/[2]", 0), Some(Serial(5)));
    assert_eq!(history.slot(Serial(5), "/-1", 0), Some(Serial(6)));
    assert_eq!(
        history
            .get(Serial(2))
            .unwrap()
            .children,
        vec![Serial(3), Serial(5)]
    );
}

#[test]
fn a_revoked_child_left_open_retires_when_its_parent_closes() {
    let mut records = journal(
        r#"
        001 /survey: Begin ()
        002 /survey:/1 Begin ()
        003 /survey:/1/[1] Begin ( "a" ~ s )
        004 /survey:/1/[1]/-1 Begin ( "a" ~ s )
        004 /survey:/1/[1]/-1 Done ()
        003 /survey:/1/[1] Done ()
        005 /survey:/1/[2] Begin ( "b" ~ s )
        006 /survey:/1/[2]/-1 Begin ( "b" ~ s )
        006 /survey:/1/[2]/-1 Done ()
        005 /survey:/1/[2] Done ()
        002 /survey:/1 Done ()
        002 /survey:/1 Revoke
        005 /survey:/1/[2] Revoke
        002 /survey:/1 Done ()
        "#,
    );
    let open = History::new(&records[..records.len() - 1]);
    assert_eq!(
        open.get(Serial(5))
            .unwrap()
            .standing,
        Standing::Reopened
    );

    let history = History::new(&records);
    let scope = history
        .get(Serial(2))
        .unwrap();
    assert_eq!(scope.standing, Standing::Closed);
    assert_eq!(scope.children, vec![Serial(3)]);
    assert_eq!(history.get(Serial(5)), None);
    assert_eq!(history.get(Serial(6)), None);
    assert_eq!(
        history
            .retired(Serial(5))
            .unwrap()
            .former_outcome,
        Some(State::Done(Some(Value::Unitus)))
    );
    assert_eq!(history.slot(Serial(2), "/[2]", 0), Some(Serial(5)));

    // Reached again, the slot is begun afresh beneath its parent.
    records.extend(journal(
        r#"
        002 /survey:/1 Revoke
        005 /survey:/1/[2] Begin ( "b" ~ s )
        "#,
    ));
    let history = History::new(&records);
    assert_eq!(
        history
            .get(Serial(2))
            .unwrap()
            .children,
        vec![Serial(3), Serial(5)]
    );
    assert_eq!(
        history
            .get(Serial(5))
            .unwrap()
            .standing,
        Standing::Open
    );
}

#[test]
fn effects_belong_to_the_activation_they_were_written_in() {
    let history = fold(
        r#"
        001 /release: Begin ()
        002 /release:/2 Begin ()
        002 /release:/2 Execute exec()
        002 /release:/2 Return "1"
        002 /release:/2 Execute now()
        002 /release:/2 Return
        002 /release:/2 Execute exec()
        "#,
    );

    let step = history
        .get(Serial(2))
        .unwrap();
    assert_eq!(
        step.effects,
        vec![
            Effect {
                function: "exec".to_string(),
                returned: Some(Some(Value::Literali("1".to_string()))),
            },
            Effect {
                function: "now".to_string(),
                returned: Some(None),
            },
            Effect {
                function: "exec".to_string(),
                returned: None,
            },
        ]
    );
    assert_eq!(step.records, vec![1, 2, 3, 4, 5, 6]);

    // A re-activation starts with none; a reopened scope keeps them.
    let history = fold(
        r#"
        001 /release: Begin ()
        001 /release: Execute exec()
        001 /release: Return "1"
        002 /release:/1 Begin ()
        002 /release:/1 Execute exec()
        002 /release:/1 Return "2"
        002 /release:/1 Done ()
        001 /release: Done ()
        001 /release: Revoke
        002 /release:/1 Begin ()
        "#,
    );
    assert_eq!(
        history
            .get(Serial(1))
            .unwrap()
            .effects
            .len(),
        1
    );
    assert_eq!(
        history
            .get(Serial(2))
            .unwrap()
            .effects,
        Vec::new()
    );
}

#[test]
fn binds_accumulate_until_begun_again() {
    let text = r#"
        000 / Start file://Survey.tq
        001 /survey: Begin ()
        002 /survey:/1 Begin ()
        002 /survey:/1 Bind ( "4" ~ a )
        002 /survey:/1 Bind ( "5" ~ b )
        002 /survey:/1 Bind ( "6" ~ a )
        "#;
    let history = fold(text);
    let first = history
        .get(Serial(2))
        .unwrap();
    assert_eq!(first.bound, vec![named("5", "b"), named("6", "a")]);
    assert_eq!(first.standing, Standing::Open);

    let history = fold(&format!("{}\n002 /survey:/1 Begin ()", text));
    let first = history
        .get(Serial(2))
        .unwrap();
    assert!(
        first
            .bound
            .is_empty()
    );
}

#[test]
fn finished_is_how_the_last_session_ended() {
    let ended = r#"
        000 / Start file://Survey.tq
        001 /survey: Begin ()
        001 /survey: Done ()
        000 / Finish
        "#;
    assert!(fold(ended).finished());
    assert!(fold(&format!("{}\n000 / Resume", ended)).finished());
    assert!(!fold(&format!("{}\n000 / Resume\n001 /survey: Revoke", ended)).finished());
    assert!(
        !fold(&format!(
            "{}\n000 / Resume\n001 /survey: Revoke\n001 /survey: Done ()",
            ended
        ))
        .finished()
    );
    assert!(!fold(&format!("{}\n000 / Resume\n001 /survey: Begin ()", ended)).finished());
    assert!(!fold(&format!("{}\n000 / Resume\n000 / Stop", ended)).finished());
}

#[test]
fn a_begin_on_the_lifecycle_serial_is_ignored() {
    let history = fold(
        r#"
        000 / Start file://Survey.tq
        000 /x: Begin ()
        000 /x: Revoke
        000 /x: Begin ()
        "#,
    );
    assert!(
        history
            .get(Serial::LIFECYCLE)
            .is_none()
    );
    assert!(
        history
            .roots()
            .is_empty()
    );
}

// Every serial written has a current activation holding only its own records,
// in the slot it was first begun at and among its parent's children. A journal
// never amended has every record held.
fn assert_consistent(file: &Path) {
    let content = fs::read_to_string(file).expect("read journal");
    let records = parse_records(&content).expect("journal parses");
    let history = History::new(&records);
    let amended = records
        .iter()
        .any(|record| record.state == State::Revoke);

    for (i, record) in records
        .iter()
        .enumerate()
    {
        if record.serial == Serial::LIFECYCLE {
            continue;
        }
        let activation = history
            .get(record.serial)
            .unwrap_or_else(|| panic!("{:?}: no activation for record {}", file, i));
        assert_eq!(activation.serial, record.serial);
        assert!(
            amended
                || activation
                    .records
                    .contains(&i),
            "{:?}: record {} not held by its activation",
            file,
            i
        );
        assert!(
            activation
                .records
                .iter()
                .all(|k| records[*k].serial == record.serial)
        );
        assert_eq!(activation.records[0], activation.begun_at);
        let kin = if activation.parent == Serial::LIFECYCLE {
            history.roots()
        } else {
            &history
                .get(activation.parent)
                .unwrap_or_else(|| panic!("{:?}: parent of {:?} missing", file, record.serial))
                .children
        };
        assert!(
            kin.contains(&record.serial),
            "{:?}: {:?} not among its parent's children",
            file,
            record.serial
        );
        assert_eq!(
            history.slot(activation.parent, &activation.edge, activation.occurrence),
            Some(record.serial)
        );
        assert_eq!(
            activation
                .outcome
                .is_some(),
            activation.standing == Standing::Closed
        );
    }
}

#[test]
fn every_recorded_journal_folds_consistently() {
    for dir in ["tests/golden/runner/", "tests/navigation/"] {
        let mut count = 0;
        for entry in fs::read_dir(dir).expect("read journals") {
            let path = entry
                .expect("directory entry")
                .path();
            if path
                .extension()
                .and_then(|s| s.to_str())
                == Some("pfftt")
            {
                assert_consistent(&path);
                count += 1;
            }
        }
        assert!(count > 0, "no journals in {}", dir);
    }
}
