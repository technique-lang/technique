use crate::engraving::{
    InvokeTarget, Journal, Motion, Position, Record, RunId, Serial, State, Supplied,
};
use crate::value::Value;

fn record(serial: u32, path: &str, state: State) -> Record {
    Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        run_id: RunId(1),
        serial: Serial(serial),
        path: path.to_string(),
        state,
    }
}

// A step invoking a procedure, which the journal writes at a path beside the
// step rather than beneath it.
fn calling() -> Vec<Record> {
    vec![
        record(
            0,
            "/",
            State::Start {
                uri: "file://x".to_string(),
            },
        ),
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(
            2,
            "/task:/1",
            State::Invoke(InvokeTarget::Procedure("check:".to_string())),
        ),
        record(3, "/task:/check:", State::Begin(Vec::new())),
        record(3, "/task:/check:", State::Done(None)),
        record(2, "/task:/1", State::Done(None)),
        record(1, "/task:", State::Done(None)),
    ]
}

/// The callee is enclosed by the step that invoked it, not by the scope its
/// path sits under, so Left out of it reaches the call site.
#[test]
fn a_callee_is_enclosed_by_its_call_site() {
    let records = calling();
    let journal = Journal::new(&records, None);

    assert_eq!(
        journal.step(Position::At(4), Motion::Left),
        Some(Position::At(2))
    );
    assert_eq!(
        journal.step(Position::At(3), Motion::Right),
        Some(Position::At(4))
    );
    assert_eq!(journal.step(Position::At(4), Motion::PageUp), None);
    assert_eq!(journal.step(Position::At(4), Motion::PageDown), None);
}

/// Down off the last record reaches the prompt the run is waiting at, and a
/// run that walked to its end has no prompt to reach.
#[test]
fn down_off_the_end_leaves_review() {
    let records = calling();
    let journal = Journal::new(&records, None);
    assert_eq!(
        journal.step(Position::At(7), Motion::Down),
        Some(Position::Live)
    );
    assert_eq!(
        journal.step(Position::Live, Motion::Up),
        Some(Position::At(7))
    );
    assert_eq!(journal.step(Position::Live, Motion::Down), None);

    let mut ended = calling();
    ended.push(record(0, "/", State::Finish));
    let journal = Journal::new(&ended, None);
    assert_eq!(journal.last(), Some(Position::At(7)));
    assert_eq!(journal.step(Position::At(7), Motion::Down), None);
    assert_eq!(journal.step(Position::At(0), Motion::Up), None);
}

/// `Stop` and `Resume` bracket a session, not the walk. Review opens on the
/// last thing the walk did, and stepping past where a run was interrupted
/// crosses the pair as though it were not there.
#[test]
fn a_session_boundary_is_not_a_position() {
    let mut records = calling();
    records.insert(6, record(0, "/", State::Resume));
    records.insert(6, record(0, "/", State::Stop));
    records.push(record(0, "/", State::Stop));
    records.push(record(0, "/", State::Resume));
    let journal = Journal::new(&records, None);

    assert_eq!(journal.last(), Some(Position::At(9)));
    assert_eq!(
        journal.step(Position::Live, Motion::Up),
        Some(Position::At(9))
    );
    // 5 and 8 are the records either side of the interruption.
    assert_eq!(
        journal.step(Position::At(8), Motion::Up),
        Some(Position::At(5))
    );
    assert_eq!(
        journal.step(Position::At(5), Motion::Down),
        Some(Position::At(8))
    );
    assert_eq!(
        journal.step(Position::At(9), Motion::Down),
        Some(Position::Live)
    );
}

// The same call, dispatched again by a resumed session that was stopped at the
// argument prompt before it reached the callee.
fn redispatched() -> Vec<Record> {
    vec![
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(
            2,
            "/task:/1",
            State::Invoke(InvokeTarget::Procedure("check:".to_string())),
        ),
        record(0, "/", State::Stop),
        record(0, "/", State::Resume),
        record(
            2,
            "/task:/1",
            State::Invoke(InvokeTarget::Procedure("check:".to_string())),
        ),
        record(
            2,
            "/task:/1",
            State::Invoke(InvokeTarget::Procedure("verify:".to_string())),
        ),
    ]
}

#[test]
fn a_redispatched_call_is_one_place_to_stand() {
    // Two sessions reaching the same call wrote the dispatch line twice, and
    // pressing Up walked both showing the same thing each time. The journal keeps
    // them; the cursor stops on the last. A second call in the same step writes
    // a different line at the same address, and stands on its own.
    let records = redispatched();
    let journal = Journal::new(&records, None);

    let at = journal
        .last()
        .expect("a position to open on");
    assert_eq!(at, Position::At(6));
    assert_eq!(journal.step(at, Motion::Up), Some(Position::At(5)));
    assert_eq!(
        journal.step(Position::At(5), Motion::Up),
        Some(Position::At(1))
    );
}

#[test]
fn two_invocations_of_one_procedure_both_stand() {
    // A procedure invoked twice records both at its own path, so the path is
    // not what tells two executions apart — the serial is. Collapsing by path
    // would have swallowed the first call's whole subtree.
    let records = vec![
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(3, "/task:/check:", State::Begin(Vec::new())),
        record(3, "/task:/check:", State::Done(None)),
        record(2, "/task:/1", State::Done(None)),
        record(4, "/task:/2", State::Begin(Vec::new())),
        record(5, "/task:/check:", State::Begin(Vec::new())),
        record(5, "/task:/check:", State::Done(None)),
        record(4, "/task:/2", State::Done(None)),
    ];
    let journal = Journal::new(&records, None);

    let mut at = Position::At(8);
    for expected in [7, 6, 5, 4, 3, 2, 1, 0] {
        at = journal
            .step(at, Motion::Up)
            .expect("every record stands as its own position");
        assert_eq!(at, Position::At(expected));
    }
}

#[test]
fn an_amended_answer_is_the_only_one_review_reaches() {
    // Withdrawing an answer and giving a different one writes a second outcome
    // at the same path under a fresh serial. Navigating back shows the value
    // the position carries now; the one it replaced, the `Revoke` that took it
    // away, and the entry line above it are all recorded and none of them are
    // places to go.
    let records = vec![
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Skip),
        record(2, "/task:/1", State::Revoke),
        record(3, "/task:/1", State::Begin(Vec::new())),
        record(3, "/task:/1", State::Done(None)),
    ];
    let journal = Journal::new(&records, None);

    let at = journal
        .last()
        .expect("a position to open on");
    assert_eq!(at, Position::At(5));
    assert_eq!(journal.step(at, Motion::Up), Some(Position::At(4)));
    assert_eq!(
        journal.step(Position::At(4), Motion::Up),
        Some(Position::At(0))
    );
}

#[test]
fn a_revoked_scope_takes_what_it_held_with_it() {
    // Revoking a call withdraws the whole subtree beneath it: the replay redoes
    // that work under fresh serials, so the records the first pass left are no
    // more current than the call that held them.
    let records = vec![
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/check:", State::Begin(Vec::new())),
        record(3, "/task:/check:/1", State::Begin(Vec::new())),
        record(3, "/task:/check:/1", State::Done(None)),
        record(2, "/task:/check:", State::Done(None)),
        record(2, "/task:/check:", State::Revoke),
        record(4, "/task:/check:", State::Begin(Vec::new())),
        record(5, "/task:/check:/1", State::Begin(Vec::new())),
        record(5, "/task:/check:/1", State::Done(None)),
        record(4, "/task:/check:", State::Done(None)),
    ];
    let journal = Journal::new(&records, None);

    let mut at = Position::At(9);
    for expected in [8, 7, 6, 0] {
        at = journal
            .step(at, Motion::Up)
            .expect("the standing execution walks back to the root");
        assert_eq!(at, Position::At(expected));
    }
    assert_eq!(journal.step(at, Motion::Up), None);
}

// A scope entered with one argument, as a replay enters it again with another.
fn entering(serial: u32, path: &str, arg: &str) -> Record {
    record(
        serial,
        path,
        State::Begin(vec![Supplied {
            value: Value::Literali(arg.to_string()),
            name: None,
        }]),
    )
}

#[test]
fn a_redone_step_takes_the_place_of_the_one_it_replaced() {
    // Step 1 is revoked from the prompt at step 4 and redone under a fresh
    // serial, written after the steps that survived it. The replay enters
    // step 2 again, and the call beneath it, with what the redone step gave.
    // Review walks the document: 1, 2, 3, 4, and only the second entry to
    // step 2 is a place to stand.
    let records = vec![
        record(
            0,
            "/",
            State::Start {
                uri: "file://x".to_string(),
            },
        ),
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Done(None)),
        entering(3, "/task:/2", "a"),
        record(
            3,
            "/task:/2",
            State::Invoke(InvokeTarget::Procedure("check:".to_string())),
        ),
        entering(4, "/check:", "a"),
        record(4, "/check:", State::Done(None)),
        record(3, "/task:/2", State::Done(None)),
        record(5, "/task:/3", State::Begin(Vec::new())),
        record(5, "/task:/3", State::Done(None)),
        record(6, "/task:/4", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Revoke),
        record(7, "/task:/1", State::Begin(Vec::new())),
        record(7, "/task:/1", State::Done(None)),
        entering(3, "/task:/2", "b"),
        record(
            3,
            "/task:/2",
            State::Invoke(InvokeTarget::Procedure("check:".to_string())),
        ),
        entering(4, "/check:", "b"),
        record(4, "/check:", State::Done(None)),
        record(3, "/task:/2", State::Done(None)),
    ];
    let journal = Journal::new(&records, None);

    let mut at = Position::At(11);
    for expected in [10, 9, 19, 18, 17, 16, 15, 14, 13, 1, 0] {
        at = journal
            .step(at, Motion::Up)
            .expect("the document walks back to the root");
        assert_eq!(at, Position::At(expected));
    }
    assert_eq!(journal.step(at, Motion::Up), None);
    assert_eq!(
        journal.step(Position::At(11), Motion::Down),
        Some(Position::Live)
    );

    assert_eq!(journal.step(Position::At(13), Motion::PageUp), None);
    assert_eq!(
        journal.step(Position::At(13), Motion::PageDown),
        Some(Position::At(15))
    );
    assert_eq!(
        journal.step(Position::At(15), Motion::PageDown),
        Some(Position::At(9))
    );
    assert_eq!(
        journal.step(Position::At(9), Motion::PageUp),
        Some(Position::At(15))
    );
    assert_eq!(
        journal.step(Position::At(11), Motion::PageUp),
        Some(Position::At(9))
    );
}

#[test]
fn a_redone_step_awaiting_its_outcome_has_none_from_the_first_pass() {
    let records = vec![
        record(
            0,
            "/",
            State::Start {
                uri: "file://x".to_string(),
            },
        ),
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Done(None)),
        entering(3, "/task:/2", "a"),
        record(
            3,
            "/task:/2",
            State::Invoke(InvokeTarget::Procedure("check:".to_string())),
        ),
        entering(4, "/check:", "a"),
        record(4, "/check:", State::Done(None)),
        record(3, "/task:/2", State::Done(None)),
        record(2, "/task:/1", State::Revoke),
        record(7, "/task:/1", State::Begin(Vec::new())),
        record(7, "/task:/1", State::Done(None)),
        entering(3, "/task:/2", "b"),
        record(
            3,
            "/task:/2",
            State::Invoke(InvokeTarget::Procedure("check:".to_string())),
        ),
        entering(4, "/check:", "b"),
        record(4, "/check:", State::Done(None)),
    ];
    let journal = Journal::new(&records, Some(Serial(3)));

    assert_eq!(journal.last(), Some(Position::At(15)));
    assert_eq!(
        journal.step(Position::At(15), Motion::Left),
        Some(Position::At(12))
    );
}

#[test]
fn a_step_redone_twice_keeps_the_place_of_the_first() {
    let records = vec![
        record(
            0,
            "/",
            State::Start {
                uri: "file://x".to_string(),
            },
        ),
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Done(None)),
        record(3, "/task:/2", State::Begin(Vec::new())),
        record(3, "/task:/2", State::Done(None)),
        record(2, "/task:/1", State::Revoke),
        record(4, "/task:/1", State::Begin(Vec::new())),
        record(4, "/task:/1", State::Done(None)),
        record(4, "/task:/1", State::Revoke),
        record(5, "/task:/1", State::Begin(Vec::new())),
        record(5, "/task:/1", State::Done(None)),
    ];
    let journal = Journal::new(&records, None);

    assert_eq!(journal.last(), Some(Position::At(5)));
    assert_eq!(
        journal.step(Position::At(4), Motion::Up),
        Some(Position::At(11))
    );
}

#[test]
fn review_opens_on_the_last_thing_the_walk_did_when_the_prompt_is_ahead_of_survivors() {
    let records = vec![
        record(
            0,
            "/",
            State::Start {
                uri: "file://x".to_string(),
            },
        ),
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Done(None)),
        record(3, "/task:/2", State::Begin(Vec::new())),
        record(3, "/task:/2", State::Done(None)),
        record(4, "/task:/3", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Revoke),
        record(5, "/task:/1", State::Begin(Vec::new())),
        record(0, "/", State::Stop),
    ];
    let journal = Journal::new(&records, Some(Serial(5)));

    assert_eq!(journal.last(), Some(Position::At(8)));
}

// Three steps done and a fourth begun, each written once.
fn stepping() -> Vec<Record> {
    let mut records = vec![
        record(
            0,
            "/",
            State::Start {
                uri: "file://x".to_string(),
            },
        ),
        record(1, "/task:", State::Begin(Vec::new())),
    ];
    for n in 1..=3 {
        let path = format!("/task:/{}", n);
        records.push(record(n + 1, &path, State::Begin(Vec::new())));
        records.push(record(n + 1, &path, State::Done(None)));
    }
    records.push(record(5, "/task:/4", State::Begin(Vec::new())));
    records
}

/// A replay passes the steps that stand without writing anything, so the
/// prompt's scope, not the last record written, is where review opens.
#[test]
fn review_opens_within_the_scope_the_prompt_belongs_to() {
    let mut records = stepping();
    records.push(record(5, "/task:/4", State::Done(None)));
    records.push(record(2, "/task:/1", State::Revoke));
    records.push(record(6, "/task:/1", State::Begin(Vec::new())));
    let journal = Journal::new(&records, Some(Serial(6)));

    assert_eq!(journal.last(), Some(Position::At(11)));

    let mut records = stepping();
    records.push(record(2, "/task:/1", State::Revoke));
    records.push(record(6, "/task:/1", State::Begin(Vec::new())));
    records.push(record(6, "/task:/1", State::Done(None)));
    let journal = Journal::new(&records, Some(Serial(5)));

    assert_eq!(journal.last(), Some(Position::At(8)));
}
