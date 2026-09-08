use crate::engraving::{Ledger, Record, RunId, Serial, State, Supplied};
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

fn fold(records: Vec<Record>) -> Ledger {
    let mut ledger = Ledger::new();
    for item in &records {
        ledger.apply(item);
    }
    ledger
}

#[test]
fn begin_and_outcome_pair_at_one_entry() {
    let ledger = fold(vec![
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Done(None)),
        record(1, "/task:", State::Done(None)),
    ]);

    let entry = ledger
        .look(Serial(1), "/task:/1")
        .expect("step keyed under its procedure");
    assert_eq!(entry.serial, Serial(2));
    assert!(
        entry
            .outcome
            .is_some()
    );
}

// The same document address reached through two different callers is two
// entries, which is what lets one instance of a procedure be redone while
// another stands.
#[test]
fn one_path_under_two_parents_is_two_entries() {
    let ledger = fold(vec![
        record(1, "/loop:", State::Begin(Vec::new())),
        record(2, "/loop:/[1]/-1", State::Begin(Vec::new())),
        record(3, "/helper:", State::Begin(Vec::new())),
        record(3, "/helper:", State::Done(None)),
        record(2, "/loop:/[1]/-1", State::Done(None)),
        record(4, "/loop:/[2]/-1", State::Begin(Vec::new())),
        record(5, "/helper:", State::Begin(Vec::new())),
    ]);

    let first = ledger
        .look(Serial(2), "/helper:")
        .expect("first instance");
    let second = ledger
        .look(Serial(4), "/helper:")
        .expect("second instance");
    assert_eq!(first.serial, Serial(3));
    assert_eq!(second.serial, Serial(5));
    assert!(
        first
            .outcome
            .is_some()
    );
    assert!(
        second
            .outcome
            .is_none()
    );
}

#[test]
fn bind_lands_on_the_scope_that_made_it() {
    let ledger = fold(vec![
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(
            2,
            "/task:/1",
            State::Bind(vec![Supplied {
                value: Value::Literali("42".to_string()),
                name: Some("answer".to_string()),
            }]),
        ),
        record(2, "/task:/1", State::Done(None)),
    ]);

    let entry = ledger
        .look(Serial(1), "/task:/1")
        .expect("entry");
    assert_eq!(
        entry
            .bound
            .len(),
        1
    );
    assert_eq!(entry.bound[0].name, Some("answer".to_string()));
}

// A scope re-entered on a later walk keeps its number, so its recorded
// descendants stay reachable. A fresh position takes the next one.
#[test]
fn serial_is_reused_on_re_entry_and_fresh_otherwise() {
    let ledger = fold(vec![
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Done(None)),
    ]);

    assert_eq!(ledger.serial_for(Serial::LIFECYCLE, "/task:"), Serial(1));
    assert_eq!(ledger.serial_for(Serial(1), "/task:/1"), Serial(2));
    assert_eq!(ledger.serial_for(Serial(1), "/task:/2"), Serial(3));
}

// A Quit mid-step leaves its Begin standing with no outcome. The next walk
// re-enters the procedure, and that Begin has to pop the stack back past the
// step that was in flight — otherwise everything the second walk records is
// parented under it, and a later fold reaches the same wrong answer.
#[test]
fn re_entering_an_open_scope_truncates_the_stack() {
    let ledger = fold(vec![
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Done(None)),
        // Quit here: step 2 opened and never closed.
        record(3, "/task:/2", State::Begin(Vec::new())),
        // The resume re-enters the procedure and redoes step 2.
        record(1, "/task:", State::Begin(Vec::new())),
        record(3, "/task:/2", State::Begin(Vec::new())),
        record(3, "/task:/2", State::Done(None)),
        record(1, "/task:", State::Done(None)),
    ]);

    let redone = ledger
        .look(Serial(1), "/task:/2")
        .expect("step 2 keyed under the procedure, not under itself");
    assert_eq!(redone.serial, Serial(3));
    assert!(
        redone
            .outcome
            .is_some()
    );

    // Step 1 stays reachable across the re-entry.
    let earlier = ledger
        .look(Serial(1), "/task:/1")
        .expect("step 1 still keyed under the procedure");
    assert_eq!(earlier.serial, Serial(2));
}

// A scope replayed silently writes no `Begin` of its own, so it is not on the
// open stack when a stale descendant beneath it records again. A serial is
// allocated per (parent, edge) pair, so its parent is fixed by its route and
// the re-entry stays where it was rather than being reparented onto whatever
// happens to be open.
#[test]
fn re_entry_keeps_a_serial_under_its_original_parent() {
    let ledger = fold(vec![
        record(1, "/audit:", State::Begin(Vec::new())),
        record(2, "/audit:/II", State::Begin(Vec::new())),
        record(3, "/audit:/II/1", State::Begin(Vec::new())),
        record(3, "/audit:/II/1", State::Done(None)),
        record(2, "/audit:/II", State::Done(None)),
        record(1, "/audit:", State::Done(None)),
        // A second walk: the entry re-runs, section II replays silently, and
        // the step beneath it is stale and redone.
        record(1, "/audit:", State::Begin(Vec::new())),
        record(3, "/audit:/II/1", State::Begin(Vec::new())),
        record(3, "/audit:/II/1", State::Done(None)),
    ]);

    let entry = ledger
        .look(Serial(2), "/1")
        .expect("the step stays keyed under the section it sits in");
    assert_eq!(entry.serial, Serial(3));
    assert!(
        ledger
            .look(Serial(1), "/audit:/II/1")
            .is_none(),
        "and is not duplicated under whatever was open at the time"
    );
}

// A revocation is a path copy: the spine from the root to the target is
// cleared, everything off it is shared unchanged, and no entry is removed.
#[test]
fn revoke_clears_the_spine_and_marks_only_its_target() {
    let ledger = fold(vec![
        record(1, "/audit:", State::Begin(Vec::new())),
        record(2, "/audit:/I", State::Begin(Vec::new())),
        record(3, "/audit:/I/1", State::Begin(Vec::new())),
        record(4, "/audit:/I/1/-1", State::Begin(Vec::new())),
        record(4, "/audit:/I/1/-1", State::Done(None)),
        record(3, "/audit:/I/1", State::Done(None)),
        record(2, "/audit:/I", State::Done(None)),
        record(5, "/audit:/II", State::Begin(Vec::new())),
        record(5, "/audit:/II", State::Done(None)),
        record(1, "/audit:", State::Done(None)),
        record(3, "/audit:/I/1", State::Revoke),
    ]);

    let target = ledger
        .look(Serial(2), "/1")
        .expect("the target entry is retained");
    assert!(
        target
            .outcome
            .is_none()
    );
    assert!(target.revoked, "the target is marked");

    for (parent, edge) in [(Serial::LIFECYCLE, "/audit:"), (Serial(1), "/I")] {
        let ancestor = ledger
            .look(parent, edge)
            .expect("ancestor");
        assert!(
            ancestor
                .outcome
                .is_none(),
            "{} is on the spine and is cleared",
            edge
        );
        assert!(
            !ancestor.revoked,
            "{} is not marked, or it would re-prompt on the way back down",
            edge
        );
    }

    assert!(
        ledger
            .look(Serial(3), "/-1")
            .expect("descendant")
            .outcome
            .is_some(),
        "descendants are shared, not cleared: the input guard reaches them"
    );
    assert!(
        ledger
            .look(Serial(1), "/II")
            .expect("sibling")
            .outcome
            .is_some(),
        "and a sibling off the spine is untouched"
    );
}

// Re-entering a revoked scope clears the mark: the entry is being rebuilt, so
// the reason not to restore from it has gone.
#[test]
fn beginning_a_revoked_scope_clears_the_mark() {
    let ledger = fold(vec![
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Done(None)),
        record(2, "/task:/1", State::Revoke),
        record(2, "/task:/1", State::Begin(Vec::new())),
    ]);

    let entry = ledger
        .look(Serial(1), "/1")
        .expect("entry");
    assert!(!entry.revoked);
}

// A revocation naming a scope no walk reaches is inert rather than an error.
#[test]
fn revoking_an_unknown_serial_changes_nothing() {
    let ledger = fold(vec![
        record(1, "/task:", State::Begin(Vec::new())),
        record(1, "/task:", State::Done(None)),
        record(9, "/task:/nowhere", State::Revoke),
    ]);

    assert!(
        ledger
            .look(Serial::LIFECYCLE, "/task:")
            .expect("entry")
            .outcome
            .is_some()
    );
}

#[test]
fn an_orphaned_scope_does_not_adopt_what_opens_after_it() {
    // A run stopped inside step 1 leaves its `Begin` unpaired. Step 2 opens
    // after it and belongs to the procedure, not to the step the walk happened
    // to die in — the outcome of the enclosing scope closes whatever it still
    // held open.
    let ledger = fold(vec![
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(3, "/task:/1/helper:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Done(None)),
        record(4, "/task:/2", State::Begin(Vec::new())),
    ]);

    let entry = ledger
        .look(Serial(1), "/task:/2")
        .expect("step 2 keyed under its procedure");
    assert_eq!(entry.serial, Serial(4));
}

#[test]
fn a_resumed_scope_keeps_the_serial_it_was_entered_at() {
    // A scope opened after an orphan is found again on the next walk, so
    // resuming does not write a second `Begin` for it. It did, and each resume
    // then mis-parented the next, ratcheting a duplicate spine onto the journal
    // one pair of records at a time.
    let ledger = fold(vec![
        record(1, "/task:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Begin(Vec::new())),
        record(3, "/task:/1/helper:", State::Begin(Vec::new())),
        record(2, "/task:/1", State::Done(None)),
        record(4, "/task:/2", State::Begin(Vec::new())),
        record(0, "/", State::Stop),
        // The second walk re-enters the procedure and finishes the step it
        // finds standing, rather than opening a fresh one beside it.
        record(0, "/", State::Resume),
        record(1, "/task:", State::Begin(Vec::new())),
        record(4, "/task:/2", State::Done(None)),
    ]);

    assert_eq!(ledger.serial_for(Serial(1), "/task:/2"), Serial(4));
    let entry = ledger
        .look(Serial(1), "/task:/2")
        .expect("step 2 still keyed under its procedure after the resume");
    assert!(
        entry
            .outcome
            .is_some(),
        "the second walk's outcome lands on the entry the first walk opened"
    );
}
