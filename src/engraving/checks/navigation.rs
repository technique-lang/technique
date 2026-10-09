use crate::engraving::{Journal, Motion, Position, Record, Serial, parse_records};

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

// Press one key after another from a position, requiring each to land.
fn walk(journal: &Journal, from: usize, motion: Motion, expected: &[usize]) {
    let mut at = Position::At(from);
    for k in expected {
        at = journal
            .step(at, motion)
            .unwrap_or_else(|| panic!("refused before reaching {}", k));
        assert_eq!(at, Position::At(*k));
    }
}

// A step invoking a procedure, which the journal writes at a path beside the
// step rather than beneath it.
const CALLING: &str = r#"
    000 / Start file://Task.tq
    001 /task: Begin ()
    002 /task:/1 Begin ()
    002 /task:/1 Invoke check:
    003 /task:/check: Begin ()
    003 /task:/check: Done ()
    002 /task:/1 Done ()
    001 /task: Done ()
"#;

/// The callee is enclosed by the step that invoked it, not by the scope its
/// path sits under, so Left out of it reaches the call site.
#[test]
fn a_callee_is_enclosed_by_its_call_site() {
    let records = journal(CALLING);
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
    let records = journal(CALLING);
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

    let ended = journal_with(CALLING, "000 / Finish");
    let journal = Journal::new(&ended, None);
    assert_eq!(journal.last(), Some(Position::At(7)));
    assert_eq!(journal.opening(), Some(Position::At(6)));
    assert_eq!(journal.step(Position::At(7), Motion::Down), None);
    assert_eq!(journal.step(Position::At(0), Motion::Up), None);
}

fn journal_with(text: &str, more: &str) -> Vec<Record> {
    journal(&format!("{}\n{}", text, more))
}

/// `Stop` and `Resume` bracket a session, not the walk. Review opens on the
/// last thing the walk did, and stepping past where a run was interrupted
/// crosses the pair as though it were not there.
#[test]
fn a_session_boundary_is_not_a_position() {
    let records = journal(
        r#"
        000 / Start file://Task.tq
        001 /task: Begin ()
        002 /task:/1 Begin ()
        002 /task:/1 Invoke check:
        003 /task:/check: Begin ()
        003 /task:/check: Done ()
        000 / Stop
        000 / Resume
        002 /task:/1 Done ()
        001 /task: Done ()
        000 / Stop
        000 / Resume
        "#,
    );
    let journal = Journal::new(&records, None);

    assert_eq!(journal.last(), Some(Position::At(9)));
    assert_eq!(
        journal.step(Position::Live, Motion::Up),
        Some(Position::At(9))
    );
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
    assert_eq!(journal.step(Position::At(6), Motion::Up), None);
}

/// A procedure invoked twice records both at its own path, so the path is not
/// what tells two executions apart — the serial is.
#[test]
fn two_invocations_of_one_procedure_both_stand() {
    let records = journal(
        r#"
        001 /task: Begin ()
        002 /task:/1 Begin ()
        003 /task:/check: Begin ()
        003 /task:/check: Done ()
        002 /task:/1 Done ()
        004 /task:/2 Begin ()
        005 /task:/check: Begin ()
        005 /task:/check: Done ()
        004 /task:/2 Done ()
        "#,
    );
    let journal = Journal::new(&records, None);

    walk(&journal, 8, Motion::Up, &[7, 6, 5, 4, 3, 2, 1, 0]);
}

/// Withdrawing an answer and giving a different one writes a second `Begin`
/// and outcome under the same serial. Only the answer standing now is a place
/// to go; the one replaced and the `Revoke` that took it away are not.
#[test]
fn an_amended_answer_is_the_only_one_review_reaches() {
    let records = journal(
        r#"
        001 /task: Begin ()
        002 /task:/1 Begin ()
        002 /task:/1 Skip
        002 /task:/1 Revoke
        002 /task:/1 Begin ()
        002 /task:/1 Done ()
        "#,
    );
    let journal = Journal::new(&records, None);

    assert_eq!(journal.last(), Some(Position::At(5)));
    walk(&journal, 5, Motion::Up, &[4, 0]);
    for at in [1, 2, 3] {
        assert_eq!(journal.step(Position::At(at), Motion::Up), None);
    }
}

/// Revoking a call withdraws its verdict alone: the work beneath it stands,
/// and the call's new outcome replaces the old.
#[test]
fn a_revoked_scope_keeps_what_it_holds() {
    let records = journal(
        r#"
        001 /task: Begin ()
        002 /task:/check: Begin ()
        003 /task:/check:/1 Begin ()
        003 /task:/check:/1 Done ()
        002 /task:/check: Done ()
        002 /task:/check: Revoke
        002 /task:/check: Skip
        "#,
    );
    let journal = Journal::new(&records, None);

    walk(&journal, 6, Motion::Up, &[3, 2, 1, 0]);
    assert_eq!(journal.step(Position::At(0), Motion::Up), None);
}

/// The replay comes back into a revoked scope without writing its `Begin`, so
/// what it opens afresh is enclosed by it, not by its parent.
#[test]
fn a_scope_opened_beneath_a_revoked_scope_is_enclosed_by_it() {
    let records = journal(
        r#"
        000 / Start file://Task.tq
        001 /task: Begin ()
        002 /task:/1 Begin ()
        003 /task:/1/[1] Begin ()
        003 /task:/1/[1] Done ()
        002 /task:/1 Done ()
        002 /task:/1 Revoke
        004 /task:/1/[2] Begin ()
        004 /task:/1/[2] Done ()
        002 /task:/1 Skip
        "#,
    );
    let journal = Journal::new(&records, None);

    walk(&journal, 9, Motion::Up, &[8, 7, 4, 3, 2, 1, 0]);
    assert_eq!(
        journal.step(Position::At(7), Motion::Left),
        Some(Position::At(2))
    );
}

// Step 1 revoked from the prompt at step 4 and redone; the replay enters step
// 2 again with what the redone step gave, and its callee in the same slot.
const REDONE: &str = r#"
    000 / Start file://Task.tq
    001 /task: Begin ()
    002 /task:/1 Begin ()
    002 /task:/1 Done ()
    003 /task:/2 Begin ( "a" )
    003 /task:/2 Invoke check:
    004 /check: Begin ( "a" )
    004 /check: Done ()
    003 /task:/2 Done ()
    005 /task:/3 Begin ()
    005 /task:/3 Done ()
    006 /task:/4 Begin ()
    002 /task:/1 Revoke
    002 /task:/1 Begin ()
    002 /task:/1 Done ()
    003 /task:/2 Begin ( "b" )
    003 /task:/2 Invoke check:
    004 /check: Begin ( "b" )
"#;

/// Review walks the document, not the journal: a redone step keeps its place
/// among its peers, and only the records of the activations now standing are
/// places to stand.
#[test]
fn a_redone_step_takes_the_place_of_the_one_it_replaced() {
    let records = journal_with(REDONE, "004 /check: Done ()\n003 /task:/2 Done ()");
    let journal = Journal::new(&records, None);

    walk(
        &journal,
        11,
        Motion::Up,
        &[10, 9, 19, 18, 17, 16, 15, 14, 13, 1, 0],
    );
    assert_eq!(journal.step(Position::At(0), Motion::Up), None);
    assert_eq!(
        journal.step(Position::At(11), Motion::Down),
        Some(Position::Live)
    );

    assert_eq!(journal.step(Position::At(13), Motion::PageUp), None);
    walk(&journal, 13, Motion::PageDown, &[15, 9, 11]);
    walk(&journal, 11, Motion::PageUp, &[9, 15, 13]);
    for at in 2..=8 {
        assert_eq!(journal.step(Position::At(at), Motion::Up), None);
    }
}

/// A callee begun again has none of the outcomes its first activation had,
/// and the prompt within it is where review opens.
#[test]
fn a_redone_callee_awaiting_its_outcome_has_none_from_the_first_pass() {
    let records = journal(REDONE);
    let journal = Journal::new(&records, Some(Serial(4)));

    assert_eq!(journal.last(), Some(Position::At(17)));
    assert_eq!(
        journal.step(Position::At(17), Motion::Left),
        Some(Position::At(15))
    );
    assert_eq!(
        journal.step(Position::At(15), Motion::Right),
        Some(Position::At(17))
    );
    assert_eq!(
        journal.step(Position::At(14), Motion::PageDown),
        Some(Position::At(15))
    );
    // Down from the prompt's scope crosses to the work that still stands
    // beyond it.
    walk(&journal, 17, Motion::Down, &[9, 10, 11]);
    assert_eq!(
        journal.step(Position::At(11), Motion::Down),
        Some(Position::Live)
    );
    assert_eq!(
        journal.step(Position::Live, Motion::Up),
        Some(Position::At(17))
    );
}

/// A step begun again whose callee has not been reached yet encloses nothing.
#[test]
fn a_redone_step_encloses_nothing_until_its_callee_is_reached() {
    let records = journal(
        r#"
        001 /task: Begin ()
        002 /task:/1 Begin ( "a" )
        002 /task:/1 Invoke check:
        003 /check: Begin ( "a" )
        003 /check: Done ()
        002 /task:/1 Done ()
        002 /task:/1 Begin ( "b" )
        "#,
    );
    let journal = Journal::new(&records, Some(Serial(2)));

    assert_eq!(journal.last(), Some(Position::At(6)));
    assert_eq!(journal.step(Position::At(6), Motion::Right), None);
    walk(&journal, 6, Motion::Up, &[0]);
    assert_eq!(
        journal.step(Position::At(6), Motion::Down),
        Some(Position::Live)
    );
}

#[test]
fn a_step_redone_twice_keeps_the_place_of_the_first() {
    let records = journal(
        r#"
        000 / Start file://Task.tq
        001 /task: Begin ()
        002 /task:/1 Begin ()
        002 /task:/1 Done ()
        003 /task:/2 Begin ()
        003 /task:/2 Done ()
        002 /task:/1 Revoke
        002 /task:/1 Begin ()
        002 /task:/1 Done ()
        002 /task:/1 Revoke
        002 /task:/1 Begin ()
        002 /task:/1 Done ()
        "#,
    );
    let journal = Journal::new(&records, None);

    assert_eq!(journal.last(), Some(Position::At(5)));
    walk(&journal, 4, Motion::Up, &[11, 10, 1]);
}

/// A withdrawn step is absent until the walk reaches it again: the cursor
/// passes across the gap it leaves, and it is no peer to stop at.
#[test]
fn a_withdrawn_step_leaves_a_gap_the_cursor_crosses() {
    let records = journal(
        r#"
        000 / Start file://Task.tq
        001 /task: Begin ()
        002 /task:/1 Begin ()
        002 /task:/1 Done ()
        003 /task:/2 Begin ()
        003 /task:/2 Bind ( "x" ~ mark )
        003 /task:/2 Done ()
        004 /task:/3 Begin ()
        003 /task:/2 Revoke
        000 / Stop
        "#,
    );
    let journal = Journal::new(&records, Some(Serial::ROOT));

    assert_eq!(journal.last(), Some(Position::At(7)));
    walk(&journal, 3, Motion::Down, &[7]);
    walk(&journal, 7, Motion::Up, &[3]);
    walk(&journal, 2, Motion::PageDown, &[7]);
    assert_eq!(
        journal.step(Position::At(1), Motion::Right),
        Some(Position::At(2))
    );
    for at in [4, 5, 6, 8] {
        assert_eq!(journal.step(Position::At(at), Motion::Up), None);
    }
}

/// An iteration a shrunken list no longer reaches, revoked as its loop closed,
/// holds no position.
#[test]
fn an_unreached_iteration_is_no_position() {
    let records = journal(
        r#"
        000 / Start file://Task.tq
        001 /task: Begin ()
        002 /task:/1 Begin ()
        003 /task:/1/[1] Begin ( "a" ~ s )
        004 /task:/1/[1]/-1 Begin ( "a" ~ s )
        004 /task:/1/[1]/-1 Done ()
        003 /task:/1/[1] Done ()
        005 /task:/1/[2] Begin ( "b" ~ s )
        006 /task:/1/[2]/-1 Begin ( "b" ~ s )
        006 /task:/1/[2]/-1 Done ()
        005 /task:/1/[2] Done ()
        002 /task:/1 Done ()
        002 /task:/1 Revoke
        005 /task:/1/[2] Revoke
        002 /task:/1 Done ()
        001 /task: Done ()
        000 / Finish
        "#,
    );
    let journal = Journal::new(&records, None);

    assert_eq!(journal.last(), Some(Position::At(15)));
    assert_eq!(journal.opening(), Some(Position::At(14)));
    walk(&journal, 15, Motion::Up, &[14, 6, 5, 4, 3, 2, 1, 0]);
    assert_eq!(journal.step(Position::At(3), Motion::PageDown), None);
}

#[test]
fn review_opens_on_the_last_thing_the_walk_did_when_the_prompt_is_ahead_of_survivors() {
    let records = journal(
        r#"
        000 / Start file://Task.tq
        001 /task: Begin ()
        002 /task:/1 Begin ()
        002 /task:/1 Done ()
        003 /task:/2 Begin ()
        003 /task:/2 Done ()
        004 /task:/3 Begin ()
        002 /task:/1 Revoke
        002 /task:/1 Begin ()
        000 / Stop
        "#,
    );
    let journal = Journal::new(&records, Some(Serial(2)));

    assert_eq!(journal.last(), Some(Position::At(8)));
    walk(&journal, 8, Motion::Down, &[4, 5, 6]);
}

// Three steps done and a fourth begun, each written once.
const STEPPING: &str = r#"
    000 / Start file://Task.tq
    001 /task: Begin ()
    002 /task:/1 Begin ()
    002 /task:/1 Done ()
    003 /task:/2 Begin ()
    003 /task:/2 Done ()
    004 /task:/3 Begin ()
    004 /task:/3 Done ()
    005 /task:/4 Begin ()
"#;

/// A replay passes the steps that stand without writing anything, so the
/// prompt's scope, not the last record written, is where review opens.
#[test]
fn review_opens_within_the_scope_the_prompt_belongs_to() {
    let records = journal_with(
        STEPPING,
        "005 /task:/4 Done ()\n002 /task:/1 Revoke\n002 /task:/1 Begin ()",
    );
    let journal = Journal::new(&records, Some(Serial(2)));

    assert_eq!(journal.last(), Some(Position::At(11)));

    let records = journal_with(
        STEPPING,
        "002 /task:/1 Revoke\n002 /task:/1 Begin ()\n002 /task:/1 Done ()",
    );
    let journal = Journal::new(&records, Some(Serial(5)));

    assert_eq!(journal.last(), Some(Position::At(8)));
}
