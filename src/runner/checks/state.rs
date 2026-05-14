use crate::runner::runner::RunnerError;
use crate::runner::state::{RunId, Store};

#[test]
fn run_id_parse_padded() {
    let id = RunId::parse("000007").expect("parse padded");
    assert_eq!(id, RunId(7));
}

#[test]
fn run_id_parse_unpadded() {
    let id = RunId::parse("7").expect("parse unpadded");
    assert_eq!(id, RunId(7));
}

#[test]
fn run_id_parse_large() {
    let id = RunId::parse("123456").expect("parse large");
    assert_eq!(id, RunId(123456));
}

#[test]
fn run_id_parse_rejects_empty() {
    match RunId::parse("") {
        Err(RunnerError::InvalidRunId(text)) => assert_eq!(text, ""),
        other => panic!("expected InvalidRunId, got {:?}", other),
    }
}

#[test]
fn run_id_parse_rejects_alphabetic() {
    match RunId::parse("abc") {
        Err(RunnerError::InvalidRunId(text)) => assert_eq!(text, "abc"),
        other => panic!("expected InvalidRunId, got {:?}", other),
    }
}

#[test]
fn run_id_parse_rejects_negative() {
    match RunId::parse("-1") {
        Err(RunnerError::InvalidRunId(text)) => assert_eq!(text, "-1"),
        other => panic!("expected InvalidRunId, got {:?}", other),
    }
}

#[test]
fn run_id_render_pads_to_six() {
    assert_eq!(RunId(7).render(), "000007");
    assert_eq!(RunId(0).render(), "000000");
    assert_eq!(RunId(142).render(), "000142");
    assert_eq!(RunId(15003).render(), "015003");
    assert_eq!(RunId(123456).render(), "123456");
}

#[test]
fn run_id_render_wider_than_six_unpadded() {
    // Six digits is the convention but larger values render naturally.
    assert_eq!(RunId(1_234_567).render(), "1234567");
}

#[test]
fn store_allocate_assigns_monotonic_ids() {
    let base = std::env::temp_dir().join("technique-allocate-monotonic");
    let _ = std::fs::remove_dir_all(&base);

    let store = Store::new(base.clone());
    let (first, _) = store
        .allocate()
        .expect("first");
    let (second, _) = store
        .allocate()
        .expect("second");
    assert_eq!(first, RunId(1));
    assert_eq!(second, RunId(2));

    let _ = std::fs::remove_dir_all(&base);
}

#[test]
fn store_allocate_resumes_from_existing_max() {
    let base = std::env::temp_dir().join("technique-allocate-resume");
    let _ = std::fs::remove_dir_all(&base);
    std::fs::create_dir_all(base.join("000007")).unwrap();

    let store = Store::new(base.clone());
    let (id, _) = store
        .allocate()
        .expect("allocate");
    assert_eq!(id, RunId(8));

    let _ = std::fs::remove_dir_all(&base);
}
