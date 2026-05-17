use std::path::{Path, PathBuf};

use crate::runner::runner::RunnerError;
use crate::runner::state::{
    format_record, parse_manifest, parse_record, Outcome, Record, RunId, Store,
};

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

#[test]
fn manifest_round_trip_through_create_and_open() {
    let base = std::env::temp_dir().join("technique-manifest-roundtrip");
    let _ = std::fs::remove_dir_all(&base);

    let document = PathBuf::from("/somewhere/NetworkProbe.tq");
    let started = "2026-05-14T12:34:56Z".to_string();

    let store = Store::new(base.clone());
    let (id, _, written) = store
        .create(&document, started.clone())
        .expect("create");
    let (read, completed, _) = store
        .open(id)
        .expect("open");

    assert_eq!(written, read);
    assert_eq!(read.document, document);
    assert_eq!(read.started, started);
    assert!(completed.is_empty());

    let _ = std::fs::remove_dir_all(&base);
}

#[test]
fn open_replays_three_result_paths() {
    let base = std::env::temp_dir().join("technique-replay-three");
    let _ = std::fs::remove_dir_all(&base);

    let run_dir = base.join("000001");
    std::fs::create_dir_all(&run_dir).unwrap();
    let mut file = String::new();
    file.push_str("[ document = file:///foo/Test.tq, started = 2026-05-14T12:00:00Z ]\n");
    file.push_str(&format_record(&Record {
        recorded: "2026-05-14T12:00:01Z".to_string(),
        path: "test:1".to_string(),
        outcome: Outcome::Done(None),
    }));
    file.push_str(&format_record(&Record {
        recorded: "2026-05-14T12:00:02Z".to_string(),
        path: "test:2".to_string(),
        outcome: Outcome::Skipped,
    }));
    file.push_str(&format_record(&Record {
        recorded: "2026-05-14T12:00:03Z".to_string(),
        path: "test:3".to_string(),
        outcome: Outcome::Failed(None),
    }));
    std::fs::write(run_dir.join("Test.pfftt"), file).unwrap();

    let store = Store::new(base.clone());
    let (manifest, completed, _) = store
        .open(RunId(1))
        .expect("open");

    assert_eq!(manifest.document, Path::new("/foo/Test.tq"));
    assert_eq!(completed.len(), 3);
    assert!(completed.contains("test:1"));
    assert!(completed.contains("test:2"));
    assert!(completed.contains("test:3"));

    let _ = std::fs::remove_dir_all(&base);
}

#[test]
fn open_missing_run_returns_no_such_run() {
    let base = std::env::temp_dir().join("technique-no-such-run");
    let _ = std::fs::remove_dir_all(&base);
    std::fs::create_dir_all(&base).unwrap();

    let store = Store::new(base.clone());
    match store.open(RunId(42)) {
        Err(RunnerError::NoSuchRun(id)) => assert_eq!(id, RunId(42)),
        other => panic!("expected NoSuchRun, got {:?}", other),
    }

    let _ = std::fs::remove_dir_all(&base);
}

#[test]
fn create_writes_pfftt_file_with_expected_content() {
    let base = std::env::temp_dir().join("technique-create-bytes");
    let _ = std::fs::remove_dir_all(&base);

    let document = PathBuf::from("/somewhere/NetworkProbe.tq");
    let started = "2026-05-14T12:34:56Z".to_string();

    let store = Store::new(base.clone());
    let (_, run_dir, _) = store
        .create(&document, started)
        .expect("create");

    let pfftt = run_dir.join("NetworkProbe.pfftt");
    let on_disk = std::fs::read_to_string(&pfftt).expect("read pfftt");
    assert_eq!(
        on_disk,
        "[ document = file:///somewhere/NetworkProbe.tq, started = 2026-05-14T12:34:56Z ]\n"
    );

    let _ = std::fs::remove_dir_all(&base);
}

#[test]
fn format_record_produces_expected_text() {
    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        path: "make_coffee:2".to_string(),
        outcome: Outcome::Done(None),
    };
    assert_eq!(
        format_record(&record),
        "[ recorded = 2026-05-14T12:00:00Z, path = make_coffee:2, outcome = Done ]\n"
    );
}

#[test]
fn format_record_done_with_result_emits_sibling_field() {
    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        path: "lookup:1".to_string(),
        outcome: Outcome::Done(Some("\"penguin\"".to_string())),
    };
    assert_eq!(
        format_record(&record),
        "[ recorded = 2026-05-14T12:00:00Z, path = lookup:1, outcome = Done, result = \"penguin\" ]\n"
    );
}

#[test]
fn format_record_failed_with_reason_emits_sibling_field() {
    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        path: "ping:1".to_string(),
        outcome: Outcome::Failed(Some("\"network unplugged\"".to_string())),
    };
    assert_eq!(
        format_record(&record),
        "[ recorded = 2026-05-14T12:00:00Z, path = ping:1, outcome = Failed, reason = \"network unplugged\" ]\n"
    );
}

#[test]
fn format_record_skipped_emits_no_sibling() {
    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        path: "wait:3".to_string(),
        outcome: Outcome::Skipped,
    };
    assert_eq!(
        format_record(&record),
        "[ recorded = 2026-05-14T12:00:00Z, path = wait:3, outcome = Skipped ]\n"
    );
}

#[test]
fn format_record_failed_without_reason_emits_no_sibling() {
    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        path: "ping:1".to_string(),
        outcome: Outcome::Failed(None),
    };
    assert_eq!(
        format_record(&record),
        "[ recorded = 2026-05-14T12:00:00Z, path = ping:1, outcome = Failed ]\n"
    );
}

#[test]
fn record_round_trips_through_format_and_parse() {
    let cases = [
        Record {
            recorded: "2026-05-14T12:00:00Z".to_string(),
            path: "a:1".to_string(),
            outcome: Outcome::Done(None),
        },
        Record {
            recorded: "2026-05-14T12:00:01Z".to_string(),
            path: "a:2".to_string(),
            outcome: Outcome::Done(Some("\"penguin\"".to_string())),
        },
        Record {
            recorded: "2026-05-14T12:00:02Z".to_string(),
            path: "a:3".to_string(),
            outcome: Outcome::Skipped,
        },
        Record {
            recorded: "2026-05-14T12:00:03Z".to_string(),
            path: "a:4".to_string(),
            outcome: Outcome::Failed(None),
        },
        Record {
            recorded: "2026-05-14T12:00:04Z".to_string(),
            path: "a:5".to_string(),
            outcome: Outcome::Failed(Some("\"unreachable\"".to_string())),
        },
    ];
    for original in &cases {
        let text = format_record(original);
        let line = text
            .strip_suffix('\n')
            .expect("trailing newline");
        let parsed = parse_record(line).expect("parse");
        assert_eq!(&parsed, original);
    }
}

#[test]
fn parse_manifest_reads_expected_text() {
    let line = "[ document = file:///foo/bar.tq, started = 2026-05-14T01:02:03Z ]";
    let manifest = parse_manifest(line).expect("parse");
    assert_eq!(manifest.document, Path::new("/foo/bar.tq"));
    assert_eq!(manifest.started, "2026-05-14T01:02:03Z");
}

#[test]
fn parse_record_yields_expected_fields() {
    let line = "[ recorded = 2026-05-14T12:00:00Z, path = make_coffee:2, outcome = Done ]";
    let record = parse_record(line).expect("parse");
    assert_eq!(
        record,
        Record {
            recorded: "2026-05-14T12:00:00Z".to_string(),
            path: "make_coffee:2".to_string(),
            outcome: Outcome::Done(None),
        }
    );
}

#[test]
fn parse_record_done_with_result_folds_into_variant() {
    let line = "[ recorded = 2026-05-14T12:00:00Z, path = lookup:1, outcome = Done, result = \"penguin\" ]";
    let record = parse_record(line).expect("parse");
    assert_eq!(
        record.outcome,
        Outcome::Done(Some("\"penguin\"".to_string()))
    );
}

#[test]
fn parse_record_failed_with_reason_folds_into_variant() {
    let line = "[ recorded = 2026-05-14T12:00:00Z, path = ping:1, outcome = Failed, reason = \"network unplugged\" ]";
    let record = parse_record(line).expect("parse");
    assert_eq!(
        record.outcome,
        Outcome::Failed(Some("\"network unplugged\"".to_string()))
    );
}

#[test]
fn parse_record_rejects_unknown_outcome() {
    let line = "[ recorded = 2026-05-14T12:00:00Z, path = x:1, outcome = Maybe ]";
    match parse_record(line) {
        Err(crate::runner::state::RecordError::UnknownOutcome(text)) => {
            assert_eq!(text, "Maybe");
        }
        other => panic!("expected UnknownOutcome, got {:?}", other),
    }
}
