use std::path::{Path, PathBuf};

use crate::runner::runner::RunnerError;
use crate::runner::state::{
    fail_reason, format_record, parse_record, InvokeTarget, Record, RecordError, RunId, State,
    Store, Value,
};

// A scratch directory under the system temp dir, cleaned up on drop so panics
// in a test do not leak it. Tests construct one per fixture they need.
struct TempDir {
    path: PathBuf,
}

impl TempDir {
    fn new(name: &str) -> Self {
        let path = std::env::temp_dir().join(format!("technique-{}", name));
        let _ = std::fs::remove_dir_all(&path);
        TempDir { path }
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.path);
    }
}

#[test]
fn run_id_parse() {
    let id = RunId::parse("7").expect("parse");
    assert_eq!(id, RunId(7));

    let id = RunId::parse("000007").expect("parse");
    assert_eq!(id, RunId(7));

    let id = RunId::parse("123456").expect("parse");
    assert_eq!(id, RunId(123456));
}

#[test]
fn run_id_parse_rejects_non_decimal() {
    for text in ["", "abc", "-1"] {
        match RunId::parse(text) {
            Err(RunnerError::InvalidRunId(got)) => assert_eq!(got, text),
            other => panic!("expected InvalidRunId for {:?}, got {:?}", text, other),
        }
    }
}

#[test]
fn run_id_render_six_digit_padding() {
    assert_eq!(RunId(0).render(), "000000");
    assert_eq!(RunId(7).render(), "000007");
    assert_eq!(RunId(15003).render(), "015003");
    // Six digits is the convention, but larger values render unpadded.
    assert_eq!(RunId(1_234_567).render(), "1234567");
}

#[test]
fn store_allocate_assigns_monotonic_ids() {
    let dir = TempDir::new("allocate-monotonic");

    let store = Store::new(
        dir.path
            .clone(),
    );
    let (first, _) = store
        .allocate()
        .expect("first");
    let (second, _) = store
        .allocate()
        .expect("second");
    assert_eq!(first, RunId(1));
    assert_eq!(second, RunId(2));
}

#[test]
fn store_allocate_resumes_from_existing_max() {
    let dir = TempDir::new("allocate-resume");
    std::fs::create_dir_all(
        dir.path
            .join("000007"),
    )
    .unwrap();

    let store = Store::new(
        dir.path
            .clone(),
    );
    let (run_id, _) = store
        .allocate()
        .expect("allocate");
    assert_eq!(run_id, RunId(8));
}

#[test]
fn create_writes_start_record_at_head() {
    let dir = TempDir::new("create-start");

    let document = PathBuf::from("/somewhere/NetworkProbe.tq");
    let started = "2026-05-14T12:34:56Z".to_string();

    let store = Store::new(
        dir.path
            .clone(),
    );
    let (run_id, run_dir) = store
        .create(&document, started, &[])
        .expect("create");

    let pfftt = run_dir.join("NetworkProbe.pfftt");
    let on_disk = std::fs::read_to_string(&pfftt).expect("read pfftt");
    assert_eq!(
        on_disk,
        format!(
            "2026-05-14T12:34:56Z {} / Start file:///somewhere/NetworkProbe.tq\n",
            run_id.render()
        )
    );
}

#[test]
fn create_and_open_round_trips_document_path() {
    let dir = TempDir::new("create-open-roundtrip");

    let document = PathBuf::from("/somewhere/NetworkProbe.tq");
    let started = "2026-05-14T12:34:56Z".to_string();

    let store = Store::new(
        dir.path
            .clone(),
    );
    let (run_id, _) = store
        .create(&document, started, &[])
        .expect("create");
    let (read_document, libraries, completed, _) = store
        .open(run_id)
        .expect("open");

    assert_eq!(read_document, document);
    assert!(libraries.is_empty());
    assert!(completed.is_empty());
}

#[test]
fn create_and_open_round_trips_libraries() {
    let dir = TempDir::new("create-open-libraries");

    let document = PathBuf::from("/somewhere/NetworkProbe.tq");
    let started = "2026-05-14T12:34:56Z".to_string();
    let selected = vec!["system".to_string(), "browser".to_string()];

    let store = Store::new(
        dir.path
            .clone(),
    );
    let (run_id, _) = store
        .create(&document, started, &selected)
        .expect("create");
    let (read_document, libraries, _, _) = store
        .open(run_id)
        .expect("open");

    assert_eq!(read_document, document);
    assert_eq!(libraries, selected);
}

#[test]
fn open_replays_done_skip_fail_into_completed() {
    let dir = TempDir::new("replay-three");

    let run_dir = dir
        .path
        .join("000001");
    std::fs::create_dir_all(&run_dir).unwrap();
    let mut file = String::new();
    file.push_str(&format_record(&Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        run_id: RunId(1),
        path: "/".to_string(),
        state: State::Start {
            uri: "file:///foo/Test.tq".to_string(),
        },
    }));
    file.push_str(&format_record(&Record {
        recorded: "2026-05-14T12:00:01Z".to_string(),
        run_id: RunId(1),
        path: "/test:1".to_string(),
        state: State::Done(None),
    }));
    file.push_str(&format_record(&Record {
        recorded: "2026-05-14T12:00:02Z".to_string(),
        run_id: RunId(1),
        path: "/test:2".to_string(),
        state: State::Skip,
    }));
    file.push_str(&format_record(&Record {
        recorded: "2026-05-14T12:00:03Z".to_string(),
        run_id: RunId(1),
        path: "/test:3".to_string(),
        state: State::Fail(None),
    }));
    std::fs::write(run_dir.join("Test.pfftt"), file).unwrap();

    let store = Store::new(
        dir.path
            .clone(),
    );
    let (document, _, completed, _) = store
        .open(RunId(1))
        .expect("open");

    assert_eq!(document, Path::new("/foo/Test.tq"));
    assert_eq!(completed.len(), 3);
    assert!(completed.contains("/test:1"));
    assert!(completed.contains("/test:2"));
    assert!(completed.contains("/test:3"));
}

// Resume and Begin records in the middle of the file are lifecycle
// events, not step completions — they must not show up in the replayed
// `completed` set.
#[test]
fn open_skips_resume_and_begin_during_replay() {
    let dir = TempDir::new("replay-skip-lifecycle");

    let run_dir = dir
        .path
        .join("000001");
    std::fs::create_dir_all(&run_dir).unwrap();
    let mut file = String::new();
    file.push_str(&format_record(&Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        run_id: RunId(1),
        path: "/".to_string(),
        state: State::Start {
            uri: "file:///foo/Test.tq".to_string(),
        },
    }));
    file.push_str(&format_record(&Record {
        recorded: "2026-05-14T12:00:01Z".to_string(),
        run_id: RunId(1),
        path: "/test:1".to_string(),
        state: State::Begin,
    }));
    file.push_str(&format_record(&Record {
        recorded: "2026-05-14T12:00:02Z".to_string(),
        run_id: RunId(1),
        path: "/test:1".to_string(),
        state: State::Done(Some(Value::Unit)),
    }));
    file.push_str(&format_record(&Record {
        recorded: "2026-05-14T12:00:03Z".to_string(),
        run_id: RunId(1),
        path: "/".to_string(),
        state: State::Resume,
    }));
    std::fs::write(run_dir.join("Test.pfftt"), file).unwrap();

    let store = Store::new(
        dir.path
            .clone(),
    );
    let (_, _, completed, _) = store
        .open(RunId(1))
        .expect("open");

    assert_eq!(completed.len(), 1);
    assert!(completed.contains("/test:1"));
}

#[test]
fn open_missing_run_returns_no_such_run() {
    let dir = TempDir::new("no-such-run");
    std::fs::create_dir_all(&dir.path).unwrap();

    let store = Store::new(
        dir.path
            .clone(),
    );
    match store.open(RunId(42)) {
        Err(RunnerError::NoSuchRun(run_id)) => assert_eq!(run_id, RunId(42)),
        other => panic!("expected NoSuchRun, got {:?}", other),
    }
}

// Each variant of `State` produces a distinct line shape. This pins exact
// bytes; the round-trip test below confirms parse compatibility.
#[test]
fn format_record_pins_on_disk_text() {
    let record = Record {
        recorded: "2026-05-16T12:50:30Z".to_string(),
        run_id: RunId(15003),
        path: "/".to_string(),
        state: State::Start {
            uri: "file:///home/user/NetworkProbe.tq".to_string(),
        },
    };
    assert_eq!(
        format_record(&record),
        "2026-05-16T12:50:30Z 015003 / Start file:///home/user/NetworkProbe.tq\n"
    );

    let record = Record {
        recorded: "2026-05-17T00:28:25Z".to_string(),
        run_id: RunId(15003),
        path: "/".to_string(),
        state: State::Resume,
    };
    assert_eq!(
        format_record(&record),
        "2026-05-17T00:28:25Z 015003 / Resume\n"
    );

    let record = Record {
        recorded: "2026-05-17T00:28:30Z".to_string(),
        run_id: RunId(15003),
        path: "/".to_string(),
        state: State::Stop,
    };
    assert_eq!(
        format_record(&record),
        "2026-05-17T00:28:30Z 015003 / Stop\n"
    );

    let record = Record {
        recorded: "2026-05-17T00:28:30Z".to_string(),
        run_id: RunId(15003),
        path: "/local_network:2".to_string(),
        state: State::Begin,
    };
    assert_eq!(
        format_record(&record),
        "2026-05-17T00:28:30Z 015003 /local_network:2 Begin\n"
    );

    let record = Record {
        recorded: "2026-05-16T12:50:42Z".to_string(),
        run_id: RunId(15003),
        path: "/local_network:1".to_string(),
        state: State::Execute {
            function: "exec".to_string(),
        },
    };
    assert_eq!(
        format_record(&record),
        "2026-05-16T12:50:42Z 015003 /local_network:1 Execute exec()\n"
    );

    let record = Record {
        recorded: "2026-05-17T00:31:30Z".to_string(),
        run_id: RunId(15003),
        path: "/local_network:5".to_string(),
        state: State::Invoke(InvokeTarget::Procedure("probe_border_router".to_string())),
    };
    assert_eq!(
        format_record(&record),
        "2026-05-17T00:31:30Z 015003 /local_network:5 Invoke probe_border_router:\n"
    );

    let record = Record {
        recorded: "2026-05-17T00:31:30Z".to_string(),
        run_id: RunId(15003),
        path: "/local_network:5".to_string(),
        state: State::Invoke(InvokeTarget::Uri("file:///tmp/Other.tq".to_string())),
    };
    assert_eq!(
        format_record(&record),
        "2026-05-17T00:31:30Z 015003 /local_network:5 Invoke file:///tmp/Other.tq\n"
    );

    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        run_id: RunId(1),
        path: "/make_coffee:2".to_string(),
        state: State::Done(None),
    };
    assert_eq!(
        format_record(&record),
        "2026-05-14T12:00:00Z 000001 /make_coffee:2 Done\n"
    );

    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        run_id: RunId(1),
        path: "/make_coffee:2".to_string(),
        state: State::Done(Some(Value::Unit)),
    };
    assert_eq!(
        format_record(&record),
        "2026-05-14T12:00:00Z 000001 /make_coffee:2 Done ()\n"
    );

    let record = Record {
        recorded: "2026-05-17T00:29:15Z".to_string(),
        run_id: RunId(15003),
        path: "/local_network:3".to_string(),
        state: State::Done(Some(Value::Tablet(
            "[ address = \"192.168.1.1\" ]".to_string(),
        ))),
    };
    assert_eq!(
        format_record(&record),
        "2026-05-17T00:29:15Z 015003 /local_network:3 Done [ address = \"192.168.1.1\" ]\n"
    );

    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        run_id: RunId(1),
        path: "/before_anesthesia:2".to_string(),
        state: State::Done(Some(Value::Literal("Not Applicable".to_string()))),
    };
    assert_eq!(
        format_record(&record),
        "2026-05-14T12:00:00Z 000001 /before_anesthesia:2 Done \"Not Applicable\"\n"
    );

    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        run_id: RunId(1),
        path: "/make_coffee:2".to_string(),
        state: State::Skip,
    };
    assert_eq!(
        format_record(&record),
        "2026-05-14T12:00:00Z 000001 /make_coffee:2 Skip\n"
    );

    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        run_id: RunId(1),
        path: "/make_coffee:2".to_string(),
        state: State::Fail(None),
    };
    assert_eq!(
        format_record(&record),
        "2026-05-14T12:00:00Z 000001 /make_coffee:2 Fail\n"
    );

    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        run_id: RunId(1),
        path: "/make_coffee:2".to_string(),
        state: State::Fail(Some(Value::Tablet(
            "[ reason = \"network unplugged\" ]".to_string(),
        ))),
    };
    assert_eq!(
        format_record(&record),
        "2026-05-14T12:00:00Z 000001 /make_coffee:2 Fail [ reason = \"network unplugged\" ]\n"
    );
}

#[test]
fn fail_reason_escapes_and_stays_on_one_line() {
    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        run_id: RunId(1),
        path: "/make_coffee:2".to_string(),
        state: State::Fail(Some(fail_reason("said \"unplug\"\nthen left"))),
    };
    let line = format_record(&record);
    assert_eq!(
        line,
        "2026-05-14T12:00:00Z 000001 /make_coffee:2 Fail [ reason = \"said \\\"unplug\\\"\\nthen left\" ]\n"
    );
    assert_eq!(
        line.matches('\n')
            .count(),
        1
    );
    assert_eq!(parse_record(&line).unwrap(), record);
}

#[test]
fn record_round_trips_through_format_and_parse() {
    let cases = vec![
        Record {
            recorded: "2026-05-16T12:50:30Z".to_string(),
            run_id: RunId(1),
            path: "/".to_string(),
            state: State::Start {
                uri: "file:///foo/Bar.tq".to_string(),
            },
        },
        Record {
            recorded: "2026-05-17T00:28:25Z".to_string(),
            run_id: RunId(1),
            path: "/".to_string(),
            state: State::Stop,
        },
        Record {
            recorded: "2026-05-17T00:28:25Z".to_string(),
            run_id: RunId(15003),
            path: "/".to_string(),
            state: State::Resume,
        },
        Record {
            recorded: "2026-05-14T12:00:00Z".to_string(),
            run_id: RunId(1),
            path: "/a:1".to_string(),
            state: State::Begin,
        },
        Record {
            recorded: "2026-05-14T12:00:00Z".to_string(),
            run_id: RunId(1),
            path: "/a:1".to_string(),
            state: State::Execute {
                function: "exec".to_string(),
            },
        },
        Record {
            recorded: "2026-05-14T12:00:00Z".to_string(),
            run_id: RunId(1),
            path: "/a:1".to_string(),
            state: State::Invoke(InvokeTarget::Procedure("helper".to_string())),
        },
        Record {
            recorded: "2026-05-14T12:00:00Z".to_string(),
            run_id: RunId(1),
            path: "/a:1".to_string(),
            state: State::Invoke(InvokeTarget::Uri("https://proc.ac/foo/Bar.tq".to_string())),
        },
        Record {
            recorded: "2026-05-14T12:00:00Z".to_string(),
            run_id: RunId(1),
            path: "/a:1".to_string(),
            state: State::Done(None),
        },
        Record {
            recorded: "2026-05-14T12:00:01Z".to_string(),
            run_id: RunId(1),
            path: "/a:2".to_string(),
            state: State::Done(Some(Value::Unit)),
        },
        Record {
            recorded: "2026-05-14T12:00:02Z".to_string(),
            run_id: RunId(1),
            path: "/a:3".to_string(),
            state: State::Done(Some(Value::Tablet(
                "[ address = \"10.0.0.1\" ]".to_string(),
            ))),
        },
        Record {
            recorded: "2026-05-14T12:00:02Z".to_string(),
            run_id: RunId(1),
            path: "/a:7".to_string(),
            state: State::Done(Some(Value::Literal("Not Applicable".to_string()))),
        },
        Record {
            recorded: "2026-05-14T12:00:02Z".to_string(),
            run_id: RunId(1),
            path: "/a:8".to_string(),
            state: State::Done(Some(Value::Literal(
                "1: lo\n    inet 127.0.0.1/8\na quote \" and a slash \\".to_string(),
            ))),
        },
        Record {
            recorded: "2026-05-14T12:00:03Z".to_string(),
            run_id: RunId(1),
            path: "/".to_string(),
            state: State::Stop,
        },
        Record {
            recorded: "2026-05-14T12:00:03Z".to_string(),
            run_id: RunId(1),
            path: "/a:4".to_string(),
            state: State::Skip,
        },
        Record {
            recorded: "2026-05-14T12:00:04Z".to_string(),
            run_id: RunId(1),
            path: "/a:5".to_string(),
            state: State::Fail(None),
        },
        Record {
            recorded: "2026-05-14T12:00:05Z".to_string(),
            run_id: RunId(1),
            path: "/a:6".to_string(),
            state: State::Fail(Some(Value::Tablet(
                "[ reason = \"unreachable\" ]".to_string(),
            ))),
        },
    ];

    for original in cases {
        let text = format_record(&original);
        let line = text
            .strip_suffix('\n')
            .expect("trailing newline");
        assert_eq!(parse_record(line).expect("parse"), original);
    }
}

#[test]
fn multiline_literal_stays_on_one_record_line() {
    // A multi-line value (e.g. captured exec output) must serialize to a
    // single record line so the line-oriented store can read it back.
    let record = Record {
        recorded: "2026-05-14T12:00:00Z".to_string(),
        run_id: RunId(1),
        path: "/a:1".to_string(),
        state: State::Done(Some(Value::Literal("first\nsecond\nthird".to_string()))),
    };
    let text = format_record(&record);
    assert_eq!(
        text.matches('\n')
            .count(),
        1
    );
    assert!(text.contains("\\n"));
}

#[test]
fn parse_record_rejects_unknown_state() {
    let line = "2026-05-14T12:00:00Z 000001 /x:1 Maybe";
    match parse_record(line) {
        Err(RecordError::UnknownState(text)) => assert_eq!(text, "Maybe"),
        other => panic!("expected UnknownState, got {:?}", other),
    }
}

#[test]
fn parse_record_rejects_too_few_fields() {
    // Missing state keyword.
    let line = "2026-05-14T12:00:00Z 000001 /x:1";
    match parse_record(line) {
        Err(RecordError::MalformedRecord) => {}
        other => panic!("expected MalformedRecord, got {:?}", other),
    }

    // Empty line.
    let line = "";
    match parse_record(line) {
        Err(RecordError::MalformedRecord) => {}
        other => panic!("expected MalformedRecord, got {:?}", other),
    }
}

#[test]
fn parse_record_rejects_non_decimal_run() {
    let line = "2026-05-14T12:00:00Z abc /x:1 Done";
    match parse_record(line) {
        Err(RecordError::MalformedRecord) => {}
        other => panic!("expected MalformedRecord, got {:?}", other),
    }
}

// `StartMissing` covers two setups: a run directory containing an empty
// PFFTT file (file present, no Start record inside), and a run directory
// with no PFFTT file at all.
#[test]
fn open_missing_start_record() {
    let dir = TempDir::new("empty-pfftt");
    let run_dir = dir
        .path
        .join("000001");
    std::fs::create_dir_all(&run_dir).unwrap();
    std::fs::write(run_dir.join("Test.pfftt"), "").unwrap();

    let store = Store::new(
        dir.path
            .clone(),
    );
    match store.open(RunId(1)) {
        Err(RunnerError::StartMissing(run_id)) => assert_eq!(run_id, RunId(1)),
        other => panic!("expected StartMissing, got {:?}", other),
    }

    let dir = TempDir::new("no-pfftt");
    std::fs::create_dir_all(
        dir.path
            .join("000001"),
    )
    .unwrap();

    let store = Store::new(
        dir.path
            .clone(),
    );
    match store.open(RunId(1)) {
        Err(RunnerError::StartMissing(run_id)) => assert_eq!(run_id, RunId(1)),
        other => panic!("expected StartMissing, got {:?}", other),
    }
}
