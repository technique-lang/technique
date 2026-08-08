use crate::engraving::{InvokeTarget, Record, RunId, State, Supplied};
use crate::formatting::Identity;
use crate::reporting::{Column, render_console, render_json};
use crate::value::Value;

fn record(recorded: &str, path: &str, state: State) -> Record {
    Record {
        recorded: recorded.to_string(),
        run_id: RunId(7),
        path: path.to_string(),
        state,
    }
}

fn trail() -> Vec<Record> {
    vec![
        record(
            "2026-08-04T22:50:36.869Z",
            "/",
            State::Start {
                uri: "file:///tmp/NetworkProbe.tq".to_string(),
            },
        ),
        record(
            "2026-08-04T22:50:36.870Z",
            "/connectivity_check:",
            State::Begin,
        ),
        record(
            "2026-08-04T22:50:40.546Z",
            "/connectivity_check:",
            State::Done(Some(Value::Unitus)),
        ),
        record("2026-08-04T22:50:40.546Z", "/", State::Finish),
    ]
}

// The heading carries the run and the document; the wall clock time it goes on
// to give is in the local zone, so only its lead-in is pinned here.
#[test]
fn heading_names_the_run() {
    let text = render_console(&trail(), &[Column::Short], &Identity);
    let heading = text
        .lines()
        .next()
        .unwrap();

    assert!(heading.starts_with("NetworkProbe #000007 started "));
}

#[test]
fn columns_align_in_the_order_given() {
    let columns = [Column::Short, Column::Offset, Column::State, Column::Value];
    let text = render_console(&trail(), &columns, &Identity);
    let body: Vec<&str> = text
        .lines()
        .skip(2)
        .collect();

    assert_eq!(
        body[0],
        "/                         +0.0  Start   file:///tmp/NetworkProbe.tq"
    );
    assert_eq!(body[1], "connectivity_check:       +0.0  Begin");
    assert_eq!(body[2], "connectivity_check:       +3.7  Done    ()");
    assert_eq!(body[3], "/                         +3.7  Finish");
}

// The short column keeps a fixed width whatever the run contains, so the
// columns beyond it stand in the same place from one run to the next. A step
// named more deeply than that is cut at the front, keeping the tail.
#[test]
fn deep_paths_are_elided_at_the_front() {
    let records = vec![
        record(
            "2026-08-04T22:50:36.869Z",
            "/",
            State::Start {
                uri: "file:///tmp/NetworkProbe.tq".to_string(),
            },
        ),
        record(
            "2026-08-04T22:50:36.870Z",
            "/connectivity_check:/VII/service_endpoint:/7/a",
            State::Skip,
        ),
    ];
    let columns = [Column::Short, Column::State];
    let text = render_console(&records, &columns, &Identity);
    let body: Vec<&str> = text
        .lines()
        .skip(2)
        .collect();

    assert_eq!(body[0], "/                         Start");
    assert_eq!(body[1], "…I/service_endpoint:/7/a  Skip");
}

// A duration is how long a scope stayed open, and it is shown on the record
// that closed it, beside the verdict: the Done paired with its Begin, the
// Finish with the run's Start. The opening records carry none of their own.
#[test]
fn durations_fall_on_the_record_that_closed_the_scope() {
    let columns = [Column::Duration, Column::Short, Column::State];
    let text = render_console(&trail(), &columns, &Identity);
    let body: Vec<&str> = text
        .lines()
        .skip(2)
        .collect();

    assert_eq!(body[0], "       /                         Start");
    assert_eq!(body[1], "       connectivity_check:       Begin");
    assert_eq!(body[2], "3.676  connectivity_check:       Done");
    assert_eq!(body[3], "3.677  /                         Finish");
}

// The inputs a procedure was given close the asking for them, so an
// invocation that had to wait on the user shows what that wait cost.
#[test]
fn inputs_carry_the_wait_to_supply_them() {
    let records = vec![
        record(
            "2026-08-04T22:50:36.869Z",
            "/",
            State::Start {
                uri: "file:///tmp/NetworkProbe.tq".to_string(),
            },
        ),
        record(
            "2026-08-04T22:50:36.869Z",
            "/decomission_customer:/I",
            State::Invoke(InvokeTarget::Procedure("delete_resources".to_string())),
        ),
        record(
            "2026-08-04T22:50:45.615Z",
            "/decomission_customer:/I/delete_resources:",
            State::Input(vec![Supplied {
                value: Value::Literali("Rebecca".to_string()),
                name: Some("authority".to_string()),
            }]),
        ),
        record(
            "2026-08-04T22:50:45.615Z",
            "/decomission_customer:/I/delete_resources:",
            State::Begin,
        ),
    ];
    let columns = [Column::Duration, Column::State];
    let text = render_console(&records, &columns, &Identity);
    let body: Vec<&str> = text
        .lines()
        .skip(2)
        .collect();

    assert_eq!(body[1], "       Invoke");
    assert_eq!(body[2], "8.746  Input");
}

// An Execute is closed by the Return that carries what the host call produced,
// so the Execute is where how long the call took is shown.
#[test]
fn executions_are_spanned_by_their_return() {
    let records = vec![
        record(
            "2026-08-04T22:50:36.869Z",
            "/",
            State::Start {
                uri: "file:///tmp/NetworkProbe.tq".to_string(),
            },
        ),
        record(
            "2026-08-04T22:50:36.870Z",
            "/connectivity_check:/1",
            State::Execute {
                function: "exec".to_string(),
            },
        ),
        record(
            "2026-08-04T22:50:40.546Z",
            "/connectivity_check:/1",
            State::Return(Some(Value::Literali("done".to_string()))),
        ),
    ];
    let columns = [Column::Short, Column::Duration, Column::State];
    let text = render_console(&records, &columns, &Identity);
    let body: Vec<&str> = text
        .lines()
        .skip(2)
        .collect();

    assert_eq!(body[1], "connectivity_check:/1            Execute");
    assert_eq!(body[2], "connectivity_check:/1     3.676  Return");
}

// The fields come out in the order the columns were asked for, elapsed times
// as seconds, and a record with nothing for a column gets null.
#[test]
fn json_carries_one_field_per_column() {
    let columns = [Column::Short, Column::Duration, Column::State];
    let text = render_json(&trail(), &columns);
    let body: Vec<&str> = text
        .lines()
        .collect();

    assert_eq!(body[0], "[");
    assert_eq!(body[1], "  {");
    assert_eq!(body[2], "    \"short\": \"/\",");
    assert_eq!(body[3], "    \"duration\": null,");
    assert_eq!(body[4], "    \"state\": \"Start\"");
    assert_eq!(body[5], "  },");
    assert_eq!(body[13], "    \"duration\": 3.676,");
    assert_eq!(body[21], "]");
}

// The recorded forms, kept whole: the UTC instant the run wrote, and the path
// as it appears in the file rather than the abbreviation the runner shows.
#[test]
fn timestamp_and_path_are_as_recorded() {
    let columns = [Column::Timestamp, Column::Path, Column::State];
    let text = render_console(&trail(), &columns, &Identity);
    let body: Vec<&str> = text
        .lines()
        .skip(2)
        .collect();

    assert_eq!(
        body[0],
        "2026-08-04T22:50:36.869Z  /                     Start"
    );
    assert_eq!(
        body[1],
        "2026-08-04T22:50:36.870Z  /connectivity_check:  Begin"
    );
}

// The keyword and the payload that followed it in the record occupy columns of
// their own, so a run can be read for its shape or for its values.
#[test]
fn state_and_value_are_separate_columns() {
    let records = vec![
        record(
            "2026-08-04T22:50:36.869Z",
            "/",
            State::Start {
                uri: "file:///tmp/NetworkProbe.tq".to_string(),
            },
        ),
        record(
            "2026-08-04T22:50:36.870Z",
            "/connectivity_check:",
            State::Input(vec![Supplied {
                value: Value::Quanticle(crate::value::Numeric::Integral(0)),
                name: Some("s".to_string()),
            }]),
        ),
        record(
            "2026-08-04T22:50:36.871Z",
            "/connectivity_check:/1",
            State::Execute {
                function: "exec".to_string(),
            },
        ),
        record(
            "2026-08-04T22:50:36.872Z",
            "/connectivity_check:/1",
            State::Fail(Some(Value::Literali("unreachable".to_string()))),
        ),
    ];
    let columns = [Column::State, Column::Value];
    let text = render_console(&records, &columns, &Identity);
    let body: Vec<&str> = text
        .lines()
        .skip(2)
        .collect();

    assert_eq!(body[0], "Start    file:///tmp/NetworkProbe.tq");
    assert_eq!(body[1], "Input    ( 0 ~ s )");
    assert_eq!(body[2], "Execute  exec()");
    assert_eq!(body[3], "Fail     \"unreachable\"");
}

#[test]
fn empty_trail_renders_nothing() {
    assert_eq!(render_console(&[], &[Column::Short], &Identity), "");
}
