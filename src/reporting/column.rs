//! Rendering of the trail recorded for a completed or interrupted run.

use std::collections::HashMap;

use time::{Date, Month, OffsetDateTime, UtcOffset};

use crate::engraving::{
    InvokeTarget, Record, State, display_path, format_record, format_supplied, parse_run_uri,
    serialize_value,
};
use crate::formatting::{Render, Syntax};

// The short column is held to this width so that the columns after it fall in
// the same place from one run to the next. A path deeper than this is elided;
// the path column, being the name as recorded, is never cut.
const SHORT_WIDTH: usize = 24;

/// The columns shown in the console by default.
pub const CONSOLE_COLUMNS: [Column; 6] = [
    Column::Time,
    Column::Offset,
    Column::Duration,
    Column::Path,
    Column::State,
    Column::Value,
];

/// The fields a PFFTT record line carries, in the order they are written.
/// Fixed: a line missing any of them does not parse back.
pub const PFFTT_COLUMNS: [Column; 5] = [
    Column::Timestamp,
    Column::Run,
    Column::Path,
    Column::State,
    Column::Value,
];

/// The columns shown by default when outputting JSON.
pub const JSON_COLUMNS: [Column; 6] = [
    Column::Timestamp,
    Column::Offset,
    Column::Duration,
    Column::Path,
    Column::State,
    Column::Value,
];

/// Available raw data (such as `timestamp`, fully qualified `path`, and
/// `state`) plus derived fields (such as `offset`, `duration`, and the
/// `short` version of the path).
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Column {
    Timestamp,
    Run,
    Date,
    Time,
    Offset,
    Duration,
    Path,
    Short,
    State,
    Value,
}

impl Column {
    pub fn parse(name: &str) -> Column {
        match name {
            "timestamp" => Column::Timestamp,
            "run" => Column::Run,
            "date" => Column::Date,
            "time" => Column::Time,
            "offset" => Column::Offset,
            "duration" => Column::Duration,
            "path" => Column::Path,
            "short" => Column::Short,
            "state" => Column::State,
            "value" => Column::Value,
            // the command-line parser restricts which names reach us
            _ => unreachable!(),
        }
    }

    /// Field names for JSON.
    pub fn name(&self) -> &'static str {
        match self {
            Column::Timestamp => "timestamp",
            Column::Run => "run",
            Column::Date => "date",
            Column::Time => "time",
            Column::Offset => "offset",
            Column::Duration => "duration",
            Column::Path => "path",
            Column::Short => "short",
            Column::State => "state",
            Column::Value => "value",
        }
    }

    // Elapsed times and other variable width numbers are right-alighned.
    fn aligned_right(&self) -> bool {
        match self {
            Column::Offset | Column::Duration => true,
            _ => false,
        }
    }

    fn fixed(&self) -> Option<usize> {
        match self {
            Column::Short => Some(SHORT_WIDTH),
            _ => None,
        }
    }
}

/// The content of one column before it is embellished for the console,
/// serialized to JSON, etc.
enum Cell {
    Text(String),
    Millis(i64),
    Empty,
}

/// Render a run's records as a column-aligned stream, one line per record.
pub fn render_console(records: &[Record], columns: &[Column], renderer: &dyn Render) -> String {
    let mut text = String::new();

    let first = match records.first() {
        Some(first) => first,
        None => return text,
    };

    let zone = local_offset();
    let (stamps, spans) = measure_times(records);

    render_heading(&mut text, first, zone, renderer);

    let rows: Vec<Vec<(Syntax, String)>> = records
        .iter()
        .enumerate()
        .map(|(i, record)| {
            columns
                .iter()
                .map(|column| {
                    let cell = extract(record, *column, stamps[i], stamps[0], spans[i], zone);
                    (tint(*column, &record.state), shown(cell, *column))
                })
                .collect()
        })
        .collect();

    let widths = measure(&rows, columns);

    for row in &rows {
        let mut line = String::new();
        for (i, (syntax, cell)) in row
            .iter()
            .enumerate()
        {
            let padding = widths[i].saturating_sub(
                cell.chars()
                    .count(),
            );
            if columns[i].aligned_right() {
                pad(&mut line, padding);
            }
            line.push_str(&renderer.style(*syntax, cell));
            if i + 1 < row.len() {
                if !columns[i].aligned_right() {
                    pad(&mut line, padding);
                }
                line.push_str("  ");
            }
        }
        // trim trailing padding if there was no value
        text.push_str(line.trim_end());
        text.push('\n');
    }

    text
}

/// Render a run's records as an array of JSON objects, one per record with a
/// field per column, laid out over lines the way a person would indent it.
/// Elapsed times are given as seconds, and a column a record has nothing for
/// is null.
pub fn render_json(records: &[Record], columns: &[Column]) -> String {
    if records.is_empty() {
        return "[]\n".to_string();
    }

    let zone = local_offset();
    let (stamps, spans) = measure_times(records);

    let objects: Vec<String> = records
        .iter()
        .enumerate()
        .map(|(i, record)| {
            // written field by field rather than through a Map so that the
            // fields come out in the order the columns were asked for
            let fields: Vec<String> = columns
                .iter()
                .map(|column| {
                    let cell = extract(record, *column, stamps[i], stamps[0], spans[i], zone);
                    format!("    \"{}\": {}", column.name(), encode(cell))
                })
                .collect();
            format!("  {{\n{}\n  }}", fields.join(",\n"))
        })
        .collect();

    format!("[\n{}\n]\n", objects.join(",\n"))
}

/// Render records back to PFFTT, through the same writer the runner records
/// with, so the text is the format by construction rather than by imitation.
pub fn render_pfftt(records: &[Record]) -> String {
    records
        .iter()
        .map(format_record)
        .collect()
}

// The line above the stream: `NetworkProbe #000001 started ...`, taking the
// document's name from the URI the opening Start record carries.
fn render_heading(out: &mut String, first: &Record, zone: UtcOffset, renderer: &dyn Render) {
    let label = match &first.state {
        State::Start { uri } => {
            let (document, _) = parse_run_uri(uri);
            document
                .file_stem()
                .map(|stem| {
                    stem.to_string_lossy()
                        .into_owned()
                })
                .unwrap_or_default()
        }
        _ => String::new(),
    };

    let started = parse_timestamp(&first.recorded)
        .and_then(|millis| format_stamp(millis, zone))
        .unwrap_or_else(|| {
            first
                .recorded
                .clone()
        });

    out.push_str(&renderer.style(
        Syntax::Neutral,
        &format!(
            "{} #{}",
            label,
            first
                .run_id
                .render()
        ),
    ));
    out.push_str(&renderer.style(Syntax::Marker, &format!(" started {}", started)));
    out.push_str("\n\n");
}

// What one column holds for one record.
fn extract(
    record: &Record,
    column: Column,
    recorded: Option<i64>,
    start: Option<i64>,
    span: Option<i64>,
    zone: UtcOffset,
) -> Cell {
    match column {
        Column::Timestamp => Cell::Text(
            record
                .recorded
                .clone(),
        ),
        Column::Run => Cell::Text(
            record
                .run_id
                .render(),
        ),
        Column::Date => optional(recorded.and_then(|millis| format_date(millis, zone))),
        Column::Time => optional(recorded.and_then(|millis| format_clock(millis, zone))),
        Column::Offset => match elapsed(start, recorded) {
            Some(millis) => Cell::Millis(millis),
            None => Cell::Empty,
        },
        Column::Duration => match span {
            Some(millis) => Cell::Millis(millis),
            None => Cell::Empty,
        },
        Column::Path => Cell::Text(
            record
                .path
                .clone(),
        ),
        Column::Short => Cell::Text(shortened(&record.path)),
        Column::State => Cell::Text(keyword(&record.state).to_string()),
        Column::Value => optional(payload(&record.state)),
    }
}

// A cell dressed for the console: elapsed times to the precision each column
// carries, and the short path cut to the width it is held at.
fn shown(cell: Cell, column: Column) -> String {
    match cell {
        Cell::Text(text) => match column {
            Column::Short => elide(text, SHORT_WIDTH),
            _ => text,
        },
        // the leading `+` marks the offset as a position in the run rather
        // than a length of time
        Cell::Millis(millis) => match column {
            Column::Offset => {
                let tenths = (millis + 50) / 100;
                format!("+{}.{}", tenths / 10, (tenths % 10).abs())
            }
            _ => format!("{}.{:03}", millis / 1000, (millis % 1000).abs()),
        },
        Cell::Empty => String::new(),
    }
}

// A cell as JSON: elapsed times become seconds, and a cell the record has
// nothing for becomes null.
fn encode(cell: Cell) -> serde_json::Value {
    match cell {
        Cell::Text(text) => serde_json::Value::String(text),
        Cell::Millis(millis) => match serde_json::Number::from_f64(millis as f64 / 1000.0) {
            Some(number) => serde_json::Value::Number(number),
            None => serde_json::Value::Null,
        },
        Cell::Empty => serde_json::Value::Null,
    }
}

fn optional(text: Option<String>) -> Cell {
    match text {
        Some(text) if !text.is_empty() => Cell::Text(text),
        _ => Cell::Empty,
    }
}

// Each record's instant, and on each closing record the span back to its
// opener: Done, Skip, and Fail from the Begin at the same path, Return from
// its Execute, Finish and Stop from Start, and Input from the record before.
fn measure_times(records: &[Record]) -> (Vec<Option<i64>>, Vec<Option<i64>>) {
    let stamps: Vec<Option<i64>> = records
        .iter()
        .map(|record| parse_timestamp(&record.recorded))
        .collect();

    let mut spans = vec![None; records.len()];
    let mut begun: HashMap<&str, usize> = HashMap::new();
    let mut executing: HashMap<&str, usize> = HashMap::new();
    let mut started: Option<usize> = None;

    for (i, record) in records
        .iter()
        .enumerate()
    {
        let path = record
            .path
            .as_str();
        match &record.state {
            State::Start { .. } => {
                started = Some(i);
            }
            State::Begin => {
                begun.insert(path, i);
            }
            State::Execute { .. } => {
                executing.insert(path, i);
            }
            State::Done(_) | State::Skip | State::Fail(_) => {
                if let Some(opened) = begun.remove(path) {
                    spans[i] = elapsed(stamps[opened], stamps[i]);
                }
            }
            State::Return(_) => {
                if let Some(opened) = executing.remove(path) {
                    spans[i] = elapsed(stamps[opened], stamps[i]);
                }
            }
            State::Finish | State::Stop => {
                if let Some(opened) = started {
                    spans[i] = elapsed(stamps[opened], stamps[i]);
                }
            }
            // The inputs a procedure was given close the asking for them: the
            // dispatch recorded on arrival, or the run's Start for the entry
            // procedure's own parameters. Either way the span is the wait for
            // the user to supply them.
            State::Input(_) => {
                if i > 0 {
                    spans[i] = elapsed(stamps[i - 1], stamps[i]);
                }
            }
            State::Resume | State::Invoke(_) => {}
        }
    }

    (stamps, spans)
}

// The abbreviated path the live trace shows, except at the root, which trims
// away to nothing and is shown as the `/` it is recorded as.
fn shortened(path: &str) -> String {
    let text = display_path(path);
    if text.is_empty() {
        return path.to_string();
    }
    text
}

// Hold text to a maximum width, cutting from the front: the tail of a path is
// the step it names, so that is the end to keep.
fn elide(text: String, width: usize) -> String {
    let count = text
        .chars()
        .count();
    if count <= width {
        return text;
    }
    let mut out = String::from("…");
    out.extend(
        text.chars()
            .skip(count - width + 1),
    );
    out
}

// The keyword written into the record.
fn keyword(state: &State) -> &'static str {
    match state {
        State::Start { .. } => "Start",
        State::Finish => "Finish",
        State::Stop => "Stop",
        State::Resume => "Resume",
        State::Invoke(_) => "Invoke",
        State::Execute { .. } => "Execute",
        State::Return(_) => "Return",
        State::Input(_) => "Input",
        State::Begin => "Begin",
        State::Done(_) => "Done",
        State::Skip => "Skip",
        State::Fail(_) => "Fail",
    }
}

// What followed that keyword in the record, so the two can occupy columns of
// their own.
fn payload(state: &State) -> Option<String> {
    match state {
        State::Start { uri } => Some(uri.clone()),
        State::Invoke(InvokeTarget::Procedure(name)) => Some(format!("{}:", name)),
        State::Invoke(InvokeTarget::Uri(uri)) => Some(uri.clone()),
        State::Execute { function } => Some(format!("{}()", function)),
        State::Return(value) | State::Done(value) | State::Fail(value) => value
            .as_ref()
            .map(serialize_value),
        State::Input(supplied) => {
            let mut text = String::new();
            format_supplied(&mut text, supplied);
            Some(text)
        }
        State::Finish | State::Stop | State::Resume | State::Begin | State::Skip => None,
    }
}

// The three states a step settles in are coloured as they are when the run
// was live; everything else is structural.
fn tint(column: Column, state: &State) -> Syntax {
    match column {
        Column::Path | Column::Short | Column::Value => Syntax::Neutral,
        Column::State => match state {
            State::Done(_) => Syntax::Done,
            State::Skip => Syntax::Skip,
            State::Fail(_) => Syntax::Fail,
            _ => Syntax::Marker,
        },
        _ => Syntax::Marker,
    }
}

// Column widths are the widest cell in each, except where the column declares
// a width of its own; the last column is left to run on.
fn measure(rows: &[Vec<(Syntax, String)>], columns: &[Column]) -> Vec<usize> {
    let mut widths = Vec::new();
    for row in rows {
        for (i, (_, cell)) in row
            .iter()
            .enumerate()
        {
            let width = cell
                .chars()
                .count();
            match widths.get_mut(i) {
                Some(existing) => {
                    if width > *existing {
                        *existing = width;
                    }
                }
                None => widths.push(width),
            }
        }
    }
    for (i, column) in columns
        .iter()
        .enumerate()
    {
        if let Some(fixed) = column.fixed() {
            if let Some(width) = widths.get_mut(i) {
                *width = fixed;
            }
        }
    }
    widths
}

fn pad(out: &mut String, count: usize) {
    for _ in 0..count {
        out.push(' ');
    }
}

// Milliseconds between two instants, unknown if either is.
fn elapsed(from: Option<i64>, to: Option<i64>) -> Option<i64> {
    match (from, to) {
        (Some(from), Some(to)) => Some(to - from),
        _ => None,
    }
}

// Milliseconds since the Unix epoch for a PFFTT timestamp of the form
// 2026-08-04T22:50:36.869Z, as written when the record was appended.
fn parse_timestamp(recorded: &str) -> Option<i64> {
    let year = recorded
        .get(0..4)?
        .parse::<i32>()
        .ok()?;
    let month = recorded
        .get(5..7)?
        .parse::<u8>()
        .ok()?;
    let day = recorded
        .get(8..10)?
        .parse::<u8>()
        .ok()?;
    let hour = recorded
        .get(11..13)?
        .parse::<u8>()
        .ok()?;
    let minute = recorded
        .get(14..16)?
        .parse::<u8>()
        .ok()?;
    let second = recorded
        .get(17..19)?
        .parse::<u8>()
        .ok()?;
    let milli = recorded
        .get(20..23)?
        .parse::<u16>()
        .ok()?;

    let date = Date::from_calendar_date(year, Month::try_from(month).ok()?, day).ok()?;
    let moment = date
        .with_hms_milli(hour, minute, second, milli)
        .ok()?
        .assume_utc();
    i64::try_from(moment.unix_timestamp_nanos() / 1_000_000).ok()
}

// The zone times are displayed in, as configured in the environment; UTC if
// the platform will not tell us.
fn local_offset() -> UtcOffset {
    UtcOffset::current_local_offset().unwrap_or(UtcOffset::UTC)
}

fn format_clock(millis: i64, zone: UtcOffset) -> Option<String> {
    let moment = moment(millis, zone)?;
    Some(format!(
        "{:02}:{:02}:{:02}",
        moment.hour(),
        moment.minute(),
        moment.second()
    ))
}

fn format_date(millis: i64, zone: UtcOffset) -> Option<String> {
    let moment = moment(millis, zone)?;
    Some(format!(
        "{:04}-{:02}-{:02}",
        moment.year(),
        u8::from(moment.month()),
        moment.day()
    ))
}

fn format_stamp(millis: i64, zone: UtcOffset) -> Option<String> {
    let moment = moment(millis, zone)?;
    let (hours, minutes, _) = moment
        .offset()
        .as_hms();
    Some(format!(
        "{} {} {}{:02}:{:02}",
        format_date(millis, zone)?,
        format_clock(millis, zone)?,
        if hours < 0 || minutes < 0 { '-' } else { '+' },
        hours.abs(),
        minutes.abs()
    ))
}

fn moment(millis: i64, zone: UtcOffset) -> Option<OffsetDateTime> {
    OffsetDateTime::from_unix_timestamp_nanos(i128::from(millis) * 1_000_000)
        .ok()
        .map(|moment| moment.to_offset(zone))
}
