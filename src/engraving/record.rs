//! The PFFTT record: the events a run writes, and the codec that puts
//! them on a line and reads them back.

use crate::value;

use super::StoreError;

/// Monotonic identifier for a run. Conventionally rendered and stored as a
/// six-digit zero-padded string.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct RunId(pub u32);

impl RunId {
    /// Parse a run identifier. Both unpadded (`7`) and zero-padded
    /// (`000007`) decimal forms are accepted.
    pub fn parse(text: &str) -> Result<RunId, StoreError> {
        text.parse::<u32>()
            .map(RunId)
            .map_err(|_| StoreError::InvalidRunId(text.to_string()))
    }

    /// Render as a six-digit zero-padded decimal string.
    pub fn render(self) -> String {
        format!("{:06}", self.0)
    }
}

/// Errors raised if a PFFTT file is malformed or invalid.
#[derive(Debug, Eq, PartialEq)]
pub enum RecordError {
    MalformedRecord,
    MalformedState,
    UnknownState(String),
}

/// One record line on disk.
#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub recorded: String,
    pub run_id: RunId,
    pub path: String,
    pub state: State,
}

/// A lifecycle or step-outcome event; the keyword written into each PFFTT
/// record line. `Start`, `Finish`, `Stop`, and `Resume` are run-lifecycle
/// events emitted at the root path `/`; `Begin` marks entry into a step or
/// scope (paired with the eventual `Done`, `Skip`, or `Fail` at the same path).
/// `Finish` closes a run that walked to its end; `Stop` records a deliberate
/// quit — the run stays resumable, and the record distinguishes the quit from a
/// crash (which records nothing) and from a `Finish`.
/// `Invoke` records dispatch into another procedure (the return is
/// implicit — the next event's path reveals the resumed procedure).
/// `Execute` and `Return` bracket an effectful host call (a `Command` or
/// `Action`) with the value it returned; Pure builtins are not recorded.
/// `Input` records the values supplied to a procedure so a resume can restore
/// the state without re-prompting for information already entered.
#[derive(Debug, Clone, PartialEq)]
pub enum State {
    Start { uri: String },
    Finish,
    Stop,
    Resume,
    Invoke(InvokeTarget),
    Execute { function: String },
    Return(Option<value::Value>),
    Input(Vec<Supplied>),
    Begin,
    Done(Option<value::Value>),
    Skip,
    Fail(Option<value::Value>),
}

/// One value supplied to a procedure's parameter: bound to a named parameter
/// (recorded as `value ~ name`), or positional when the parameter is unnamed
/// (recorded as a bare `value`).
#[derive(Debug, Clone, PartialEq)]
pub struct Supplied {
    pub value: value::Value,
    pub name: Option<String>,
}

/// The target of an `Invoke`: either a named procedure (rendered as
/// `name:`) or a URI to an external technique.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum InvokeTarget {
    Procedure(String),
    Uri(String),
}

/// Trim a rendered PFFTT path to the live-prompt form: drop the leading `/`,
/// and the entry procedure's `name:` head when a section immediately follows.
pub fn display_path(qualified: &str) -> String {
    let body = qualified
        .strip_prefix('/')
        .unwrap_or(qualified);
    let mut parts: Vec<&str> = body
        .split('/')
        .collect();
    if parts.len() >= 2 && parts[0].ends_with(':') && is_section_component(parts[1]) {
        parts.remove(0);
    }
    parts.join("/")
}

/// Leading token, not the whole component: an acquire label can glue an
/// ` <invocation>` annotation after the numeral.
fn is_section_component(part: &str) -> bool {
    let token = part
        .split_whitespace()
        .next()
        .unwrap_or("");
    !token.is_empty()
        && token
            .bytes()
            .all(|b| b"IVXLCDM".contains(&b))
}

// Serialize a Record in PFFTT line form. The format is:
// Timestamp RunId Path (State Value) followed by a newline.
pub(crate) fn format_record(record: &Record) -> String {
    let mut text = String::new();
    text.push_str(&record.recorded);
    text.push(' ');
    text.push_str(
        &record
            .run_id
            .render(),
    );
    text.push(' ');
    text.push_str(&record.path);
    text.push(' ');
    format_state(&mut text, &record.state);
    text.push('\n');
    text
}

fn format_state(out: &mut String, state: &State) {
    match state {
        State::Start { uri } => {
            out.push_str("Start ");
            out.push_str(uri);
        }
        State::Finish => out.push_str("Finish"),
        State::Stop => out.push_str("Stop"),
        State::Resume => out.push_str("Resume"),
        State::Invoke(target) => {
            out.push_str("Invoke ");
            match target {
                InvokeTarget::Procedure(name) => {
                    out.push_str(name);
                    out.push(':');
                }
                InvokeTarget::Uri(uri) => out.push_str(uri),
            }
        }
        State::Execute { function } => {
            out.push_str("Execute ");
            out.push_str(function);
            out.push_str("()");
        }
        State::Return(value) => {
            out.push_str("Return");
            if let Some(v) = value {
                out.push(' ');
                out.push_str(&serialize_value(v));
            }
        }
        State::Input(supplied) => {
            out.push_str("Input ");
            format_supplied(out, supplied);
        }
        State::Begin => out.push_str("Begin"),
        State::Done(value) => {
            out.push_str("Done");
            if let Some(v) = value {
                out.push(' ');
                out.push_str(&serialize_value(v));
            }
        }
        State::Skip => out.push_str("Skip"),
        State::Fail(value) => {
            out.push_str("Fail");
            if let Some(v) = value {
                out.push(' ');
                out.push_str(&serialize_value(v));
            }
        }
    }
}

// Format a procedure's supplied inputs as `( value ~ name, value, … )`: each
// value serialized by the value codec, a named parameter followed by `~ name`,
// an unnamed one left bare.
pub(crate) fn format_supplied(out: &mut String, supplied: &[Supplied]) {
    out.push('(');
    for (i, item) in supplied
        .iter()
        .enumerate()
    {
        if i > 0 {
            out.push(',');
        }
        out.push(' ');
        out.push_str(&serialize_value(&item.value));
        if let Some(name) = &item.name {
            out.push_str(" ~ ");
            out.push_str(name);
        }
    }
    out.push_str(" )");
}

// Reverse `format_supplied`: parse `( value ~ name, value, … )` into the
// supplied inputs. Each top-level item is a codec value optionally followed by
// a top-level ` ~ ` and the parameter name.
fn parse_supplied(text: &str) -> Result<Vec<Supplied>, RecordError> {
    let inner = text
        .trim()
        .strip_prefix('(')
        .and_then(|t| t.strip_suffix(')'))
        .ok_or(RecordError::MalformedState)?
        .trim();
    if inner.is_empty() {
        return Ok(Vec::new());
    }
    let mut out = Vec::new();
    for part in split_top_level(inner, ',')? {
        let part = part.trim();
        let supplied = match split_once_top_level_tilde(part) {
            Some((value, name)) => Supplied {
                value: deserialize_value(value.trim())?,
                name: Some(
                    name.trim()
                        .to_string(),
                ),
            },
            None => Supplied {
                value: deserialize_value(part)?,
                name: None,
            },
        };
        out.push(supplied);
    }
    Ok(out)
}

// Escape a literal so it occupies a single record line: backslash and quote
// are protected, and newlines/carriage returns become `\n` / `\r` so an
// embedded multi-line value (e.g. captured exec output) survives the
// line-oriented PFFTT format.
fn escape_literal(out: &mut String, text: &str) {
    for c in text.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            _ => out.push(c),
        }
    }
}

// Reverse `escape_literal`. An unknown escape (or a trailing backslash) is a
// malformed record.
pub(crate) fn unescape_literal(text: &str) -> Result<String, RecordError> {
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('\\') => out.push('\\'),
                Some('"') => out.push('"'),
                Some('n') => out.push('\n'),
                Some('r') => out.push('\r'),
                _ => return Err(RecordError::MalformedState),
            }
        } else {
            out.push(c);
        }
    }
    Ok(out)
}

// Build the `Value` recorded for a failed step: a single-entry tablet
// `[ "reason" = "<reason>" ]` carrying the user's free-text reason.
pub(crate) fn fail_reason(reason: &str) -> value::Value {
    value::Value::Tabularum(vec![(
        "reason".to_string(),
        value::Value::Literali(reason.to_string()),
    )])
}

/// Parse the lines of a PFFTT file into records, blank lines passed over.
pub fn parse_records(content: &str) -> Result<Vec<Record>, RecordError> {
    content
        .lines()
        .filter(|line| {
            !line
                .trim()
                .is_empty()
        })
        .map(parse_record)
        .collect()
}

// Parse a single PFFTT record line into a Record.
pub(crate) fn parse_record(line: &str) -> Result<Record, RecordError> {
    let line = line.trim_end_matches(['\r', '\n']);
    let mut parts = line.splitn(4, ' ');
    let recorded = parts
        .next()
        .ok_or(RecordError::MalformedRecord)?;
    let run_text = parts
        .next()
        .ok_or(RecordError::MalformedRecord)?;
    let path = parts
        .next()
        .ok_or(RecordError::MalformedRecord)?;
    let rest = parts
        .next()
        .ok_or(RecordError::MalformedRecord)?;
    if recorded.is_empty() || run_text.is_empty() || path.is_empty() || rest.is_empty() {
        return Err(RecordError::MalformedRecord);
    }
    let run_id = run_text
        .parse::<u32>()
        .map(RunId)
        .map_err(|_| RecordError::MalformedRecord)?;
    let state = parse_state(rest)?;
    Ok(Record {
        recorded: recorded.to_string(),
        run_id,
        path: path.to_string(),
        state,
    })
}

fn parse_state(text: &str) -> Result<State, RecordError> {
    let (keyword, rest) = match text.split_once(' ') {
        Some((k, r)) => (k, Some(r)),
        None => (text, None),
    };
    match keyword {
        "Start" => {
            let uri = rest.ok_or(RecordError::MalformedState)?;
            if uri.is_empty() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Start {
                uri: uri.to_string(),
            })
        }
        "Finish" => {
            if rest.is_some() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Finish)
        }
        "Stop" => {
            if rest.is_some() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Stop)
        }
        "Resume" => {
            if rest.is_some() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Resume)
        }
        "Invoke" => {
            let payload = rest.ok_or(RecordError::MalformedState)?;
            if payload.is_empty() {
                return Err(RecordError::MalformedState);
            }
            if payload.starts_with("https://") || payload.starts_with("file:///") {
                Ok(State::Invoke(InvokeTarget::Uri(payload.to_string())))
            } else if let Some(name) = payload.strip_suffix(':') {
                if name.is_empty() {
                    return Err(RecordError::MalformedState);
                }
                Ok(State::Invoke(InvokeTarget::Procedure(name.to_string())))
            } else {
                Err(RecordError::MalformedState)
            }
        }
        "Execute" => {
            let payload = rest.ok_or(RecordError::MalformedState)?;
            let name = payload
                .strip_suffix("()")
                .ok_or(RecordError::MalformedState)?;
            if name.is_empty() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Execute {
                function: name.to_string(),
            })
        }
        "Return" => Ok(State::Return(parse_optional_value(rest)?)),
        "Input" => {
            let payload = rest.ok_or(RecordError::MalformedState)?;
            Ok(State::Input(parse_supplied(payload)?))
        }
        "Begin" => {
            if rest.is_some() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Begin)
        }
        "Done" => Ok(State::Done(parse_optional_value(rest)?)),
        "Skip" => {
            if rest.is_some() {
                return Err(RecordError::MalformedState);
            }
            Ok(State::Skip)
        }
        "Fail" => Ok(State::Fail(parse_optional_value(rest)?)),
        other => Err(RecordError::UnknownState(other.to_string())),
    }
}

fn parse_optional_value(rest: Option<&str>) -> Result<Option<value::Value>, RecordError> {
    match rest {
        None => Ok(None),
        Some(text) => Ok(Some(deserialize_value(text)?)),
    }
}

// Single-line PFFTT text form for a runtime `value::Value`, so a completed
// step's result survives in the trail and rehydrates on resume.
//
//   Unitus            -> ()
//   Literali(s)       -> "<escaped>"
//   Enumerati(s)      -> '<value>'
//   Quanticle(n)      -> canonical numeric text (via formatting::render_numeric)
//   Intratempse(n)    -> $(<canonical numeric text>), mirroring source syntax
//   Arraeum(items)    -> [item, item]       (empty: [])
//   Tabularum(pairs)  -> ["label" = value]  (empty: [=], unambiguous vs [])
//   Parametriq(vals)  -> (val, val)
//   Futurae(name)     -> {name}
pub(crate) fn serialize_value(value: &value::Value) -> String {
    let mut out = String::new();
    write_value(&mut out, value);
    out
}

fn write_value(out: &mut String, value: &value::Value) {
    match value {
        value::Value::Unitus => out.push_str("()"),
        value::Value::Literali(text) => {
            out.push('"');
            escape_literal(out, text);
            out.push('"');
        }
        value::Value::Enumerati(text) => {
            out.push('\'');
            out.push_str(text);
            out.push('\'');
        }
        value::Value::Quanticle(numeric) => out.push_str(&render_value_numeric(numeric)),
        value::Value::Intratempse(numeric) => {
            out.push_str("$(");
            out.push_str(&render_value_numeric(numeric));
            out.push(')');
        }
        value::Value::Futurae(name) => {
            out.push('{');
            out.push_str(name);
            out.push('}');
        }
        value::Value::Arraeum(items) => {
            if items.is_empty() {
                out.push_str("[]");
                return;
            }
            out.push_str("[ ");
            for (i, item) in items
                .iter()
                .enumerate()
            {
                if i > 0 {
                    out.push_str(", ");
                }
                write_value(out, item);
            }
            out.push_str(" ]");
        }
        value::Value::Tabularum(pairs) => {
            if pairs.is_empty() {
                out.push_str("[=]");
                return;
            }
            out.push_str("[ ");
            for (i, (label, item)) in pairs
                .iter()
                .enumerate()
            {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push('"');
                escape_literal(out, label);
                out.push('"');
                out.push_str(" = ");
                write_value(out, item);
            }
            out.push_str(" ]");
        }
        value::Value::Parametriq(values) => {
            out.push_str("( ");
            for (i, item) in values
                .iter()
                .enumerate()
            {
                if i > 0 {
                    out.push_str(", ");
                }
                write_value(out, item);
            }
            out.push_str(" )");
        }
    }
}

// Render an owned value::Numeric by reconstructing the borrowed
// language::Numeric and delegating to the shared number renderer.
fn render_value_numeric(numeric: &value::Numeric) -> String {
    match numeric {
        value::Numeric::Integral(i) => {
            let n = crate::language::Numeric::Integral(*i);
            crate::formatting::render_numeric(&n, &crate::formatting::Identity)
        }
        value::Numeric::Scientific(q) => {
            let qb = crate::language::Quantity {
                mantissa: q.mantissa,
                uncertainty: q.uncertainty,
                magnitude: q.magnitude,
                symbol: &q.symbol,
            };
            let n = crate::language::Numeric::Scientific(qb);
            crate::formatting::render_numeric(&n, &crate::formatting::Identity)
        }
    }
}

pub(crate) fn deserialize_value(text: &str) -> Result<value::Value, RecordError> {
    let text = text.trim();
    if text.is_empty() {
        return Err(RecordError::MalformedState);
    }
    if text == "()" {
        return Ok(value::Value::Unitus);
    }
    let first = text
        .chars()
        .next()
        .unwrap();
    match first {
        '"' => {
            if text.len() < 2 || !text.ends_with('"') {
                return Err(RecordError::MalformedState);
            }
            let inner = &text[1..text.len() - 1];
            Ok(value::Value::Literali(unescape_literal(inner)?))
        }
        '\'' => {
            if text.len() < 2 || !text.ends_with('\'') {
                return Err(RecordError::MalformedState);
            }
            Ok(value::Value::Enumerati(text[1..text.len() - 1].to_string()))
        }
        '{' => {
            if !text.ends_with('}') {
                return Err(RecordError::MalformedState);
            }
            Ok(value::Value::Futurae(text[1..text.len() - 1].to_string()))
        }
        '(' => {
            if !text.ends_with(')') {
                return Err(RecordError::MalformedState);
            }
            let inner = text[1..text.len() - 1].trim();
            if inner.is_empty() {
                return Ok(value::Value::Parametriq(Vec::new()));
            }
            let parts = split_top_level(inner, ',')?;
            let mut values = Vec::with_capacity(parts.len());
            for part in parts {
                values.push(deserialize_value(part.trim())?);
            }
            Ok(value::Value::Parametriq(values))
        }
        '[' => {
            if !text.ends_with(']') {
                return Err(RecordError::MalformedState);
            }
            let inner = text[1..text.len() - 1].trim();
            if inner.is_empty() {
                return Ok(value::Value::Arraeum(Vec::new()));
            }
            if inner == "=" {
                return Ok(value::Value::Tabularum(Vec::new()));
            }
            let parts = split_top_level(inner, ',')?;
            // A bracket is a tablet if its first entry carries a top-level
            // ` = `; otherwise it is a list.
            if has_top_level_equals(parts[0]) {
                let mut pairs = Vec::with_capacity(parts.len());
                for part in parts {
                    let (label, rest) =
                        split_once_top_level_equals(part).ok_or(RecordError::MalformedState)?;
                    let label = label.trim();
                    if label.len() < 2 || !label.starts_with('"') || !label.ends_with('"') {
                        return Err(RecordError::MalformedState);
                    }
                    let label = unescape_literal(&label[1..label.len() - 1])?;
                    pairs.push((label, deserialize_value(rest.trim())?));
                }
                Ok(value::Value::Tabularum(pairs))
            } else {
                let mut items = Vec::with_capacity(parts.len());
                for part in parts {
                    items.push(deserialize_value(part.trim())?);
                }
                Ok(value::Value::Arraeum(items))
            }
        }
        '$' => {
            if !text.starts_with("$(") || !text.ends_with(')') {
                return Err(RecordError::MalformedState);
            }
            let inner = &text[2..text.len() - 1];
            let n = crate::parsing::parse_numeric(inner).ok_or(RecordError::MalformedState)?;
            Ok(value::Value::Intratempse(value::Numeric::from(&n)))
        }
        _ => {
            let n = crate::parsing::parse_numeric(text).ok_or(RecordError::MalformedState)?;
            Ok(value::Value::Quanticle(value::Numeric::from(&n)))
        }
    }
}

// Split `text` on `delim`, but only at top level: not inside double quotes
// (honouring `\"` escapes), not inside single-quoted Enumerati values (which
// carry no escapes or interior quote), nor inside nested `[]`, `()`, or `{}`.
pub(crate) fn split_top_level(text: &str, delim: char) -> Result<Vec<&str>, RecordError> {
    let mut parts = Vec::new();
    let bytes = text.as_bytes();
    let mut depth = 0i32;
    let mut in_quote = false;
    let mut in_squote = false;
    let mut start = 0usize;
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i] as char;
        if in_quote {
            if c == '\\' {
                i += 2;
                continue;
            }
            if c == '"' {
                in_quote = false;
            }
            i += 1;
            continue;
        }
        if in_squote {
            if c == '\'' {
                in_squote = false;
            }
            i += 1;
            continue;
        }
        match c {
            '"' => in_quote = true,
            '\'' => in_squote = true,
            '[' | '(' | '{' => depth += 1,
            ']' | ')' | '}' => depth -= 1,
            _ if c == delim && depth == 0 => {
                parts.push(&text[start..i]);
                start = i + 1;
            }
            _ => {}
        }
        i += 1;
    }
    if in_quote || in_squote || depth != 0 {
        return Err(RecordError::MalformedState);
    }
    parts.push(&text[start..]);
    Ok(parts)
}

// True if `text` contains a top-level ` = ` separator (outside quotes/brackets).
fn has_top_level_equals(text: &str) -> bool {
    split_once_top_level_equals(text).is_some()
}

// Split `text` once at the first top-level ` = ` separator.
fn split_once_top_level_equals(text: &str) -> Option<(&str, &str)> {
    let bytes = text.as_bytes();
    let mut depth = 0i32;
    let mut in_quote = false;
    let mut in_squote = false;
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i] as char;
        if in_quote {
            if c == '\\' {
                i += 2;
                continue;
            }
            if c == '"' {
                in_quote = false;
            }
            i += 1;
            continue;
        }
        if in_squote {
            if c == '\'' {
                in_squote = false;
            }
            i += 1;
            continue;
        }
        match c {
            '"' => in_quote = true,
            '\'' => in_squote = true,
            '[' | '(' | '{' => depth += 1,
            ']' | ')' | '}' => depth -= 1,
            ' ' if depth == 0
                && bytes.get(i + 1) == Some(&b'=')
                && bytes.get(i + 2) == Some(&b' ') =>
            {
                return Some((&text[..i], &text[i + 3..]));
            }
            _ => {}
        }
        i += 1;
    }
    None
}

// Split `text` once at the first top-level ` ~ ` separator (the binding
// user joining a supplied value to its parameter name).
fn split_once_top_level_tilde(text: &str) -> Option<(&str, &str)> {
    let bytes = text.as_bytes();
    let mut depth = 0i32;
    let mut in_quote = false;
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i] as char;
        if in_quote {
            if c == '\\' {
                i += 2;
                continue;
            }
            if c == '"' {
                in_quote = false;
            }
            i += 1;
            continue;
        }
        match c {
            '"' => in_quote = true,
            '[' | '(' | '{' => depth += 1,
            ']' | ')' | '}' => depth -= 1,
            ' ' if depth == 0
                && bytes.get(i + 1) == Some(&b'~')
                && bytes.get(i + 2) == Some(&b' ') =>
            {
                return Some((&text[..i], &text[i + 3..]));
            }
            _ => {}
        }
        i += 1;
    }
    None
}

#[cfg(test)]
#[path = "checks/record.rs"]
mod check;

#[cfg(test)]
mod codec_check {
    use super::{deserialize_value, serialize_value};
    use crate::language::Decimal;
    use crate::value::{Numeric, Quantity, Value};

    fn roundtrip(v: Value) {
        let text = serialize_value(&v);
        let back = deserialize_value(&text)
            .unwrap_or_else(|e| panic!("deserialize failed for {:?}: {:?}", text, e));
        assert_eq!(v, back, "round-trip mismatch via {:?}", text);
    }

    #[test]
    fn primitives() {
        roundtrip(Value::Unitus);
        roundtrip(Value::Futurae("x".to_string()));
        roundtrip(Value::Quanticle(Numeric::Integral(42)));
        roundtrip(Value::Quanticle(Numeric::Integral(-7)));
        roundtrip(Value::Quanticle(Numeric::Integral(0)));
        roundtrip(Value::Enumerati("BOTTOM".to_string()));
        assert_eq!(
            serialize_value(&Value::Enumerati("BOTTOM".to_string())),
            "'BOTTOM'"
        );
    }

    #[test]
    fn empty_collections_stay_distinct() {
        roundtrip(Value::Arraeum(Vec::new()));
        roundtrip(Value::Tabularum(Vec::new()));
        assert_eq!(serialize_value(&Value::Arraeum(Vec::new())), "[]");
        assert_eq!(serialize_value(&Value::Tabularum(Vec::new())), "[=]");
        assert_eq!(deserialize_value("[]").unwrap(), Value::Arraeum(Vec::new()));
        assert_eq!(
            deserialize_value("[=]").unwrap(),
            Value::Tabularum(Vec::new())
        );
    }

    #[test]
    fn nested_lists_and_tablets() {
        roundtrip(Value::Arraeum(vec![
            Value::Literali("a".to_string()),
            Value::Literali("b".to_string()),
        ]));
        roundtrip(Value::Arraeum(vec![Value::Tabularum(vec![(
            "k".to_string(),
            Value::Literali("v".to_string()),
        )])]));
        roundtrip(Value::Tabularum(vec![(
            "system".to_string(),
            Value::Enumerati("Monarchy".to_string()),
        )]));
        // A response value carrying a comma must not split mid-value.
        roundtrip(Value::Arraeum(vec![
            Value::Enumerati("Not applicable, see note".to_string()),
            Value::Literali("after".to_string()),
        ]));
        roundtrip(Value::Tabularum(vec![
            ("reason".to_string(), Value::Literali("boom".to_string())),
            ("count".to_string(), Value::Quanticle(Numeric::Integral(3))),
        ]));
        roundtrip(Value::Parametriq(vec![
            Value::Literali("a".to_string()),
            Value::Quanticle(Numeric::Integral(42)),
            Value::Unitus,
        ]));
    }

    #[test]
    fn literals_with_specials_do_not_break_splitter() {
        roundtrip(Value::Literali(
            "comma, equals = brack [ ] quote \" newline\nend".to_string(),
        ));
        roundtrip(Value::Arraeum(vec![
            Value::Literali("a, b".to_string()),
            Value::Literali("c = d".to_string()),
            Value::Literali("[ ( {".to_string()),
        ]));
        roundtrip(Value::Tabularum(vec![(
            "weird, key = ]".to_string(),
            Value::Literali("v, w".to_string()),
        )]));
    }

    #[test]
    fn quantities() {
        roundtrip(Value::Quanticle(Numeric::Scientific(Quantity {
            mantissa: Decimal {
                number: 149,
                precision: 0,
            },
            uncertainty: None,
            magnitude: None,
            symbol: "kg".to_string(),
        })));
        roundtrip(Value::Quanticle(Numeric::Scientific(Quantity {
            mantissa: Decimal {
                number: 59722,
                precision: 4,
            },
            uncertainty: Some(Decimal {
                number: 6,
                precision: 4,
            }),
            magnitude: Some(24),
            symbol: "kg".to_string(),
        })));
    }

    #[test]
    fn deeply_nested_mixed() {
        roundtrip(Value::Parametriq(vec![
            Value::Tabularum(vec![
                (
                    "list".to_string(),
                    Value::Arraeum(vec![
                        Value::Quanticle(Numeric::Integral(1)),
                        Value::Tabularum(vec![(
                            "inner".to_string(),
                            Value::Literali("deep, \"quoted\"".to_string()),
                        )]),
                    ]),
                ),
                ("future".to_string(), Value::Futurae("pending".to_string())),
            ]),
            Value::Unitus,
            Value::Arraeum(Vec::new()),
            Value::Tabularum(Vec::new()),
        ]));
    }
}
