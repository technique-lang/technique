use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::parsing;
use crate::program::{Operation, Ordinal, Program, Subroutine};
use crate::runner::prompt::{Event, Mock, UserInput};
use crate::runner::runner::{Outcome, Runner};
use crate::runner::state::{parse_record, Appender, Outcome as RecordOutcome, Store};
use crate::translation::translate;
use crate::value::Value;

// A small fixture builder. The Program borrows from its inputs, so the
// caller has to keep storage alive for the duration of the run. We
// return a (base, run_dir, appender) tuple so the test can clean up
// the temp directory afterwards.
struct StoreFixture {
    base: PathBuf,
    appender: Option<Appender>,
}

impl StoreFixture {
    fn new(test_name: &str) -> Self {
        let base = std::env::temp_dir().join(format!("technique-runner-{}", test_name));
        let _ = std::fs::remove_dir_all(&base);
        let store = Store::new(base.clone());
        let document = PathBuf::from("/tmp/Test.tq");
        let (_, run_dir, _) = store
            .create(&document, "2026-05-16T00:00:00Z".to_string())
            .expect("create");
        let pfftt = crate::runner::state::construct_state_path(&run_dir, &document);
        let appender = Appender::open(pfftt).expect("open appender");
        StoreFixture {
            base,
            appender: Some(appender),
        }
    }

    fn take_appender(&mut self) -> Appender {
        self.appender
            .take()
            .expect("appender")
    }

    fn pfftt_contents(&self) -> String {
        let entries = std::fs::read_dir(&self.base)
            .expect("read base")
            .next()
            .expect("at least one run dir")
            .expect("entry");
        for f in std::fs::read_dir(entries.path()).expect("read run dir") {
            let f = f.expect("entry");
            if f.path()
                .extension()
                .and_then(|e| e.to_str())
                == Some("pfftt")
            {
                return std::fs::read_to_string(f.path()).expect("read pfftt");
            }
        }
        panic!("no pfftt file");
    }
}

impl Drop for StoreFixture {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.base);
    }
}

fn step(ordinal: Ordinal<'static>, body: Operation<'static>) -> Operation<'static> {
    Operation::Step {
        ordinal,
        attributes: Vec::new(),
        description: Vec::new(),
        body: Box::new(body),
        responses: Vec::new(),
    }
}

fn anonymous_with_body(body: Operation<'static>) -> Program<'static> {
    let mut program = Program::new();
    let mut sub = Subroutine::anonymous();
    sub.body = body;
    program
        .subroutines
        .push(sub);
    program
}

#[test]
fn single_step_prompts_and_records() {
    let mut fixture = StoreFixture::new("single-step");
    let body = Operation::Sequence(vec![step(
        Ordinal::Dependent("1"),
        Operation::Sequence(vec![]),
    )]);
    let program = anonymous_with_body(body);

    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::with_pieces(&program, fixture.take_appender(), HashSet::new(), prompt);
    let outcome = runner
        .run()
        .expect("run");
    assert_eq!(outcome, Outcome::Done(Value::Unitus));

    let prompt = runner.into_prompt();
    assert_eq!(
        prompt.events(),
        &[
            Event::Step {
                qualified: "1".to_string(),
                description: String::new(),
            },
            Event::Ask,
        ]
    );

    let pfftt = fixture.pfftt_contents();
    let lines: Vec<&str> = pfftt
        .lines()
        .filter(|line| {
            !line
                .trim()
                .is_empty()
        })
        .collect();
    assert_eq!(lines.len(), 2);
    assert_eq!(
        lines[0],
        "[ document = file:///tmp/Test.tq, started = 2026-05-16T00:00:00Z ]"
    );
    let record = parse_record(lines[1]).expect("parse record");
    assert_eq!(record.path, "1");
    assert_eq!(record.outcome, RecordOutcome::Done(Some("()".to_string())));
}

#[test]
fn skip_outcome_is_recorded() {
    let mut fixture = StoreFixture::new("skip-records");
    let body = Operation::Sequence(vec![step(
        Ordinal::Dependent("1"),
        Operation::Sequence(vec![]),
    )]);
    let program = anonymous_with_body(body);

    let prompt = Mock::with_answers([UserInput::Skip]);
    let mut runner = Runner::with_pieces(&program, fixture.take_appender(), HashSet::new(), prompt);
    let outcome = runner
        .run()
        .expect("run");
    assert_eq!(outcome, Outcome::Done(Value::Unitus));

    let pfftt = fixture.pfftt_contents();
    let lines: Vec<&str> = pfftt
        .lines()
        .filter(|line| {
            !line
                .trim()
                .is_empty()
        })
        .collect();
    assert_eq!(lines.len(), 2);
    let record = parse_record(lines[1]).expect("parse record");
    assert_eq!(record.path, "1");
    assert_eq!(record.outcome, RecordOutcome::Skipped);
}

#[test]
fn fail_outcome_is_recorded_with_reason() {
    let mut fixture = StoreFixture::new("fail-records");
    let body = Operation::Sequence(vec![step(
        Ordinal::Dependent("1"),
        Operation::Sequence(vec![]),
    )]);
    let program = anonymous_with_body(body);

    let prompt = Mock::with_answers([UserInput::Fail]);
    let mut runner = Runner::with_pieces(&program, fixture.take_appender(), HashSet::new(), prompt);
    runner
        .run()
        .expect("run");

    let pfftt = fixture.pfftt_contents();
    let lines: Vec<&str> = pfftt
        .lines()
        .filter(|line| {
            !line
                .trim()
                .is_empty()
        })
        .collect();
    assert_eq!(lines.len(), 2);
    let record = parse_record(lines[1]).expect("parse record");
    assert_eq!(record.path, "1");
    assert_eq!(
        record.outcome,
        RecordOutcome::Failed(Some("\"Failed\"".to_string()))
    );
}

#[test]
fn two_steps_prompted_in_source_order() {
    let mut fixture = StoreFixture::new("two-steps");
    let body = Operation::Sequence(vec![
        step(Ordinal::Dependent("1"), Operation::Sequence(vec![])),
        step(Ordinal::Dependent("2"), Operation::Sequence(vec![])),
    ]);
    let program = anonymous_with_body(body);

    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::with_pieces(&program, fixture.take_appender(), HashSet::new(), prompt);
    runner
        .run()
        .expect("run");

    let prompt = runner.into_prompt();
    let step_fqns: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Step { qualified, .. } = e {
                Some(qualified.as_str())
            } else {
                None
            }
        })
        .collect();
    assert_eq!(step_fqns, vec!["1", "2"]);
}

#[test]
fn pre_completed_step_short_circuits() {
    let mut fixture = StoreFixture::new("short-circuit");
    let body = Operation::Sequence(vec![
        step(Ordinal::Dependent("1"), Operation::Sequence(vec![])),
        step(Ordinal::Dependent("2"), Operation::Sequence(vec![])),
    ]);
    let program = anonymous_with_body(body);

    // Pre-mark step 1 completed; the walker should skip its prompt
    // and only ask about step 2.
    let mut completed = HashSet::new();
    completed.insert("1".to_string());

    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::with_pieces(&program, fixture.take_appender(), completed, prompt);
    runner
        .run()
        .expect("run");

    let prompt = runner.into_prompt();
    let step_fqns: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Step { qualified, .. } = e {
                Some(qualified.as_str())
            } else {
                None
            }
        })
        .collect();
    assert_eq!(step_fqns, vec!["2"]);
}

#[test]
fn quit_propagates_and_stops_walking() {
    let mut fixture = StoreFixture::new("quit-propagates");
    let body = Operation::Sequence(vec![
        step(Ordinal::Dependent("1"), Operation::Sequence(vec![])),
        step(Ordinal::Dependent("2"), Operation::Sequence(vec![])),
    ]);
    let program = anonymous_with_body(body);

    let prompt = Mock::with_answers([UserInput::Quit]);
    let mut runner = Runner::with_pieces(&program, fixture.take_appender(), HashSet::new(), prompt);
    let outcome = runner
        .run()
        .expect("run");
    assert_eq!(outcome, Outcome::Quit);

    let prompt = runner.into_prompt();
    let step_fqns: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Step { qualified, .. } = e {
                Some(qualified.as_str())
            } else {
                None
            }
        })
        .collect();
    // Only the first Step was prompted; the second never fired.
    assert_eq!(step_fqns, vec!["1"]);

    // Quit doesn't record an outcome — the PFFTT file contains the
    // manifest tablet and nothing else.
    let pfftt = fixture.pfftt_contents();
    assert_eq!(
        pfftt
            .matches("outcome =")
            .count(),
        0
    );
}

#[test]
fn section_renders_path_segment() {
    let mut fixture = StoreFixture::new("section-path");
    let inner = step(Ordinal::Dependent("1"), Operation::Sequence(vec![]));
    let body = Operation::Sequence(vec![Operation::Section {
        numeral: "I",
        title: None,
        body: Box::new(Operation::Sequence(vec![inner])),
        responses: Vec::new(),
    }]);
    let program = anonymous_with_body(body);

    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::with_pieces(&program, fixture.take_appender(), HashSet::new(), prompt);
    runner
        .run()
        .expect("run");

    let prompt = runner.into_prompt();
    let events = prompt.events();
    let section_fqns: Vec<&str> = events
        .iter()
        .filter_map(|e| {
            if let Event::Section { qualified, .. } = e {
                Some(qualified.as_str())
            } else {
                None
            }
        })
        .collect();
    let step_fqns: Vec<&str> = events
        .iter()
        .filter_map(|e| {
            if let Event::Step { qualified, .. } = e {
                Some(qualified.as_str())
            } else {
                None
            }
        })
        .collect();
    assert_eq!(section_fqns, vec!["I"]);
    assert_eq!(step_fqns, vec!["I/1"]);
}

#[test]
fn section_title_renders_from_paragraph() {
    use crate::program::Fragment;

    let mut fixture = StoreFixture::new("section-title");
    let title = Operation::String(vec![Fragment::Text("Setup")]);
    let inner = step(Ordinal::Dependent("1"), Operation::Sequence(vec![]));
    let body = Operation::Sequence(vec![Operation::Section {
        numeral: "I",
        title: Some(Box::new(title)),
        body: Box::new(Operation::Sequence(vec![inner])),
        responses: Vec::new(),
    }]);
    let program = anonymous_with_body(body);

    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::with_pieces(&program, fixture.take_appender(), HashSet::new(), prompt);
    runner
        .run()
        .expect("run");

    let prompt = runner.into_prompt();
    let section_title = prompt
        .events()
        .iter()
        .find_map(|e| {
            if let Event::Section { title, .. } = e {
                Some(title.as_str())
            } else {
                None
            }
        })
        .expect("section event");
    assert_eq!(section_title, "Setup");
}

#[test]
fn parallel_step_index_starts_at_one() {
    let mut fixture = StoreFixture::new("parallel-index");
    let body = Operation::Sequence(vec![
        step(Ordinal::Parallel, Operation::Sequence(vec![])),
        step(Ordinal::Parallel, Operation::Sequence(vec![])),
    ]);
    let program = anonymous_with_body(body);

    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::with_pieces(&program, fixture.take_appender(), HashSet::new(), prompt);
    runner
        .run()
        .expect("run");

    let prompt = runner.into_prompt();
    let step_fqns: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Step { qualified, .. } = e {
                Some(qualified.as_str())
            } else {
                None
            }
        })
        .collect();
    assert_eq!(step_fqns, vec!["-1", "-2"]);
}

#[test]
fn bind_in_body_interpolates_into_description() {
    let source = r#"
% technique v1

test :

1.  { 42 ~ answer }
2.  Result: { answer }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("test.tq"), source).expect("parse");
    let program = translate(&document).expect("translate");

    let mut fixture = StoreFixture::new("bind-then-interpolate");
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::with_pieces(&program, fixture.take_appender(), HashSet::new(), prompt);
    runner
        .run()
        .expect("run");

    let prompt = runner.into_prompt();
    let descriptions: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Step { description, .. } = e {
                Some(description.as_str())
            } else {
                None
            }
        })
        .collect();
    // Step 1 only binds, so its description renders to empty. Step 2
    // sees the binding established by step 1 and interpolates it.
    // The parser strips whitespace adjacent to inline `{ ... }` fragments,
    // hence "Result:42" rather than "Result: 42".
    assert_eq!(descriptions, vec!["", "Result:42"]);
}

#[test]
fn resolved_invoke_descends_into_subroutine() {
    let source = r#"
% technique v1

main :

{
    <helper>()
}

helper :

1.  helper step
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("test.tq"), source).expect("parse");
    let program = translate(&document).expect("translate");

    let mut fixture = StoreFixture::new("invoke-descent");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::with_pieces(&program, fixture.take_appender(), HashSet::new(), prompt);
    runner
        .run()
        .expect("run");

    let prompt = runner.into_prompt();
    let step_fqns: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Step { qualified, .. } = e {
                Some(qualified.as_str())
            } else {
                None
            }
        })
        .collect();
    // The Step inside `helper` was reached and prompted, with the
    // helper procedure as the FQN prefix (the outer `main` frame is
    // overridden by the inner Procedure segment).
    assert_eq!(step_fqns, vec!["helper:1"]);
}

#[test]
fn loop_inside_step_produces_one_result() {
    let mut fixture = StoreFixture::new("loop-in-step");

    // A Step whose body contains a Loop. The Loop announces but does
    // not record a Result; the enclosing Step records exactly one.
    let loop_op = Operation::Loop {
        names: &[],
        over: None,
        body: Box::new(Operation::Sequence(vec![])),
        responses: Vec::new(),
    };
    let the_step = Operation::Step {
        ordinal: Ordinal::Dependent("1"),
        attributes: Vec::new(),
        description: Vec::new(),
        body: Box::new(loop_op),
        responses: Vec::new(),
    };
    let body = Operation::Sequence(vec![the_step]);
    let program = anonymous_with_body(body);

    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::with_pieces(&program, fixture.take_appender(), HashSet::new(), prompt);
    runner
        .run()
        .expect("run");

    let pfftt = fixture.pfftt_contents();
    assert_eq!(
        pfftt
            .matches("outcome =")
            .count(),
        1
    );
}
