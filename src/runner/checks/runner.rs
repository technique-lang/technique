use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::language::Identifier;
use crate::parsing;
use crate::program::{Fragment, Operation, Ordinal, Program, Subroutine};
use crate::runner::evaluator::Environment;
use crate::runner::prompt::{Event, Mock, UserInput};
use crate::runner::runner::{bind_parameters, Outcome, Runner, RunnerError};
use crate::runner::state::{parse_record, Appender, State, Store, Value as RecordValue};
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
        let (run_id, run_dir) = store
            .create(&document, "2026-05-16T00:00:00Z".to_string())
            .expect("create");
        let pfftt = crate::runner::state::construct_state_path(&run_dir, &document);
        let appender = Appender::open(pfftt, run_id).expect("open appender");
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
fn step_outcomes_recorded() {
    let mut fixture = StoreFixture::new("step-done");
    let body = Operation::Sequence(vec![step(
        Ordinal::Dependent("1"),
        Operation::Sequence(vec![]),
    )]);
    let program = anonymous_with_body(body);
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
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
    // Start + Begin + Done — three lines.
    assert_eq!(lines.len(), 3);
    assert_eq!(
        lines[0],
        "2026-05-16T00:00:00Z 000001 / Start file:///tmp/Test.tq"
    );
    let begin = parse_record(lines[1]).expect("parse begin");
    assert_eq!(begin.path, "/1");
    assert_eq!(begin.state, State::Begin);
    let record = parse_record(lines[2]).expect("parse record");
    assert_eq!(record.path, "/1");
    let State::Done(_) = record.state else {
        panic!("expected Done, got {:?}", record.state);
    };

    let mut fixture = StoreFixture::new("step-skip");
    let body = Operation::Sequence(vec![step(
        Ordinal::Dependent("1"),
        Operation::Sequence(vec![]),
    )]);
    let program = anonymous_with_body(body);
    let prompt = Mock::with_answers([UserInput::Skip]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
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
    // lines[1] is the Begin; lines[2] is the Skip outcome.
    let record = parse_record(lines[2]).expect("parse record");
    assert_eq!(record.state, State::Skip);

    let mut fixture = StoreFixture::new("step-fail");
    let body = Operation::Sequence(vec![step(
        Ordinal::Dependent("1"),
        Operation::Sequence(vec![]),
    )]);
    let program = anonymous_with_body(body);
    let prompt = Mock::with_answers([UserInput::Fail]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
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
    // lines[1] is the Begin; lines[2] is the Fail outcome.
    let record = parse_record(lines[2]).expect("parse record");
    assert_eq!(
        record.state,
        State::Fail(Some(RecordValue::Tablet(
            "[ reason = \"Failed\" ]".to_string()
        )))
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
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
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
    assert_eq!(step_fqns, vec!["/1", "/2"]);
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
    completed.insert("/1".to_string());

    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        completed,
        prompt,
        Environment::new(),
    );
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
    assert_eq!(step_fqns, vec!["/2"]);
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
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
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
    assert_eq!(step_fqns, vec!["/1"]);

    // Quit records the step's Begin (the operator started looking at it)
    // but no Done/Skip/Fail — so the file is exactly the opening Start
    // plus that single Begin.
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
    assert!(lines[0].contains(" Start "));
    assert!(lines[1].ends_with(" Begin"));
}

#[test]
fn section_walking() {
    use crate::program::Fragment;

    let mut fixture = StoreFixture::new("section-no-title");
    let inner = step(Ordinal::Dependent("1"), Operation::Sequence(vec![]));
    let body = Operation::Sequence(vec![Operation::Section {
        numeral: "I",
        title: None,
        body: Box::new(Operation::Sequence(vec![inner])),
        responses: Vec::new(),
    }]);
    let program = anonymous_with_body(body);
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
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
    assert_eq!(section_fqns, vec!["/I"]);
    assert_eq!(step_fqns, vec!["/I/1"]);

    let mut fixture = StoreFixture::new("section-with-title");
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
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
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
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
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
    assert_eq!(step_fqns, vec!["/-1", "/-2"]);
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
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let program = translate(&document).expect("translate");

    let mut fixture = StoreFixture::new("bind-then-interpolate");
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
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
    assert_eq!(descriptions, vec!["", "Result: 42"]);
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
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let program = translate(&document).expect("translate");

    let mut fixture = StoreFixture::new("invoke-descent");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
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
    assert_eq!(step_fqns, vec!["/helper:1"]);
}

#[test]
fn execute_announces_function_call() {
    let source = r#"
% technique v1

test :

1.  Do this { journal("hello") }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let program = translate(&document).expect("translate");

    let mut fixture = StoreFixture::new("execute-announce");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
    runner
        .run()
        .expect("run");

    let prompt = runner.into_prompt();
    let announcements: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Announce(text) = e {
                Some(text.as_str())
            } else {
                None
            }
        })
        .collect();
    assert_eq!(announcements, vec!["journal()"]);
}

#[test]
fn loop_inside_step_produces_one_result() {
    let mut fixture = StoreFixture::new("loop-in-step");

    // A Step whose body contains a Loop over an empty list. The Loop
    // announces and walks its body zero times, recording nothing; the
    // enclosing Step records exactly one Result.
    let loop_op = Operation::Loop {
        names: &[],
        over: Some(Box::new(Operation::Variable(Identifier::new("empty")))),
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

    let mut env = Environment::new();
    env.extend("empty".to_string(), Value::Arraeum(Vec::new()));
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        env,
    );
    runner
        .run()
        .expect("run");

    // One Start record, then the enclosing step's Begin and Done — the
    // Loop inside the step body does not record events of its own.
    let pfftt = fixture.pfftt_contents();
    let lines: Vec<&str> = pfftt
        .lines()
        .filter(|line| {
            !line
                .trim()
                .is_empty()
        })
        .collect();
    assert_eq!(lines.len(), 3);
    assert!(lines[1].ends_with(" Begin"));
    assert!(lines[2].contains(" Done"));
}

#[test]
fn repeat_loops_until_quit() {
    let mut fixture = StoreFixture::new("repeat-until-quit");

    // A `repeat` whose body is a single step. Each pass walks the step with
    // a distinct `[n]` iteration segment; the operator quits on the third
    // pass, ending the loop.
    let inner = Operation::Step {
        ordinal: Ordinal::Dependent("1"),
        attributes: Vec::new(),
        description: Vec::new(),
        body: Box::new(Operation::Sequence(Vec::new())),
        responses: Vec::new(),
    };
    let loop_op = Operation::Loop {
        names: &[],
        over: None,
        body: Box::new(Operation::Sequence(vec![inner])),
        responses: Vec::new(),
    };
    let program = anonymous_with_body(loop_op);

    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Quit,
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
    runner
        .run()
        .expect("run");

    let prompt = runner.into_prompt();
    let steps: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|event| match event {
            Event::Step { qualified, .. } => Some(qualified.as_str()),
            _ => None,
        })
        .collect();
    assert_eq!(steps, vec!["/[1]/1", "/[2]/1", "/[3]/1"]);
}

#[test]
fn foreach_walks_body_once_per_list_element() {
    let mut fixture = StoreFixture::new("foreach-list");

    // foreach item in items: a substep whose description interpolates the
    // iteration variable, so each walk reveals which element it saw.
    let description = Operation::String(vec![Fragment::Interpolation(Operation::Variable(
        Identifier::new("item"),
    ))]);
    let substep = Operation::Step {
        ordinal: Ordinal::Dependent("a"),
        attributes: Vec::new(),
        description: vec![description],
        body: Box::new(Operation::Sequence(Vec::new())),
        responses: Vec::new(),
    };
    // `names` borrows from the IR, so the array must outlive the program.
    let names = [Identifier::new("item")];
    let loop_op = Operation::Loop {
        names: &names,
        over: Some(Box::new(Operation::Variable(Identifier::new("items")))),
        body: Box::new(Operation::Sequence(vec![substep])),
        responses: Vec::new(),
    };
    let mut sub = Subroutine::anonymous();
    sub.body = loop_op;
    let mut program = Program::new();
    program
        .subroutines
        .push(sub);

    let mut env = Environment::new();
    env.extend(
        "items".to_string(),
        Value::Arraeum(vec![
            Value::Literali("first".to_string()),
            Value::Literali("second".to_string()),
        ]),
    );

    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        env,
    );
    runner
        .run()
        .expect("run");

    // The body is walked once per element. Each Step event carries an
    // `[n]` iteration segment in its path and the description it saw,
    // confirming the iteration variable was bound to that element.
    let prompt = runner.into_prompt();
    let steps: Vec<(&str, &str)> = prompt
        .events()
        .iter()
        .filter_map(|event| match event {
            Event::Step {
                qualified,
                description,
            } => Some((qualified.as_str(), description.as_str())),
            _ => None,
        })
        .collect();
    assert_eq!(steps, vec![("/[1]/a", "first"), ("/[2]/a", "second")]);
}

#[test]
fn foreach_destructures_tuple_elements() {
    let mut fixture = StoreFixture::new("foreach-destructure");

    // foreach (first, second) in pairs: two names destructure each
    // tuple-shaped element positionally.
    let description = Operation::String(vec![
        Fragment::Interpolation(Operation::Variable(Identifier::new("first"))),
        Fragment::Text("/"),
        Fragment::Interpolation(Operation::Variable(Identifier::new("second"))),
    ]);
    let substep = Operation::Step {
        ordinal: Ordinal::Dependent("a"),
        attributes: Vec::new(),
        description: vec![description],
        body: Box::new(Operation::Sequence(Vec::new())),
        responses: Vec::new(),
    };
    // `names` borrows from the IR, so the array must outlive the program.
    let names = [Identifier::new("first"), Identifier::new("second")];
    let loop_op = Operation::Loop {
        names: &names,
        over: Some(Box::new(Operation::Variable(Identifier::new("pairs")))),
        body: Box::new(Operation::Sequence(vec![substep])),
        responses: Vec::new(),
    };
    let mut sub = Subroutine::anonymous();
    sub.body = loop_op;
    let mut program = Program::new();
    program
        .subroutines
        .push(sub);

    let mut env = Environment::new();
    env.extend(
        "pairs".to_string(),
        Value::Arraeum(vec![
            Value::Parametriq(vec![
                Value::Literali("a".to_string()),
                Value::Literali("b".to_string()),
            ]),
            Value::Parametriq(vec![
                Value::Literali("c".to_string()),
                Value::Literali("d".to_string()),
            ]),
        ]),
    );

    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        env,
    );
    runner
        .run()
        .expect("run");

    let prompt = runner.into_prompt();
    let steps: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|event| match event {
            Event::Step { description, .. } => Some(description.as_str()),
            _ => None,
        })
        .collect();
    assert_eq!(steps, vec!["a/b", "c/d"]);
}

#[test]
fn foreach_over_non_list_or_unbound_errors() {
    // foreach item in source, where `source` is supplied by the caller's
    // environment. A scalar source is `NotIterable`; an unbound source
    // propagates `UnboundVariable` rather than being swallowed.
    let names = [Identifier::new("item")];
    let loop_op = Operation::Loop {
        names: &names,
        over: Some(Box::new(Operation::Variable(Identifier::new("source")))),
        body: Box::new(Operation::Sequence(Vec::new())),
        responses: Vec::new(),
    };
    let mut sub = Subroutine::anonymous();
    sub.body = loop_op;
    let mut program = Program::new();
    program
        .subroutines
        .push(sub);

    // A scalar bound to `source` is not a list.
    let mut scalar_fixture = StoreFixture::new("foreach-scalar");
    let mut env = Environment::new();
    env.extend(
        "source".to_string(),
        Value::Literali("not a list".to_string()),
    );
    let mut runner = Runner::new(
        &program,
        scalar_fixture.take_appender(),
        HashSet::new(),
        Mock::new(),
        env,
    );
    match runner.run() {
        Err(RunnerError::NotIterable) => {}
        other => panic!("expected NotIterable, got {:?}", other),
    }

    // An unbound `source` propagates the evaluation error.
    let mut unbound_fixture = StoreFixture::new("foreach-unbound");
    let mut runner = Runner::new(
        &program,
        unbound_fixture.take_appender(),
        HashSet::new(),
        Mock::new(),
        Environment::new(),
    );
    match runner.run() {
        Err(RunnerError::UnboundVariable(name)) => assert_eq!(name, "source"),
        other => panic!("expected UnboundVariable, got {:?}", other),
    }
}

#[test]
fn bind_parameters_arity_and_errors() {
    // Procedure with two parameters and matching arity: the returned
    // Environment contains both parameter bindings in `Value::Literali`
    // form.
    let source = r#"
% technique v1

connectivity_check(e, s) :

1.  step
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let program = translate(&document).expect("translate");
    let args = ["foo".to_string(), "192.168.1.5".to_string()];
    let env = bind_parameters(&program, &args).expect("bind");
    assert_eq!(env.lookup("e"), Some(&Value::Literali("foo".to_string())));
    assert_eq!(
        env.lookup("s"),
        Some(&Value::Literali("192.168.1.5".to_string()))
    );

    // Too few arguments: ParameterArityMismatch.
    let args = ["foo".to_string()];
    let error = bind_parameters(&program, &args).expect_err("expected arity error");
    let RunnerError::ParameterArityMismatch { expected, actual } = error else {
        panic!("expected ParameterArityMismatch, got {:?}", error);
    };
    assert_eq!(expected, 2);
    assert_eq!(actual, 1);

    // Too many arguments: also ParameterArityMismatch.
    let args = [
        "foo".to_string(),
        "192.168.1.5".to_string(),
        "extra".to_string(),
    ];
    let error = bind_parameters(&program, &args).expect_err("expected arity error");
    let RunnerError::ParameterArityMismatch { expected, actual } = error else {
        panic!("expected ParameterArityMismatch, got {:?}", error);
    };
    assert_eq!(expected, 2);
    assert_eq!(actual, 3);

    // Procedure declares no parameters but args supplied: ParameterUnexpected.
    let source = r#"
% technique v1

test :

1.  step
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let program = translate(&document).expect("translate");
    let args = ["unwanted".to_string()];
    let error = bind_parameters(&program, &args).expect_err("expected unexpected error");
    let RunnerError::ParameterUnexpected { actual } = error else {
        panic!("expected ParameterUnexpected, got {:?}", error);
    };
    assert_eq!(actual, 1);

    // No parameters and no args: empty environment, no error.
    let env = bind_parameters(&program, &[]).expect("bind");
    assert!(env
        .lookup("anything")
        .is_none());
}

#[test]
fn entry_procedure_parameters_visible_in_descriptions() {
    let source = r#"
% technique v1

greet(name) :

1.  Hello { name }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let program = translate(&document).expect("translate");

    let mut fixture = StoreFixture::new("entry-param-interpolate");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut env = Environment::new();
    env.extend("name".to_string(), Value::Literali("world".to_string()));
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        env,
    );
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
    assert_eq!(descriptions, vec!["Hello world"]);
}

#[test]
fn step_with_responses_prompts_choices_and_records() {
    let source = r#"
% technique v1

test :

1.  Is the site marked?
        'Yes' | 'No'
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let program = translate(&document).expect("translate");

    let mut fixture = StoreFixture::new("step-responses");
    let prompt = Mock::with_answers([UserInput::Done(Value::Literali("Yes".to_string()))]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashSet::new(),
        prompt,
        Environment::new(),
    );
    runner
        .run()
        .expect("run");

    // The prompt offered the two declared responses as choices.
    let prompt = runner.into_prompt();
    let asked: Vec<&Vec<String>> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Ask { choices } = e {
                Some(choices)
            } else {
                None
            }
        })
        .collect();
    assert_eq!(asked, vec![&vec!["Yes".to_string(), "No".to_string()]]);

    // The chosen response is recorded as a quoted literal in the PFFTT.
    let pfftt = fixture.pfftt_contents();
    let lines: Vec<&str> = pfftt
        .lines()
        .filter(|line| {
            !line
                .trim()
                .is_empty()
        })
        .collect();
    let record = parse_record(lines[2]).expect("parse record");
    assert_eq!(
        record.state,
        State::Done(Some(RecordValue::Literal("Yes".to_string())))
    );
}
