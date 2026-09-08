use std::path::{Path, PathBuf};

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::engraving::{
    Appender, InvokeTarget, Ledger, Motion, Record, RunId, Serial, State, Store, Supplied,
    parse_record,
};
use crate::language;
use crate::language::{Identifier, Numeric as LangNumeric};
use crate::parsing;
use crate::program::{
    Executable, ExecutableRef, Fragment, Invocable, Operation, Ordinal, Program, Subroutine,
    SubroutineRef,
};
use crate::resolution::resolve;
use crate::runner::driver::{
    Automatic, Console, Event, Mock, MockKeyboard, Offer, Review, Scripted, UserInput,
};
use crate::runner::evaluator::Environment;
use crate::runner::library::Library;
use crate::runner::runner::{
    Conclusion, Outcome, Runner, RunnerError, bind_parameters, render_argument_echo,
};
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
            .create(&document, "2026-05-16T00:00:00Z".to_string(), &[])
            .expect("create");
        let pfftt = crate::engraving::construct_state_path(&run_dir, &document);
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

fn scope_for(ordinal: Ordinal<'static>) -> &'static language::Scope<'static> {
    scope_with(ordinal, Vec::new())
}

fn scope_with(
    ordinal: Ordinal<'static>,
    description: Vec<language::Paragraph<'static>>,
) -> &'static language::Scope<'static> {
    let scope = match ordinal {
        Ordinal::Dependent(s) => language::Scope::DependentBlock {
            ordinal: s,
            description,
            subscopes: Vec::new(),
            span: language::Span::default(),
        },
        Ordinal::Parallel => language::Scope::ParallelBlock {
            bullet: '-',
            description,
            subscopes: Vec::new(),
            span: language::Span::default(),
        },
    };
    Box::leak(Box::new(scope))
}

fn step(ordinal: Ordinal<'static>, body: Operation<'static>) -> Operation<'static> {
    Operation::Step {
        ordinal,
        attributes: Vec::new(),
        source: scope_for(ordinal),
        body: Box::new(body),
        responses: Vec::new(),
        span: language::Span::default(),
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
    let body = Operation::Sequence(
        vec![step(
            Ordinal::Dependent("1"),
            Operation::Sequence(vec![], language::Span::default()),
        )],
        language::Span::default(),
    );
    let program = anonymous_with_body(body);
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    let outcome = runner
        .run(env)
        .expect("run");
    assert_eq!(outcome, Conclusion::Completed(Outcome::Done(Value::Unitus)));
    let pfftt = fixture.pfftt_contents();
    let lines: Vec<&str> = pfftt
        .lines()
        .filter(|line| {
            !line
                .trim()
                .is_empty()
        })
        .collect();
    // Start + the entry scope's Begin + the step's Begin and Done + the entry
    // scope's Done + Finish — six lines.
    assert_eq!(lines.len(), 6);
    assert_eq!(
        lines[0],
        "2026-05-16T00:00:00Z 000001 000 / Start file:///tmp/Test.tq"
    );
    let begin = parse_record(lines[2]).expect("parse begin");
    assert_eq!(begin.path, "/1");
    assert_eq!(begin.state, State::Begin(Vec::new()));
    let record = parse_record(lines[3]).expect("parse record");
    assert_eq!(record.path, "/1");
    let State::Done(_) = record.state else {
        panic!("expected Done, got {:?}", record.state);
    };

    let mut fixture = StoreFixture::new("step-skip");
    let body = Operation::Sequence(
        vec![step(
            Ordinal::Dependent("1"),
            Operation::Sequence(vec![], language::Span::default()),
        )],
        language::Span::default(),
    );
    let program = anonymous_with_body(body);
    let prompt = Mock::with_answers([UserInput::Skip]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
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
    // lines[2] is the step's Begin; lines[3] is its Skip outcome.
    let record = parse_record(lines[3]).expect("parse record");
    assert_eq!(record.state, State::Skip);

    let mut fixture = StoreFixture::new("step-fail");
    let body = Operation::Sequence(
        vec![step(
            Ordinal::Dependent("1"),
            Operation::Sequence(vec![], language::Span::default()),
        )],
        language::Span::default(),
    );
    let program = anonymous_with_body(body);
    let prompt = Mock::with_answers([UserInput::Fail("cable unplugged".to_string())]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
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
    // lines[2] is the step's Begin; lines[3] is its Fail outcome.
    let record = parse_record(lines[3]).expect("parse record");
    assert_eq!(
        record.state,
        State::Fail(Some(Value::Tabularum(vec![(
            "reason".to_string(),
            Value::Literali("cable unplugged".to_string()),
        )])))
    );
}

#[test]
fn empty_fail_reason_records_none() {
    // Failing a step without giving a reason records Fail with no reason at
    // all, not an empty-string reason tablet.
    let mut fixture = StoreFixture::new("fail-no-reason");
    let body = Operation::Sequence(
        vec![step(
            Ordinal::Dependent("1"),
            Operation::Sequence(vec![], language::Span::default()),
        )],
        language::Span::default(),
    );
    let program = anonymous_with_body(body);
    let prompt = Mock::with_answers([UserInput::Fail(String::new())]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
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
    let record = parse_record(lines[3]).expect("parse record");
    assert_eq!(record.state, State::Fail(None));
}

#[test]
fn same_procedure_invoked_twice_runs_twice() {
    // Two calls to the same procedure at the same path each run: `completed` is
    // the resume snapshot, not a live within-run dedup, so the second call is
    // not wrongly skipped just because the first reached the same FQP.
    let source = r#"
% technique v1

main :

{ <helper> }
{ <helper> }

helper :

    1.  do the thing
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("double-invoke");
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        Automatic::with_handle(Vec::new()),
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let pfftt = fixture.pfftt_contents();
    let invokes = pfftt
        .lines()
        .filter(|line| line.contains("Invoke helper:"))
        .count();
    let begins = pfftt
        .lines()
        .filter(|line| line.contains("/helper:/1 Begin"))
        .count();
    assert_eq!(invokes, 2, "both call sites should invoke helper");
    assert_eq!(begins, 2, "helper's body should run on both calls");

    // Each execution is its own scope, so each takes its own serial.
    let serials: Vec<&str> = pfftt
        .lines()
        .filter(|line| line.contains("/helper: Begin"))
        .filter_map(|line| {
            line.split(' ')
                .nth(2)
        })
        .collect();
    assert_eq!(serials.len(), 2);
    assert_ne!(
        serials[0], serials[1],
        "two executions of one address do not share a serial: {}",
        pfftt
    );
}

#[test]
fn two_steps_prompted_in_source_order() {
    let mut fixture = StoreFixture::new("two-steps");
    let body = Operation::Sequence(
        vec![
            step(
                Ordinal::Dependent("1"),
                Operation::Sequence(vec![], language::Span::default()),
            ),
            step(
                Ordinal::Dependent("2"),
                Operation::Sequence(vec![], language::Span::default()),
            ),
        ],
        language::Span::default(),
    );
    let program = anonymous_with_body(body);

    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
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
    let body = Operation::Sequence(
        vec![
            step(
                Ordinal::Dependent("1"),
                Operation::Sequence(vec![], language::Span::default()),
            ),
            step(
                Ordinal::Dependent("2"),
                Operation::Sequence(vec![], language::Span::default()),
            ),
        ],
        language::Span::default(),
    );
    let program = anonymous_with_body(body);

    // Pre-mark step 1 completed; the walker replays it — shown but not
    // prompted for — and only asks about step 2.
    let ledger = ledger_of(&[
        (1, "/", State::Begin(Vec::new())),
        (2, "/1", State::Begin(Vec::new())),
        (2, "/1", State::Done(None)),
    ]);

    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        ledger,
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
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

    let asked: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Ask {
                qualified, marker, ..
            } = e
            {
                if marker == "→" {
                    Some(qualified.as_str())
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect();
    assert_eq!(asked, vec!["/2"]);
}

#[test]
fn pre_completed_run_replays_writing_nothing() {
    let mut fixture = StoreFixture::new("completed-replay");
    let body = Operation::Sequence(
        vec![step(
            Ordinal::Dependent("1"),
            Operation::Sequence(vec![], language::Span::default()),
        )],
        language::Span::default(),
    );
    let program = anonymous_with_body(body);

    // A run sealed at its entry: resuming it shows the walk again but states
    // nothing, the journal already carrying the whole of it.
    let ledger = ledger_of(&[
        (1, "/", State::Begin(Vec::new())),
        (2, "/1", State::Begin(Vec::new())),
        (2, "/1", State::Done(None)),
        (1, "/", State::Done(None)),
    ]);

    let prompt = Mock::with_answers([]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        ledger,
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let contents = fixture.pfftt_contents();
    let appended: Vec<&str> = contents
        .lines()
        .filter(|line| !line.contains("Start"))
        .collect();
    assert!(
        appended.is_empty(),
        "a completed run appended {:?}",
        appended
    );

    let prompt = runner.into_driver();
    let asked: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Ask { qualified, .. } = e {
                Some(qualified.as_str())
            } else {
                None
            }
        })
        .collect();
    assert!(
        asked.is_empty(),
        "a completed run took a prompt at {:?}",
        asked
    );
}

#[test]
fn quit_propagates_and_stops_walking() {
    let mut fixture = StoreFixture::new("quit-propagates");
    let body = Operation::Sequence(
        vec![
            step(
                Ordinal::Dependent("1"),
                Operation::Sequence(vec![], language::Span::default()),
            ),
            step(
                Ordinal::Dependent("2"),
                Operation::Sequence(vec![], language::Span::default()),
            ),
        ],
        language::Span::default(),
    );
    let program = anonymous_with_body(body);

    let prompt = Mock::with_answers([UserInput::Quit]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    let outcome = runner
        .run(env)
        .expect("run");
    assert_eq!(outcome, Conclusion::Stopping);

    let prompt = runner.into_driver();
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

    // Quit records the step's Begin (the user started looking at it), then
    // a Stop lifecycle line at the root path — the deliberate-stop marker that
    // tells a quit from a crash. No Done/Skip/Fail for the step itself.
    let pfftt = fixture.pfftt_contents();
    let lines: Vec<&str> = pfftt
        .lines()
        .filter(|line| {
            !line
                .trim()
                .is_empty()
        })
        .collect();
    assert_eq!(lines.len(), 4);
    assert!(lines[0].contains(" Start "));
    assert!(lines[1].ends_with(" / Begin ()"));
    assert!(lines[2].ends_with(" Begin ()"));
    assert!(lines[3].ends_with(" / Stop"));
}

#[test]
fn section_walking() {
    use crate::program::Fragment;

    let mut fixture = StoreFixture::new("section-no-title");
    let inner = step(
        Ordinal::Dependent("1"),
        Operation::Sequence(vec![], language::Span::default()),
    );
    let body = Operation::Sequence(
        vec![Operation::Section {
            numeral: "I",
            title: None,
            body: Box::new(Operation::Sequence(vec![inner], language::Span::default())),
            responses: Vec::new(),
            span: language::Span::default(),
        }],
        language::Span::default(),
    );
    let program = anonymous_with_body(body);
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");
    let prompt = runner.into_driver();
    let events = prompt.events();
    // A section descends with a `Section` heading (its numeral) and signs off
    // at its close with a `Seal` carrying the section's qualified path.
    let section_numerals: Vec<&str> = events
        .iter()
        .filter_map(|e| {
            if let Event::Section { numeral, .. } = e {
                Some(numeral.as_str())
            } else {
                None
            }
        })
        .collect();
    let section_fqns: Vec<&str> = events
        .iter()
        .filter_map(|e| {
            if let Event::Ask {
                qualified, marker, ..
            } = e
            {
                if marker == "↙" {
                    Some(qualified.as_str())
                } else {
                    None
                }
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
    assert_eq!(section_numerals, vec!["I"]);
    assert_eq!(section_fqns, vec!["/I", "/"]);
    assert_eq!(step_fqns, vec!["/I/1"]);

    let mut fixture = StoreFixture::new("section-with-title");
    let title = Operation::String(vec![Fragment::Text("Setup")], language::Span::default());
    let inner = step(
        Ordinal::Dependent("1"),
        Operation::Sequence(vec![], language::Span::default()),
    );
    let body = Operation::Sequence(
        vec![Operation::Section {
            numeral: "I",
            title: Some(Box::new(title)),
            body: Box::new(Operation::Sequence(vec![inner], language::Span::default())),
            responses: Vec::new(),
            span: language::Span::default(),
        }],
        language::Span::default(),
    );
    let program = anonymous_with_body(body);
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");
    let prompt = runner.into_driver();
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
    let body = Operation::Sequence(
        vec![
            step(
                Ordinal::Parallel,
                Operation::Sequence(vec![], language::Span::default()),
            ),
            step(
                Ordinal::Parallel,
                Operation::Sequence(vec![], language::Span::default()),
            ),
        ],
        language::Span::default(),
    );
    let program = anonymous_with_body(body);

    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
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
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("bind-then-interpolate");
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
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
    // Step 1's binding syntax shows as-written (the value isn't bound until
    // its body walks). By step 2 `answer` is bound, so its interpolation
    // renders the value in place of the variable name.
    assert_eq!(descriptions, vec!["1.  { 42 ~ answer }", "2.  Result: 42"]);
}

#[test]
fn hole_argument_acquired_at_entry() {
    // main invokes cycle with `?`, declining to supply the Situation. The
    // user is asked for `s` once, when cycle is entered, and the bound
    // value serves both reads in the body.
    let source = r#"
% technique v1

main :

{
    <cycle>(?)
}

cycle(s) : Situation -> Done

1.  First { s ~ x }
2.  Second { s ~ y }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("hole-at-entry");
    // acquire (for `?`) pops first at entry, then the two step completions.
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Literali("the situation".to_string())),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let prompt = runner.into_driver();
    let acquired: Vec<(Option<&str>, Option<&str>)> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Acquire { name, forma, .. } = e {
                Some((
                    name.as_ref()
                        .map(String::as_str),
                    forma
                        .as_ref()
                        .map(String::as_str),
                ))
            } else {
                None
            }
        })
        .collect();
    assert_eq!(
        acquired,
        vec![(Some("s"), Some("Situation"))],
        "asked once, at entry, for s : Situation"
    );
}

#[test]
fn quit_while_acquiring_stops_the_run() {
    // Ctrl-C at the implicit-argument prompt quits the run rather than
    // accepting an empty value: the walk stops and a Stop event is recorded,
    // and the callee's body never runs.
    let source = r#"
% technique v1

main :

{
    <cycle>(?)
}

cycle(s) : Situation -> Done

1.  First { s }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("quit-acquire");
    let prompt = Mock::with_answers([UserInput::Quit]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let outcome = runner
        .run(Environment::new())
        .expect("run");
    assert_eq!(outcome, Conclusion::Stopping);

    let pfftt = fixture.pfftt_contents();
    assert!(
        pfftt
            .lines()
            .any(|line| line.contains(" / Stop")),
        "a Stop lifecycle event is recorded at the root"
    );
    assert!(
        !pfftt
            .lines()
            .any(|line| line.contains("/cycle:/1 Begin")),
        "the callee's body must not run after the quit"
    );
    assert!(
        pfftt
            .lines()
            .any(|line| line.contains("Invoke cycle:")),
        "the dispatch is recorded on arrival, before the prompt the user quit at"
    );
}

#[test]
fn skip_while_acquiring_records_the_skipped_invocation() {
    // Skip at the implicit-argument prompt skips the invocation: the dispatch
    // is recorded, a Skip is recorded at the callee's path (not silently
    // swallowed), and the callee's body never runs.
    let source = r#"
% technique v1

main :

{
    <cycle>(?)
}

cycle(s) : Situation -> Done

1.  First { s }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("skip-acquire");
    let prompt = Mock::with_answers([UserInput::Skip]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let pfftt = fixture.pfftt_contents();
    assert!(
        pfftt
            .lines()
            .any(|line| line.contains("/cycle: Skip")),
        "the skipped invocation is recorded at the callee's path"
    );
    assert!(
        pfftt
            .lines()
            .any(|line| line.contains("Invoke cycle:")),
        "the dispatch is recorded on arrival, the Skip standing as the decline"
    );
    assert!(
        !pfftt
            .lines()
            .any(|line| line.contains("/cycle:/1 Begin")),
        "the callee's body never runs"
    );
}

#[test]
fn review_while_acquiring_puts_the_prompt_again() {
    // <Up> at the implicit-argument prompt is not an answer: it records
    // nothing and puts the same prompt again.
    let source = r#"
% technique v1

main :

{
    <cycle>(?)
}

cycle(s) : Situation -> Done

1.  First { s }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("review-acquire");
    // Review, then the value; the rest answer the step and the scopes it closes.
    let prompt = Mock::with_answers([
        UserInput::Review,
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let prompt = runner.into_driver();
    let asked = prompt
        .events()
        .iter()
        .filter(|event| {
            if let Event::Acquire { .. } = event {
                true
            } else {
                false
            }
        })
        .count();
    assert_eq!(asked, 2, "the prompt is put again after the review pass");

    let pfftt = fixture.pfftt_contents();
    assert_eq!(
        pfftt
            .lines()
            .filter(|line| line.contains("/cycle: Begin"))
            .count(),
        1,
        "the review pass records nothing, so the callee is entered once"
    );
    assert!(
        pfftt
            .lines()
            .any(|line| line.contains("/cycle:/1 Begin")),
        "the value given on the second pass supplies the call, which runs"
    );
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
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("invoke-descent");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
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
    // The Step inside `helper` was reached and prompted. `helper` is a
    // top-level procedure, so its lexical address is its own root — the
    // call from `main` does not nest it under the call site.
    assert_eq!(step_fqns, vec!["/helper:/1"]);
}

#[test]
fn bound_invoke_descends_into_subroutine() {
    // A bound invocation `<helper>() ~ result` must still descend into the
    // callee, not evaluate to Unit. The binding is the regression: a bare
    // `<helper>()` always descended, but wrapping it in a `~` bind once
    // settled the step without entering the procedure.
    let source = r#"
% technique v1

main :

1.  <helper>() ~ result

helper :

1.  helper step
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("bound-invoke-descent");
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let prompt = runner.into_driver();
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
    // `main:/1` is presented, then its bound body descends into `helper`,
    // reaching the step inside it — previously skipped entirely.
    assert_eq!(step_fqns, vec!["/main:/1", "/helper:/1"]);
}

#[test]
fn section_holding_procedure_descends() {
    let source = r#"
% technique v1

outer :

I. Setup

inner :

1.  inner step
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("section-holding-procedure");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
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
    // A section whose body declares a procedure descends into it: the step
    // inside `inner` is reached, its FQN carrying the `outer` entry frame,
    // the `I` section, then the invoked `inner` frame.
    assert_eq!(step_fqns, vec!["/outer:/I/inner:/1"]);
}

#[test]
fn invoke_binds_arguments_to_parameters() {
    let source = r#"
% technique v1

main :

{
    <greet>("World")
}

greet(name) :

1.  Hello { name }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("invoke-args");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
    let steps: Vec<(&str, &str)> = prompt
        .events()
        .iter()
        .filter_map(|e| match e {
            Event::Step {
                qualified,
                description,
            } => Some((qualified.as_str(), description.as_str())),
            _ => None,
        })
        .collect();
    // `name` is bound to the invocation argument, so the step renders the
    // value in place of the interpolation. `greet` is a top-level procedure,
    // addressed at its own root.
    assert_eq!(steps, vec![("/greet:/1", "1.  Hello \"World\"")]);
}

#[test]
fn execute_announces_function_call() {
    let source = r#"
% technique v1

test :

1.  Do this { journal("hello") }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parsed");
    let mut program = translate(&document).expect("translated");
    resolve(&mut program).expect("resolve");
    crate::linking::link(&mut program, &Library::stub()).expect("linked");

    let mut fixture = StoreFixture::new("execute-announce");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
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
fn exec_step_solicits_command_then_judges() {
    let source = r#"
% technique v1

test :

1.  Run it { exec("ip addr") }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parsed");
    let mut program = translate(&document).expect("translated");
    resolve(&mut program).expect("resolve");
    crate::linking::link(&mut program, &Library::stub()).expect("linked");

    let mut fixture = StoreFixture::new("exec-command");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let prompt = runner.into_driver();
    // The exec is gated. A Command beat shows the script at the step's path,
    // and only once commanded does the step's own verdict prompt judge it.
    let commands: Vec<(&str, &str)> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Command { qualified, script } = e {
                Some((qualified.as_str(), script.as_str()))
            } else {
                None
            }
        })
        .collect();
    let asks: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Ask {
                qualified, marker, ..
            } = e
            {
                if marker == "→" {
                    Some(qualified.as_str())
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect();
    assert_eq!(commands, vec![("/test:/1", "ip addr")]);
    assert_eq!(asks, vec!["/test:/1"]);
}

#[test]
fn action_step_presents_call_read_only() {
    let source = r#"
% technique v1

test :

1.  Open the menu { click("Actions") }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parsed");
    let mut program = translate(&document).expect("translated");
    resolve(&mut program).expect("resolve");
    crate::linking::link(&mut program, &Library::stub()).expect("linked");

    let mut fixture = StoreFixture::new("action-confirm");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let prompt = runner.into_driver();
    // A `click` is an Action, not a Command: its imperative verb and bare
    // label are presented read-only for the user to confirm, then the step's
    // verdict prompt judges it. No Command beat appears.
    let actions: Vec<(&str, &str, &str, &str)> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Action {
                qualified,
                name,
                verb,
                label,
            } = e
            {
                Some((
                    qualified.as_str(),
                    name.as_str(),
                    verb.as_str(),
                    label.as_str(),
                ))
            } else {
                None
            }
        })
        .collect();
    let commanded = prompt
        .events()
        .iter()
        .any(|e| {
            if let Event::Command { .. } = e {
                true
            } else {
                false
            }
        });
    assert_eq!(actions, vec![("/test:/1", "click", "Click", "Actions")]);
    assert!(
        !commanded,
        "an action must not route through the command gate"
    );
}

#[test]
fn automatic_settles_computable_steps_done_prose_skip() {
    // An exec step settles Done; a pure-prose sibling Skips; the procedure
    // seals Done since one step beneath it was computable.
    let source = r#"
% technique v1

check :

1.  Run it { exec("true") }

2.  Just read this step
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parsed");
    let mut program = translate(&document).expect("translated");
    resolve(&mut program).expect("resolve");
    let mut library = Library::core();
    library.extend(Library::system());
    crate::linking::link(&mut program, &library).expect("linked");

    let mut fixture = StoreFixture::new("automatic-substantiation");
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        Automatic::with_handle(Vec::new()),
        library,
    );
    let outcome = runner
        .run(Environment::new())
        .expect("run");
    match outcome {
        Conclusion::Completed(Outcome::Done(_)) => {}
        other => panic!("expected Done, got {:?}", other),
    }
    let trail = String::from_utf8(
        runner
            .into_driver()
            .into_output(),
    )
    .expect("utf8");
    assert!(trail.contains("→ check:/1 ✓"));
    assert!(trail.contains("→ check:/2 ⊘"));
    assert!(trail.contains("↙ check: ✓"));
}

#[test]
fn automatic_failing_exec_fails_run_and_continues() {
    // A non-zero exec exit settles its step Fail and, with no user present to
    // overrule it, rolls the autonomous run up to Failed; the walk still
    // continues to the sibling below rather than aborting at the failure.
    let source = r#"
% technique v1

check :

1.  Run a failing command { exec("exit 3") }

2.  This step is still reached
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parsed");
    let mut program = translate(&document).expect("translated");
    resolve(&mut program).expect("resolve");
    let mut library = Library::core();
    library.extend(Library::system());
    crate::linking::link(&mut program, &library).expect("linked");

    let mut fixture = StoreFixture::new("automatic-failing-exec");
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        Automatic::with_handle(Vec::new()),
        library,
    );
    let outcome = runner
        .run(Environment::new())
        .expect("run");
    match outcome {
        Conclusion::Completed(Outcome::Fail(_)) => {}
        other => panic!("expected Failed, got {:?}", other),
    }

    let pfftt = fixture.pfftt_contents();
    let records: Vec<_> = pfftt
        .lines()
        .filter_map(|line| parse_record(line).ok())
        .collect();
    let step_one_failed = records
        .iter()
        .any(|r| {
            if let State::Fail(_) = r.state {
                r.path == "/check:/1"
            } else {
                false
            }
        });
    assert!(step_one_failed, "step 1 should record Fail");
    assert!(
        records
            .iter()
            .any(|r| r.path == "/check:/2")
    );
}

const ONE_FAILED_STEP: &str = r#"
% technique v1

check :

1.  A step the user fails
        "#;

#[test]
fn interactive_override_severs_the_rollup_to_done() {
    // The user fails the step, then deliberately Overrides the procedure's
    // sign-off: the override settles it Done, severing the rollup so the failed
    // child does not propagate. Only an interactive run can do this.
    let source = ONE_FAILED_STEP.trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parsed");
    let mut program = translate(&document).expect("translated");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("interactive-override");
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        Mock::with_answers([UserInput::Fail("not done".to_string()), UserInput::Override]),
        Library::stub(),
    );
    let outcome = runner
        .run(Environment::new())
        .expect("run");
    match outcome {
        Conclusion::Completed(Outcome::Done(_)) => {}
        other => panic!("expected Done after override, got {:?}", other),
    }
}

#[test]
fn interactive_accepting_a_failure_propagates() {
    // Without an Override, the failed step stands: the procedure rolls up to
    // Failed even with the user at the controls.
    let source = ONE_FAILED_STEP.trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parsed");
    let mut program = translate(&document).expect("translated");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("interactive-propagate");
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        // The step's Fail, then the sign-off accepts the standing failure.
        Mock::with_answers([
            UserInput::Fail("not done".to_string()),
            UserInput::Fail(String::new()),
        ]),
        Library::stub(),
    );
    let outcome = runner
        .run(Environment::new())
        .expect("run");
    match outcome {
        Conclusion::Completed(Outcome::Fail(_)) => {}
        other => panic!("expected Failed, got {:?}", other),
    }
}

#[test]
fn description_instruction_records_under_step_zero() {
    // An exec in the procedure description runs in the anonymous step-0
    // Prologue scope, recording under the `/0` path ahead of the steps.
    let source = r#"
% technique v1

check :

Prepare the ground { exec("true") } before the steps.

1.  Do the work.
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parsed");
    let mut program = translate(&document).expect("translated");
    resolve(&mut program).expect("resolve");
    let mut library = Library::core();
    library.extend(Library::system());
    crate::linking::link(&mut program, &library).expect("linked");

    let mut fixture = StoreFixture::new("description-step-zero");
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        Automatic::with_handle(Vec::new()),
        library,
    );
    runner
        .run(Environment::new())
        .expect("run");

    let pfftt = fixture.pfftt_contents();
    let records: Vec<_> = pfftt
        .lines()
        .filter_map(|line| parse_record(line).ok())
        .collect();
    let zero: Vec<_> = records
        .iter()
        .filter(|r| r.path == "/check:/0")
        .map(|r| &r.state)
        .collect();
    // The exec runs (its record between Begin and the outcome); the prologue
    // holds real work, so it records that work's outcome — Done — rather than
    // being stamped Skip by its prose tail.
    let State::Begin(_) = zero[0] else {
        panic!("expected Begin first at /check:/0, got {:?}", zero[0]);
    };
    let State::Done(_) = zero[zero.len() - 1] else {
        panic!(
            "expected Done last at /check:/0, got {:?}",
            zero[zero.len() - 1]
        );
    };
    assert!(
        zero.iter()
            .any(|state| {
                if let State::Execute { .. } = state {
                    true
                } else {
                    false
                }
            })
    );
}

#[test]
fn loop_inside_step_produces_one_result() {
    let mut fixture = StoreFixture::new("loop-in-step");

    // A Step whose body contains a Loop over an empty list. The Loop
    // announces and walks its body zero times, recording nothing; the
    // enclosing Step records exactly one Result.
    let loop_op = Operation::Loop {
        names: &[],
        over: Some(Box::new(Operation::Variable(
            Identifier::new("empty"),
            language::Span::default(),
        ))),
        body: Box::new(Operation::Sequence(vec![], language::Span::default())),
        responses: Vec::new(),
        span: language::Span::default(),
    };
    let the_step = Operation::Step {
        ordinal: Ordinal::Dependent("1"),
        attributes: Vec::new(),
        source: scope_for(Ordinal::Dependent("1")),
        body: Box::new(loop_op),
        responses: Vec::new(),
        span: language::Span::default(),
    };
    let body = Operation::Sequence(vec![the_step], language::Span::default());
    let program = anonymous_with_body(body);

    let mut env = Environment::new();
    env.extend("empty".to_string(), Value::Arraeum(Vec::new()));
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(env)
        .expect("run");

    // One Start record, then the enclosing step's Begin and Done, then the
    // closing Finish — the Loop inside the step body does not record events of
    // its own.
    let pfftt = fixture.pfftt_contents();
    let lines: Vec<&str> = pfftt
        .lines()
        .filter(|line| {
            !line
                .trim()
                .is_empty()
        })
        .collect();
    assert_eq!(lines.len(), 6);
    // The step reads the collection its Loop iterates, so its Begin states it.
    assert!(lines[2].ends_with(" Begin ( [] ~ empty )"), "{}", lines[2]);
    assert!(lines[3].contains(" Done"));
    assert!(lines[5].ends_with(" Finish"));
}

#[test]
fn repeat_loops_until_quit() {
    let mut fixture = StoreFixture::new("repeat-until-quit");

    // A `repeat` whose body is a single step. Each pass walks the step with
    // a distinct `[n]` iteration segment; the user quits on the third
    // pass, ending the loop.
    let inner = Operation::Step {
        ordinal: Ordinal::Dependent("1"),
        attributes: Vec::new(),
        source: scope_for(Ordinal::Dependent("1")),
        body: Box::new(Operation::Sequence(Vec::new(), language::Span::default())),
        responses: Vec::new(),
        span: language::Span::default(),
    };
    let loop_op = Operation::Loop {
        names: &[],
        over: None,
        body: Box::new(Operation::Sequence(vec![inner], language::Span::default())),
        responses: Vec::new(),
        span: language::Span::default(),
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
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
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
    let source_paragraphs = vec![language::Paragraph::new(vec![
        language::Descriptive::CodeInline(vec![language::Expression::Variable(
            Identifier::new("item"),
            language::Span::default(),
        )]),
    ])];
    let substep = Operation::Step {
        ordinal: Ordinal::Dependent("a"),
        attributes: Vec::new(),
        source: scope_with(Ordinal::Dependent("a"), source_paragraphs),
        body: Box::new(Operation::Sequence(Vec::new(), language::Span::default())),
        responses: Vec::new(),
        span: language::Span::default(),
    };
    // `names` borrows from the IR, so the array must outlive the program.
    let names = [Identifier::new("item")];
    let loop_op = Operation::Loop {
        names: &names,
        over: Some(Box::new(Operation::Variable(
            Identifier::new("items"),
            language::Span::default(),
        ))),
        body: Box::new(Operation::Sequence(
            vec![substep],
            language::Span::default(),
        )),
        responses: Vec::new(),
        span: language::Span::default(),
    };
    let mut sub = Subroutine::anonymous();
    sub.body = loop_op;
    let mut program = Program::new();
    program
        .subroutines
        .push(sub);

    // A string and a number, so the echo pins both: a string value shows
    // quoted, a number bare.
    let mut env = Environment::new();
    env.extend(
        "items".to_string(),
        Value::Arraeum(vec![
            Value::Literali("first".to_string()),
            Value::Quanticle(crate::value::Numeric::Integral(5)),
        ]),
    );

    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(env)
        .expect("run");

    // The body is walked once per element. Each Step event carries an
    // `[n]` iteration segment in its path and the description it saw,
    // confirming the iteration variable was bound to that element.
    let prompt = runner.into_driver();
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
    assert_eq!(
        steps,
        vec![("/[1]/a", "a.  \"first\""), ("/[2]/a", "a.  5")]
    );

    // Each iteration's descent echoes the loop variable bound for that pass,
    // in the same `value ~ name` form a procedure call's arguments use.
    let enters: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|event| match event {
            Event::Enter { qualified } => Some(qualified.as_str()),
            _ => None,
        })
        .collect();
    assert_eq!(enters, vec!["/[1] (\"first\" ~ item)", "/[2] (5 ~ item)"]);
}

#[test]
fn foreach_over_seq_builtin_runs() {
    let mut fixture = StoreFixture::new("foreach-seq");

    // The iterable is the result of the `seq` builtin rather than a seeded
    // env binding, exercising the evaluator's Execute dispatch end to end.
    let library = Library::core();
    let seq = library
        .resolve("seq")
        .expect("seq registered");

    let source_paragraphs = vec![language::Paragraph::new(vec![
        language::Descriptive::CodeInline(vec![language::Expression::Variable(
            Identifier::new("n"),
            language::Span::default(),
        )]),
    ])];
    let substep = Operation::Step {
        ordinal: Ordinal::Dependent("a"),
        attributes: Vec::new(),
        source: scope_with(Ordinal::Dependent("a"), source_paragraphs),
        body: Box::new(Operation::Sequence(Vec::new(), language::Span::default())),
        responses: Vec::new(),
        span: language::Span::default(),
    };
    let names = [Identifier::new("n")];
    let over = Operation::Execute(
        Executable {
            target: ExecutableRef::Resolved(seq),
            arguments: vec![
                Operation::Number(LangNumeric::Integral(1), language::Span::default()),
                Operation::Number(LangNumeric::Integral(3), language::Span::default()),
            ],
        },
        language::Span::default(),
    );
    let loop_op = Operation::Loop {
        names: &names,
        over: Some(Box::new(over)),
        body: Box::new(Operation::Sequence(
            vec![substep],
            language::Span::default(),
        )),
        responses: Vec::new(),
        span: language::Span::default(),
    };
    let mut sub = Subroutine::anonymous();
    sub.body = loop_op;
    let mut program = Program::new();
    program
        .subroutines
        .push(sub);

    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        library,
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
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
    // seq(1, 3) yields [1, 2, 3]; the body walks once per element with `n`
    // bound to each in turn.
    assert_eq!(
        steps,
        vec![
            ("/[1]/a", "a.  1"),
            ("/[2]/a", "a.  2"),
            ("/[3]/a", "a.  3"),
        ]
    );
}

#[test]
fn foreach_destructures_tuple_elements() {
    let mut fixture = StoreFixture::new("foreach-destructure");

    // foreach (first, second) in pairs: two names destructure each
    // tuple-shaped element positionally.
    let source_paragraphs = vec![language::Paragraph::new(vec![
        language::Descriptive::CodeInline(vec![language::Expression::Variable(
            Identifier::new("first"),
            language::Span::default(),
        )]),
        language::Descriptive::Text("/"),
        language::Descriptive::CodeInline(vec![language::Expression::Variable(
            Identifier::new("second"),
            language::Span::default(),
        )]),
    ])];
    let substep = Operation::Step {
        ordinal: Ordinal::Dependent("a"),
        attributes: Vec::new(),
        source: scope_with(Ordinal::Dependent("a"), source_paragraphs),
        body: Box::new(Operation::Sequence(Vec::new(), language::Span::default())),
        responses: Vec::new(),
        span: language::Span::default(),
    };
    // `names` borrows from the IR, so the array must outlive the program.
    let names = [Identifier::new("first"), Identifier::new("second")];
    let loop_op = Operation::Loop {
        names: &names,
        over: Some(Box::new(Operation::Variable(
            Identifier::new("pairs"),
            language::Span::default(),
        ))),
        body: Box::new(Operation::Sequence(
            vec![substep],
            language::Span::default(),
        )),
        responses: Vec::new(),
        span: language::Span::default(),
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
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
    let steps: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|event| match event {
            Event::Step { description, .. } => Some(description.as_str()),
            _ => None,
        })
        .collect();
    assert_eq!(steps, vec!["a.  \"a\" / \"b\"", "a.  \"c\" / \"d\""]);
}

#[test]
fn foreach_widens_primitive_to_singleton() {
    let mut fixture = StoreFixture::new("foreach-widen");

    // foreach item in source, where `source` is a bare scalar: it widens
    // to a one-element list and the body walks exactly once.
    let source_paragraphs = vec![language::Paragraph::new(vec![
        language::Descriptive::CodeInline(vec![language::Expression::Variable(
            Identifier::new("item"),
            language::Span::default(),
        )]),
    ])];
    let substep = Operation::Step {
        ordinal: Ordinal::Dependent("a"),
        attributes: Vec::new(),
        source: scope_with(Ordinal::Dependent("a"), source_paragraphs),
        body: Box::new(Operation::Sequence(Vec::new(), language::Span::default())),
        responses: Vec::new(),
        span: language::Span::default(),
    };
    let names = [Identifier::new("item")];
    let loop_op = Operation::Loop {
        names: &names,
        over: Some(Box::new(Operation::Variable(
            Identifier::new("source"),
            language::Span::default(),
        ))),
        body: Box::new(Operation::Sequence(
            vec![substep],
            language::Span::default(),
        )),
        responses: Vec::new(),
        span: language::Span::default(),
    };
    let mut sub = Subroutine::anonymous();
    sub.body = loop_op;
    let mut program = Program::new();
    program
        .subroutines
        .push(sub);

    let mut env = Environment::new();
    env.extend("source".to_string(), Value::Literali("lonely".to_string()));

    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
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
    assert_eq!(steps, vec![("/[1]/a", "a.  \"lonely\"")]);
}

#[test]
fn foreach_over_unit_iterates_nothing() {
    // foreach item in source, where `source` is Unit (the value of an empty
    // sequence or a pure-prose step). Unit is the absence of a value, so the
    // loop iterates over nothing: the body never runs and the run completes.
    let names = [Identifier::new("item")];
    let substep = step(
        Ordinal::Dependent("a"),
        Operation::Sequence(Vec::new(), language::Span::default()),
    );
    let loop_op = Operation::Loop {
        names: &names,
        over: Some(Box::new(Operation::Variable(
            Identifier::new("source"),
            language::Span::default(),
        ))),
        body: Box::new(Operation::Sequence(
            vec![substep],
            language::Span::default(),
        )),
        responses: Vec::new(),
        span: language::Span::default(),
    };
    let mut sub = Subroutine::anonymous();
    sub.body = loop_op;
    let mut program = Program::new();
    program
        .subroutines
        .push(sub);

    let mut fixture = StoreFixture::new("foreach-unit");
    let mut env = Environment::new();
    env.extend("source".to_string(), Value::Unitus);

    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        Mock::new(),
        Library::stub(),
    );
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
    let ran = prompt
        .events()
        .iter()
        .any(|event| match event {
            Event::Step { .. } => true,
            _ => false,
        });
    assert!(!ran, "loop body must not run when iterating Unit");
}

#[test]
fn foreach_over_non_list_errors_unbound_is_empty() {
    // foreach item in source, where `source` is supplied by the caller's
    // environment. A tuple or tablet source is `NotIterable` (lists iterate
    // and scalars widen, but a tablet is a record that must be projected via
    // values()/labels()/pairs() first, and a tuple does neither). An unbound
    // `source` iterates nothing rather than aborting: a statically undefined
    // name is caught earlier by resolution, so a name unbound at runtime is one
    // a zero-iteration or skipped loop never populated.
    let names = [Identifier::new("item")];
    let loop_op = Operation::Loop {
        names: &names,
        over: Some(Box::new(Operation::Variable(
            Identifier::new("source"),
            language::Span::default(),
        ))),
        body: Box::new(Operation::Sequence(Vec::new(), language::Span::default())),
        responses: Vec::new(),
        span: language::Span::default(),
    };
    let mut sub = Subroutine::anonymous();
    sub.body = loop_op;
    let mut program = Program::new();
    program
        .subroutines
        .push(sub);

    // A tuple bound to `source` is not a list and does not widen.
    let mut tuple_fixture = StoreFixture::new("foreach-tuple");
    let mut env = Environment::new();
    env.extend(
        "source".to_string(),
        Value::Parametriq(vec![
            Value::Literali("a".to_string()),
            Value::Literali("b".to_string()),
        ]),
    );
    let mut runner = Runner::new(
        &program,
        tuple_fixture.take_appender(),
        Ledger::new(),
        Mock::new(),
        Library::stub(),
    );
    match runner.run(env) {
        Err(RunnerError::NotIterable) => {}
        other => panic!("expected NotIterable, got {:?}", other),
    }

    // A tablet bound to `source` is a record, not a sequence: it must be
    // projected with values()/labels()/pairs() rather than iterated directly.
    let mut tablet_fixture = StoreFixture::new("foreach-tablet");
    let mut env = Environment::new();
    env.extend(
        "source".to_string(),
        Value::Tabularum(vec![(
            "label".to_string(),
            Value::Literali("v".to_string()),
        )]),
    );
    let mut runner = Runner::new(
        &program,
        tablet_fixture.take_appender(),
        Ledger::new(),
        Mock::new(),
        Library::stub(),
    );
    match runner.run(env) {
        Err(RunnerError::NotIterable) => {}
        other => panic!("expected NotIterable, got {:?}", other),
    }

    // An unbound `source` iterates nothing: the run completes and the loop
    // body never executes.
    let mut unbound_fixture = StoreFixture::new("foreach-unbound");
    let mut runner = Runner::new(
        &program,
        unbound_fixture.take_appender(),
        Ledger::new(),
        Mock::new(),
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");
    let ran = runner
        .into_driver()
        .events()
        .iter()
        .any(|event| match event {
            Event::Step { .. } => true,
            _ => false,
        });
    assert!(!ran, "loop body must not run when source is unbound");
}

#[test]
fn cost_of_non_quantity_value_errors_invalid_cost() {
    // $(...) constructs an Intratempse from its inner expression's value; a
    // bare string is not a quantity, so it cannot become a cost.
    let cost_op = Operation::Cost(
        Box::new(Operation::String(
            vec![Fragment::Text("not a quantity")],
            language::Span::default(),
        )),
        language::Span::default(),
    );
    let mut sub = Subroutine::anonymous();
    sub.body = cost_op;
    let mut program = Program::new();
    program
        .subroutines
        .push(sub);

    let mut fixture = StoreFixture::new("cost-non-quantity");
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        Mock::new(),
        Library::stub(),
    );
    match runner.run(Environment::new()) {
        Err(RunnerError::InvalidCost) => {}
        other => panic!("expected InvalidCost, got {:?}", other),
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
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");
    let args = ["foo".to_string(), "192.168.1.5".to_string()];
    let env = bind_parameters(&program, &args).expect("bind");
    assert_eq!(env.lookup("e"), Some(&Value::Literali("foo".to_string())));
    assert_eq!(
        env.lookup("s"),
        Some(&Value::Literali("192.168.1.5".to_string()))
    );

    // Too few arguments: ParameterArityMismatch names procedure and parameters.
    let args = ["foo".to_string()];
    let error = bind_parameters(&program, &args).expect_err("expected arity error");
    let RunnerError::ParameterArityMismatch {
        procedure,
        parameters,
        actual,
    } = error
    else {
        panic!("expected ParameterArityMismatch, got {:?}", error);
    };
    assert_eq!(procedure, "connectivity_check");
    assert_eq!(parameters, vec!["e".to_string(), "s".to_string()]);
    assert_eq!(actual, 1);

    // Too many arguments: also ParameterArityMismatch.
    let args = [
        "foo".to_string(),
        "192.168.1.5".to_string(),
        "extra".to_string(),
    ];
    let error = bind_parameters(&program, &args).expect_err("expected arity error");
    let RunnerError::ParameterArityMismatch {
        parameters, actual, ..
    } = error
    else {
        panic!("expected ParameterArityMismatch, got {:?}", error);
    };
    assert_eq!(parameters.len(), 2);
    assert_eq!(actual, 3);

    // With a signature, parameters are described as `name : Type`.
    let source = r#"
% technique v1

connectivity_check(e, s) : LocalEnvironment, TargetService -> NetworkHealth

1.  step
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");
    let error = bind_parameters(&program, &[]).expect_err("expected arity error");
    let RunnerError::ParameterArityMismatch { parameters, .. } = error else {
        panic!("expected ParameterArityMismatch, got {:?}", error);
    };
    assert_eq!(
        parameters,
        vec![
            "e : LocalEnvironment".to_string(),
            "s : TargetService".to_string()
        ]
    );

    // Procedure declares no parameters but args supplied: ParameterUnexpected.
    let source = r#"
% technique v1

test :

1.  step
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");
    let args = ["unwanted".to_string()];
    let error = bind_parameters(&program, &args).expect_err("expected unexpected error");
    let RunnerError::ParameterUnexpected { procedure, actual } = error else {
        panic!("expected ParameterUnexpected, got {:?}", error);
    };
    assert_eq!(procedure, "test");
    assert_eq!(actual, 1);

    // No parameters and no args: empty environment, no error.
    let env = bind_parameters(&program, &[]).expect("bind");
    assert!(
        env.lookup("anything")
            .is_none()
    );
}

#[test]
fn list_argument_binds_as_a_list() {
    let source = r#"
% technique v1

sweep(regions) :

1.  step
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    // A bracketed argument binds as a list.
    let args = ["[sydney, hobart]".to_string()];
    let env = bind_parameters(&program, &args).expect("bind");
    assert_eq!(
        env.lookup("regions"),
        Some(&Value::Arraeum(vec![
            Value::Literali("sydney".to_string()),
            Value::Literali("hobart".to_string()),
        ]))
    );

    // Text that was never a list stays text, brackets being the cue.
    let args = ["sydney".to_string()];
    let env = bind_parameters(&program, &args).expect("bind");
    assert_eq!(
        env.lookup("regions"),
        Some(&Value::Literali("sydney".to_string()))
    );

    // An argument that reads as a list but doesn't parse is rejected rather
    // than silently taken as text: here the quote is unbalanced.
    let args = [r#"["Sydney, NSW]"#.to_string()];
    let error = bind_parameters(&program, &args).expect_err("expected malformed error");
    let RunnerError::MalformedArgument {
        parameter,
        argument,
    } = error
    else {
        panic!("expected MalformedArgument, got {:?}", error);
    };
    assert_eq!(parameter, "regions");
    assert_eq!(argument, r#"["Sydney, NSW]"#);
}

#[test]
fn argument_echo_binds_each_parameter() {
    let source = r#"
% technique v1

connectivity_check(e, s, address) :

1.  step
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");
    let args = [
        "[]".to_string(),
        "0".to_string(),
        "10 Downing Street".to_string(),
    ];
    let env = bind_parameters(&program, &args).expect("bind");
    let params = program
        .subroutines
        .first()
        .unwrap()
        .parameters
        .unwrap();
    let echo = render_argument_echo(params, &env);
    assert_eq!(echo, "([] ~ e, 0 ~ s, \"10 Downing Street\" ~ address)");
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
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("entry-param-interpolate");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut env = Environment::new();
    env.extend("name".to_string(), Value::Literali("world".to_string()));
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(env)
        .expect("run");

    let prompt = runner.into_driver();
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
    assert_eq!(descriptions, vec!["1.  Hello \"world\""]);
}

#[test]
fn entry_procedure_description_displayed_intact() {
    let source = r#"
% technique v1

make_coffee :

Brew using { 42 ~ water } then serve it hot.

1.  Pour
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("entry-description-intact");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    // The description renders from its source paragraph: the binding hoisted
    // into the implicit step 0 still runs, but the prose is shown whole, with
    // the binding syntax as-written.
    let prompt = runner.into_driver();
    let displayed: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Display(content) = e {
                Some(content.as_str())
            } else {
                None
            }
        })
        .collect();
    assert!(displayed.contains(&"Brew using { 42 ~ water } then serve it hot."));
}

#[test]
fn metadata_header_displayed_as_prelude() {
    let source = r#"
% technique v1
! MIT; © 2026 ACME
& procedure

make_coffee :

1.  Pour
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("metadata-prelude");
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    // The header is shown once, before the entry procedure's declaration.
    let prompt = runner.into_driver();
    let first = prompt
        .events()
        .iter()
        .find_map(|e| {
            if let Event::Display(content) = e {
                Some(content.as_str())
            } else {
                None
            }
        })
        .expect("a Display event");
    assert_eq!(first, "% technique v1\n! MIT; © 2026 ACME\n& procedure");
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
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("step-responses");
    let prompt = Mock::with_answers([UserInput::Done(Value::Literali("Yes".to_string()))]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    runner
        .run(env)
        .expect("run");

    // The prompt offered the two declared responses as choices.
    let prompt = runner.into_driver();
    let asked: Vec<&Vec<String>> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Ask {
                marker, choices, ..
            } = e
            {
                if marker == "→" { Some(choices) } else { None }
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
    // Start, the entry `Begin`, the step `Begin`, then the step's outcome.
    let record = parse_record(lines[3]).expect("parse record");
    assert_eq!(
        record.state,
        State::Done(Some(Value::Literali("Yes".to_string())))
    );
}

#[test]
fn automatic_records_done_for_computable_step_skip_for_prose() {
    fn record_of(label: &str, body: Operation<'static>) -> (Conclusion, State) {
        let mut fixture = StoreFixture::new(label);
        let program = anonymous_with_body(Operation::Sequence(
            vec![step(Ordinal::Dependent("1"), body)],
            language::Span::default(),
        ));
        let mut runner = Runner::new(
            &program,
            fixture.take_appender(),
            Ledger::new(),
            Automatic::with_handle(Vec::new()),
            Library::stub(),
        );
        let outcome = runner
            .run(Environment::new())
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
        let state = parse_record(lines[3])
            .expect("parse record")
            .state;
        (outcome, state)
    }

    // A single-line value computes and records Done with the literal.
    let (outcome, state) = record_of(
        "automatic-records-value",
        Operation::String(
            vec![Fragment::Text("probe output")],
            language::Span::default(),
        ),
    );
    assert_eq!(
        outcome,
        Conclusion::Completed(Outcome::Done(Value::Literali("probe output".to_string())))
    );
    assert_eq!(
        state,
        State::Done(Some(Value::Literali("probe output".to_string())))
    );

    // Multi-line text propagates intact and records faithfully; the codec
    // escapes the newlines so the value stays on one record line.
    let (outcome, state) = record_of(
        "multiline-records-value",
        Operation::String(
            vec![Fragment::Text("1: lo\n2: eth0\n3: wlan0")],
            language::Span::default(),
        ),
    );
    assert_eq!(
        outcome,
        Conclusion::Completed(Outcome::Done(Value::Literali(
            "1: lo\n2: eth0\n3: wlan0".to_string()
        )))
    );
    assert_eq!(
        state,
        State::Done(Some(Value::Literali(
            "1: lo\n2: eth0\n3: wlan0".to_string()
        )))
    );

    // A pure-prose step (empty body) has nothing to compute and records Skip.
    let (_, state) = record_of(
        "automatic-empty-body",
        Operation::Sequence(vec![], language::Span::default()),
    );
    assert_eq!(state, State::Skip);
}

#[test]
fn sequence_value_is_last_member() {
    // A body sequence takes the last member's value, not the first or a fold.
    let mut fixture = StoreFixture::new("sequence-last-member");
    let body = Operation::Sequence(
        vec![
            step(
                Ordinal::Dependent("1"),
                Operation::String(vec![Fragment::Text("first")], language::Span::default()),
            ),
            step(
                Ordinal::Dependent("2"),
                Operation::String(vec![Fragment::Text("second")], language::Span::default()),
            ),
        ],
        language::Span::default(),
    );
    let program = anonymous_with_body(body);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        Automatic::with_handle(Vec::new()),
        Library::stub(),
    );
    let outcome = runner
        .run(Environment::new())
        .expect("run");
    assert_eq!(
        outcome,
        Conclusion::Completed(Outcome::Done(Value::Literali("second".to_string())))
    );

    let pfftt = fixture.pfftt_contents();
    let dones = pfftt
        .lines()
        .filter_map(|line| parse_record(line).ok())
        .filter(|record| {
            if let State::Done(_) = record.state {
                true
            } else {
                false
            }
        })
        .count();
    assert_eq!(dones, 3);
}

#[test]
fn deferred_invoke_is_prompted_and_recorded() {
    // A call to an external procedure this run cannot resolve (it lives in
    // another document or system) is presented for the user to settle: the
    // run does not descend into it, but the user can mark it Done (it was
    // performed, or recorded elsewhere), Skip, or Fail. The call site and the
    // settled outcome are both recorded.
    fn deferred_program() -> Program<'static> {
        let external = language::External {
            value: "https://example.com/probe",
            span: language::Span::default(),
        };
        let invoke = Operation::Invoke(
            Invocable {
                target: SubroutineRef::Deferred(external),
                arguments: Vec::new(),
                elided: true,
            },
            language::Span::default(),
        );
        anonymous_with_body(Operation::Sequence(vec![invoke], language::Span::default()))
    }

    // The user confirms the departure, then marks the external procedure
    // Done at the return.
    let mut fixture = StoreFixture::new("deferred-done");
    let program = deferred_program();
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    let outcome = runner
        .run(Environment::new())
        .expect("run");
    assert_eq!(outcome, Conclusion::Completed(Outcome::Done(Value::Unitus)));

    // The user was prompted about the external node, by its FQP.
    let prompt = runner.into_driver();
    let asked: Vec<&str> = prompt
        .events()
        .iter()
        .filter_map(|event| match event {
            Event::External { qualified } => Some(qualified.as_str()),
            _ => None,
        })
        .collect();
    assert_eq!(asked, vec!["/<https://example.com/probe>"]);

    // The journal records the Invoke call site at the caller's path and the
    // Done outcome at the external's FQP.
    let pfftt = fixture.pfftt_contents();
    let records: Vec<(String, State)> = pfftt
        .lines()
        .filter_map(|line| parse_record(line).ok())
        .map(|record| (record.path, record.state))
        .collect();
    assert!(records.contains(&(
        "/".to_string(),
        State::Invoke(InvokeTarget::Uri("https://example.com/probe".to_string()))
    )));
    assert!(records.contains(&(
        "/<https://example.com/probe>".to_string(),
        State::Done(Some(Value::Unitus))
    )));

    // The user declines: Skip is recorded at the external's FQP. (The
    // enclosing sequence proceeds and returns its last Done value, so the
    // run's overall outcome is not itself the Skip — that is walk_sequence's
    // concern, tested elsewhere; what matters here is the recorded Skip.)
    let mut fixture = StoreFixture::new("deferred-skip");
    let program = deferred_program();
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus), UserInput::Skip]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");
    let pfftt = fixture.pfftt_contents();
    let settled: Vec<State> = pfftt
        .lines()
        .filter_map(|line| parse_record(line).ok())
        .filter(|record| record.path == "/<https://example.com/probe>")
        .map(|record| record.state)
        .collect();
    assert_eq!(settled, vec![State::Begin(Vec::new()), State::Skip]);

    // Under an automatic run there is no user to attest the external work
    // and nothing executed it, so it records Skip rather than a fabricated Done.
    let mut fixture = StoreFixture::new("deferred-automatic");
    let program = deferred_program();
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        Automatic::with_handle(Vec::new()),
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");
    let pfftt = fixture.pfftt_contents();
    let settled: Vec<State> = pfftt
        .lines()
        .filter_map(|line| parse_record(line).ok())
        .filter(|record| record.path == "/<https://example.com/probe>")
        .map(|record| record.state)
        .collect();
    assert_eq!(settled, vec![State::Begin(Vec::new()), State::Skip]);
}

#[test]
fn descriptive_binding_acquires_list_for_foreach() {
    // A descriptive `~ items` binding has no executable value, so the user
    // is asked to supply it. They enter a `[ … ]` literal, which coerces to a
    // list, and the following foreach walks its body once per element.
    let source = r#"
% technique v1

cleanup :

    1.  enumerate things ~ items
    2.  { foreach item in items }
        -   handle { item }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("descriptive-binding-acquire");
    // The acquire for `items` doubles as step 1's completion; step 2 and its
    // two substeps then take their verdicts.
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Literali(r#"["east", "west"]"#.to_string())),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let prompt = runner.into_driver();
    let acquired: Vec<Option<&str>> = prompt
        .events()
        .iter()
        .filter_map(|event| {
            if let Event::Acquire { name, .. } = event {
                Some(
                    name.as_ref()
                        .map(String::as_str),
                )
            } else {
                None
            }
        })
        .collect();
    assert_eq!(acquired, vec![Some("items")]);

    let substeps = prompt
        .events()
        .iter()
        .filter(|event| {
            if let Event::Step { description, .. } = event {
                description.contains("handle")
            } else {
                false
            }
        })
        .count();
    assert_eq!(substeps, 2);
}

#[test]
fn descriptive_tuple_binding_acquires_each_name() {
    // A descriptive `~ (a, b)` binding has no executable value, so each name is
    // solicited in turn; the following step reads both, so the run only
    // completes if both were bound.
    let source = r#"
% technique v1

task :

1.  Lookup account details ~ (account_number, account_name)
2.  Use { account_number } and { account_name }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("descriptive-tuple-acquire");
    // Two acquires (account_number, then account_name); the second doubles as
    // step 1's completion, then step 2 takes its verdict.
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Literali("12345".to_string())),
        UserInput::Done(Value::Literali("Acme".to_string())),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let prompt = runner.into_driver();
    let acquired: Vec<Option<&str>> = prompt
        .events()
        .iter()
        .filter_map(|event| {
            if let Event::Acquire { name, .. } = event {
                Some(
                    name.as_ref()
                        .map(String::as_str),
                )
            } else {
                None
            }
        })
        .collect();
    assert_eq!(acquired, vec![Some("account_number"), Some("account_name")]);
}

#[test]
fn descriptive_binding_settles_without_a_second_prompt() {
    // `Do something ~ answer` solicits the value once at its acquire prompt;
    // that input is the step's verdict, so the step settles Done with the
    // acquired value rather than asking a redundant second time. The plain step
    // that follows is the only one to take an `ask`.
    let source = r#"
% technique v1

task :

    1.  Do something ~ answer
    2.  Then check the result
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("descriptive-binding-settles");
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Literali("42".to_string())),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let driver = runner.into_driver();
    let acquires = driver
        .events()
        .iter()
        .filter(|event| {
            if let Event::Acquire { .. } = event {
                true
            } else {
                false
            }
        })
        .count();
    let asks = driver
        .events()
        .iter()
        .filter(|event| {
            if let Event::Ask { marker, .. } = event {
                marker == "→"
            } else {
                false
            }
        })
        .count();
    // One acquire (for `answer`) and one ask (for step 2 only): the binding
    // step never reaches an `ask`.
    assert_eq!(acquires, 1);
    assert_eq!(asks, 1);

    // A binding does not have the value it captures, so the step settles
    // `Done ()` and the acquired value is reachable only on its `Bind`.
    let pfftt = fixture.pfftt_contents();
    let records: Vec<_> = pfftt
        .lines()
        .filter_map(|line| parse_record(line).ok())
        .filter(|record| {
            record
                .path
                .ends_with("/1")
        })
        .collect();

    let bind = records
        .iter()
        .find_map(|record| {
            if let State::Bind(bound) = &record.state {
                Some(bound)
            } else {
                None
            }
        })
        .expect("step 1 recorded Bind");
    assert_eq!(bind.len(), 1);
    assert_eq!(bind[0].name, Some("answer".to_string()));
    assert_eq!(bind[0].value, Value::Literali("42".to_string()));

    let done = records
        .iter()
        .find_map(|record| {
            if let State::Done(value) = &record.state {
                Some(value)
            } else {
                None
            }
        })
        .expect("step 1 recorded Done");
    assert_eq!(*done, Some(Value::Unitus));
}

#[test]
fn response_choice_binds_to_the_step_variable() {
    // A step carrying both a descriptive binding and response choices takes its
    // value from the chosen response: the menu is the only prompt (no separate
    // acquire), and the choice binds to the variable for later steps to read.
    let source = r#"
% technique v1

task :

    1.  Is it working ~ answer
        'Yes' | 'No'
    2.  You said { answer }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("response-binds-variable");
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Literali("Yes".to_string())),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let driver = runner.into_driver();
    // The response menu replaces the acquire: the binding step never prompts to
    // type a value.
    let acquires = driver
        .events()
        .iter()
        .filter(|event| {
            if let Event::Acquire { .. } = event {
                true
            } else {
                false
            }
        })
        .count();
    assert_eq!(acquires, 0);

    // Step 2 interpolates `answer`, proving the chosen response bound to it.
    let descriptions: Vec<&str> = driver
        .events()
        .iter()
        .filter_map(|event| {
            if let Event::Step { description, .. } = event {
                Some(description.as_str())
            } else {
                None
            }
        })
        .collect();
    assert_eq!(
        descriptions,
        vec!["1.  Is it working ~ answer", "2.  You said \"Yes\""]
    );
}

#[test]
fn resume_rehydrates_binding_made_inside_a_completed_loop() {
    // The DeleteAccount resume bug: a completed `foreach` step binds a variable
    // inside its loop body that a later `foreach` consumes. Skipping the
    // completed step wholesale lost that binding; resume must re-walk it so the
    // nested binding rehydrates from the recorded values, leaving the later
    // loop with something to iterate rather than an UnboundVariable.
    let source = r#"
% technique v1

cleanup :

    1.  enumerate things ~ items
    2.  { foreach item in items }
        -   identify the active things ~ seen
    3.  { foreach token in seen }
        -   record { token }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    // The prior run completed steps 1 and 2 (and 2's two iterations); only
    // step 3 remains. `items` was acquired as a two-element list; `seen` was
    // bound once per iteration inside the loop.
    let items = Value::Arraeum(vec![
        Value::Literali("a".to_string()),
        Value::Literali("b".to_string()),
    ]);
    let ledger = ledger_of(&[
        (1, "/cleanup:", State::Begin(Vec::new())),
        (2, "/cleanup:/1", State::Begin(Vec::new())),
        (2, "/cleanup:/1", bind_of("items", items.clone())),
        (2, "/cleanup:/1", State::Done(None)),
        (
            3,
            "/cleanup:/2",
            State::Begin(vec![Supplied {
                value: items,
                name: Some("items".to_string()),
            }]),
        ),
        (4, "/cleanup:/2/[1]/-1", State::Begin(Vec::new())),
        (
            4,
            "/cleanup:/2/[1]/-1",
            bind_of("seen", Value::Literali("x".to_string())),
        ),
        (4, "/cleanup:/2/[1]/-1", State::Done(None)),
        (5, "/cleanup:/2/[2]/-1", State::Begin(Vec::new())),
        (
            5,
            "/cleanup:/2/[2]/-1",
            bind_of("seen", Value::Literali("y".to_string())),
        ),
        (5, "/cleanup:/2/[2]/-1", State::Done(None)),
        (3, "/cleanup:/2", State::Done(None)),
    ]);

    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        ledger,
        Automatic::with_handle(Vec::new()),
        Library::stub(),
    );
    let outcome = runner
        .run(Environment::new())
        .expect("resume must not raise UnboundVariable");
    assert!(if let Conclusion::Completed(Outcome::Done(_)) = outcome {
        true
    } else {
        false
    });
}

// An elided invocation `<hail>` acquires its parameter `name` from the
// user. The argument it was called with is recorded as an `Input` at the
// callee's path so a resume can restore it.
// Fold a prior run's records into a Ledger the way `Store::open` does, so a
// test can stand a resume on a journal it states rather than one it has to run.
fn ledger_of(records: &[(u32, &str, State)]) -> Ledger {
    let mut ledger = Ledger::new();
    for (serial, path, state) in records {
        ledger.apply(&Record {
            recorded: "2026-05-14T12:00:00Z".to_string(),
            run_id: RunId(1),
            serial: Serial(*serial),
            path: path.to_string(),
            state: state.clone(),
        });
    }
    ledger
}

fn bind_of(name: &str, value: Value) -> State {
    State::Bind(vec![Supplied {
        value,
        name: Some(name.to_string()),
    }])
}

const ACQUIRE_INPUT_SOURCE: &str = r#"
% technique v1

greet :

    1.  <hail>

hail(name) : Text -> ()

    1.  Say { name }
"#;

#[test]
fn invoke_records_supplied_input() {
    let source = ACQUIRE_INPUT_SOURCE.trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    // The user supplies "World" at the acquire prompt; the rest are step
    // and scope sign-offs.
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Literali("World".to_string())),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let journal = runner
        .into_appender()
        .contents()
        .to_string();
    assert!(
        journal.contains("/hail: Begin ( \"World\" ~ name )"),
        "journal was:\n{}",
        journal
    );
}

#[test]
fn resume_restores_invoke_input_without_reprompting() {
    let source = ACQUIRE_INPUT_SOURCE.trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    // The prior run recorded the argument acquired for `<hail>`. Resume with
    // that input preloaded: the acquire must not fire, and `name` is bound from
    // the record (so `{ name }` evaluates rather than raising UnboundVariable).
    let ledger = ledger_of(&[
        (1, "/greet:", State::Begin(Vec::new())),
        (2, "/greet:/1", State::Begin(Vec::new())),
        (
            3,
            "/hail:",
            State::Begin(vec![Supplied {
                value: Value::Literali("World".to_string()),
                name: Some("name".to_string()),
            }]),
        ),
    ]);

    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        ledger,
        prompt,
        Library::stub(),
    );
    let outcome = runner
        .run(Environment::new())
        .expect("resume runs without re-acquiring");
    assert!(if let Conclusion::Completed(Outcome::Done(_)) = outcome {
        true
    } else {
        false
    });

    let prompt = runner.into_driver();
    let acquired = prompt
        .events()
        .iter()
        .filter(|event| {
            if let Event::Acquire { .. } = event {
                true
            } else {
                false
            }
        })
        .count();
    assert_eq!(acquired, 0);
}

#[test]
fn iterated_binding_prompts_as_list() {
    // `regions` is bound by a descriptive step and then iterated by a foreach,
    // so resolution marks it `[*]` and the prompt carries that forma. An empty
    // answer (`[]`) iterates zero times, so the substep never runs.
    let source = r#"
% technique v1

sweep :

1.  enumerate the regions ~ regions
2.  { foreach region in regions }
    -   note { region }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("iterated-binding");
    // The acquire for `regions` doubles as step 1's completion; step 2's
    // foreach iterates zero times, so only its own verdict follows.
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Literali("[]".to_string())),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let prompt = runner.into_driver();
    let acquired: Vec<(Option<&str>, Option<&str>)> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Acquire { name, forma, .. } = e {
                Some((
                    name.as_ref()
                        .map(String::as_str),
                    forma
                        .as_ref()
                        .map(String::as_str),
                ))
            } else {
                None
            }
        })
        .collect();
    assert_eq!(acquired, vec![(Some("regions"), Some("[*]"))]);
}

#[test]
fn declared_list_parameter_prompts_bracketed() {
    // A procedure declaring a single list parameter `[Region]` is invoked with
    // a hole, so its argument is acquired at entry — and the forma renders
    // bracketed so the driver offers list entry.
    let source = r#"
% technique v1

main :

{
    <sweep>(?)
}

sweep(regions) : [Region] -> ()

1.  note { regions }
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut fixture = StoreFixture::new("declared-list-param");
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Literali("[]".to_string())),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        Ledger::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let prompt = runner.into_driver();
    let acquired: Vec<(Option<&str>, Option<&str>)> = prompt
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Acquire { name, forma, .. } = e {
                Some((
                    name.as_ref()
                        .map(String::as_str),
                    forma
                        .as_ref()
                        .map(String::as_str),
                ))
            } else {
                None
            }
        })
        .collect();
    assert_eq!(acquired, vec![(Some("regions"), Some("[Region]"))]);
}

/// A procedure invoked once per item of a `foreach` records its steps at one
/// lexical path for every invocation, so a resume cannot tell one instance's
/// work from another's. Stop partway through the first towel, resume, and the
/// second towel's steps must still be performed.
#[test]
fn resume_reenters_invoked_procedure_per_item() {
    let source = r#"
% technique v1

launder_towels :

    1.  Gather the towels { ["blue", "green"] ~ towels }
    2.  { foreach towel in towels }
        -   <dry_towel>(towel)

dry_towel(towel) : Towel -> ()

    1.  Hang { towel } on the line
    2.  Wait for the sun
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let first = {
        let mut runner = Runner::new(
            &program,
            Appender::memory(),
            Ledger::new(),
            Scripted::new([("/dry_towel:/2".to_string(), UserInput::Quit)]),
            Library::stub(),
        );
        let _ = runner.run(Environment::new());
        runner
            .into_appender()
            .contents()
            .to_string()
    };

    let mut ledger = Ledger::new();
    for record in crate::engraving::parse_records(&first).expect("journal parses") {
        ledger.apply(&record);
    }

    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        ledger,
        Scripted::new([]),
        Library::stub(),
    );
    let _ = runner.run(Environment::new());
    let second = runner
        .into_appender()
        .contents()
        .to_string();

    let begins = second
        .lines()
        .filter(|line| line.contains("/dry_towel:/1 Begin"))
        .count();
    assert_eq!(
        begins, 1,
        "the second towel's first step must be performed on resume"
    );
}

/// A completed Section is descended rather than returned from, so the walk
/// reaches the work nested beneath it — but the descent is a replay: nothing
/// under it is prompted for, and nothing under it is recorded again.
#[test]
fn resume_descends_completed_section_without_recording() {
    let source = r#"
% technique v1

audit :

I.  Delete Resources

    1.  Check the manifest
    2.  Remove the instances

II. Close Account

    1.  File the paperwork
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let first = {
        let mut runner = Runner::new(
            &program,
            Appender::memory(),
            Ledger::new(),
            Scripted::new([("/audit:/II/1".to_string(), UserInput::Quit)]),
            Library::stub(),
        );
        let _ = runner.run(Environment::new());
        runner
            .into_appender()
            .contents()
            .to_string()
    };
    // Unattended, prose steps settle as Skip, so the section does too. What
    // matters here is that it has an outcome at all.
    assert!(first.contains("/audit:/I Skip"), "section I settled");

    let mut ledger = Ledger::new();
    for record in crate::engraving::parse_records(&first).expect("journal parses") {
        ledger.apply(&record);
    }

    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        ledger,
        Scripted::new([]),
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("resume");
    let second = runner
        .into_appender()
        .contents()
        .to_string();

    let replayed: Vec<&str> = second
        .lines()
        .filter(|line| line.contains("/audit:/I ") || line.contains("/audit:/I/"))
        .collect();
    assert!(
        replayed.is_empty(),
        "the completed section records nothing on replay: {:?}",
        replayed
    );
    assert!(
        !second.contains("/audit:/II/1 Begin"),
        "its Begin already stands from the first run and is not written again"
    );
    assert!(
        second.contains("/audit:/II/1 Skip"),
        "the step the first run quit at is performed"
    );
}

/// Descending into a completed step reaches the host calls it made. They
/// happened already, so a replay announces each rather than dispatching it.
#[test]
fn replayed_execute_announces_rather_than_dispatching() {
    let source = r#"
% technique v1

deploy :

    1.  Publish the build { exec("make release") }
    2.  Announce it
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");
    crate::linking::link(&mut program, &Library::stub()).expect("link");

    let first = {
        let mut runner = Runner::new(
            &program,
            Appender::memory(),
            Ledger::new(),
            Scripted::new([("/deploy:/2".to_string(), UserInput::Quit)]),
            Library::stub(),
        );
        let _ = runner.run(Environment::new());
        runner
            .into_appender()
            .contents()
            .to_string()
    };
    assert!(
        first.contains("/deploy:/1 Execute exec()"),
        "the call was made"
    );

    let mut ledger = Ledger::new();
    for record in crate::engraving::parse_records(&first).expect("journal parses") {
        ledger.apply(&record);
    }

    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        ledger,
        Mock::with_answers([UserInput::Done(Value::Unitus)]),
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("resume");

    let events = runner
        .into_driver()
        .events()
        .to_vec();
    assert!(
        !events
            .iter()
            .any(|e| {
                if let Event::Command { .. } = e {
                    true
                } else {
                    false
                }
            }),
        "the command is not put to the user again: {:?}",
        events
    );
    assert!(
        events.contains(&Event::Announce("exec()".to_string())),
        "the call it made is announced: {:?}",
        events
    );
}

const GUARD_SOURCE: &str = r#"
% technique v1

audit :

I.  Survey

    1.  Count the assets { 7 ~ assets }

II. Report

    1.  File a return on { assets }
"#;

// The journal of a complete run of GUARD_SOURCE, with `/audit:/II/1` recorded
// as having read `assets` at whatever `stated` says. A prior value there is
// what an amendment upstream leaves behind.
fn guard_ledger(stated: i64) -> Ledger {
    ledger_of(&[
        (1, "/audit:", State::Begin(Vec::new())),
        (2, "/audit:/I", State::Begin(Vec::new())),
        (3, "/audit:/I/1", State::Begin(Vec::new())),
        (
            3,
            "/audit:/I/1",
            bind_of(
                "assets",
                Value::Quanticle(crate::value::Numeric::Integral(7)),
            ),
        ),
        (3, "/audit:/I/1", State::Done(None)),
        (2, "/audit:/I", State::Done(None)),
        (4, "/audit:/II", State::Begin(Vec::new())),
        (
            5,
            "/audit:/II/1",
            State::Begin(vec![Supplied {
                value: Value::Quanticle(crate::value::Numeric::Integral(stated)),
                name: Some("assets".to_string()),
            }]),
        ),
        (5, "/audit:/II/1", State::Done(None)),
        (4, "/audit:/II", State::Done(None)),
    ])
}

/// An amendment reaches the descendants of a completed sibling Section. The
/// Section's own `Begin` is empty so its guard says nothing about what is
/// nested beneath it; only descending finds the step whose recorded input no
/// longer holds, and that step is redone.
#[test]
fn amended_input_reaches_beneath_completed_section() {
    let journal = walk_guarded(3);

    assert!(
        journal.contains("005 /audit:/II/1 Begin"),
        "the stale step is redone, at the serial it was recorded under: {}",
        journal
    );
    assert!(
        !journal.contains("/audit:/I/1"),
        "the step whose inputs still hold is replayed, writing nothing: {}",
        journal
    );
    assert!(
        !journal.contains("/audit:/II Begin"),
        "the section itself is replayed, writing nothing: {}",
        journal
    );
}

/// The converse: a recorded step whose inputs are unchanged is not redone,
/// however deep inside a replayed scope it sits.
#[test]
fn unamended_input_leaves_completed_step_alone() {
    let journal = walk_guarded(7);

    assert!(
        !journal.contains("/audit:/II/1"),
        "nothing is redone when every recorded input still holds: {}",
        journal
    );
}

// Walk GUARD_SOURCE against a ledger stating `assets` was `stated` at
// `/audit:/II/1`, returning the journal the walk appends.
fn walk_guarded(stated: i64) -> String {
    walk_recorded(GUARD_SOURCE, guard_ledger(stated))
}

/// Two `foreach` loops in one step body share the scope's numbering, so the
/// second continues where the first left off. Numbering each from 1 gave both
/// loops iterations at `[1]` and `[2]`, and once iterations bracket themselves
/// the second loop's items would pool-match the first loop's records.
#[test]
fn sibling_loops_do_not_collide_on_an_index() {
    let source = r#"
% technique v1

siblings :

    1.  Go { foreach a in ["x","y"] ; foreach b in ["p","q"] }
        "#
    .trim_ascii();
    let journal = walk_fresh(source);

    let iterations: Vec<&str> = journal
        .lines()
        .filter(|line| line.contains("Begin ( \""))
        .filter_map(|line| {
            line.split(' ')
                .nth(3)
        })
        .collect();
    assert_eq!(
        iterations,
        vec![
            "/siblings:/1/[1]",
            "/siblings:/1/[2]",
            "/siblings:/1/[3]",
            "/siblings:/1/[4]"
        ]
    );
}

const TALLY_SOURCE: &str = r#"
% technique v1

tally :

    1.  Count { foreach item in ["one","one","two"] }
        -   note it
"#;

// A journal of a run of TALLY_SOURCE whose loop recorded iterations at the given
// indices and items. The enclosing step is left unfinished, so the re-walk
// reaches the loop rather than replaying the step whole.
fn tally_ledger(recorded: &[(u32, usize, &str)]) -> Ledger {
    let mut records = vec![
        (1, "/tally:".to_string(), State::Begin(Vec::new())),
        (2, "/tally:/1".to_string(), State::Begin(Vec::new())),
    ];
    for (serial, number, item) in recorded {
        let began = State::Begin(vec![Supplied {
            value: Value::Literali(item.to_string()),
            name: Some("item".to_string()),
        }]);
        let path = format!("/tally:/1/[{}]", number);
        records.push((*serial, path.clone(), began.clone()));
        records.push((serial + 1, format!("{}/-1", path), began));
        records.push((serial + 1, format!("{}/-1", path), State::Skip));
        records.push((*serial, path, State::Skip));
    }
    let borrowed: Vec<(u32, &str, State)> = records
        .iter()
        .map(|(serial, path, state)| (*serial, path.as_str(), state.clone()))
        .collect();
    ledger_of(&borrowed)
}

/// Consumption is what preserves multiplicity. A list of three items runs
/// three times whatever the prior run recorded: the two identical items
/// cannot collapse onto the one recorded iteration matching them, because
/// claiming it takes it out of the pool.
#[test]
fn identical_items_each_get_their_own_iteration() {
    let journal = walk_recorded(TALLY_SOURCE, tally_ledger(&[(3, 1, "one"), (5, 2, "two")]));

    assert!(
        journal.contains(r#"/tally:/1/[3] Begin ( "one" ~ item )"#),
        "the second `one` has no unclaimed match and runs fresh: {}",
        journal
    );
    assert!(
        !journal.contains("/tally:/1/[1] Begin") && !journal.contains("/tally:/1/[2] Begin"),
        "the two that matched are replayed, writing nothing: {}",
        journal
    );
}

/// A fresh item takes the numeric maximum plus one. Taking the last key of
/// the prefix range instead would hand back an index from the middle, since
/// `[10]` sorts lexicographically between `[1]` and `[2]`.
#[test]
fn fresh_iteration_takes_the_numeric_maximum_plus_one() {
    let source = r#"
% technique v1

tally :

    1.  Count { foreach item in ["one","two","three"] }
        -   note it
"#;
    let journal = walk_recorded(
        source,
        tally_ledger(&[(3, 2, "one"), (5, 3, "orphan"), (7, 10, "two")]),
    );

    assert!(
        journal.contains(r#"/tally:/1/[11] Begin ( "three" ~ item )"#),
        "the unmatched item lands beyond every recorded index: {}",
        journal
    );
}

// Walk a source with an empty ledger, returning the journal it appends.
fn walk_fresh(source: &str) -> String {
    walk_recorded(source, Ledger::new())
}

// Walk a source against a prior run's ledger, returning the journal it appends.
fn walk_recorded(source: &str, ledger: Ledger) -> String {
    let document = parsing::parse(Path::new("Test.tq"), source.trim_ascii()).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        ledger,
        Scripted::new([]),
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");
    runner
        .into_appender()
        .contents()
        .to_string()
}

/// Amending is a `Revoke` plus ordinary re-execution: nothing is collected at
/// the review prompt and nothing is written from it beyond the revocation.
/// The walk restarts, replays what still stands, and reaches the amended step
/// again — which is what `drive()` loops for.
#[test]
fn amend_revokes_and_the_walk_reaches_the_step_again() {
    let source = r#"
% technique v1

survey :

    1.  Note the reading ~ reading
    2.  File the report
    3.  Post the notice
"#;
    let document = parsing::parse(Path::new("Test.tq"), source.trim_ascii()).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    // At step 3 ask to review. The cursor opens on the last record written —
    // step 3's own `Begin` — and one back reaches step 2's outcome, where
    // amending withdraws it. The restart is not
    // driven here — the runner returns Restarting and `drive()` is what walks
    // again — so this asserts on the journal the first walk leaves.
    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        Ledger::new(),
        Scripted::reviewing(
            [("/survey:/3".to_string(), UserInput::Review)],
            [Review::Move(Motion::Up), Review::Chose(Offer::Edit)],
        ),
        Library::stub(),
    );
    let conclusion = runner
        .run(Environment::new())
        .expect("run");
    assert_eq!(conclusion, Conclusion::Restarting);

    let journal = runner
        .into_appender()
        .contents()
        .to_string();
    assert!(
        journal.contains("/survey:/2 Revoke"),
        "the revocation reaches the file before the restart: {}",
        journal
    );
    assert!(
        !journal.contains("/ Finish") && !journal.contains("/ Stop"),
        "a restart is neither a finish nor a stop: {}",
        journal
    );

    // Folding that journal is the state the restart walks against: step 2 has
    // been withdrawn, and step 1 — an ancestor's sibling, off the spine — has
    // not.
    let mut ledger = Ledger::new();
    for record in crate::engraving::parse_records(&journal).expect("journal parses") {
        ledger.apply(&record);
    }
    let step = ledger
        .look(Serial(1), "/2")
        .expect("step 2");
    assert!(step.revoked);
    assert!(
        step.outcome
            .is_none()
    );
    assert!(
        ledger
            .look(Serial(1), "/1")
            .expect("step 1")
            .outcome
            .is_some(),
        "the step that was not amended still stands"
    );

    // The second walk redoes step 2 and leaves step 1 alone.
    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        ledger,
        Scripted::new([]),
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("resume");
    let second = runner
        .into_appender()
        .contents()
        .to_string();
    assert!(second.contains("/survey:/2 Begin"), "step 2 is redone");
    assert!(
        !second.contains("/survey:/1 Begin"),
        "step 1 is replayed, writing nothing: {}",
        second
    );
    assert!(
        !second.contains("/survey: Begin"),
        "the enclosing procedure already stands and is not written again: {}",
        second
    );
    // Redone work is new work: the revoked line's serial is spent, and the
    // redo is recorded under a fresh one.
    let spent = journal
        .lines()
        .find(|line| line.contains("/survey:/2 Begin"))
        .and_then(|line| {
            line.split(' ')
                .nth(2)
        })
        .expect("step 2 began in the first walk");
    assert!(
        !second.contains(&format!("{} /survey:/2 Begin", spent)),
        "the redo does not reuse serial {}: {}",
        spent,
        second
    );
}

/// The re-prompt at an amended acquire opens on what was recorded there, so
/// correcting one character of a long list is not retyping the whole list.
/// This is a default the user still commits at the real prompt, against the
/// real type check — not a value written from the review prompt.
#[test]
fn a_revoked_acquire_prompt_opens_on_the_old_value() {
    let source = r#"
% technique v1

survey :

    1.  Note the reading ~ reading
"#;
    let document = parsing::parse(Path::new("Test.tq"), source.trim_ascii()).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let ledger = ledger_of(&[
        (1, "/survey:", State::Begin(Vec::new())),
        (2, "/survey:/1", State::Begin(Vec::new())),
        (
            2,
            "/survey:/1",
            bind_of("reading", Value::Literali("i-1369139".to_string())),
        ),
        (2, "/survey:/1", State::Done(None)),
        (2, "/survey:/1", State::Revoke),
    ]);

    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        ledger,
        Mock::with_answers([UserInput::Done(Value::Literali("i-1369193".to_string()))]),
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let seeds: Vec<Option<Value>> = runner
        .into_driver()
        .events()
        .iter()
        .filter_map(|e| {
            if let Event::Acquire { seed, .. } = e {
                Some(seed.clone())
            } else {
                None
            }
        })
        .collect();
    assert_eq!(
        seeds,
        vec![Some(Value::Literali("i-1369139".to_string()))],
        "the acquire prompt is seeded from what the revoked step bound"
    );
}

/// Up out of the live prompt, back over the sections the walk has closed, then
/// Amend: the `Revoke` lands at the *reviewed* position, not at the node being
/// prompted, and the walk restarts.
#[test]
fn amending_a_reviewed_position_revokes_there_not_here() {
    let source = r#"
% technique v1

survey :

I.  Preparation

    1.  Gather the samples

II. Analysis

    1.  Weigh the samples
"#;
    let document = parsing::parse(Path::new("Test.tq"), source.trim_ascii()).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    // At II/1 ask to review. The cursor opens on the last record written and
    // walks back over II's entry and I's close to the step inside I.
    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        Ledger::new(),
        Scripted::reviewing(
            [("/survey:/II/1".to_string(), UserInput::Review)],
            [
                Review::Move(Motion::Up),
                Review::Move(Motion::Up),
                Review::Move(Motion::Up),
                Review::Chose(Offer::Edit),
            ],
        ),
        Library::stub(),
    );
    let conclusion = runner
        .run(Environment::new())
        .expect("run");
    assert_eq!(conclusion, Conclusion::Restarting);

    let journal = runner
        .into_appender()
        .contents()
        .to_string();
    let revocations: Vec<&str> = journal
        .lines()
        .filter(|line| line.contains("Revoke"))
        .collect();
    assert_eq!(
        revocations
            .iter()
            .map(|line| {
                line.split(' ')
                    .nth(3)
                    .unwrap_or("")
            })
            .collect::<Vec<&str>>(),
        vec!["/survey:/I/1"],
        "one revocation, at the reviewed position rather than the live one: {}",
        journal
    );
}

/// Right descends into a call that has returned. Reviewing back to the call's
/// close and stepping in reaches the callee's own steps, and amending there
/// revokes inside the callee rather than at the call site.
#[test]
fn review_reaches_inside_a_returned_call() {
    let source = r#"
% technique v1

launder :

    1.  <dry_towel>("blue")
    2.  Fold it

dry_towel(towel) : Towel -> ()

    1.  Hang { towel } on the line
    2.  Wait for the sun
"#;
    let document = parsing::parse(Path::new("Test.tq"), source.trim_ascii()).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    // Standing at step 2, what lies behind is step 1's outcome, dry_towel's
    // close, and then its second step. Back walks into the returned call
    // without any descent: the records inside it are on the axis like the
    // rest.
    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        Ledger::new(),
        Scripted::reviewing(
            [("/launder:/2".to_string(), UserInput::Review)],
            [
                Review::Move(Motion::Up),
                Review::Move(Motion::Up),
                Review::Move(Motion::Up),
                Review::Chose(Offer::Edit),
            ],
        ),
        Library::stub(),
    );
    let conclusion = runner
        .run(Environment::new())
        .expect("run");
    assert_eq!(conclusion, Conclusion::Restarting);

    let journal = runner
        .into_appender()
        .contents()
        .to_string();
    let revoked: Vec<&str> = journal
        .lines()
        .filter(|line| line.contains("Revoke"))
        .filter_map(|line| {
            line.split(' ')
                .nth(3)
        })
        .collect();
    assert_eq!(
        revoked,
        vec!["/dry_towel:/2"],
        "the revocation lands inside the callee: {}",
        journal
    );
}

/// At the run root the arrows do not strand the user. `↑` stays at `/`, which
/// can never have a sibling to page to, and `↓` goes back in to the entry
/// procedure rather than ejecting to the live prompt a hundred steps away.
#[test]
fn review_at_the_root_pages_into_the_entry_procedure() {
    let source = r#"
% technique v1

survey :

    1.  Note the reading ~ reading
    2.  File the report
    3.  Post the notice
"#;
    let document = parsing::parse(Path::new("Test.tq"), source.trim_ascii()).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    // From step 3 the cursor opens on step 2; two climbs reach `/survey:` and
    // then `/`. Up there stays put, Down returns to `/survey:`, and Right
    // descends to its first step, which is what the amendment names.
    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        Ledger::new(),
        Scripted::reviewing(
            [("/survey:/3".to_string(), UserInput::Review)],
            [
                Review::Move(Motion::Left),
                Review::Move(Motion::Left),
                Review::Move(Motion::Up),
                Review::Move(Motion::Down),
                Review::Move(Motion::Right),
                Review::Chose(Offer::Edit),
            ],
        ),
        Library::stub(),
    );
    let conclusion = runner
        .run(Environment::new())
        .expect("run");
    assert_eq!(conclusion, Conclusion::Restarting);

    let journal = runner
        .into_appender()
        .contents()
        .to_string();
    let revoked: Vec<&str> = journal
        .lines()
        .filter(|line| line.contains("Revoke"))
        .filter_map(|line| {
            line.split(' ')
                .nth(3)
        })
        .collect();
    assert_eq!(
        revoked,
        vec!["/survey:/1"],
        "Down at the root reached the entry procedure rather than leaving review: {}",
        journal
    );
}

/// A scope the walk is still inside can never have a later sibling, so Down
/// from one must not eject: it goes back in, to the first thing that happened
/// inside, rather than to the live prompt the user climbed out of.
#[test]
fn review_down_from_an_enclosing_scope_goes_back_in() {
    let source = r#"
% technique v1

survey :

    1.  Note the reading
    2.  File the report
    3.  Post the notice
"#;
    let document = parsing::parse(Path::new("Test.tq"), source.trim_ascii()).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    // From step 3 the cursor opens on step 2; one climb reaches `survey:`,
    // which the walk is standing in and so has nothing beside it. Down there
    // returns to its first step, which is what the amendment names.
    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        Ledger::new(),
        Scripted::reviewing(
            [("/survey:/3".to_string(), UserInput::Review)],
            [
                Review::Move(Motion::Left),
                Review::Move(Motion::Down),
                Review::Chose(Offer::Edit),
            ],
        ),
        Library::stub(),
    );
    let conclusion = runner
        .run(Environment::new())
        .expect("run");
    assert_eq!(conclusion, Conclusion::Restarting);

    let journal = runner
        .into_appender()
        .contents()
        .to_string();
    let revoked: Vec<&str> = journal
        .lines()
        .filter(|line| line.contains("Revoke"))
        .filter_map(|line| {
            line.split(' ')
                .nth(3)
        })
        .collect();
    assert_eq!(
        revoked,
        vec!["/survey:/1"],
        "Down from the enclosing scope went back in rather than leaving review: {}",
        journal
    );
}

/// The dispatch record is written when the `Begin` it introduces is. Resuming
/// into a call that had already started records neither, so the journal never
/// shows a dispatch that opened nothing.
#[test]
fn resume_into_a_started_call_records_no_dispatch() {
    let source = r#"
% technique v1

main :

    1.  <helper>
    2.  After

helper :

    1.  Inside one
    2.  Inside two
"#;
    let document = parsing::parse(Path::new("Test.tq"), source.trim_ascii()).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        Ledger::new(),
        Scripted::new([("/helper:/2".to_string(), UserInput::Quit)]),
        Library::stub(),
    );
    let _ = runner.run(Environment::new());
    let first = runner
        .into_appender()
        .contents()
        .to_string();
    assert!(first.contains("Invoke helper:"), "the call was dispatched");

    let mut ledger = Ledger::new();
    for record in crate::engraving::parse_records(&first).expect("journal parses") {
        ledger.apply(&record);
    }
    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        ledger,
        Scripted::new([]),
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("resume");
    let second = runner
        .into_appender()
        .contents()
        .to_string();

    assert!(
        !second.contains("Invoke"),
        "the dispatch already stands and is not written again: {}",
        second
    );
    assert!(
        !second.contains("/helper: Begin"),
        "nor is the callee's entry: {}",
        second
    );
    assert!(
        second.contains("/helper:/2 Skip"),
        "the step in flight is settled: {}",
        second
    );
}

/// Up from a climbed scope that has nothing before it drops back to the
/// settled order, reaching whatever settled last before that scope was
/// entered. A first child has no earlier sibling, and must not be a dead end.
#[test]
fn review_pages_over_a_call_to_the_peer_before_it() {
    let source = r#"
% technique v1

survey :

    1.  First step
    2.  <helper>
    3.  Last step

helper :

    1.  Inside one
    2.  Inside two
"#;
    let document = parsing::parse(Path::new("Test.tq"), source.trim_ascii()).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    // At step 3 the cursor opens on its `Begin`; one back reaches step 2's
    // outcome. Step 2 is the call, so its records run the whole length of
    // helper: — and PageUp crosses all of them in one press, landing on the
    // outcome of step 1 because that is the plane the cursor was on.
    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        Ledger::new(),
        Scripted::reviewing(
            [("/survey:/3".to_string(), UserInput::Review)],
            [
                Review::Move(Motion::Up),
                Review::Move(Motion::PageUp),
                Review::Chose(Offer::Edit),
            ],
        ),
        Library::stub(),
    );
    let conclusion = runner
        .run(Environment::new())
        .expect("run");
    assert_eq!(conclusion, Conclusion::Restarting);

    let journal = runner
        .into_appender()
        .contents()
        .to_string();
    let revoked: Vec<&str> = journal
        .lines()
        .filter(|line| line.contains("Revoke"))
        .filter_map(|line| {
            line.split(' ')
                .nth(3)
        })
        .collect();
    assert_eq!(
        revoked,
        vec!["/survey:/1"],
        "Up reached what settled before helper: was entered: {}",
        journal
    );
}

// A run driven the way a person drives it: keystrokes in, records out. Every
// other review test enters through `Scripted::reviewing` or `Mock`, both of
// which begin after the keystroke that would have opened review.
fn struck(code: KeyCode) -> KeyEvent {
    KeyEvent::new(code, KeyModifiers::NONE)
}

#[test]
fn keystrokes_amend_a_recorded_value() {
    let source = r#"
% technique v1

survey :
    1.  Note the reading ~ reading
    2.  File it
"#;
    let document = parsing::parse(Path::new("Test.tq"), source.trim_ascii()).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");

    let keys = [
        // The first walk: supply the reading, accept step 1.
        struck(KeyCode::Char('5')),
        struck(KeyCode::Enter),
        // Step 1's binding is what answers it, so the next prompt is step 2's.
        struck(KeyCode::Enter),
        // At the scope's close, <Up> steps back into what has settled, opening
        // review on the last record written — step 2's outcome. Two more reach
        // step 1's, past step 2's `Begin`.
        struck(KeyCode::Up),
        struck(KeyCode::Up),
        struck(KeyCode::Up),
        // Nothing is standing at a reviewed position, so the offers have to be
        // opened before a shortcut can be read: `e` is the fifth key, not the
        // fourth. Edit withdraws the value and the walk restarts.
        struck(KeyCode::Esc),
        struck(KeyCode::Char('e')),
        // The replay reaches step 1 again and asks, its buffer seeded from what
        // was withdrawn. Withdrawing a value and giving a different one are the
        // two halves of the one feature.
        struck(KeyCode::Backspace),
        struck(KeyCode::Char('7')),
        struck(KeyCode::Enter),
        struck(KeyCode::Enter),
        struck(KeyCode::Enter),
    ];

    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        Ledger::new(),
        Console::with_keys(Vec::new(), MockKeyboard::new(keys)),
        Library::stub(),
    );
    let conclusion = runner
        .run(Environment::new())
        .expect("first walk");
    assert_eq!(conclusion, Conclusion::Restarting);

    // `drive` is private, so do for ourselves what it does: the restart carries
    // the driver, and so the unspent keystrokes with it.
    let mut runner = runner.restart();
    let conclusion = runner
        .run(Environment::new())
        .expect("replay");
    assert_eq!(
        conclusion,
        Conclusion::Completed(Outcome::Done(Value::Unitus))
    );

    let journal = runner
        .into_appender()
        .contents()
        .to_string();
    assert!(
        journal.contains("/survey:/1 Revoke"),
        "the revocation reaches the file before the restart: {}",
        journal
    );
    assert!(
        journal.contains(r#"Bind ( "7" ~ reading )"#),
        "the replay records the value the user gave the second time: {}",
        journal
    );
    assert!(
        journal.contains("/ Finish"),
        "the amended run walks through to its end: {}",
        journal
    );
}
