use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::language;
use crate::language::{Identifier, Numeric as LangNumeric};
use crate::parsing;
use crate::program::{
    Executable, ExecutableRef, Fragment, Invocable, Operation, Ordinal, Program, Subroutine,
    SubroutineRef,
};
use crate::resolution::resolve;
use crate::runner::driver::{Automatic, Event, Mock, UserInput};
use crate::runner::evaluator::Environment;
use crate::runner::library::Library;
use crate::runner::runner::{bind_parameters, render_argument_echo, Outcome, Runner, RunnerError};
use crate::runner::state::{parse_record, Appender, InvokeTarget, State, Store, Supplied};
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
        HashMap::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    let outcome = runner
        .run(env)
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
        HashMap::new(),
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
    // lines[1] is the Begin; lines[2] is the Skip outcome.
    let record = parse_record(lines[2]).expect("parse record");
    assert_eq!(record.state, State::Skip);

    let mut fixture = StoreFixture::new("step-fail");
    let body = Operation::Sequence(vec![step(
        Ordinal::Dependent("1"),
        Operation::Sequence(vec![]),
    )]);
    let program = anonymous_with_body(body);
    let prompt = Mock::with_answers([UserInput::Fail("cable unplugged".to_string())]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashMap::new(),
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
    // lines[1] is the Begin; lines[2] is the Fail outcome.
    let record = parse_record(lines[2]).expect("parse record");
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
    let body = Operation::Sequence(vec![step(
        Ordinal::Dependent("1"),
        Operation::Sequence(vec![]),
    )]);
    let program = anonymous_with_body(body);
    let prompt = Mock::with_answers([UserInput::Fail(String::new())]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashMap::new(),
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
    let record = parse_record(lines[2]).expect("parse record");
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
        HashMap::new(),
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
        HashMap::new(),
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
    let body = Operation::Sequence(vec![
        step(Ordinal::Dependent("1"), Operation::Sequence(vec![])),
        step(Ordinal::Dependent("2"), Operation::Sequence(vec![])),
    ]);
    let program = anonymous_with_body(body);

    // Pre-mark step 1 completed; the walker should skip its prompt
    // and only ask about step 2.
    let mut completed = HashMap::new();
    completed.insert("/1".to_string(), Value::Unitus);

    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        completed,
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
        HashMap::new(),
        prompt,
        Library::stub(),
    );
    let env = Environment::new();
    let outcome = runner
        .run(env)
        .expect("run");
    assert_eq!(outcome, Outcome::Stopped);

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

    // Quit records the step's Begin (the operator started looking at it), then
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
    assert_eq!(lines.len(), 3);
    assert!(lines[0].contains(" Start "));
    assert!(lines[1].ends_with(" Begin"));
    assert!(lines[2].ends_with(" / Stop"));
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
        HashMap::new(),
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
            if let Event::Seal { qualified } = e {
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
    assert_eq!(section_numerals, vec!["I"]);
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
        HashMap::new(),
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
        HashMap::new(),
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
        HashMap::new(),
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
    // operator is asked for `s` once, when cycle is entered, and the bound
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
        HashMap::new(),
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
            if let Event::Acquire { name, forma } = e {
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
        HashMap::new(),
        prompt,
        Library::stub(),
    );
    let outcome = runner
        .run(Environment::new())
        .expect("run");
    assert_eq!(outcome, Outcome::Stopped);

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
        !pfftt
            .lines()
            .any(|line| line.contains("Invoke")),
        "declining at the prompt records no Invoke — the call never began"
    );
}

#[test]
fn skip_while_acquiring_records_the_skipped_invocation() {
    // Skip at the implicit-argument prompt skips the invocation: a Skip is
    // recorded at the callee's path (not silently swallowed) with no Invoke,
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

    let mut fixture = StoreFixture::new("skip-acquire");
    let prompt = Mock::with_answers([UserInput::Skip]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashMap::new(),
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
        !pfftt
            .lines()
            .any(|line| line.contains("Invoke")),
        "a declined call records no Invoke"
    );
    assert!(
        !pfftt
            .lines()
            .any(|line| line.contains("/cycle:/1 Begin")),
        "the callee's body never runs"
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
        HashMap::new(),
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
        HashMap::new(),
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
        HashMap::new(),
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
        HashMap::new(),
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
        HashMap::new(),
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
        HashMap::new(),
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
            if let Event::Ask { qualified, .. } = e {
                Some(qualified.as_str())
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
        HashMap::new(),
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
        HashMap::new(),
        Automatic::with_handle(Vec::new()),
        library,
    );
    let outcome = runner
        .run(Environment::new())
        .expect("run");
    match outcome {
        Outcome::Done(_) => {}
        other => panic!("expected Done, got {:?}", other),
    }
    let trace = String::from_utf8(
        runner
            .into_driver()
            .into_output(),
    )
    .expect("utf8");
    assert!(trace.contains("→ check:/1 ✓"));
    assert!(trace.contains("→ check:/2 ⊘"));
    assert!(trace.contains("↙ check: ✓"));
}

#[test]
fn automatic_failing_exec_fails_step_and_continues() {
    // A non-zero exec exit settles its step Fail; the walk continues to the
    // sibling below rather than aborting the run.
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
        HashMap::new(),
        Automatic::with_handle(Vec::new()),
        library,
    );
    let outcome = runner
        .run(Environment::new())
        .expect("run");
    match outcome {
        Outcome::Done(_) => {}
        other => panic!("expected Done, got {:?}", other),
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
    assert!(records
        .iter()
        .any(|r| r.path == "/check:/2"));
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
        source: scope_for(Ordinal::Dependent("1")),
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
        HashMap::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(env)
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
        source: scope_for(Ordinal::Dependent("1")),
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
        HashMap::new(),
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
        HashMap::new(),
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
        vec![("/[1]/a", "a.  \"first\""), ("/[2]/a", "a.  \"second\"")]
    );
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
        body: Box::new(Operation::Sequence(Vec::new())),
        responses: Vec::new(),
    };
    let names = [Identifier::new("n")];
    let over = Operation::Execute(Executable {
        target: ExecutableRef::Resolved(seq),
        arguments: vec![
            Operation::Number(LangNumeric::Integral(1)),
            Operation::Number(LangNumeric::Integral(3)),
        ],
    });
    let loop_op = Operation::Loop {
        names: &names,
        over: Some(Box::new(over)),
        body: Box::new(Operation::Sequence(vec![substep])),
        responses: Vec::new(),
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
        HashMap::new(),
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
        HashMap::new(),
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
        body: Box::new(Operation::Sequence(Vec::new())),
        responses: Vec::new(),
    };
    let names = [Identifier::new("item")];
    let loop_op = Operation::Loop {
        names: &names,
        over: Some(Box::new(Operation::Variable(Identifier::new("source")))),
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
    env.extend("source".to_string(), Value::Literali("lonely".to_string()));

    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashMap::new(),
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
    let substep = step(Ordinal::Dependent("a"), Operation::Sequence(Vec::new()));
    let loop_op = Operation::Loop {
        names: &names,
        over: Some(Box::new(Operation::Variable(Identifier::new("source")))),
        body: Box::new(Operation::Sequence(vec![substep])),
        responses: Vec::new(),
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
        HashMap::new(),
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
fn foreach_over_non_list_or_unbound_errors() {
    // foreach item in source, where `source` is supplied by the caller's
    // environment. A tuple or tablet source is `NotIterable` (lists iterate
    // and scalars widen, but a tablet is a record that must be projected via
    // values()/labels()/pairs() first, and a tuple does neither); an unbound
    // source propagates `UnboundVariable` rather than being swallowed.
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
        HashMap::new(),
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
        HashMap::new(),
        Mock::new(),
        Library::stub(),
    );
    match runner.run(env) {
        Err(RunnerError::NotIterable) => {}
        other => panic!("expected NotIterable, got {:?}", other),
    }

    // An unbound `source` propagates the evaluation error.
    let mut unbound_fixture = StoreFixture::new("foreach-unbound");
    let mut runner = Runner::new(
        &program,
        unbound_fixture.take_appender(),
        HashMap::new(),
        Mock::new(),
        Library::stub(),
    );
    let env = Environment::new();
    match runner.run(env) {
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
    assert!(env
        .lookup("anything")
        .is_none());
}

#[test]
fn argument_echo_binds_each_parameter() {
    let source = r#"
% technique v1

connectivity_check(e, s) :

1.  step
        "#
    .trim_ascii();
    let document = parsing::parse(Path::new("Test.tq"), source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");
    let args = ["[]".to_string(), "0".to_string()];
    let env = bind_parameters(&program, &args).expect("bind");
    let params = program
        .subroutines
        .first()
        .unwrap()
        .parameters
        .unwrap();
    let echo = render_argument_echo("connectivity_check", params, &env);
    assert_eq!(echo, "connectivity_check: ([] ~ e, 0 ~ s)");
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
        HashMap::new(),
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
        HashMap::new(),
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
        HashMap::new(),
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
        HashMap::new(),
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
            if let Event::Ask { choices, .. } = e {
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
    // Start, the entry `Begin`, the step `Begin`, then the step's outcome.
    let record = parse_record(lines[3]).expect("parse record");
    assert_eq!(
        record.state,
        State::Done(Some(Value::Literali("Yes".to_string())))
    );
}

#[test]
fn automatic_records_done_for_computable_step_skip_for_prose() {
    fn record_of(label: &str, body: Operation<'static>) -> (Outcome, State) {
        let mut fixture = StoreFixture::new(label);
        let program = anonymous_with_body(Operation::Sequence(vec![step(
            Ordinal::Dependent("1"),
            body,
        )]));
        let mut runner = Runner::new(
            &program,
            fixture.take_appender(),
            HashMap::new(),
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
        let state = parse_record(lines[2])
            .expect("parse record")
            .state;
        (outcome, state)
    }

    // A single-line value computes and records Done with the literal.
    let (outcome, state) = record_of(
        "automatic-records-value",
        Operation::String(vec![Fragment::Text("probe output")]),
    );
    assert_eq!(
        outcome,
        Outcome::Done(Value::Literali("probe output".to_string()))
    );
    assert_eq!(
        state,
        State::Done(Some(Value::Literali("probe output".to_string())))
    );

    // Multi-line text propagates intact and records faithfully; the codec
    // escapes the newlines so the value stays on one record line.
    let (outcome, state) = record_of(
        "multiline-records-value",
        Operation::String(vec![Fragment::Text("1: lo\n2: eth0\n3: wlan0")]),
    );
    assert_eq!(
        outcome,
        Outcome::Done(Value::Literali("1: lo\n2: eth0\n3: wlan0".to_string()))
    );
    assert_eq!(
        state,
        State::Done(Some(Value::Literali(
            "1: lo\n2: eth0\n3: wlan0".to_string()
        )))
    );

    // A pure-prose step (empty body) has nothing to compute and records Skip.
    let (_, state) = record_of("automatic-empty-body", Operation::Sequence(vec![]));
    assert_eq!(state, State::Skip);
}

#[test]
fn sequence_value_is_last_member() {
    // A body sequence takes the last member's value, not the first or a fold.
    let mut fixture = StoreFixture::new("sequence-last-member");
    let body = Operation::Sequence(vec![
        step(
            Ordinal::Dependent("1"),
            Operation::String(vec![Fragment::Text("first")]),
        ),
        step(
            Ordinal::Dependent("2"),
            Operation::String(vec![Fragment::Text("second")]),
        ),
    ]);
    let program = anonymous_with_body(body);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashMap::new(),
        Automatic::with_handle(Vec::new()),
        Library::stub(),
    );
    let outcome = runner
        .run(Environment::new())
        .expect("run");
    assert_eq!(
        outcome,
        Outcome::Done(Value::Literali("second".to_string()))
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
    assert_eq!(dones, 2);
}

#[test]
fn deferred_invoke_is_prompted_and_recorded() {
    // A call to an external procedure this run cannot resolve (it lives in
    // another document or system) is presented for the operator to settle: the
    // run does not descend into it, but the operator can mark it Done (it was
    // performed, or recorded elsewhere), Skip, or Fail. The call site and the
    // settled outcome are both recorded.
    fn deferred_program() -> Program<'static> {
        let external = language::External {
            value: "https://example.com/probe",
            span: language::Span::default(),
        };
        let invoke = Operation::Invoke(Invocable {
            target: SubroutineRef::Deferred(external),
            arguments: Vec::new(),
            elided: true,
        });
        anonymous_with_body(Operation::Sequence(vec![invoke]))
    }

    // The operator marks the external procedure Done.
    let mut fixture = StoreFixture::new("deferred-done");
    let program = deferred_program();
    let prompt = Mock::with_answers([UserInput::Done(Value::Unitus)]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashMap::new(),
        prompt,
        Library::stub(),
    );
    let outcome = runner
        .run(Environment::new())
        .expect("run");
    assert_eq!(outcome, Outcome::Done(Value::Unitus));

    // The operator was prompted about the external node, by its FQP.
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

    // The trail records the Invoke call site at the caller's path and the
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

    // The operator declines: Skip is recorded at the external's FQP. (The
    // enclosing sequence proceeds and returns its last Done value, so the
    // run's overall outcome is not itself the Skip — that is walk_sequence's
    // concern, tested elsewhere; what matters here is the recorded Skip.)
    let mut fixture = StoreFixture::new("deferred-skip");
    let program = deferred_program();
    let prompt = Mock::with_answers([UserInput::Skip]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashMap::new(),
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
    assert_eq!(settled, vec![State::Begin, State::Skip]);

    // Under an automatic run there is no operator to attest the external work
    // and nothing executed it, so it records Skip rather than a fabricated Done.
    let mut fixture = StoreFixture::new("deferred-automatic");
    let program = deferred_program();
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashMap::new(),
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
    assert_eq!(settled, vec![State::Begin, State::Skip]);
}

#[test]
fn descriptive_binding_acquires_list_for_foreach() {
    // A descriptive `~ items` binding has no executable value, so the operator
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
    // The acquire for `items` pops first, then the step and substep verdicts.
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Literali(r#"["east", "west"]"#.to_string())),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashMap::new(),
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
    // Two acquires (account_number, then account_name), then the two step
    // verdicts.
    let prompt = Mock::with_answers([
        UserInput::Done(Value::Literali("12345".to_string())),
        UserInput::Done(Value::Literali("Acme".to_string())),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        fixture.take_appender(),
        HashMap::new(),
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
    let mut completed = HashMap::new();
    completed.insert(
        "/cleanup:/1".to_string(),
        Value::Arraeum(vec![
            Value::Literali("a".to_string()),
            Value::Literali("b".to_string()),
        ]),
    );
    completed.insert(
        "/cleanup:/2/[1]/-1".to_string(),
        Value::Literali("x".to_string()),
    );
    completed.insert(
        "/cleanup:/2/[2]/-1".to_string(),
        Value::Literali("y".to_string()),
    );
    completed.insert("/cleanup:/2".to_string(), Value::Unitus);

    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        completed,
        Automatic::new(false),
        Library::stub(),
    );
    let outcome = runner
        .run(Environment::new())
        .expect("resume must not raise UnboundVariable");
    assert!(if let Outcome::Done(_) = outcome {
        true
    } else {
        false
    });
}

// An elided invocation `<hail>` acquires its parameter `name` from the
// operator. The argument it was called with is recorded as an `Input` at the
// callee's path so a resume can restore it.
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

    // The operator supplies "World" at the acquire prompt; the rest are step
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
        HashMap::new(),
        prompt,
        Library::stub(),
    );
    runner
        .run(Environment::new())
        .expect("run");

    let trail = runner
        .into_appender()
        .contents()
        .to_string();
    assert!(
        trail.contains("/hail: Input ( \"World\" ~ name )"),
        "trail was:\n{}",
        trail
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
    let mut inputs = HashMap::new();
    inputs.insert(
        "/hail:".to_string(),
        vec![Supplied {
            value: Value::Literali("World".to_string()),
            name: Some("name".to_string()),
        }],
    );

    let prompt = Mock::with_answers([
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
        UserInput::Done(Value::Unitus),
    ]);
    let mut runner = Runner::new(
        &program,
        Appender::memory(),
        HashMap::new(),
        prompt,
        Library::stub(),
    )
    .with_inputs(inputs);
    let outcome = runner
        .run(Environment::new())
        .expect("resume runs without re-acquiring");
    assert!(if let Outcome::Done(_) = outcome {
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
