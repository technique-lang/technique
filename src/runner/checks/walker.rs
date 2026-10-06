// Walks of documents parsed through the real parser, driven unattended or
// from prepared answers, checked by the journal they write.

use std::path::Path;

use crate::engraving::Motion;
use crate::engraving::{Appender, Record, State, format_record};
use crate::linking::link;
use crate::parsing;
use crate::resolution::resolve;
use crate::runner::context::Context;
use crate::runner::driver::{Answer, Driver, Headless, Mock, Offer, Review, Scripted};
use crate::runner::error::RunnerError;
use crate::runner::library::Library;
use crate::runner::session::{Conclusion, Runner, bind_parameters};
use crate::runner::walker::Outcome;
use crate::translation::translate;
use crate::value::Value;

// Walk `source` against `records` with the given CLI arguments, answering
// through `driver`. Returns every record, the new ones appended.
fn walk<D: Driver>(
    source: &str,
    records: Vec<Record>,
    arguments: &[&str],
    driver: D,
) -> (Vec<Record>, Conclusion) {
    let (records, conclusion, _) = drive(source, records, arguments, driver);
    (records, conclusion)
}

fn drive<D: Driver>(
    source: &str,
    records: Vec<Record>,
    arguments: &[&str],
    driver: D,
) -> (Vec<Record>, Conclusion, D) {
    let (records, result, driver) = attempt(source, records, arguments, driver);
    (records, result.expect("run"), driver)
}

fn attempt<D: Driver>(
    source: &str,
    records: Vec<Record>,
    arguments: &[&str],
    driver: D,
) -> (Vec<Record>, Result<Conclusion, RunnerError>, D) {
    let path = Path::new("Test.tq");
    let document = parsing::parse(path, source).expect("parse");
    let mut program = translate(&document).expect("translate");
    resolve(&mut program).expect("resolve");
    let mut library = Library::core();
    library.extend(Library::system());
    link(&mut program, &library).expect("link");
    let arguments: Vec<String> = arguments
        .iter()
        .map(|a| a.to_string())
        .collect();
    let supplied = bind_parameters(&program, &arguments).expect("arguments");
    let mut runner = Runner::new(&program, Appender::memory(), records, driver, library)
        .with_context(Context::capture());
    let result = runner.run(supplied);
    let records = runner
        .records
        .clone();
    (records, result, runner.into_driver())
}

// `serial path state` for each record, as the goldens compare them.
fn lines(records: &[Record]) -> Vec<String> {
    records
        .iter()
        .map(|record| {
            format_record(record)
                .trim_end()
                .splitn(3, ' ')
                .nth(2)
                .unwrap_or_default()
                .to_string()
        })
        .collect()
}

#[test]
fn attribute_frames_are_not_repeated_on_substeps() {
    let source = r#"
% technique v1

serve :

    @waiter
        1.  Bring the menu
            a.  Recite the specials
    "#
    .trim_ascii();
    let (records, _) = walk(source, Vec::new(), &[], Headless::new());
    let paths: Vec<String> = records
        .iter()
        .map(|r| {
            r.path
                .clone()
        })
        .collect();
    assert!(paths.contains(&"/serve:/@waiter/1/a".to_string()));
    assert!(
        !paths
            .iter()
            .any(|p| p.contains("@waiter/1/@waiter"))
    );
}

#[test]
fn unbounded_recursion_stops_at_the_depth_limit() {
    let source = r#"
% technique v1

countdown(n) :

    1.  Count { n }
    2.  <countdown>(n)
    "#
    .trim_ascii();
    // The main thread's stack, not the smaller one test threads get.
    std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(move || {
            let (_, result, _) = attempt(source, Vec::new(), &["5"], Headless::new());
            let Err(RunnerError::RecursionLimit { procedure, depth }) = result else {
                panic!("expected RecursionLimit");
            };
            assert_eq!(procedure, "countdown");
            assert_eq!(depth, 100);
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn sibling_loops_continue_numbering() {
    let source = r#"
% technique v1

survey :

    1.  Go { foreach s in [1, 2] ; foreach s in [1, 2] }
    "#
    .trim_ascii();
    let (records, _) = walk(source, Vec::new(), &[], Headless::new());
    let begun: Vec<String> = lines(&records)
        .into_iter()
        .filter(|l| l.contains("/[") && l.ends_with(" ~ s )"))
        .collect();
    assert_eq!(
        begun,
        vec![
            "003 /survey:/1/[1] Begin ( 1 ~ s )",
            "004 /survey:/1/[2] Begin ( 2 ~ s )",
            "005 /survey:/1/[3] Begin ( 1 ~ s )",
            "006 /survey:/1/[4] Begin ( 2 ~ s )",
        ]
    );
}

#[test]
fn skipped_tuple_binding_binds_unit_to_each_name() {
    let source = r#"
% technique v1

survey :

    1.  Pair up { exec("echo a") ~ (a, b) }
    "#
    .trim_ascii();
    let driver = Scripted::new([("/survey:/1".to_string(), Answer::Skip)]);
    let (records, _) = walk(source, Vec::new(), &[], driver);
    let lines = lines(&records);
    assert!(lines.contains(&"002 /survey:/1 Bind ( () ~ a, () ~ b )".to_string()));
    assert!(lines.contains(&"002 /survey:/1 Skip".to_string()));
}

#[test]
fn wildcard_entry_parameter_records_its_value() {
    let source = r#"
% technique v1

main : * -> ()

    1.  Carry on
    "#
    .trim_ascii();
    let (records, _) = walk(source, Vec::new(), &["towel"], Headless::new());
    assert_eq!(lines(&records)[0], r#"001 /main: Begin ( "towel" )"#);
}

#[test]
fn nested_exec_in_an_argument_is_gated_and_recorded() {
    let source = r#"
% technique v1

main :

    1.  Tell { <helper>(exec("echo sneaky")) }

helper(v) : Thing -> ()

    1.  Hear { v }
    "#
    .trim_ascii();
    let (records, _) = walk(source, Vec::new(), &[], Headless::new());
    let lines = lines(&records);
    assert_eq!(
        &lines[1..6],
        &[
            "002 /main:/1 Begin ()",
            "002 /main:/1 Execute exec()",
            r#"002 /main:/1 Return "sneaky""#,
            "002 /main:/1 Invoke helper:",
            r#"003 /helper: Begin ( "sneaky" ~ v )"#,
        ]
    );
}

#[test]
fn bind_is_written_by_the_scope_that_bound() {
    let source = r#"
% technique v1

main :

    { "early" ~ e }

    1.  First { exec("echo hi") ~ x }
        a.  Then this
    "#
    .trim_ascii();
    let (records, _) = walk(source, Vec::new(), &[], Headless::new());
    let lines = lines(&records);
    assert!(lines.contains(&r#"002 /main:/1 Bind ( "hi" ~ x )"#.to_string()));
    assert!(lines.contains(&r#"001 /main: Bind ( "early" ~ e )"#.to_string()));
    assert!(
        !lines
            .iter()
            .any(|l| l.starts_with("003") && l.contains("Bind"))
    );
}

#[test]
fn skipped_descriptive_binding_is_recorded() {
    let source = r#"
% technique v1

survey :

    1.  Take a reading ~ reading
    2.  Note { reading } on the chart
    "#
    .trim_ascii();
    let driver = Scripted::new([("/survey:/1".to_string(), Answer::Skip)]);
    let (records, _) = walk(source, Vec::new(), &[], driver);
    let lines = lines(&records);
    assert_eq!(lines[2], "002 /survey:/1 Bind ( () ~ reading )");
    assert_eq!(lines[3], "002 /survey:/1 Skip");
    assert_eq!(lines[4], "003 /survey:/2 Begin ( () ~ reading )");
}

#[test]
fn resume_continues_without_writing_again() {
    let source = r#"
% technique v1

survey :

    1.  Take a reading ~ reading
    2.  Note { reading } on the chart
    3.  Sign the log
    "#
    .trim_ascii();
    let driver = Scripted::new([
        (
            "/survey:/1".to_string(),
            Answer::Done(Value::Literali("42".to_string())),
        ),
        ("/survey:/2".to_string(), Answer::Quit),
    ]);
    let (records, conclusion) = walk(source, Vec::new(), &[], driver);
    assert_eq!(conclusion, Conclusion::Stopping);
    let stopped = records.len();
    let (records, conclusion) = walk(source, records, &[], Headless::new());
    assert_eq!(
        conclusion,
        Conclusion::Completed(Outcome::Done(Value::Unitus))
    );
    assert_eq!(
        lines(&records[stopped..]),
        vec![
            "000 / Resume",
            "003 /survey:/2 Skip",
            "004 /survey:/3 Begin ()",
            "004 /survey:/3 Skip",
            "001 /survey: Done ()",
            "000 / Finish",
        ]
    );
}

#[test]
fn restored_failure_rolls_up_as_failure() {
    let source = r#"
% technique v1

survey :

    1.  Check the gauge
    2.  Sign the log
    "#
    .trim_ascii();
    let driver = Scripted::new([
        ("/survey:/1".to_string(), Answer::Fail("broke".to_string())),
        ("/survey:/2".to_string(), Answer::Quit),
    ]);
    let (records, _) = walk(source, Vec::new(), &[], driver);
    let stopped = records.len();
    let (records, conclusion) = walk(source, records, &[], Headless::new());
    assert_eq!(
        conclusion,
        Conclusion::Completed(Outcome::Fail("broke".to_string()))
    );
    assert_eq!(
        lines(&records[stopped..]),
        vec![
            "000 / Resume",
            "003 /survey:/2 Skip",
            "001 /survey: Fail [ \"reason\" = \"broke\" ]",
            "000 / Finish",
        ]
    );
}

#[test]
fn restored_command_is_not_run_again() {
    let source = r#"
% technique v1

survey :

    1.  Stamp it { exec("echo 1") }
        a.  Look at it
    "#
    .trim_ascii();
    let driver = Scripted::new([("/survey:/1/a".to_string(), Answer::Quit)]);
    let (records, _) = walk(source, Vec::new(), &[], driver);
    let stopped = records.len();
    let (records, _) = walk(source, records, &[], Headless::new());
    let resumed = lines(&records[stopped..]);
    assert_eq!(
        resumed,
        vec![
            "000 / Resume",
            "003 /survey:/1/a Skip",
            "002 /survey:/1 Skip",
            "001 /survey: Skip",
            "000 / Finish",
        ]
    );
}

#[test]
fn reopened_bare_return_is_put_again() {
    let source = r#"
% technique v1

survey :

    1.  Stamp it <helper>() then { exec("false") }

helper :

    1.  Look at it
    "#
    .trim_ascii();
    let (mut records, _) = walk(source, Vec::new(), &[], Headless::new());
    let mut revoke = records
        .iter()
        .find(|r| r.path == "/helper:/1")
        .unwrap()
        .clone();
    revoke.state = State::Revoke;
    records.push(revoke);
    let amended = records.len();
    let (records, _) = walk(source, records, &[], Headless::new());
    assert_eq!(
        lines(&records[amended..]),
        vec![
            "000 / Resume",
            "004 /helper:/1 Begin ()",
            "004 /helper:/1 Skip",
            "003 /helper: Skip",
            "002 /survey:/1 Return",
            "002 /survey:/1 Fail [ \"reason\" = \"External command exited with status 1\" ]",
            "001 /survey: Fail [ \"reason\" = \"External command exited with status 1\" ]",
            "000 / Finish",
        ]
    );
}

#[test]
fn choice_asked_again_opens_on_its_former_answer() {
    let source = r#"
% technique v1

survey :

    1.  Are the hatches open? ~ hatches
            'Open' | 'Closed'
    2.  Record { hatches } in the log
    "#
    .trim_ascii();
    let driver = Mock::with_answers([
        Answer::Done(Value::Literali("Closed".to_string())),
        Answer::Review(None),
        Answer::Done(Value::Literali("Open".to_string())),
        Answer::Done(Value::Unitus),
    ])
    .reviewing([Review::Move(Motion::Up), Review::Chose(Offer::Edit)]);
    let (records, _, driver) = drive(source, Vec::new(), &[], driver);
    let asked: Vec<&String> = driver
        .log()
        .iter()
        .filter(|entry| entry.starts_with("Question") && entry.contains(r#"path: "/survey:/1""#))
        .collect();
    assert_eq!(asked.len(), 2);
    assert!(asked[0].contains("draft: None"));
    assert!(asked[1].contains(r#"draft: Some("Closed")"#));
    assert!(lines(&records).contains(&r#"003 /survey:/2 Begin ( "Open" ~ hatches )"#.to_string()));
}

#[test]
fn argument_echo_serializes_values() {
    let mut env = crate::runner::evaluator::Environment::new();
    env.extend("name".to_string(), Value::Unitus);
    env.extend(
        "colour".to_string(),
        Value::Literali(r#"it's "blue""#.to_string()),
    );
    let params = [Some("name".to_string()), Some("colour".to_string())];
    assert_eq!(
        super::render_argument_echo(&params, &env),
        r#"(() ~ name, "it's \"blue\"" ~ colour)"#
    );
}

#[test]
fn restored_empty_callee_is_announced_on_resume() {
    let source = r#"
% technique v1

survey :

    1.  Tidy up <noop>
    2.  Sign the log

noop :
    "#
    .trim_ascii();
    let driver = Mock::with_answers([Answer::Done(Value::Unitus), Answer::Quit]);
    let (records, _) = walk(source, Vec::new(), &[], driver);
    let driver = Mock::with_answers([Answer::Done(Value::Unitus)]);
    let (_, _, driver) = drive(source, records, &[], driver);
    let log = driver.log();
    let entered = log
        .iter()
        .position(|entry| entry.starts_with(r#"Enter { path: "/noop:""#))
        .expect("entry line");
    assert!(log[entered + 1].contains("noop :"));
}
