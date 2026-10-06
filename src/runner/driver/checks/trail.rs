use crate::formatting::Identity;
use crate::highlighting::Terminal;
use crate::value::Value;

use super::*;

fn drawn(event: Event<'_>) -> String {
    let mut out = Vec::new();
    render(&mut out, &Identity, &event);
    String::from_utf8(out).expect("utf8")
}

fn coloured(event: Event<'_>) -> String {
    let mut out = Vec::new();
    render(&mut out, &Terminal, &event);
    String::from_utf8(out).expect("utf8")
}

#[test]
fn boundaries_and_prelude() {
    assert_eq!(
        drawn(Event::Commence {
            label: "/ Probe,1 #000236"
        }),
        "⇒ / Probe,1 #000236\n\n"
    );
    assert_eq!(
        drawn(Event::Display("% technique v1")),
        "% technique v1\n\n"
    );
    assert_eq!(
        drawn(Event::Conclude {
            label: "/ Probe,1 #000236",
            verdict: &Verdict::Done(Value::Unitus)
        }),
        "⇐ / Probe,1 #000236 ✓\n"
    );
}

#[test]
fn scopes_enter_with_their_echo() {
    assert_eq!(
        drawn(Event::Enter {
            path: "/probe:",
            echo: ""
        }),
        "↘ probe:\n\n"
    );
    assert_eq!(
        drawn(Event::Enter {
            path: "/probe:/4/greet:",
            echo: "(\"Arthur\" ~ name)"
        }),
        "↘ probe:/4/greet: (\"Arthur\" ~ name)\n\n"
    );
    assert_eq!(
        drawn(Event::Section {
            path: "/high_pressure_cleaning:/I",
            numeral: "I",
            title: "Setup"
        }),
        "↘ I\n\nI. Setup\n\n"
    );
    assert_eq!(
        drawn(Event::Section {
            path: "/probe:/II",
            numeral: "II",
            title: ""
        }),
        "↘ II\n\nII.\n\n"
    );
}

#[test]
fn a_step_indents_its_text_to_depth() {
    assert_eq!(
        drawn(Event::Step {
            path: "/probe:/1",
            constraints: "",
            text: "1.  Read this plain step",
            depth: 0
        }),
        "→ probe:/1\n\n    1.  Read this plain step\n\n"
    );
    assert_eq!(
        drawn(Event::Step {
            path: "/probe:/1/a",
            constraints: "@waiter",
            text: "a.  Inspect\nthe connector",
            depth: 2
        }),
        "→ probe:/1/a @waiter\n\n        a.  Inspect\n        the connector\n\n"
    );
}

#[test]
fn verdict_lines() {
    let done = Verdict::Done(Value::Quanticle(crate::value::Numeric::Integral(42)));
    assert_eq!(
        drawn(Event::Verdict {
            marker: Marker::Step,
            path: "/probe:/1",
            verdict: &done,
            restored: false
        }),
        "→ probe:/1 ✓\n"
    );
    assert_eq!(
        drawn(Event::Verdict {
            marker: Marker::Close,
            path: "/probe:/2/[1]",
            verdict: &Verdict::Skip,
            restored: false
        }),
        "↙ probe:/2/[1] ⊘\n"
    );
    assert_eq!(
        drawn(Event::Verdict {
            marker: Marker::Return,
            path: "/probe:/7/<https://example.com/Helper>",
            verdict: &Verdict::Fail("no".to_string()),
            restored: true
        }),
        "⇐ probe:/7/<https://example.com/Helper> ✗\n"
    );
}

#[test]
fn a_restored_verdict_is_grey_throughout() {
    let live = coloured(Event::Verdict {
        marker: Marker::Step,
        path: "/probe:/1",
        verdict: &Verdict::Done(Value::Unitus),
        restored: false,
    });
    assert!(live.contains("78;154;6"));

    let restored = coloured(Event::Verdict {
        marker: Marker::Step,
        path: "/probe:/1",
        verdict: &Verdict::Done(Value::Unitus),
        restored: true,
    });
    assert!(!restored.contains("78;154;6"));
    assert_eq!(
        restored,
        Terminal.style(Syntax::Marker, "→ probe:/1 ✓") + "\n"
    );
}

#[test]
fn effects_and_announcements() {
    assert_eq!(
        drawn(Event::Command {
            path: "/probe:/6",
            script: "echo hello world\n"
        }),
        "→ probe:/6 $ echo hello world\n"
    );
    assert_eq!(
        drawn(Event::Action {
            path: "/probe:/5",
            function: "click"
        }),
        "» probe:/5 click()\n"
    );
    assert_eq!(
        drawn(Event::Depart {
            path: "/probe:/7/<https://example.com/Helper>",
            echo: ""
        }),
        "⇒ probe:/7/<https://example.com/Helper>\n"
    );
    assert_eq!(
        drawn(Event::Depart {
            path: "/probe:/7/<https://example.com/Helper>",
            echo: "(\"x\" ~ name)"
        }),
        "⇒ probe:/7/<https://example.com/Helper> (\"x\" ~ name)\n"
    );
    assert_eq!(drawn(Event::Announce("exec()")), "    exec()\n");
}

#[test]
fn a_restart_is_one_grey_separator() {
    assert_eq!(drawn(Event::Restart), format!("{}\n", "─".repeat(40)));
    assert_eq!(
        coloured(Event::Restart),
        Terminal.style(Syntax::Marker, &"─".repeat(40)) + "\n"
    );
}

#[test]
fn visual_writes_what_it_is_shown() {
    let mut visual = Visual {
        output: Vec::new(),
        renderer: &Identity,
    };
    visual.show(Event::Announce("now()"));
    assert_eq!(visual.output, b"    now()\n");
}
