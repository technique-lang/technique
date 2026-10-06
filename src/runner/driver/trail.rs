//! The trail: each `Event` drawn as the lines it leaves behind.

use std::io::{self, Write};

use crate::engraving::display_path;
use crate::formatting::{Identity, Render, Syntax};

use super::{Event, Marker, Output, Verdict};

/// The trail drawn to a terminal.
pub struct Visual<W: Write> {
    pub(super) output: W,
    pub(super) renderer: &'static dyn Render,
}

impl<W: Write> Output for Visual<W> {
    type Surface = W;

    fn show(&mut self, event: Event<'_>) {
        render(&mut self.output, self.renderer, &event);
        let _ = self
            .output
            .flush();
    }

    fn surface(&mut self) -> &mut W {
        &mut self.output
    }

    fn renderer(&self) -> &'static dyn Render {
        self.renderer
    }
}

/// No trail at all, leaving only executed commands' output on the terminal.
pub struct Silent {
    pub(super) discard: io::Sink,
}

impl Output for Silent {
    type Surface = io::Sink;

    fn show(&mut self, _event: Event<'_>) {}

    fn surface(&mut self) -> &mut io::Sink {
        &mut self.discard
    }

    fn renderer(&self) -> &'static dyn Render {
        &Identity
    }
}

pub(super) fn glyph(marker: Marker) -> &'static str {
    match marker {
        Marker::Depart => "⇒",
        Marker::Return => "⇐",
        Marker::Enter => "↘",
        Marker::Close => "↙",
        Marker::Step => "→",
        Marker::Action => "»",
    }
}

pub(super) fn verdict_glyph(verdict: &Verdict) -> (&'static str, Syntax) {
    match verdict {
        Verdict::Done(_) => ("✓", Syntax::Done),
        Verdict::Skip => ("⊘", Syntax::Skip),
        Verdict::Fail(_) => ("✗", Syntax::Fail),
    }
}

/// A path with an annotation set off by a space; empty `text` leaves it alone.
pub(super) fn announce(path: &str, text: &str) -> String {
    if text.is_empty() {
        path.to_string()
    } else {
        format!("{} {}", path, text)
    }
}

pub(super) fn render<W: Write>(out: &mut W, renderer: &dyn Render, event: &Event<'_>) {
    match event {
        Event::Commence { label } => {
            marker_line(out, renderer, &format!("⇒ {}", label));
            let _ = writeln!(out);
        }
        Event::Display(content) => {
            let _ = writeln!(out, "{}", content);
            let _ = writeln!(out);
        }
        Event::Enter { path, echo } => {
            let path = display_path(path);
            marker_line(out, renderer, &format!("↘ {}", announce(&path, echo)));
            let _ = writeln!(out);
        }
        Event::Section {
            path,
            numeral,
            title,
        } => {
            marker_line(out, renderer, &format!("↘ {}", display_path(path)));
            let _ = writeln!(out);
            let numeral = renderer.style(Syntax::StepItem, numeral);
            if title.is_empty() {
                let _ = writeln!(out, "{}.", numeral);
            } else {
                let _ = writeln!(out, "{}. {}", numeral, title);
            }
            let _ = writeln!(out);
        }
        Event::Step {
            path,
            constraints,
            text,
            depth,
        } => {
            let path = display_path(path);
            marker_line(
                out,
                renderer,
                &format!("→ {}", announce(&path, constraints)),
            );
            let _ = writeln!(out);
            indented(out, text, *depth);
            let _ = writeln!(out);
        }
        Event::Verdict {
            marker,
            path,
            verdict,
            restored,
        } => {
            let line = format!("{} {}", glyph(*marker), display_path(path));
            let (mark, syntax) = verdict_glyph(verdict);
            if *restored {
                marker_line(out, renderer, &format!("{} {}", line, mark));
            } else {
                let _ = writeln!(
                    out,
                    "{} {}",
                    renderer.style(Syntax::Marker, &line),
                    renderer.style(syntax, mark)
                );
            }
        }
        Event::Command { path, script } => {
            let line = format!("→ {} $ {}", display_path(path), script.trim_end());
            marker_line(out, renderer, &line);
        }
        Event::Action { path, function } => {
            marker_line(
                out,
                renderer,
                &format!("» {} {}()", display_path(path), function),
            );
        }
        Event::Depart { path, echo } => {
            let line = format!("⇒ {}", display_path(path));
            if echo.is_empty() {
                marker_line(out, renderer, &line);
            } else {
                marker_line(out, renderer, &format!("{} {}", line, echo));
            }
        }
        Event::Announce(message) => indented(out, message, 1),
        Event::Restart => marker_line(out, renderer, SEPARATOR),
        Event::Conclude { label, verdict } => {
            let (mark, syntax) = verdict_glyph(verdict);
            let _ = writeln!(
                out,
                "{} {}",
                renderer.style(Syntax::Marker, &format!("⇐ {}", label)),
                renderer.style(syntax, mark)
            );
        }
    }
}

const SEPARATOR: &str = "────────────────────────────────────────";

fn marker_line<W: Write>(out: &mut W, renderer: &dyn Render, text: &str) {
    let _ = writeln!(out, "{}", renderer.style(Syntax::Marker, text));
}

/// Four spaces per `depth`, and at least one level.
pub(super) fn indented<W: Write>(out: &mut W, text: &str, depth: usize) {
    for line in text.lines() {
        let _ = writeln!(out, "{:w$}{}", "", line, w = 4 * depth.max(1));
    }
}

#[cfg(test)]
#[path = "checks/trail.rs"]
mod check;
