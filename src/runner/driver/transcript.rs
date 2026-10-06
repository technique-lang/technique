//! The value trail behind `run --output=native`.

use std::io::{self, Write};

use crate::formatting::Render;
use crate::value::Value;

use super::trail::announce;
use super::{Answer, Driver, Event, Frame, Prompt, Question, Review, Standing};

#[derive(Debug)]
#[allow(dead_code)] // fields are read only via Debug
enum Trail {
    Enter {
        path: String,
    },
    Leave {
        path: String,
        // None marks a Quit.
        outcome: Option<Standing>,
        result: Value,
    },
    Execute {
        path: String,
        script: String,
    },
    Acquire {
        path: String,
        name: Option<String>,
        forma: Option<String>,
        supplied: Value,
    },
    External {
        path: String,
    },
}

/// Wraps a driver, writing each value-bearing moment as a `Trail`.
pub struct Transcript<D, W> {
    inner: D,
    output: W,
}

impl<D> Transcript<D, io::Stderr> {
    pub fn new(inner: D) -> Self {
        Transcript {
            inner,
            output: io::stderr(),
        }
    }
}

impl<D, W: Write> Transcript<D, W> {
    pub fn with_output(inner: D, output: W) -> Self {
        Transcript { inner, output }
    }

    pub fn into_inner(self) -> (D, W) {
        (self.inner, self.output)
    }

    fn emit(&mut self, trail: Trail) {
        let _ = writeln!(self.output, "{:#?}", trail);
    }
}

fn disposition(answer: &Answer) -> Option<Standing> {
    match answer {
        Answer::Done(_) | Answer::Override => Some(Standing::Done),
        Answer::Skip => Some(Standing::Skip),
        Answer::Fail(_) => Some(Standing::Fail),
        Answer::Review(_) | Answer::Quit => None,
    }
}

impl<D: Driver, W: Write> Driver for Transcript<D, W> {
    fn show(&mut self, event: Event<'_>) {
        match &event {
            Event::Step {
                path, constraints, ..
            } => self.emit(Trail::Enter {
                path: announce(path, constraints),
            }),
            Event::Enter { path, echo } => self.emit(Trail::Enter {
                path: announce(path, echo),
            }),
            Event::Section { path, .. } => self.emit(Trail::Enter {
                path: path.to_string(),
            }),
            _ => {}
        }
        self.inner
            .show(event);
    }

    fn ask(&mut self, question: Question<'_>) -> Answer {
        let path = question
            .path
            .to_string();
        let prompt = question
            .prompt
            .clone();
        let answer = self
            .inner
            .ask(question);
        // The same question is put again once review is left.
        if let Answer::Review(_) = answer {
            return answer;
        }
        match prompt {
            Prompt::Confirm { produced, .. } => self.emit(Trail::Leave {
                path,
                outcome: disposition(&answer),
                result: if let Answer::Done(value) = &answer {
                    value.clone()
                } else {
                    produced.clone()
                },
            }),
            Prompt::Acquire {
                text, name, forma, ..
            } => self.emit(Trail::Acquire {
                path: announce(&path, text),
                name: name.map(|n| n.to_string()),
                forma: forma.map(|f| f.to_string()),
                supplied: if let Answer::Done(value) = &answer {
                    value.clone()
                } else {
                    Value::Unitus
                },
            }),
            Prompt::Command { script } => self.emit(Trail::Execute {
                path,
                script: if let Answer::Done(Value::Literali(text)) = &answer {
                    text.clone()
                } else {
                    script.to_string()
                },
            }),
            Prompt::Action { verb, value, .. } => self.emit(Trail::Execute {
                path,
                script: format!("{} {}", verb, value.label())
                    .trim_end()
                    .to_string(),
            }),
            Prompt::External => self.emit(Trail::External { path }),
            Prompt::Depart { .. } => {}
        }
        answer
    }

    fn review(&mut self, frame: Frame<'_>) -> Review {
        self.inner
            .review(frame)
    }

    fn renderer(&self) -> &'static dyn Render {
        self.inner
            .renderer()
    }
}

#[cfg(test)]
#[path = "checks/transcript.rs"]
mod check;
