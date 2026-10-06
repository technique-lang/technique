//! The policy behind automatic and quiet modes, where nobody is at the
//! terminal: each question gets the answer `unattended()` gives it, and a
//! review is left at once.

use std::io::Write;

use crate::value::Value;

use super::trail::indented;
use super::{Answer, Frame, Kind, Output, Policy, Prompt, Question, Review, Standing};

/// Takes each body's value where there is one to take, otherwise skips.
pub struct Batch;

impl Policy for Batch {
    fn ask<O: Output>(&mut self, out: &mut O, question: Question<'_>) -> Answer {
        if let Prompt::Action { verb, value, .. } = &question.prompt {
            let label = value.label();
            let text = if label.is_empty() {
                format!("» {}", verb)
            } else {
                format!("» {} {}", verb, label)
            };
            let surface = out.surface();
            indented(surface, &text, 1);
            let _ = surface.flush();
        }
        unattended(&question)
    }

    fn review<O: Output>(&mut self, _out: &mut O, _frame: Frame<'_>) -> Review {
        Review::Leave
    }
}

/// The answer given with nobody there: a standing Fail or Skip is kept, and
/// only what a program produced is taken as done.
pub fn unattended(question: &Question<'_>) -> Answer {
    match &question.prompt {
        Prompt::Confirm {
            standing,
            kind,
            produced,
            ..
        } => match standing {
            Standing::Fail => Answer::Fail(String::new()),
            Standing::Skip => Answer::Skip,
            Standing::Done => match kind {
                Kind::Computable | Kind::System => Answer::Done((*produced).clone()),
                Kind::Prose | Kind::Action | Kind::Choice => Answer::Skip,
            },
        },
        Prompt::Depart { .. } => Answer::Done(Value::Unitus),
        Prompt::Command { script } => Answer::Done(Value::Literali(script.to_string())),
        Prompt::Acquire { .. } | Prompt::Action { .. } | Prompt::External => Answer::Skip,
    }
}

#[cfg(test)]
#[path = "checks/batch.rs"]
mod check;
