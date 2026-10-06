//! Test drivers: prepared answers without a terminal.

use std::collections::{HashMap, VecDeque};

use crate::value::Value;

use super::batch::{Batch, unattended};
use super::{
    Answer, Driver, Event, Frame, Marker, Output, Policy, Prompt, Question, Review, Standing,
};
use crate::formatting::{Identity, Render};

/// A prepared answer at named paths, each given the first time its path is
/// asked; every other question is answered unattended. Review frames take
/// prepared moves in order, then leave.
pub struct Script {
    pub(super) answers: HashMap<String, Answer>,
    pub(super) reviews: VecDeque<Review>,
}

impl Policy for Script {
    fn ask<O: Output>(&mut self, out: &mut O, question: Question<'_>) -> Answer {
        match self
            .answers
            .remove(question.path)
        {
            Some(answer) => answer,
            None => Batch.ask(out, question),
        }
    }

    fn review<O: Output>(&mut self, _out: &mut O, _frame: Frame<'_>) -> Review {
        self.reviews
            .pop_front()
            .unwrap_or(Review::Leave)
    }
}

/// Answers from a queue and keeps a log of everything shown and asked, each
/// entry the `Debug` form of the `Event` or `Question`. A close standing at
/// Done, a command and an action answer themselves without taking from the
/// queue; a standing Fail or Skip takes one if any is left.
#[derive(Debug, Default)]
pub struct Mock {
    answers: VecDeque<Answer>,
    reviews: VecDeque<Review>,
    log: Vec<String>,
}

impl Mock {
    pub fn new() -> Self {
        Mock::default()
    }

    pub fn with_answers<I: IntoIterator<Item = Answer>>(answers: I) -> Self {
        Mock {
            answers: answers
                .into_iter()
                .collect(),
            ..Mock::default()
        }
    }

    pub fn reviewing<R: IntoIterator<Item = Review>>(mut self, reviews: R) -> Self {
        self.reviews = reviews
            .into_iter()
            .collect();
        self
    }

    pub fn log(&self) -> &[String] {
        &self.log
    }
}

impl Driver for Mock {
    fn show(&mut self, event: Event<'_>) {
        self.log
            .push(format!("{:?}", event));
    }

    fn ask(&mut self, question: Question<'_>) -> Answer {
        self.log
            .push(format!("{:?}", question));
        match &question.prompt {
            Prompt::Confirm {
                standing: Standing::Done,
                ..
            } if question.marker == Marker::Close => Answer::Done(Value::Unitus),
            Prompt::Confirm {
                standing: Standing::Done,
                ..
            }
            | Prompt::Depart { .. }
            | Prompt::External => self
                .answers
                .pop_front()
                .expect("Mock::ask called with no canned answers remaining"),
            Prompt::Confirm { .. } => self
                .answers
                .pop_front()
                .unwrap_or_else(|| unattended(&question)),
            Prompt::Acquire { .. } => self
                .answers
                .pop_front()
                .unwrap_or(Answer::Done(Value::Unitus)),
            Prompt::Command { script } => Answer::Done(Value::Literali(script.to_string())),
            Prompt::Action { .. } => Answer::Done(Value::Unitus),
        }
    }

    fn review(&mut self, frame: Frame<'_>) -> Review {
        self.log
            .push(format!("{:?}", frame));
        self.reviews
            .pop_front()
            .unwrap_or(Review::Leave)
    }

    fn renderer(&self) -> &'static dyn Render {
        &Identity
    }
}

#[cfg(test)]
#[path = "checks/script.rs"]
mod check;
