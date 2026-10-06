//! The keyboard state behind a live prompt and a review frame: each folds one
//! `Intent` at a time and returns an answer once the user has given one.

use crate::engraving::{Motion, serialize_value};
use crate::parsing::parse_numeric;
use crate::runner::evaluator::parse_list_literal;
use crate::value::{Numeric, Value};

use super::keys::Intent;
use super::{Answer, Offer, Prompt, Question, Review, Standing, Verdict};

/// Longest scalar editable on the prompt line.
const INLINE_MAX: usize = 78;

impl Offer {
    pub(super) fn label(self) -> &'static str {
        match self {
            Offer::Edit => "Edit",
            Offer::Skip => "Skip",
            Offer::Fail => "Fail",
            Offer::Override => "Override",
            Offer::Quit => "Quit",
        }
    }

    fn shortcut(self) -> char {
        match self {
            Offer::Edit => 'e',
            Offer::Skip => 's',
            Offer::Fail => 'f',
            Offer::Override => 'o',
            Offer::Quit => 'q',
        }
    }
}

/// What the prompt line holds when no menu is open.
pub(super) enum Field {
    Edit {
        buffer: String,
        cursor: usize,
        edited: bool,
        /// Returned verbatim when the buffer is accepted unedited.
        original: Value,
        /// Drawn between `[` and `]` and submitted as a list.
        bracketed: bool,
    },
    Frozen {
        produced: Value,
    },
    Choose {
        choices: Vec<String>,
        active: usize,
    },
}

/// A row of offers with at most one highlighted.
pub(super) struct Menu {
    pub(super) offers: Vec<Offer>,
    pub(super) active: Option<usize>,
}

impl Menu {
    fn open(offers: &[Offer], active: Option<usize>) -> Self {
        Menu {
            offers: offers.to_vec(),
            active,
        }
    }

    // Clamps at either end rather than wrapping.
    fn step(&mut self, motion: Motion, enabled: impl Fn(Offer) -> bool) {
        let len = self
            .offers
            .len();
        let found = match motion {
            Motion::Left => (0..self
                .active
                .unwrap_or(len))
                .rev()
                .find(|&i| enabled(self.offers[i])),
            Motion::Right => (self
                .active
                .map_or(0, |at| at + 1)..len)
                .find(|&i| enabled(self.offers[i])),
            _ => None,
        };
        if found.is_some() {
            self.active = found;
        }
    }

    fn select(&mut self, typed: char, enabled: impl Fn(Offer) -> bool) -> Option<Offer> {
        let typed = typed.to_ascii_lowercase();
        let at = self
            .offers
            .iter()
            .position(|offer| offer.shortcut() == typed && enabled(*offer))?;
        self.active = Some(at);
        Some(self.offers[at])
    }

    fn chosen(&self) -> Option<Offer> {
        self.active
            .and_then(|at| {
                self.offers
                    .get(at)
                    .copied()
            })
    }
}

/// The text typed for a Fail reason.
#[derive(Default)]
pub(super) struct Reason {
    pub(super) buffer: String,
    pub(super) cursor: usize,
}

/// A live prompt. `reason` is open only while `menu` rests on Fail.
pub(super) struct Asking {
    pub(super) field: Field,
    pub(super) menu: Option<Menu>,
    pub(super) reason: Option<Reason>,
    pub(super) standing: Standing,
    offers: Vec<Offer>,
    reviewable: bool,
}

impl Asking {
    pub(super) fn begin(question: &Question<'_>) -> Self {
        let draft = question.draft;
        let (field, standing) = match &question.prompt {
            Prompt::Confirm {
                standing,
                produced,
                choices,
                ..
            } => {
                let field = if !choices.is_empty() {
                    let active = draft
                        .and_then(|text| {
                            choices
                                .iter()
                                .position(|c| *c == text)
                        })
                        .unwrap_or(0);
                    Field::Choose {
                        choices: choices
                            .iter()
                            .map(|c| c.to_string())
                            .collect(),
                        active,
                    }
                } else {
                    match (draft, editable_seed(produced)) {
                        (Some(text), Some(_)) => revise(text, (*produced).clone(), false),
                        _ => Field::Frozen {
                            produced: (*produced).clone(),
                        },
                    }
                };
                (field, *standing)
            }
            Prompt::Acquire { forma, seed, .. } => {
                let list = is_list_forma(*forma);
                let text = match seed {
                    Some(value) if list => list_seed(value),
                    Some(value) => editable_seed(value),
                    None => None,
                }
                .unwrap_or_default();
                let field = match draft {
                    Some(typed) => revise(typed, Value::Literali(text), list),
                    None => edit(text.clone(), Value::Literali(text), list),
                };
                (field, Standing::Done)
            }
            Prompt::Command { script } => {
                let script = script.trim_end();
                let original = Value::Literali(script.to_string());
                let field = match draft {
                    Some(typed) => revise(typed, original, false),
                    None => edit(script.to_string(), original, false),
                };
                (field, Standing::Done)
            }
            Prompt::Action { .. } | Prompt::Depart { .. } | Prompt::External => (
                Field::Frozen {
                    produced: Value::Unitus,
                },
                Standing::Done,
            ),
        };
        Asking {
            field,
            menu: None,
            reason: None,
            standing,
            offers: question
                .offers
                .to_vec(),
            reviewable: question.reviewable,
        }
    }

    /// Whether an offer can be taken. Edit needs a frozen value that is an
    /// editable scalar, which only the value can tell.
    pub(super) fn offerable(&self, item: Offer) -> bool {
        offerable(&self.field, item)
    }

    fn landing(&self) -> Option<usize> {
        let target = match self.standing {
            Standing::Fail => Some(Offer::Fail),
            Standing::Skip => Some(Offer::Skip),
            Standing::Done => None,
        };
        landing(&self.offers, target)
    }

    fn draft(&self) -> Option<String> {
        match &self.field {
            Field::Edit {
                buffer,
                edited: true,
                ..
            } => Some(buffer.clone()),
            Field::Choose { choices, active } => Some(choices[*active].clone()),
            _ => None,
        }
    }

    pub(super) fn handle(&mut self, intent: Intent) -> Option<Answer> {
        if self
            .reason
            .is_some()
        {
            self.reason_key(intent)
        } else if self
            .menu
            .is_some()
        {
            self.menu_key(intent)
        } else {
            self.field_key(intent)
        }
    }

    fn activate(&mut self, item: Offer) -> Option<Answer> {
        match item {
            Offer::Edit => {
                if let Field::Frozen { produced } = &mut self.field {
                    let taken = std::mem::replace(produced, Value::Unitus);
                    self.field = match editable_seed(&taken) {
                        Some(seed) => edit(seed, taken, false),
                        None => Field::Frozen { produced: taken },
                    };
                }
                self.menu = None;
                None
            }
            Offer::Skip => Some(Answer::Skip),
            Offer::Fail => {
                self.reason = Some(Reason::default());
                None
            }
            Offer::Override => Some(Answer::Override),
            Offer::Quit => Some(Answer::Quit),
        }
    }

    fn menu_key(&mut self, intent: Intent) -> Option<Answer> {
        let field = &self.field;
        let menu = self
            .menu
            .as_mut()?;
        let item = match intent {
            Intent::Move(motion) => {
                menu.step(motion, |item| offerable(field, item));
                return None;
            }
            Intent::Accept => match menu.chosen() {
                Some(item) => item,
                None => {
                    self.menu = None;
                    return None;
                }
            },
            Intent::Typed(c) => menu.select(c, |item| offerable(field, item))?,
            Intent::Decline => {
                self.menu = None;
                return None;
            }
            Intent::Erase | Intent::End => return None,
        };
        self.activate(item)
    }

    // Esc goes back to the menu, still on Fail.
    fn reason_key(&mut self, intent: Intent) -> Option<Answer> {
        let reason = self
            .reason
            .as_mut()?;
        match intent {
            Intent::Accept => Some(Answer::Fail(std::mem::take(&mut reason.buffer))),
            Intent::Decline => {
                self.reason = None;
                None
            }
            other => {
                text_key(&mut reason.buffer, &mut reason.cursor, other);
                None
            }
        }
    }

    fn field_key(&mut self, intent: Intent) -> Option<Answer> {
        if let Intent::Decline = intent {
            self.menu = Some(Menu::open(&self.offers, self.landing()));
            return None;
        }
        if let Intent::Move(Motion::Up) = intent {
            if self.reviewable {
                return Some(Answer::Review(self.draft()));
            }
        }
        let standing = self.standing;
        match &mut self.field {
            Field::Edit {
                buffer,
                cursor,
                edited,
                original,
                bracketed,
            } => match intent {
                Intent::Accept => {
                    if *bracketed {
                        // A buffer that does not parse is refused, leaving the edit open.
                        parse_list_literal(&format!("[{}]", buffer))
                            .map(|items| Answer::Done(Value::Arraeum(items)))
                    } else if buffer.is_empty() {
                        // Text requires a value; declining is Skip or Fail from the menu.
                        None
                    } else if !*edited {
                        Some(Answer::Done(std::mem::replace(original, Value::Unitus)))
                    } else if let Value::Quanticle(_) = original {
                        parse_numeric(buffer)
                            .map(|numeric| Answer::Done(Value::Quanticle(Numeric::from(&numeric))))
                    } else {
                        Some(Answer::Done(Value::Literali(std::mem::take(buffer))))
                    }
                }
                other => {
                    if text_key(buffer, cursor, other) {
                        *edited = true;
                    }
                    None
                }
            },
            Field::Frozen { produced } => match intent {
                Intent::Accept => Some(match standing {
                    Standing::Done => Answer::Done(std::mem::replace(produced, Value::Unitus)),
                    Standing::Skip => Answer::Skip,
                    Standing::Fail => Answer::Fail(String::new()),
                }),
                _ => None,
            },
            Field::Choose { choices, active } => match intent {
                Intent::Move(Motion::Left) => {
                    if *active > 0 {
                        *active -= 1;
                    }
                    None
                }
                Intent::Move(Motion::Right) => {
                    if *active + 1 < choices.len() {
                        *active += 1;
                    }
                    None
                }
                Intent::Accept => Some(Answer::Done(Value::Literali(std::mem::take(
                    &mut choices[*active],
                )))),
                _ => None,
            },
        }
    }
}

/// A review frame. Nothing offered is ever disabled here: the walker withholds
/// an offer rather than greying it.
pub(super) struct Reviewing {
    offers: Vec<Offer>,
    target: Option<Offer>,
    pub(super) menu: Option<Menu>,
    pub(super) reason: Option<Reason>,
}

impl Reviewing {
    pub(super) fn begin(offers: &[Offer], verdict: Option<&Verdict>) -> Self {
        let target = match verdict {
            Some(Verdict::Fail(_)) => Some(Offer::Fail),
            Some(Verdict::Skip) => Some(Offer::Skip),
            Some(Verdict::Done(_)) | None => None,
        };
        Reviewing {
            offers: offers.to_vec(),
            target,
            menu: None,
            reason: None,
        }
    }

    pub(super) fn handle(&mut self, intent: Intent) -> Option<Review> {
        if let Some(reason) = &mut self.reason {
            // Esc goes back to the cursor, not the menu.
            return match intent {
                Intent::Accept => Some(Review::Reason(std::mem::take(&mut reason.buffer))),
                Intent::Decline => {
                    self.reason = None;
                    self.menu = None;
                    None
                }
                other => {
                    text_key(&mut reason.buffer, &mut reason.cursor, other);
                    None
                }
            };
        }
        if let Intent::End = intent {
            return Some(Review::Leave);
        }
        match &mut self.menu {
            Some(menu) => {
                let item = match intent {
                    Intent::Move(motion) => {
                        menu.step(motion, |_| true);
                        return None;
                    }
                    Intent::Accept => menu.chosen()?,
                    Intent::Typed(c) => menu.select(c, |_| true)?,
                    Intent::Decline => {
                        self.menu = None;
                        return None;
                    }
                    Intent::Erase | Intent::End => return None,
                };
                if let Offer::Fail = item {
                    self.reason = Some(Reason::default());
                    None
                } else {
                    Some(Review::Chose(item))
                }
            }
            None => match intent {
                Intent::Move(motion) => Some(Review::Move(motion)),
                Intent::Decline => {
                    let at = landing(&self.offers, self.target).unwrap_or(0);
                    self.menu = Some(Menu::open(&self.offers, Some(at)));
                    None
                }
                Intent::Accept | Intent::Typed(_) | Intent::Erase | Intent::End => None,
            },
        }
    }
}

/// Where a menu opens: on the offer the standing names, else on Edit if it
/// leads, else on nothing.
fn landing(offers: &[Offer], target: Option<Offer>) -> Option<usize> {
    target
        .and_then(|item| {
            offers
                .iter()
                .position(|m| *m == item)
        })
        .or_else(|| match offers.first() {
            Some(Offer::Edit) => Some(0),
            _ => None,
        })
}

fn offerable(field: &Field, item: Offer) -> bool {
    match item {
        Offer::Edit => match field {
            Field::Frozen { produced } => editable_seed(produced).is_some(),
            _ => false,
        },
        _ => true,
    }
}

/// The text an editable scalar opens its buffer with.
fn editable_seed(value: &Value) -> Option<String> {
    match value {
        Value::Quanticle(_) => Some(value.to_string()),
        Value::Literali(text) if is_inline(text) => Some(text.clone()),
        _ => None,
    }
}

/// A list's elements as typed between the brackets.
fn list_seed(value: &Value) -> Option<String> {
    match value {
        Value::Arraeum(items) => Some(
            items
                .iter()
                .map(serialize_value)
                .collect::<Vec<_>>()
                .join(", "),
        ),
        _ => None,
    }
}

fn edit(buffer: String, original: Value, bracketed: bool) -> Field {
    let cursor = buffer.len();
    Field::Edit {
        buffer,
        cursor,
        edited: false,
        original,
        bracketed,
    }
}

/// An edit field restored from a draft typed before leaving for review.
fn revise(draft: &str, original: Value, bracketed: bool) -> Field {
    Field::Edit {
        buffer: draft.to_string(),
        cursor: draft.len(),
        edited: true,
        original,
        bracketed,
    }
}

fn is_list_forma(forma: Option<&str>) -> bool {
    match forma {
        Some(text) => text.starts_with('[') && text.ends_with(']'),
        None => false,
    }
}

fn is_inline(text: &str) -> bool {
    !text.contains('\n')
        && text
            .chars()
            .count()
            <= INLINE_MAX
}

fn prev_boundary(s: &str, i: usize) -> usize {
    s[..i]
        .chars()
        .next_back()
        .map_or(i, |c| i - c.len_utf8())
}

fn next_boundary(s: &str, i: usize) -> usize {
    s[i..]
        .chars()
        .next()
        .map_or(i, |c| i + c.len_utf8())
}

/// Apply one editing key, returning whether the text changed.
fn text_key(buffer: &mut String, cursor: &mut usize, intent: Intent) -> bool {
    match intent {
        Intent::Typed(c) => {
            buffer.insert(*cursor, c);
            *cursor += c.len_utf8();
            true
        }
        Intent::Erase => {
            if *cursor > 0 {
                let start = prev_boundary(buffer, *cursor);
                buffer.replace_range(start..*cursor, "");
                *cursor = start;
                true
            } else {
                false
            }
        }
        Intent::Move(Motion::Left) => {
            *cursor = prev_boundary(buffer, *cursor);
            false
        }
        Intent::Move(Motion::Right) => {
            *cursor = next_boundary(buffer, *cursor);
            false
        }
        _ => false,
    }
}
