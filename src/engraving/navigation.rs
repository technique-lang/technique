//! Moving over the records of a journal: where the review cursor can rest and
//! what each keystroke reaches. The records of the activations that stand are
//! the positions, in document order; the tree the motions climb is the one the
//! walk took, a callee enclosed by the step that invoked it rather than by the
//! path it was written at.
//!
//!   Up          previous record; from `Live`, the last record of the scope
//!               the prompt belongs to
//!   Down        next record; from the last record, `Live`, unless the run
//!               reached `Finish` and has no prompt to return to
//!   Left        out to the enclosing scope
//!   Right       in to a scope enclosed
//!   PageUp      across to the previous peer scope
//!   PageDown    across to the next peer scope
//!
//! Those four keep the plane the cursor is on: from an outcome record they
//! reach the scope's outcome record, from any other record its `Begin`, and
//! the `Begin` also where the scope has no outcome. `Right` descends into the
//! last scope enclosed from an outcome record and the first otherwise, which
//! is what makes it the inverse of `Left`.
//!
//! A record belongs to the scope it was written against, so `Left` leaves that
//! scope in one press wherever in it the cursor was resting. A motion with
//! nowhere to go refuses and changes nothing: `Up` at the first record, `Left`
//! at the root, `Right` with nothing to descend into, `PageUp`/`PageDown` with
//! no peer that way, and all four of those at `Live`, where the run is at a
//! prompt and they are cursor motion within the answer being typed.
//!
//! After an amendment the prompt need not follow the last position: work
//! beyond it that still stands keeps its place, and `Up`/`Down` pass across
//! the prompt as though it were not there.
//!
//! `tests/navigation/` holds the worked examples.

use super::history::{History, Standing};
use super::record::{Record, Serial, State};

/// Where the cursor rests. `Live` is the prompt the run is waiting at, which
/// sits immediately after the last record and is not itself a record.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Position {
    At(usize),
    Live,
}

/// The keystrokes the review cursor takes.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Motion {
    Up,
    Down,
    Left,
    Right,
    PageUp,
    PageDown,
}

/// The standing tree of a journal, and the motions over it.
pub struct Journal<'i> {
    records: &'i [Record],
    history: History,
    /// The root's own entry, which is a position.
    start: Option<usize>,
    /// The records the cursor stops on, in document order.
    order: Vec<usize>,
    /// Each record's place in `order`, where it has one.
    rank: Vec<Option<usize>>,
    /// The scope the run is prompting in, or `None` if it is not at a prompt.
    prompt: Option<Serial>,
}

impl<'i> Journal<'i> {
    /// Fold a journal and lay out its positions: each activation's own
    /// records in the order written, with each child enclosed placed where
    /// its current `Begin` falls among them, children in slot order.
    pub fn new(records: &'i [Record], prompt: Option<Serial>) -> Journal<'i> {
        let history = History::new(records);
        let start = records
            .iter()
            .position(|record| {
                if let State::Start { .. } = record.state {
                    true
                } else {
                    false
                }
            });

        let mut order = Vec::new();
        order.extend(start);
        for serial in history.roots() {
            place(&history, *serial, &mut order);
        }
        let mut rank = vec![None; records.len()];
        for (k, i) in order
            .iter()
            .enumerate()
        {
            rank[*i] = Some(k);
        }

        Journal {
            records,
            history,
            start,
            order,
            rank,
            prompt,
        }
    }

    pub fn records(&self) -> &'i [Record] {
        self.records
    }

    /// Where review opens: the last position within the scope the prompt
    /// belongs to, which the journal alone cannot name when a replay passed
    /// steps without writing anything.
    pub fn last(&self) -> Option<Position> {
        self.order
            .iter()
            .rev()
            .find(|at| {
                self.prompt
                    .map_or(true, |prompt| self.inside(self.within(**at), prompt))
            })
            .map(|at| Position::At(*at))
    }

    /// Where review opens: a finished run on the outcome of the last scope the
    /// root encloses rather than the root's own close, otherwise `last()`.
    pub fn opening(&self) -> Option<Position> {
        let last = self.last()?;
        if self
            .history
            .finished()
        {
            return self
                .step(last, Motion::Right)
                .or(Some(last));
        }
        Some(last)
    }

    /// Take one keystroke. `None` is a refusal, which changes nothing.
    pub fn step(&self, from: Position, motion: Motion) -> Option<Position> {
        let at = match from {
            Position::Live => {
                return match motion {
                    Motion::Up => self.last(),
                    _ => None,
                };
            }
            Position::At(at) => at,
        };
        // A record the cursor cannot rest on is no origin.
        let k = (*self
            .rank
            .get(at)?)?;
        match motion {
            Motion::Up => {
                let back = k.checked_sub(1)?;
                Some(Position::At(self.order[back]))
            }
            Motion::Down => {
                match self
                    .order
                    .get(k + 1)
                {
                    Some(at) => Some(Position::At(*at)),
                    // A run that walked to its end has no live prompt to
                    // return to.
                    None if self
                        .history
                        .finished() =>
                    {
                        None
                    }
                    None => Some(Position::Live),
                }
            }
            Motion::Left => {
                let parent = self.parent_of(self.within(at))?;
                self.rest_at(self.landing(at, parent)?)
            }
            Motion::Right => {
                let kin = self.kin(self.within(at));
                let child = if self.is_outcome(at) {
                    kin.last()
                } else {
                    kin.first()
                }?;
                self.rest_at(self.landing(at, *child)?)
            }
            Motion::PageUp => self.peer(at, true),
            Motion::PageDown => self.peer(at, false),
        }
    }

    // A record the cursor cannot rest on is not a meaningful destination.
    fn rest_at(&self, at: usize) -> Option<Position> {
        self.rank[at]?;
        Some(Position::At(at))
    }

    // The peer scope beside this record's.
    fn peer(&self, at: usize, back: bool) -> Option<Position> {
        let serial = self.within(at);
        let kin = self.kin(self.parent_of(serial)?);
        let k = kin
            .iter()
            .position(|s| *s == serial)?;
        let next = if back { k.checked_sub(1)? } else { k + 1 };
        self.rest_at(self.landing(at, *kin.get(next)?)?)
    }

    // Whether the cursor rests on the plane a scope closes on rather than the
    // one it opens on.
    fn is_outcome(&self, at: usize) -> bool {
        match &self.records[at].state {
            State::Done(_) | State::Skip | State::Fail(_) => true,
            _ => false,
        }
    }

    // Where a motion lands in the scope it reaches, holding the plane the
    // cursor was on.
    fn landing(&self, at: usize, serial: Serial) -> Option<usize> {
        if serial == Serial::ROOT {
            return self.start;
        }
        let activation = self
            .history
            .get(serial)?;
        match (self.is_outcome(at), activation.closed_at) {
            (true, Some(closed)) => Some(closed),
            _ => Some(activation.begun_at),
        }
    }

    fn within(&self, at: usize) -> Serial {
        self.records[at].serial
    }

    // The standing scopes a scope encloses.
    fn kin(&self, serial: Serial) -> Vec<Serial> {
        let children = if serial == Serial::ROOT {
            self.history
                .roots()
        } else {
            match self
                .history
                .get(serial)
            {
                Some(activation) => &activation.children,
                None => return Vec::new(),
            }
        };
        children
            .iter()
            .copied()
            .filter(|s| standing(&self.history, *s))
            .collect()
    }

    // Whether a scope is the one given or lies within it.
    fn inside(&self, mut serial: Serial, scope: Serial) -> bool {
        while serial != scope {
            match self.parent_of(serial) {
                Some(parent) => serial = parent,
                None => return false,
            }
        }
        true
    }

    fn parent_of(&self, serial: Serial) -> Option<Serial> {
        if serial == Serial::ROOT {
            return None;
        }
        Some(
            self.history
                .get(serial)?
                .parent,
        )
    }
}

// A withdrawn activation holds nothing to rest on until it is begun again.
fn standing(history: &History, serial: Serial) -> bool {
    match history.get(serial) {
        Some(activation) => activation.standing != Standing::Withdrawn,
        None => false,
    }
}

fn place(history: &History, serial: Serial, order: &mut Vec<usize>) {
    let activation = match history.get(serial) {
        Some(activation) if standing(history, serial) => activation,
        _ => return,
    };
    let mut own = activation
        .records
        .iter()
        .copied()
        .peekable();
    for child in &activation.children {
        let begun = match history.get(*child) {
            Some(child) => child.begun_at,
            None => continue,
        };
        while let Some(i) = own.next_if(|i| *i < begun) {
            order.push(i);
        }
        place(history, *child, order);
    }
    order.extend(own);
}

#[cfg(test)]
#[path = "checks/navigation.rs"]
mod check;
