//! Moving over the records of a journal: where the review cursor can rest and
//! what each keystroke reaches. Every record is a position; the tree the
//! motions climb is the one the walk took, a callee enclosed by the step that
//! invoked it rather than by the path it was written at.
//!
//!   Up          previous record; from `Live`, the last record
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
//! `tests/navigation/` holds the worked examples.

use std::collections::{HashMap, HashSet};

use super::record::{Record, Serial, State, format_state};

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

// One scope the walk entered, as the cursor sees it.
struct Scope {
    parent: Option<Serial>,
    begin: usize,
    outcome: Option<usize>,
}

/// The live tree of a journal, and the motions over it.
pub struct Journal<'i> {
    records: &'i [Record],
    scopes: HashMap<Serial, Scope>,
    /// The scope each record was written against.
    within: Vec<Serial>,
    /// Scopes in the order they opened, which is the order peers stand in.
    opened: Vec<Serial>,
    /// The records the cursor stops on, in journal order. `Stop`, `Resume` and
    /// `Finish` bracket a session rather than state anything the walk did, so
    /// they are not positions; `Start` is, being the root's own entry.
    order: Vec<usize>,
    /// Each record's place in `order`, where it has one.
    rank: Vec<Option<usize>>,
    finished: bool,
}

impl<'i> Journal<'i> {
    /// Fold a journal into the tree it built. The enclosing scope of each is
    /// whichever was innermost open when its `Begin` landed, so a procedure is
    /// enclosed by the step that invoked it.
    pub fn new(records: &'i [Record]) -> Journal<'i> {
        let mut scopes: HashMap<Serial, Scope> = HashMap::new();
        let mut within = Vec::with_capacity(records.len());
        let mut opened = Vec::new();
        let mut open: Vec<Serial> = Vec::new();
        let mut finished = false;

        for (i, record) in records
            .iter()
            .enumerate()
        {
            let serial = record.serial;
            match &record.state {
                State::Start { .. } => {
                    scopes.insert(
                        serial,
                        Scope {
                            parent: None,
                            begin: i,
                            outcome: None,
                        },
                    );
                    opened.push(serial);
                    open.push(serial);
                }
                State::Begin(_) => {
                    // Re-entry keeps the scope it already opened, and closes
                    // whatever the previous walk left open inside it.
                    if let Some(at) = open
                        .iter()
                        .position(|s| *s == serial)
                    {
                        open.truncate(at);
                    } else {
                        scopes.insert(
                            serial,
                            Scope {
                                parent: open
                                    .last()
                                    .copied(),
                                begin: i,
                                outcome: None,
                            },
                        );
                        opened.push(serial);
                    }
                    open.push(serial);
                }
                State::Done(_) | State::Skip | State::Fail(_) => {
                    if let Some(scope) = scopes.get_mut(&serial) {
                        scope.outcome = Some(i);
                    }
                    if open.last() == Some(&serial) {
                        open.pop();
                    }
                }
                State::Finish => finished = true,
                _ => {}
            }
            within.push(serial);
        }

        // A revoked serial is nowhere to go back to, nor is anything under it:
        // the replay redid that work under fresh serials.
        let revoked: HashSet<Serial> = records
            .iter()
            .filter(|record| {
                if let State::Revoke = record.state {
                    true
                } else {
                    false
                }
            })
            .map(|record| record.serial)
            .collect();
        let superseded = |serial: Serial| {
            let mut at = serial;
            loop {
                if revoked.contains(&at) {
                    return true;
                }
                match scopes
                    .get(&at)
                    .and_then(|scope| scope.parent)
                {
                    Some(parent) => at = parent,
                    None => return false,
                }
            }
        };

        // A scope the cursor cannot rest in is not one to cross into either.
        opened.retain(|serial| !superseded(*serial));

        // A resumed walk rewrites lines it already recorded; only the last
        // stands. Two records are the same record when they would write the
        // same line, so a step calling two procedures keeps both.
        let stated: Vec<String> = records
            .iter()
            .map(|record| {
                let mut out = String::new();
                format_state(&mut out, &record.state);
                out
            })
            .collect();
        let mut latest: HashMap<(Serial, &str, &str), usize> = HashMap::new();
        for (i, record) in records
            .iter()
            .enumerate()
        {
            latest.insert(
                (
                    record.serial,
                    record
                        .path
                        .as_str(),
                    stated[i].as_str(),
                ),
                i,
            );
        }

        let mut order = Vec::with_capacity(records.len());
        let mut rank = vec![None; records.len()];
        for (i, record) in records
            .iter()
            .enumerate()
        {
            match record.state {
                State::Stop | State::Resume | State::Finish => continue,
                _ => {}
            }
            if superseded(record.serial) {
                continue;
            }
            let address = (
                record.serial,
                record
                    .path
                    .as_str(),
                stated[i].as_str(),
            );
            if latest.get(&address) != Some(&i) {
                continue;
            }
            rank[i] = Some(order.len());
            order.push(i);
        }

        Journal {
            records,
            scopes,
            within,
            opened,
            order,
            rank,
            finished,
        }
    }

    pub fn records(&self) -> &'i [Record] {
        self.records
    }

    /// Where review opens: the last position, which on a resumed run is the
    /// last thing the walk did rather than the `Resume` that reopened it.
    pub fn last(&self) -> Option<Position> {
        self.order
            .last()
            .map(|at| Position::At(*at))
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
        let k = self.rank[at]?;
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
                    None if self.finished => None,
                    None => Some(Position::Live),
                }
            }
            Motion::Left => {
                let parent = self
                    .scope(at)?
                    .parent?;
                self.rest_at(self.landing(at, parent)?)
            }
            Motion::Right => {
                let serial = self.within[at];
                let mut kin = self
                    .opened
                    .iter()
                    .filter(|s| self.parent_of(**s) == Some(serial));
                let child = if self.is_outcome(at) {
                    kin.last()
                } else {
                    kin.next()
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
        let serial = self.within[at];
        let parent = self.parent_of(serial);
        let kin: Vec<Serial> = self
            .opened
            .iter()
            .copied()
            .filter(|s| self.parent_of(*s) == parent)
            .collect();
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
        let scope = self
            .scopes
            .get(&serial)?;
        match (self.is_outcome(at), scope.outcome) {
            (true, Some(outcome)) => Some(outcome),
            _ => Some(scope.begin),
        }
    }

    fn scope(&self, at: usize) -> Option<&Scope> {
        self.scopes
            .get(&self.within[at])
    }

    fn parent_of(&self, serial: Serial) -> Option<Serial> {
        self.scopes
            .get(&serial)?
            .parent
    }
}

#[cfg(test)]
#[path = "checks/navigation.rs"]
mod check;
