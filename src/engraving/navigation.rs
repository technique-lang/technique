//! Moving over the records of a journal: where the review cursor can rest and
//! what each keystroke reaches. Every record that still stands is a position;
//! the tree the motions climb is the one the walk took, a callee enclosed by
//! the step that invoked it rather than by the path it was written at.
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
//! `tests/navigation/` holds the worked examples.

use std::collections::{HashMap, HashSet};

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

// One scope the walk entered, as the cursor sees it.
struct Scope {
    parent: Option<Serial>,
    /// Where it first opened, which is where it stands among its peers.
    first: usize,
    /// Its latest activation's `Begin`.
    begin: usize,
    outcome: Option<usize>,
}

/// The live tree of a journal, and the motions over it.
pub struct Journal<'i> {
    records: &'i [Record],
    scopes: HashMap<Serial, Scope>,
    /// The scope each record was written against.
    within: Vec<Serial>,
    /// The scopes that stand, in document order, which is the order peers
    /// stand in.
    opened: Vec<Serial>,
    /// The records the cursor stops on, in document order. `Stop`, `Resume`
    /// and `Finish` bracket a session rather than state anything the walk did,
    /// so they are not positions; `Start` is, being the root's own entry.
    order: Vec<usize>,
    /// Each record's place in `order`, where it has one.
    rank: Vec<Option<usize>>,
    finished: bool,
    /// The scope the run is prompting in, or `None` if it is not at a prompt.
    prompt: Option<Serial>,
}

impl<'i> Journal<'i> {
    /// Fold a journal into the tree it built. The enclosing scope of each is
    /// whichever was innermost open when its `Begin` landed, so a procedure is
    /// enclosed by the step that invoked it.
    ///
    /// A `Begin` written again for a scope already open is a new activation of
    /// it, and nothing recorded in or beneath it before then stands. A `Revoke`
    /// withdraws what its scope recorded but not what it encloses. Positions
    /// run in document order: a scope keeps the place its first `Begin` took.
    pub fn new(records: &'i [Record], prompt: Option<Serial>) -> Journal<'i> {
        let mut scopes: HashMap<Serial, Scope> = HashMap::new();
        let mut within = Vec::with_capacity(records.len());
        let mut open: Vec<Serial> = Vec::new();
        let mut revoked: HashMap<Serial, usize> = HashMap::new();
        let mut bound: HashMap<Serial, usize> = HashMap::new();
        // An `Invoke` introduces the next scope to open beneath its caller.
        let mut pending: HashMap<Serial, usize> = HashMap::new();
        let mut introduces: HashMap<usize, Serial> = HashMap::new();
        let mut introduced: HashMap<Serial, usize> = HashMap::new();
        let mut redispatched: HashSet<usize> = HashSet::new();
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
                            first: i,
                            begin: i,
                            outcome: None,
                        },
                    );
                    open.push(serial);
                }
                State::Begin(_) => {
                    match scopes.get_mut(&serial) {
                        Some(scope) => {
                            scope.begin = i;
                            scope.outcome = None;
                            if let Some(at) = open
                                .iter()
                                .position(|s| *s == serial)
                            {
                                open.truncate(at);
                            }
                        }
                        None => {
                            scopes.insert(
                                serial,
                                Scope {
                                    parent: open
                                        .last()
                                        .copied(),
                                    first: i,
                                    begin: i,
                                    outcome: None,
                                },
                            );
                        }
                    }
                    if let Some(caller) = scopes[&serial].parent {
                        if let Some(at) = pending.remove(&caller) {
                            introduces.insert(at, serial);
                            introduced.insert(serial, at);
                        }
                    }
                    open.push(serial);
                }
                State::Invoke(_) => {
                    // A later session reaching the same call writes it again.
                    if let Some(at) = pending.insert(serial, i) {
                        if records[at].state == record.state {
                            redispatched.insert(at);
                        }
                    }
                }
                State::Bind(_) => {
                    bound.insert(serial, i);
                }
                State::Done(_) | State::Skip | State::Fail(_) => {
                    if let Some(scope) = scopes.get_mut(&serial) {
                        scope.outcome = Some(i);
                    }
                    if open.last() == Some(&serial) {
                        open.pop();
                    }
                }
                State::Revoke => {
                    revoked.insert(serial, i);
                    bound.remove(&serial);
                    // The verdicts it rolled up into are withdrawn with it,
                    // and the replay comes back into it, writing nothing to
                    // reopen what it encloses.
                    let mut chain = vec![serial];
                    let mut at = Some(serial);
                    while let Some(scope) = at.and_then(|s| scopes.get_mut(&s)) {
                        scope.outcome = None;
                        at = scope.parent;
                        if let Some(parent) = at {
                            chain.push(parent);
                        }
                    }
                    chain.reverse();
                    open = chain;
                }
                State::Finish => finished = true,
                _ => {}
            }
            within.push(serial);
        }

        // Whether a record falls within its scope's latest activation and
        // after that of every scope enclosing it.
        let live = |i: usize, serial: Serial| {
            let mut at = match scopes.get(&serial) {
                Some(scope) if i >= scope.begin => scope.parent,
                _ => return false,
            };
            while let Some(parent) = at {
                let scope = &scopes[&parent];
                if i <= scope.begin {
                    return false;
                }
                at = scope.parent;
            }
            true
        };
        let place = |serial: Serial| {
            let mut key = Vec::new();
            let mut at = Some(serial);
            while let Some(scope) = at.map(|s| &scopes[&s]) {
                key.push(scope.first + 1);
                at = scope.parent;
            }
            key.reverse();
            key
        };

        let mut order: Vec<usize> = records
            .iter()
            .enumerate()
            .filter(|(i, record)| {
                let i = *i;
                let serial = record.serial;
                let scope = match scopes.get(&serial) {
                    Some(scope) => scope,
                    None => return false,
                };
                match record.state {
                    State::Stop | State::Resume | State::Finish | State::Revoke => false,
                    State::Start { .. } => true,
                    State::Begin(_) => scope.begin == i && live(i, serial),
                    State::Done(_) | State::Skip | State::Fail(_) => {
                        scope.outcome == Some(i) && live(i, serial)
                    }
                    State::Bind(_) => bound.get(&serial) == Some(&i) && live(i, serial),
                    State::Invoke(_) => match introduces.get(&i) {
                        Some(callee) => {
                            introduced[callee] == i && live(scopes[callee].begin, *callee)
                        }
                        None => !redispatched.contains(&i) && live(i, serial),
                    },
                    _ => {
                        live(i, serial)
                            && revoked
                                .get(&serial)
                                .map_or(true, |at| i > *at)
                    }
                }
            })
            .map(|(i, _)| i)
            .collect();
        // An `Invoke` stands just ahead of the scope it introduced, and every
        // other record after its scope's `Begin`, in the order written.
        order.sort_by_cached_key(|i| {
            let record = &records[*i];
            match record.state {
                State::Invoke(_) if introduces.contains_key(i) => place(introduces[i]),
                State::Start { .. } | State::Begin(_) => {
                    let mut key = place(record.serial);
                    key.push(0);
                    key
                }
                _ => {
                    let mut key = place(record.serial);
                    key.push(i + 1);
                    key
                }
            }
        });
        let opened: Vec<Serial> = order
            .iter()
            .map(|i| &records[*i])
            .filter(|record| match record.state {
                State::Start { .. } | State::Begin(_) => true,
                _ => false,
            })
            .map(|record| record.serial)
            .collect();
        let mut rank = vec![None; records.len()];
        for (k, i) in order
            .iter()
            .enumerate()
        {
            rank[*i] = Some(k);
        }

        Journal {
            records,
            scopes,
            within,
            opened,
            order,
            rank,
            finished,
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
            .filter(|at| {
                self.prompt
                    .map_or(true, |prompt| self.inside(self.within[**at], prompt))
            })
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
        self.scopes
            .get(&serial)?
            .parent
    }
}

#[cfg(test)]
#[path = "checks/navigation.rs"]
mod check;
