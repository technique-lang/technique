//! Execute a fold over a series of PFFTT records.

use std::collections::BTreeMap;
use std::collections::HashMap;

use super::record::{Record, Serial, State, Supplied};

/// A single record line in a PFFTT file. This is refined by the enclosing
/// scope and the edge reaching it, so two executions of the same document
/// address will be recorded as separate entries with different serials.
#[derive(Debug, Clone, PartialEq)]
pub struct Entry {
    pub serial: Serial,
    /// Input values given to scope.
    pub began: Vec<Supplied>,
    /// Results bound to variables coming out of scope.
    pub bound: Vec<Supplied>,
    pub outcome: Option<State>,
    /// Only the target of a revocation is marked, never an ancestor, which is
    /// what stops `walk_invoke` restoring the very argument being amended.
    pub revoked: bool,
}

// The location a scope represents.
#[derive(Debug, Clone)]
struct Scope {
    parent: Serial,
    edge: String,
    path: String,
}

/// A summary of the state of a run, after the fold used to construct it.
#[derive(Debug)]
pub struct Ledger {
    entries: BTreeMap<(Serial, String), Entry>,
    scopes: HashMap<Serial, Scope>,
    open: Vec<Serial>,
    highest: Serial,
}

impl Ledger {
    pub fn new() -> Ledger {
        Ledger {
            entries: BTreeMap::new(),
            scopes: HashMap::new(),
            open: Vec::new(),
            highest: Serial::LIFECYCLE,
        }
    }

    /// Whether the journal already states this and still truly: the recorded
    /// line stands, so there is nothing here to record.
    pub fn carries(&self, record: &Record) -> bool {
        match &record.state {
            State::Begin(supplied) => self.standing(record.serial, &record.path, supplied),
            // Invoke is guarded at its call site, which knows the callee.
            State::Bind(_)
            | State::Done(_)
            | State::Skip
            | State::Fail(_)
            | State::Revoke
            | State::Start { .. }
            | State::Finish
            | State::Stop
            | State::Resume
            | State::Invoke(_)
            | State::Execute { .. }
            | State::Return(_) => false,
        }
    }

    /// Fold one record in. The live runner calls this on each record it
    /// appends and on each it finds already standing; a reader calls it on
    /// each record it reads.
    pub fn apply(&mut self, record: &Record) {
        if record.serial > self.highest {
            self.highest = record.serial;
        }
        match &record.state {
            State::Begin(supplied) => self.open_scope(record, supplied.clone()),
            State::Bind(bound) => {
                if let Some(key) = self.key_of(record.serial) {
                    if let Some(entry) = self
                        .entries
                        .get_mut(&key)
                    {
                        entry.bound = bound.clone();
                    }
                }
            }
            State::Done(_) | State::Skip | State::Fail(_) => {
                if let Some(key) = self.key_of(record.serial) {
                    if let Some(entry) = self
                        .entries
                        .get_mut(&key)
                    {
                        entry.outcome = Some(
                            record
                                .state
                                .clone(),
                        );
                    }
                }
                // The stack pops back past this scope, dropping what it still
                // held open.
                if let Some(at) = self
                    .open
                    .iter()
                    .position(|s| *s == record.serial)
                {
                    self.open
                        .truncate(at);
                }
            }
            State::Revoke => self.revoke(record.serial),
            State::Start { .. }
            | State::Finish
            | State::Stop
            | State::Resume
            | State::Invoke(_)
            | State::Execute { .. }
            | State::Return(_) => {}
        }
    }

    // A Section short-circuits before descending, so an ancestor left standing
    // on its empty `Begin` would be skipped on the replay.
    fn revoke(&mut self, serial: Serial) {
        if let Some(key) = self.key_of(serial) {
            if let Some(entry) = self
                .entries
                .get_mut(&key)
            {
                entry.outcome = None;
                entry.revoked = true;
            }
        }
        // Descendants are left standing, their entries keeping them reachable.
        let mut at = serial;
        while let Some(parent) = self
            .scopes
            .get(&at)
            .map(|scope| scope.parent)
        {
            if parent == Serial::LIFECYCLE {
                break;
            }
            if let Some(key) = self.key_of(parent) {
                if let Some(entry) = self
                    .entries
                    .get_mut(&key)
                {
                    entry.outcome = None;
                }
            }
            at = parent;
        }
    }

    fn open_scope(&mut self, record: &Record, supplied: Vec<Supplied>) {
        let standing = self.standing(record.serial, &record.path, &supplied);
        // Without this, a resume after a mid-step Quit parents the second
        // walk's records under the step that was in flight.
        if let Some(at) = self
            .open
            .iter()
            .position(|s| *s == record.serial)
        {
            self.open
                .truncate(at);
        }
        // A serial is allocated per (parent, edge) pair, so re-entry does not
        // move it; the open stack answers only for one seen for the first time.
        let parent = match self
            .scopes
            .get(&record.serial)
        {
            Some(scope) => scope.parent,
            None => self
                .open
                .last()
                .copied()
                .unwrap_or(Serial::LIFECYCLE),
        };
        let edge = self.edge_under(parent, &record.path);
        let key = (parent, edge.clone());

        // Serial to path is a function: the same scope re-entered on a later
        // walk keeps its number, so a serial appearing at a path it has not
        // been seen at is a save/restore bug in the walker.
        debug_assert!(
            self.scopes
                .get(&record.serial)
                .map(|scope| scope.path == record.path)
                .unwrap_or(true),
            "serial {} recorded at {} but previously at {}",
            record
                .serial
                .render(),
            record.path,
            self.scopes
                .get(&record.serial)
                .map(|scope| scope
                    .path
                    .as_str())
                .unwrap_or("")
        );

        self.scopes
            .insert(
                record.serial,
                Scope {
                    parent,
                    edge,
                    path: record
                        .path
                        .clone(),
                },
            );
        if !standing {
            self.entries
                .insert(
                    key,
                    Entry {
                        serial: record.serial,
                        began: supplied,
                        bound: Vec::new(),
                        outcome: None,
                        revoked: false,
                    },
                );
        }
        self.open
            .push(record.serial);
    }

    // The suffix of a path beyond its parent's. Usually one component, but an
    // attributed step's edge is `@waiter/1` — the attribute frame is a path
    // segment that opens no scope of its own.
    fn edge_under(&self, parent: Serial, path: &str) -> String {
        let prefix = self.path_of(parent);
        path.strip_prefix(prefix)
            .unwrap_or(path)
            .to_string()
    }

    fn key_of(&self, serial: Serial) -> Option<(Serial, String)> {
        self.scopes
            .get(&serial)
            .map(|scope| {
                (
                    scope.parent,
                    scope
                        .edge
                        .clone(),
                )
            })
    }

    /// The document address a serial was recorded at; empty for the lifecycle
    /// serial, which brackets no scope.
    pub fn path_of(&self, serial: Serial) -> &str {
        self.scopes
            .get(&serial)
            .map(|scope| {
                scope
                    .path
                    .as_str()
            })
            .unwrap_or("")
    }

    /// Whether this serial's `Begin` already stands and the scope it opened is
    /// still unfinished: same path, same values, no outcome, not revoked. A
    /// completed entry reached again is a second execution, not a re-entry.
    pub fn standing(&self, serial: Serial, path: &str, supplied: &[Supplied]) -> bool {
        let scope = match self
            .scopes
            .get(&serial)
        {
            Some(scope) => scope,
            None => return false,
        };
        if scope.path != path {
            return false;
        }
        match self
            .entries
            .get(&(
                scope.parent,
                scope
                    .edge
                    .clone(),
            )) {
            Some(entry) => {
                !entry.revoked
                    && entry
                        .outcome
                        .is_none()
                    && entry
                        .began
                        .as_slice()
                        == supplied
            }
            None => false,
        }
    }

    /// What a prior walk recorded at this position, if it reached it.
    pub fn look(&self, parent: Serial, path: &str) -> Option<&Entry> {
        let edge = self.edge_under(parent, path);
        self.entries
            .get(&(parent, edge))
    }

    /// Every loop iteration a prior walk recorded within this scope, in index
    /// order, as `(index, entry)`. Read as a prefix range rather than by
    /// probing `[1]`, `[2]`, … in turn: an orphaned `[2]` between `[1]` and
    /// `[3]` would stop a probe at one and silently redo `[3]`'s work. The
    /// index is parsed rather than taken from key order, `[10]` sorting
    /// between `[1]` and `[2]`.
    pub fn iterations(&self, parent: Serial) -> Vec<(usize, &Entry)> {
        let low = (parent, "/[".to_string());
        let high = (parent, "/]".to_string());
        let mut found: Vec<(usize, &Entry)> = self
            .entries
            .range(low..high)
            .filter_map(|((_, edge), entry)| {
                let number = edge
                    .strip_prefix("/[")?
                    .strip_suffix(']')?
                    .parse()
                    .ok()?;
                Some((number, entry))
            })
            .collect();
        found.sort_by_key(|(number, _)| *number);
        found
    }

    /// The serial to record work at this address under: the one it already
    /// wears while it still stands, a fresh one once revoked.
    pub fn serial_for(&self, parent: Serial, path: &str) -> Serial {
        match self.look(parent, path) {
            Some(entry) if !entry.revoked => entry.serial,
            _ => self.next_serial(),
        }
    }

    /// The next serial never yet written.
    pub fn next_serial(&self) -> Serial {
        Serial(
            self.highest
                .0
                + 1,
        )
    }
}

#[cfg(test)]
#[path = "checks/ledger.rs"]
mod check;
