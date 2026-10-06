//! The fold of a journal into the tree of activations it records. The walker,
//! the review cursor, and `log` all read a run through this.

use std::collections::HashMap;

use super::record::{InvokeTarget, Record, Serial, State, Supplied};

/// Where an activation stands after the fold.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Standing {
    /// Begun and not closed: in flight, or stopped mid-way.
    Open,
    Closed,
    /// A revoked activation enclosing nothing; the next walk re-activates it.
    Withdrawn,
    /// A revoked activation enclosing children, or an ancestor of any revoked
    /// one; the next walk continues it and closes it again.
    Reopened,
}

/// One effectful host call made within an activation.
#[derive(Debug, Clone, PartialEq)]
pub struct Effect {
    pub function: String,
    /// `None` until its `Return` is read; `Some(None)` for a bare `Return`.
    pub returned: Option<Option<crate::value::Value>>,
}

/// The current activation of one slot.
#[derive(Debug, Clone, PartialEq)]
pub struct Activation {
    pub serial: Serial,
    pub parent: Serial,
    pub path: String,
    pub edge: String,
    pub occurrence: usize,
    pub began: Vec<Supplied>,
    pub bound: Vec<Supplied>,
    /// `Done`, `Skip` or `Fail`.
    pub outcome: Option<State>,
    pub standing: Standing,
    pub effects: Vec<Effect>,
    pub invoked: Vec<InvokeTarget>,
    /// Current children, in the order their slots were first begun.
    pub children: Vec<Serial>,
    /// What this slot last bound and concluded before a `Revoke` or a
    /// superseding `Begin`, for seeding the prompts that ask it again.
    pub former_bound: Vec<Supplied>,
    pub former_outcome: Option<State>,
    /// Whether a `Revoke` named this activation, not only something beneath it.
    pub revoked: bool,
    /// Indices into the journal of its `Begin`, `Bind`s and outcome.
    pub begun_at: usize,
    pub bound_at: Vec<usize>,
    pub closed_at: Option<usize>,
    /// Every record that stands for it, in journal order.
    pub records: Vec<usize>,
}

/// A journal folded.
#[derive(Debug)]
pub struct History {
    activations: HashMap<Serial, Activation>,
    slots: HashMap<(Serial, String, usize), Serial>,
    /// Children of the lifecycle root, i.e. the entry procedure's activation.
    roots: Vec<Serial>,
    highest: Serial,
    finished: bool,
    /// The last activation of each slot lost when an ancestor was superseded,
    /// until the slot is begun again.
    retired: HashMap<Serial, Activation>,
    /// How many slots each `(parent, edge)` holds.
    counts: HashMap<(Serial, String), usize>,
}

impl History {
    pub fn new(records: &[Record]) -> History {
        let mut history = History {
            activations: HashMap::new(),
            slots: HashMap::new(),
            roots: Vec::new(),
            highest: Serial::LIFECYCLE,
            finished: false,
            retired: HashMap::new(),
            counts: HashMap::new(),
        };
        let mut open: Vec<Serial> = Vec::new();

        for (i, record) in records
            .iter()
            .enumerate()
        {
            let serial = record.serial;
            match &record.state {
                // A corrupt journal's `Begin` on 000 would make the root its own ancestor.
                State::Begin(_) if serial == Serial::LIFECYCLE => {}
                State::Begin(began) => {
                    history.finished = false;
                    if history
                        .activations
                        .contains_key(&serial)
                        || history
                            .retired
                            .contains_key(&serial)
                    {
                        history.supersede(serial, i, record, began);
                        open = history.lineage(serial);
                    } else {
                        let parent = open
                            .last()
                            .copied()
                            .unwrap_or(Serial::LIFECYCLE);
                        history.introduce(serial, parent, i, record, began);
                        open.push(serial);
                    }
                }
                State::Done(_) | State::Skip | State::Fail(_) => {
                    if let Some(activation) = history
                        .activations
                        .get_mut(&serial)
                    {
                        if let Some(prior) = activation.closed_at {
                            activation
                                .records
                                .retain(|k| *k != prior);
                        }
                        activation.outcome = Some(
                            record
                                .state
                                .clone(),
                        );
                        activation.standing = Standing::Closed;
                        activation.closed_at = Some(i);
                        activation
                            .records
                            .push(i);
                        history.prune(serial);
                    }
                    if let Some(at) = open
                        .iter()
                        .position(|s| *s == serial)
                    {
                        open.truncate(at);
                    }
                }
                State::Bind(bound) => {
                    if let Some(activation) = history
                        .activations
                        .get_mut(&serial)
                    {
                        for item in bound {
                            activation
                                .bound
                                .retain(|b| b.name != item.name);
                            activation
                                .bound
                                .push(item.clone());
                        }
                        activation
                            .bound_at
                            .push(i);
                        activation
                            .records
                            .push(i);
                    }
                }
                State::Revoke => {
                    history.finished = false;
                    if let Some(standing) = history.revoke(serial) {
                        open = history.lineage(serial);
                        if standing == Standing::Withdrawn {
                            open.pop();
                        }
                    }
                }
                State::Execute { function } => {
                    if let Some(activation) = history
                        .activations
                        .get_mut(&serial)
                    {
                        activation
                            .effects
                            .push(Effect {
                                function: function.clone(),
                                returned: None,
                            });
                        activation
                            .records
                            .push(i);
                    }
                }
                State::Return(value) => {
                    if let Some(activation) = history
                        .activations
                        .get_mut(&serial)
                    {
                        // A gate put again replaces its bare `Return`.
                        if let Some(effect) = activation
                            .effects
                            .last_mut()
                        {
                            if let None | Some(None) = effect.returned {
                                effect.returned = Some(value.clone());
                            }
                        }
                        activation
                            .records
                            .push(i);
                    }
                }
                State::Invoke(target) => {
                    if let Some(activation) = history
                        .activations
                        .get_mut(&serial)
                    {
                        activation
                            .invoked
                            .push(target.clone());
                        activation
                            .records
                            .push(i);
                    }
                }
                State::Finish => history.finished = true,
                State::Stop => history.finished = false,
                State::Start { .. } | State::Resume => {}
            }
        }
        history
    }

    pub fn get(&self, serial: Serial) -> Option<&Activation> {
        self.activations
            .get(&serial)
    }

    /// The last activation of a slot that no longer stands because an
    /// enclosing scope was begun again, while the slot itself has not been.
    pub fn retired(&self, serial: Serial) -> Option<&Activation> {
        self.retired
            .get(&serial)
    }

    /// The serial recorded for a slot, if any walk reached it.
    pub fn slot(&self, parent: Serial, edge: &str, occurrence: usize) -> Option<Serial> {
        self.slots
            .get(&(parent, edge.to_string(), occurrence))
            .copied()
    }

    pub fn roots(&self) -> &[Serial] {
        &self.roots
    }

    /// The next serial never yet written.
    pub fn next_serial(&self) -> Serial {
        Serial(
            self.highest
                .0
                + 1,
        )
    }

    /// Whether the last session to write ended with `Finish`.
    pub fn finished(&self) -> bool {
        self.finished
    }

    // A fresh slot beneath the innermost open scope.
    fn introduce(
        &mut self,
        serial: Serial,
        parent: Serial,
        i: usize,
        record: &Record,
        began: &[Supplied],
    ) {
        let above = match self
            .activations
            .get(&parent)
        {
            Some(activation) => activation
                .path
                .as_str(),
            None => "",
        };
        let edge = edge(above, &record.path).to_string();
        let count = self
            .counts
            .entry((parent, edge.clone()))
            .or_insert(0);
        let occurrence = *count;
        *count += 1;
        self.slots
            .insert((parent, edge.clone(), occurrence), serial);
        if serial > self.highest {
            self.highest = serial;
        }
        self.activations
            .insert(
                serial,
                Activation {
                    serial,
                    parent,
                    path: record
                        .path
                        .clone(),
                    edge,
                    occurrence,
                    began: began.to_vec(),
                    bound: Vec::new(),
                    outcome: None,
                    standing: Standing::Open,
                    effects: Vec::new(),
                    invoked: Vec::new(),
                    children: Vec::new(),
                    former_bound: Vec::new(),
                    former_outcome: None,
                    revoked: false,
                    begun_at: i,
                    bound_at: Vec::new(),
                    closed_at: None,
                    records: vec![i],
                },
            );
        self.attach(parent, serial);
    }

    // A known slot begun again: the prior activation and all beneath it
    // no longer stand.
    fn supersede(&mut self, serial: Serial, i: usize, record: &Record, began: &[Supplied]) {
        let prior = match self
            .activations
            .remove(&serial)
        {
            Some(prior) => prior,
            None => self
                .retired
                .remove(&serial)
                .expect("a known serial"),
        };
        for child in &prior.children {
            self.retire(*child);
        }
        let (former_bound, former_outcome) = match prior.outcome {
            Some(_) => (prior.bound, prior.outcome),
            None => (prior.former_bound, prior.former_outcome),
        };
        self.activations
            .insert(
                serial,
                Activation {
                    serial,
                    parent: prior.parent,
                    path: record
                        .path
                        .clone(),
                    edge: prior.edge,
                    occurrence: prior.occurrence,
                    began: began.to_vec(),
                    bound: Vec::new(),
                    outcome: None,
                    standing: Standing::Open,
                    effects: Vec::new(),
                    invoked: Vec::new(),
                    children: Vec::new(),
                    former_bound,
                    former_outcome,
                    revoked: false,
                    begun_at: i,
                    bound_at: Vec::new(),
                    closed_at: None,
                    records: vec![i],
                },
            );
        self.attach(prior.parent, serial);
    }

    fn retire(&mut self, serial: Serial) {
        if let Some(activation) = self
            .activations
            .remove(&serial)
        {
            for child in &activation.children {
                self.retire(*child);
            }
            self.retired
                .insert(serial, activation);
        }
    }

    // A child revoked and not closed again by the time its parent closes was
    // not reached, and retires as a superseded one does.
    fn prune(&mut self, serial: Serial) {
        let Some(activation) = self
            .activations
            .get(&serial)
        else {
            return;
        };
        let stale: Vec<Serial> = activation
            .children
            .iter()
            .copied()
            .filter(|child| match self.get(*child) {
                Some(child) => child.revoked && child.standing != Standing::Closed,
                None => false,
            })
            .collect();
        for child in &stale {
            self.retire(*child);
        }
        if let Some(activation) = self
            .activations
            .get_mut(&serial)
        {
            activation
                .children
                .retain(|child| !stale.contains(child));
        }
    }

    // Place a child among its parent's in slot order.
    fn attach(&mut self, parent: Serial, serial: Serial) {
        let children = if parent == Serial::LIFECYCLE {
            &mut self.roots
        } else {
            match self
                .activations
                .get_mut(&parent)
            {
                Some(activation) => &mut activation.children,
                None => return,
            }
        };
        let at = children.partition_point(|s| *s < serial);
        if children.get(at) != Some(&serial) {
            children.insert(at, serial);
        }
    }

    // Withdraw an outcome and those it rolled up into, answering how the
    // revoked activation itself now stands.
    fn revoke(&mut self, serial: Serial) -> Option<Standing> {
        let activation = self
            .activations
            .get_mut(&serial)?;
        withdraw(activation);
        activation.revoked = true;
        activation.standing = if activation
            .children
            .is_empty()
        {
            // Only one enclosing nothing loses its values.
            activation
                .bound
                .clear();
            let bound = std::mem::take(&mut activation.bound_at);
            activation
                .records
                .retain(|k| !bound.contains(k));
            Standing::Withdrawn
        } else {
            Standing::Reopened
        };
        let standing = activation.standing;
        let mut at = activation.parent;
        while let Some(ancestor) = self
            .activations
            .get_mut(&at)
        {
            withdraw(ancestor);
            ancestor.standing = Standing::Reopened;
            at = ancestor.parent;
        }
        Some(standing)
    }

    // The serials from the outermost scope down to this one.
    fn lineage(&self, serial: Serial) -> Vec<Serial> {
        let mut chain = Vec::new();
        let mut at = serial;
        while let Some(activation) = self
            .activations
            .get(&at)
        {
            chain.push(at);
            at = activation.parent;
        }
        chain.reverse();
        chain
    }
}

fn withdraw(activation: &mut Activation) {
    if activation
        .outcome
        .is_some()
    {
        activation.former_outcome = activation
            .outcome
            .take();
        activation.former_bound = activation
            .bound
            .clone();
    }
    let closed = activation
        .closed_at
        .take();
    activation
        .records
        .retain(|k| Some(*k) != closed);
}

/// A path relative to its parent's: the suffix when the parent's path is a
/// prefix, the whole path otherwise (a callee's lexical address).
pub fn edge<'a>(parent: &str, path: &'a str) -> &'a str {
    match path.strip_prefix(parent) {
        Some(rest) if !parent.is_empty() && rest.starts_with('/') => rest,
        _ => path,
    }
}

#[cfg(test)]
#[path = "checks/history.rs"]
mod check;
