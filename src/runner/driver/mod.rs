//! What the walker says to whoever is running the Technique, and what it is
//! told back. Everything shown goes through `show`, every question through
//! `ask`, and each frame of the review cursor through `review`.

use std::collections::VecDeque;
use std::io::{self, Write};

use crate::engraving::Motion;
use crate::formatting::{Identity, Render};
use crate::highlighting::Terminal;
use crate::value::Value;

mod batch;
mod console;
mod keys;
mod prompt;
mod script;
mod trail;
mod transcript;

pub use batch::{Batch, unattended};
pub use console::Interactive;
pub use keys::{Intent, Keys, MockKeyboard, Raw, RealKeyboard, intent};
pub use script::{Mock, Script};
pub use trail::{Silent, Visual};
pub use transcript::Transcript;

/// The glyph a trail line opens with.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Marker {
    /// `⇒` document entry, external departure.
    Depart,
    /// `⇐` document exit, external return.
    Return,
    /// `↘` scope entry; also the acquire prompt.
    Enter,
    /// `↙` scope close.
    Close,
    /// `→` step, command.
    Step,
    /// `»` action.
    Action,
}

/// A recorded or given verdict.
#[derive(Debug, Clone, PartialEq)]
pub enum Verdict {
    Done(Value),
    Skip,
    Fail(String),
}

/// What a bare `<Enter>` answers, and where the `<Esc>` menu opens. Declaration
/// order is verdict precedence: Fail beats Done beats Skip.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Standing {
    Skip,
    Done,
    Fail,
}

/// What a step is, for the unattended answer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Prose,
    Computable,
    System,
    Action,
    Choice,
}

/// A menu item.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Offer {
    Edit,
    Skip,
    Fail,
    Override,
    Quit,
}

/// Everything displayed, in walk order.
#[derive(Debug, Clone, PartialEq)]
pub enum Event<'a> {
    Commence {
        label: &'a str,
    },
    /// Pre-rendered text: header, declaration, title, description.
    Display(&'a str),
    Enter {
        path: &'a str,
        echo: &'a str,
    },
    Section {
        path: &'a str,
        numeral: &'a str,
        title: &'a str,
    },
    Step {
        path: &'a str,
        constraints: &'a str,
        text: &'a str,
        depth: usize,
    },
    /// `restored` marks a position a replay passed without asking.
    Verdict {
        marker: Marker,
        path: &'a str,
        verdict: &'a Verdict,
        restored: bool,
    },
    /// A gated command, as run.
    Command {
        path: &'a str,
        script: &'a str,
    },
    /// A gated action, as done.
    Action {
        path: &'a str,
        function: &'a str,
    },
    Depart {
        path: &'a str,
        echo: &'a str,
    },
    /// A Pure builtin called, or an effect a replay passed.
    Announce(&'a str),
    /// The walk starts again from the top after an amendment.
    Restart,
    Conclude {
        label: &'a str,
        verdict: &'a Verdict,
    },
}

/// The shape of what is being asked.
#[derive(Debug, Clone, PartialEq)]
pub enum Prompt<'a> {
    Confirm {
        standing: Standing,
        kind: Kind,
        produced: &'a Value,
        choices: &'a [&'a str],
    },
    Acquire {
        text: &'a str,
        name: Option<&'a str>,
        forma: Option<&'a str>,
        seed: Option<&'a Value>,
    },
    Command {
        script: &'a str,
    },
    Action {
        function: &'a str,
        verb: &'a str,
        value: &'a Value,
    },
    Depart {
        echo: &'a str,
    },
    External,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Question<'a> {
    pub marker: Marker,
    pub path: &'a str,
    pub prompt: Prompt<'a>,
    pub offers: &'a [Offer],
    /// Whether `<Up>` has anything to review.
    pub reviewable: bool,
    /// Text the user had typed, or the choice highlighted, before leaving for
    /// review.
    pub draft: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Answer {
    Done(Value),
    Skip,
    Fail(String),
    Override,
    /// Carries the draft typed so far, handed back on the same question.
    Review(Option<String>),
    Quit,
}

/// One position under the review cursor.
#[derive(Debug, Clone, PartialEq)]
pub struct Frame<'a> {
    pub marker: Marker,
    pub path: &'a str,
    /// `~ a, b` at a `Bind`, empty elsewhere.
    pub bound: &'a str,
    pub verdict: Option<&'a Verdict>,
    pub offers: &'a [Offer],
}

#[derive(Debug, Clone, PartialEq)]
pub enum Review {
    Move(Motion),
    Chose(Offer),
    /// `Fail` chosen, with the reason typed for it.
    Reason(String),
    Leave,
    Quit,
}

pub trait Driver {
    fn show(&mut self, event: Event<'_>);
    fn ask(&mut self, question: Question<'_>) -> Answer;
    fn review(&mut self, frame: Frame<'_>) -> Review;
    fn renderer(&self) -> &'static dyn Render;
}

/// Which driver walks a run: `Interactive` prompts the user, `Automatic` answers
/// unattended showing the trail, `Quiet` answers unattended showing nothing.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mode {
    Interactive,
    Automatic,
    Quiet,
}

/// What a driver shows.
pub trait Output {
    type Surface: Write;

    fn show(&mut self, event: Event<'_>);

    /// Where an interactive prompt draws.
    fn surface(&mut self) -> &mut Self::Surface;

    fn renderer(&self) -> &'static dyn Render;
}

/// How a driver answers.
pub trait Policy {
    fn ask<O: Output>(&mut self, out: &mut O, question: Question<'_>) -> Answer;
    fn review<O: Output>(&mut self, out: &mut O, frame: Frame<'_>) -> Review;
}

/// A driver composed of what it shows and how it answers.
pub struct Interface<O: Output, P: Policy> {
    out: O,
    policy: P,
}

impl<O: Output, P: Policy> Driver for Interface<O, P> {
    fn show(&mut self, event: Event<'_>) {
        self.out
            .show(event);
    }

    fn ask(&mut self, question: Question<'_>) -> Answer {
        self.policy
            .ask(&mut self.out, question)
    }

    fn review(&mut self, frame: Frame<'_>) -> Review {
        self.policy
            .review(&mut self.out, frame)
    }

    fn renderer(&self) -> &'static dyn Render {
        self.out
            .renderer()
    }
}

pub type Console<W = io::Stdout, K = RealKeyboard> = Interface<Visual<W>, Interactive<K>>;

pub type Automatic<W = io::Stdout> = Interface<Visual<W>, Batch>;

pub type Headless = Interface<Silent, Batch>;

pub type Scripted = Interface<Silent, Script>;

impl Console<io::Stdout> {
    pub fn new() -> Self {
        Interface {
            out: Visual {
                output: io::stdout(),
                renderer: &Terminal,
            },
            policy: Interactive { keys: RealKeyboard },
        }
    }
}

impl<W: Write, K: Keys> Console<W, K> {
    /// The trail is drawn uncoloured; the prompt line keeps its colours.
    pub fn with_keys(output: W, keys: K) -> Self {
        Interface {
            out: Visual {
                output,
                renderer: &Identity,
            },
            policy: Interactive { keys },
        }
    }
}

impl<W: Write, P: Policy> Interface<Visual<W>, P> {
    pub fn into_output(self) -> W {
        self.out
            .output
    }
}

impl Automatic<io::Stdout> {
    pub fn new(colour: bool) -> Self {
        Interface {
            out: Visual {
                output: io::stdout(),
                renderer: if colour { &Terminal } else { &Identity },
            },
            policy: Batch,
        }
    }
}

impl<W: Write> Automatic<W> {
    pub fn with_handle(output: W) -> Self {
        Interface {
            out: Visual {
                output,
                renderer: &Identity,
            },
            policy: Batch,
        }
    }
}

impl Headless {
    pub fn new() -> Self {
        Interface {
            out: Silent {
                discard: io::sink(),
            },
            policy: Batch,
        }
    }
}

impl Scripted {
    pub fn new<I: IntoIterator<Item = (String, Answer)>>(answers: I) -> Self {
        Scripted::reviewing(answers, [])
    }

    /// Review frames take `reviews` in order, then leave.
    pub fn reviewing<I, R>(answers: I, reviews: R) -> Self
    where
        I: IntoIterator<Item = (String, Answer)>,
        R: IntoIterator<Item = Review>,
    {
        Interface {
            out: Silent {
                discard: io::sink(),
            },
            policy: Script {
                answers: answers
                    .into_iter()
                    .collect(),
                reviews: reviews
                    .into_iter()
                    .collect::<VecDeque<_>>(),
            },
        }
    }
}
