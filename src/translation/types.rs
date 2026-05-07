// Intermediate Representation types for translated Technique procedures.
//
// These types complement those found in `crate::language::types` for the
// parser's abstract syntax tree but lift and reshape constructs toward a form
// that can be walked by an interpreter. Where the parsed input already
// carries the right shape - descriptive paragraphs, signatures, response
// values, attributes - the types here borrow from the parser's internal
// abstract syntax tree output rather than mirroring it. Where translation
// adds information - resolved procedure references, attributes lifted onto
// steps, the desugared spine of a procedure body - then the object here will
// own the data.

use crate::language;

/// Top-level Technique translated to a runnable program.
#[derive(Debug, Eq, PartialEq, Default)]
pub struct Program<'i> {
    /// All procedures declared in the input document, in source order. If an
    /// anonymous wrapper for a top-level `Technique::Steps`-only document was
    /// created it will be at index 0.
    pub subroutines: Vec<Subroutine<'i>>,
}

impl<'i> Program<'i> {
    pub fn new() -> Self {
        Program {
            subroutines: Vec::new(),
        }
    }
}

/// Index of a subroutine in `Program.subroutines`. Used as the resolved form
/// for the target of an invocation.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct SubroutineId(pub usize);

#[derive(Debug, Eq, PartialEq)]
pub struct Subroutine<'i> {
    /// If this is a synthetic wrapper around a top-level `Technique::Steps`
    /// then `None`, otherwise all procedures have names.
    pub name: Option<language::Identifier<'i>>,
    pub title: Option<&'i str>,
    pub description: &'i [language::Paragraph<'i>],
    pub parameters: Option<&'i [language::Identifier<'i>]>,
    pub signature: Option<&'i language::Signature<'i>>,
    pub body: Operation<'i>,
}

impl<'i> Subroutine<'i> {
    /// Stub procedure with the given name and otherwise empty fields.
    /// Subsequent translation passes fill in title, description, parameters,
    /// signature, and body.
    pub fn new(name: language::Identifier<'i>) -> Self {
        Subroutine {
            name: Some(name),
            title: None,
            description: &[],
            parameters: None,
            signature: None,
            body: Operation::Sequence(Vec::new()),
        }
    }

    /// Synthetic anonymous-wrapper procedure used when a document has no
    /// procedure shell (a top-level `Technique::Steps`-only document).
    pub fn anonymous() -> Self {
        Subroutine {
            name: None,
            title: None,
            description: &[],
            parameters: None,
            signature: None,
            body: Operation::Sequence(Vec::new()),
        }
    }
}

/// Every node of the Intermediate Representation form resulting from
/// desugaring the surface language is an `Operation` (c.f. opcode in an
/// instruction set). Later this will be instantiated into a tree that the
/// interpreter can walks recursively and reduce.
#[derive(Debug, Eq, PartialEq)]
pub enum Operation<'i> {
    Variable(language::Identifier<'i>),
    Number(language::Numeric<'i>),
    String(Vec<Fragment<'i>>),
    Multiline(Option<&'i str>, Vec<&'i str>),
    Tablet(Vec<Entry<'i>>),
    Invoke(Invoke<'i>),
    Execution {
        target: language::Identifier<'i>,
        arguments: Vec<Operation<'i>>,
    },
    Sequence(Vec<Operation<'i>>),
    Section {
        numeral: &'i str,
        title: Option<&'i language::Paragraph<'i>>,
        body: Box<Operation<'i>>,
    },
    Step {
        ordinal: Ordinal<'i>,
        attributes: Vec<&'i [language::Attribute<'i>]>,
        description: &'i [language::Paragraph<'i>],
        body: Box<Operation<'i>>,
        expects: Option<&'i [language::Response<'i>]>,
    },
    Loop {
        names: &'i [language::Identifier<'i>],
        over: Option<Box<Operation<'i>>>,
        body: Box<Operation<'i>>,
    },
    Bind {
        names: &'i [language::Identifier<'i>],
        value: Box<Operation<'i>>,
    },
}

/// A step's lexical kind. `Dependent` carries the verbatim ordinal string as
/// captured by the parser (`"1"`, `"a"`, `"iii"`, ...); `Parallel` has no
/// captured form, its position deriving from its index in the surrounding
/// `Sequence`.
#[derive(Debug, Eq, PartialEq)]
pub enum Ordinal<'i> {
    Dependent(&'i str),
    Parallel,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Invoke<'i> {
    pub target: SubroutineRef<'i>,
    pub arguments: Vec<Operation<'i>>,
}

/// Reference to a subroutine. The collect pass registers every declared
/// subroutine into `Program.subroutines`; the resolve pass walks the
/// translated tree replacing matching `Unresolved` references with
/// `Resolved`. Names that don't match any declared subroutine remain
/// `Unresolved` - they are typically builtin functions (`exec`, `now`,
/// `zip`, ...) and are not translation errors.
#[derive(Debug, Eq, PartialEq)]
pub enum SubroutineRef<'i> {
    Unresolved(language::Identifier<'i>),
    Resolved(SubroutineId),
}

/// A fragment of a string literal: either inline text or an interpolated
/// expression. Defined here (rather than reusing `language::Piece`) because
/// interpolations are themselves `Operation`s and may carry resolved
/// subroutine references.
#[derive(Debug, Eq, PartialEq)]
pub enum Fragment<'i> {
    Text(&'i str),
    Interpolation(Operation<'i>),
}

/// An entry in a tablet: a label paired with a value-producing operation.
#[derive(Debug, Eq, PartialEq)]
pub struct Entry<'i> {
    pub label: &'i str,
    pub value: Operation<'i>,
}
