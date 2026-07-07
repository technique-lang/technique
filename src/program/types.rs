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
    /// The document's metadata header lines, shown as a preface before the
    /// steps of the program are run.
    pub prelude: Option<&'i language::Metadata<'i>>,
    /// All procedures declared in the input document, in source order. If an
    /// anonymous wrapper for a top-level `Technique::Steps`-only document was
    /// created it will be at index 0.
    pub subroutines: Vec<Subroutine<'i>>,
}

impl<'i> Program<'i> {
    pub fn new() -> Self {
        Program {
            prelude: None,
            subroutines: Vec::new(),
        }
    }
}

/// Index of a subroutine in `Program.subroutines`. Used as the resolved form
/// for the target of an invocation.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct SubroutineId(pub usize);

/// One link in a subroutine's lexical address: an enclosing procedure or section.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Locale<'i> {
    Procedure(&'i str),
    Section(&'i str),
}

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
    pub responses: Vec<&'i language::Response<'i>>,
    /// Where this procedure is declared, root outward.
    pub locale: Vec<Locale<'i>>,
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
            responses: Vec::new(),
            locale: Vec::new(),
        }
    }

    /// The number of arguments an invocation must supply. The signature's
    /// `requires` is authoritative when present; otherwise the count falls
    /// back to the named parameter list, or zero when neither is declared.
    /// Translation guarantees the two agree when both are present.
    pub fn arity(&self) -> usize {
        match (self.signature, self.parameters) {
            (Some(signature), _) => signature
                .requires
                .cardinality(),
            (None, Some(parameters)) => parameters.len(),
            (None, None) => 0,
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
            responses: Vec::new(),
            locale: Vec::new(),
        }
    }
}

/// Every node of the Intermediate Representation resulting from desugaring
/// the surface language is an `Operation`. Collectively they form a tree that
/// the runner can walk directly, being immutable means it can be revisited
/// when needed by loop nodes. Structural nodes (`Section`, `Step`, `Loop`,
/// ...) are traversed for their effects, and value-bearing nodes are reduced
/// to a `Value` against a separate, mutable `Environment` (see
/// `evaluator::evaluate`).
///
/// The Operation tree is not evaluated in place. All per-run state lives in
/// the `runner::Environment`.
#[derive(Debug, Eq, PartialEq)]
pub enum Operation<'i> {
    Variable(language::Identifier<'i>),
    Number(language::Numeric<'i>),
    String(Vec<Fragment<'i>>),
    Response(&'i str),
    Multiline(Option<&'i str>, Vec<&'i str>),
    Tablet(Vec<Entry<'i>>),
    List(Vec<Operation<'i>>),
    Tuple(Vec<Operation<'i>>),
    Invoke(Invocable<'i>),
    Execute(Executable<'i>),
    Hole,
    Unit,
    Prose(&'i str),
    Sequence(Vec<Operation<'i>>),
    /// The executable work hoisted from a procedure's description is an
    /// anonymous "step 0". Present only when the description carries
    /// instructions, not for prose alone. Its outcome folds into the
    /// procedure's own sign-off rather than prompting itself.
    Prologue(Vec<Operation<'i>>),
    Section {
        numeral: &'i str,
        title: Option<Box<Operation<'i>>>,
        body: Box<Operation<'i>>,
        responses: Vec<&'i language::Response<'i>>,
    },
    Step {
        ordinal: Ordinal<'i>,
        attributes: Vec<&'i [language::Attribute<'i>]>,
        source: &'i language::Scope<'i>,
        body: Box<Operation<'i>>,
        responses: Vec<&'i language::Response<'i>>,
    },
    Loop {
        names: &'i [language::Identifier<'i>],
        over: Option<Box<Operation<'i>>>,
        body: Box<Operation<'i>>,
        responses: Vec<&'i language::Response<'i>>,
    },
    Bind {
        names: &'i [language::Identifier<'i>],
        value: Box<Operation<'i>>,
        /// Inferred shape of the bound value, set in resolution: a wildcard
        /// list `[*]` when the name is iterated by a `foreach`, else `None`.
        inferred: Option<language::Genus<'i>>,
    },
}

/// A step's lexical kind. `Dependent` carries the verbatim ordinal string as
/// captured by the parser (`"1"`, `"a"`, `"iii"`, ...); `Parallel` has no
/// captured form, its position deriving from its index in the surrounding
/// `Sequence`.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Ordinal<'i> {
    Dependent(&'i str),
    Parallel,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Invocable<'i> {
    pub target: SubroutineRef<'i>,
    pub arguments: Vec<Operation<'i>>,
    /// Elided is `true` when the invocation is make with no argument list at
    /// all (the author wrote `<thing>` rather than `<thing>(x, y, z)`). A
    /// bare invocation is the equivalent to an all-`?` list of the
    /// target's arity — and so is passes the arity check. A parenthesised
    /// list, including an empty `()`, must match the arity of the target.
    pub elided: bool,
}

/// Reference to a subroutine. The collect pass registers every declared
/// subroutine into `Program.subroutines`; the resolve pass walks the
/// translated tree replacing matching `Unresolved` references with
/// `Resolved`. Names that don't match any declared subroutine become a
/// translation error. `Deferred` references (ie URLs) are left as-is; they
/// are resolved at a later phase or at runtime.
#[derive(Debug, Eq, PartialEq)]
pub enum SubroutineRef<'i> {
    Unresolved(language::Identifier<'i>),
    Resolved(SubroutineId),
    Deferred(language::External<'i>),
}

/// Lowered form of `language::Function`. Functions live in a separate
/// namespace from procedures: they are built-in or host-provided. The target
/// is an `ExecutableRef`, resolved against the available function table
/// during the linking phase.
#[derive(Debug, Eq, PartialEq)]
pub struct Executable<'i> {
    pub target: ExecutableRef<'i>,
    pub arguments: Vec<Operation<'i>>,
}

/// Index of a function in the linked `Library`. This is the resolved form of
/// the target of an `Executable`, analogous to `SubroutineId` for procedures.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct ExecutableId(pub usize);

/// Reference to a function. The translation phase emits these as
/// `Unresolved`; the linking phase replaces references where a function is in
/// the `Library` table with `Resolved`. Mirrors `SubroutineRef` for
/// procedures.
#[derive(Debug, Eq, PartialEq)]
pub enum ExecutableRef<'i> {
    Unresolved(language::Identifier<'i>),
    Resolved(ExecutableId),
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
