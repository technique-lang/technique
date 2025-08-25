//! Internal abstract representation for the Technique interpreter
//!
//! Core principle: Everything is an asynchronous invocation that produces a value.
//! Dependencies between steps are expressed through named promises.

use std::collections::HashMap;
use std::fmt;

/// A resolved value from evaluation of expressions or procedure results
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Unit/empty value (like unit type () in Rust)
    Unit,

    /// Text literal value
    Text(String),

    /// Numeric value (could be extended with Quantity)
    Number(i64),

    /// Tablet - a collection of labeled values
    Tablet(Vec<(Label, Value)>),

    /// Tuple of values for compound results
    Tuple(Vec<Value>),
}

/// Label for tablet entries
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label(pub String);

/// Variable name bound to a promise
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(pub String);

/// Function identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionId(pub String);

/// A resolved function - either user-defined or builtin
#[derive(Clone)]
pub enum Function {
    /// Unresolved function (during first translation pass)
    Unresolved { id: FunctionId },

    /// User-defined procedure translated to executable steps
    Procedure {
        id: FunctionId,
        parameters: Vec<Name>,
        body: Step,
    },

    /// Builtin/primitive function with native implementation
    Builtin {
        id: FunctionId,
        action: fn(Value, &mut ExecutionContext) -> Result<Value, RuntimeError>,
    },
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::Unresolved { id } => write!(f, "Unresolved({:?})", id),
            Function::Procedure { id, .. } => write!(f, "Procedure({:?})", id),
            Function::Builtin { id, .. } => write!(f, "Builtin({:?})", id),
        }
    }
}

/// Attributes that can modify step execution (roles, places, etc)
#[derive(Debug, Clone, PartialEq)]
pub enum Attribute {
    Role(String),
    Place(String),
    Inherit, // Use parent's attribute
}

/// Executable steps - each produces a promise that resolves to a Value
#[derive(Debug, Clone)]
pub enum Step {
    /// A literal value - returns immediately
    Literal(Value),

    /// Wait for a named promise to resolve and return its value
    Await(Name),

    /// Function application - waits for argument, then applies function
    Apply {
        function: Function,
        argument: Box<Step>,
        attribute: Attribute,
    },

    /// Async invocation - evaluate step and bind result to name(s)
    /// This creates a promise that other steps can await
    Async { names: Vec<Name>, step: Box<Step> },

    /// Sequential execution - each step must complete before the next starts
    /// Returns the value of the last step
    Sequential(Vec<Step>),

    /// Parallel execution - all steps can run concurrently
    /// Returns tuple of all results in order
    Parallel(Vec<Step>),

    /// Create a tablet from labeled steps
    Record(Vec<(Label, Step)>),

    /// Create a tuple from steps
    Tuple(Vec<Step>),

    /// Conditional execution based on choice/response
    Choice {
        prompt: Box<Step>,
        branches: Vec<(String, Option<String>, Step)>, // (value, condition, step)
    },

    /// Iteration constructs
    Repeat(Box<Step>),
    Foreach {
        bindings: Vec<Name>,
        collection: Box<Step>,
        body: Box<Step>,
    },

    /// No operation - returns Unit immediately
    Noop,
}

/// Unique identifier for a step in execution
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StepId(pub usize);

/// A promise that will eventually resolve to a value
#[derive(Debug, Clone)]
pub struct Promise {
    pub id: StepId,
    pub state: PromiseState,
    /// Steps that are waiting for this promise
    pub waiters: Vec<StepId>,
}

/// State of a promise
#[derive(Debug, Clone)]
pub enum PromiseState {
    /// Not yet started
    Pending,
    /// Currently executing
    Running,
    /// Successfully completed with value
    Resolved(Value),
    /// Failed with error
    Failed(RuntimeError),
}

/// Runtime execution context/state
pub struct ExecutionContext {
    /// Named promises in current scope
    pub promises: HashMap<Name, Promise>,

    /// All steps by ID
    pub steps: HashMap<StepId, StepState>,

    /// Function definitions
    pub functions: HashMap<FunctionId, Function>,

    /// Current role/attribute context
    pub attribute: Attribute,

    /// Next step ID
    next_step_id: usize,
}

impl ExecutionContext {
    pub fn new() -> Self {
        ExecutionContext {
            promises: HashMap::new(),
            steps: HashMap::new(),
            functions: HashMap::new(),
            attribute: Attribute::Inherit,
            next_step_id: 0,
        }
    }

    pub fn create_step_id(&mut self) -> StepId {
        let id = StepId(self.next_step_id);
        self.next_step_id += 1;
        id
    }
}

/// Execution state for a single step
#[derive(Debug, Clone)]
pub struct StepState {
    pub step: Step,
    pub status: StepStatus,
    /// Dependencies this step is waiting on
    pub dependencies: Vec<Name>,
    /// Role/attribute for this step
    pub attribute: Attribute,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StepStatus {
    /// Waiting for dependencies
    Blocked,
    /// Ready to execute (all dependencies satisfied)
    Ready,
    /// Currently being executed
    Running,
    /// Completed successfully
    Completed(Value),
    /// Failed
    Failed(RuntimeError),
}

/// Runtime errors during execution
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
    pub kind: ErrorKind,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnboundVariable(Name),
    UnresolvedFunction(FunctionId),
    TypeError(String),
    DependencyFailed(Name),
    Other(String),
}

/// The complete executable program
pub struct Executable {
    /// All functions defined in the program
    pub functions: Vec<Function>,

    /// Entry point (first procedure)
    pub entry: Option<FunctionId>,
}

impl Executable {
    pub fn new(functions: Vec<Function>) -> Self {
        let entry = functions
            .first()
            .and_then(|f| match f {
                Function::Procedure { id, .. } => Some(id.clone()),
                Function::Builtin { id, .. } => Some(id.clone()),
                Function::Unresolved { .. } => None,
            });

        Executable { functions, entry }
    }
}
