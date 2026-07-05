//! Resolution phase: resolve a program's internal references and check its
//! intra-program semantics — procedure invocations bound to their declarations,
//! call arity, and variable scoping. The counterpart to `linking`, which
//! resolves external references (builtins) against a `Library`: resolution
//! binds internal names, linking binds external symbols.

mod resolver;

pub use resolver::{ResolutionError, resolve};
