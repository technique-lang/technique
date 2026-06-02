//! Linking phase: resolve the function references against the function table
//! the program will run against.

mod linker;

pub use linker::{link, LinkingError};
