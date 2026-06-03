//! Host capabilities available to native functions when they execute. For now
//! the only capability is passing output through to the operator, and there is
//! no state to carry — output goes straight to standard output. A future GUI
//! or web frontend would hold a sink here and route through it, at which point
//! `native()` stays the empty/default context and a separate constructor
//! carries the real one.

use std::io::{self, Write};

pub struct Context;

impl Context {
    /// The default context of native host capabilities. Builtins that are
    /// pure functions to manipulate Values ignore it.
    pub fn native() -> Self {
        Context
    }

    /// Pass a slice of bytes through to the user immediately. This is the
    /// streaming primitive: a function teeing a child process's stdout reads
    /// it in chunks and writes each chunk here (while separately accumulating
    /// those bytes for its return value). No intermediate `String` is
    /// allocated and a chunk split mid-UTF-8 is harmless. This calls
    /// `flush()` so output appears to the user live.
    #[allow(dead_code)]
    pub fn write(&self, bytes: &[u8]) -> io::Result<()> {
        let mut out = io::stdout();
        out.write_all(bytes)?;
        out.flush()
    }

    /// Pass a complete, already known, text string through to the user. This
    /// is just a convenience over `write()` for whole messages such as status
    /// lines or announcements.
    #[allow(dead_code)]
    pub fn emit(&self, message: &str) -> io::Result<()> {
        self.write(message.as_bytes())
    }
}
