//! Host capabilities available to native functions when they execute. For now
//! the only capability is passing output through to the operator: output goes
//! straight to standard output, or — for tests — into an in-memory buffer. A
//! future GUI or web frontend would hold its own sink here, with
//! `native()` staying the terminal default and a separate constructor carrying
//! the real one.

use std::cell::RefCell;
use std::io::{self, Write};

pub struct Context {
    sink: Sink,
}

enum Sink {
    // The terminal. `colour` records whether ANSI escapes are wanted, decided
    // once by the caller from the same `raw_output || is_terminal()` gate the
    // rest of the runner uses, so a redirected stream stays plain.
    Stdout { colour: bool },
    // An in-memory buffer, used by tests. Kept a distinct variant from
    // `Stdout` so the terminal path carries no interior-mutability wrapper.
    // The `RefCell` lets `write(&self)` append through the shared reference
    // the `Context` is threaded by.
    Capture(RefCell<Vec<u8>>),
}

impl Context {
    /// The default context of native host capabilities, writing through to
    /// standard output. `colour` says whether builtins that tee child output
    /// (e.g. `exec`) may tag it with ANSI escapes. Builtins that are pure
    /// functions to manipulate Values ignore the context entirely.
    pub fn native(colour: bool) -> Self {
        Context {
            sink: Sink::Stdout { colour },
        }
    }

    /// A context that captures everything written through it in memory, for
    /// use in tests; the bytes are read back with `captured()`. Touches no
    /// terminal.
    pub fn capture() -> Self {
        Context {
            sink: Sink::Capture(RefCell::new(Vec::new())),
        }
    }

    /// Pass a slice of bytes through to the user immediately. This is the
    /// streaming primitive: a function teeing a child process's stdout reads
    /// it in chunks and writes each chunk here (while separately accumulating
    /// those bytes for its return value). No intermediate `String` is
    /// allocated and a chunk split mid-UTF-8 is harmless. The terminal sink
    /// calls `flush()` so output appears to the user live.
    pub fn write(&self, bytes: &[u8]) -> io::Result<()> {
        match &self.sink {
            Sink::Stdout { .. } => {
                let mut out = io::stdout();
                out.write_all(bytes)?;
                out.flush()
            }
            Sink::Capture(buffer) => {
                buffer
                    .borrow_mut()
                    .extend_from_slice(bytes);
                Ok(())
            }
        }
    }

    /// Pass a run of a child process's output through to the user, wrapping it
    /// in red on a colour terminal when it came from `stderr` so the operator
    /// can tell the streams apart; anything else passes through unchanged. The
    /// colour decision lives here because the sink owns whether it is
    /// colour-capable. The reset closes the run immediately, so the terminal is
    /// never left reddened between writes — an interrupt finds it already clean.
    pub fn write_run(&self, run: &[u8], stderr: bool) -> io::Result<()> {
        if stderr && self.colour() {
            self.write(b"\x1b[31m")?;
            self.write(run)?;
            self.write(b"\x1b[0m")
        } else {
            self.write(run)
        }
    }

    /// Pass a complete, already known, text string through to the user. This
    /// is just a convenience over `write()` for whole messages such as status
    /// lines or announcements.
    #[allow(dead_code)]
    pub fn emit(&self, message: &str) -> io::Result<()> {
        self.write(message.as_bytes())
    }

    /// Whether output written through this context should carry ANSI colour.
    /// True only for a terminal sink the caller marked colour-capable; a capture
    /// buffer is always plain. Internal to `write_run`'s decision.
    fn colour(&self) -> bool {
        match &self.sink {
            Sink::Stdout { colour } => *colour,
            Sink::Capture(_) => false,
        }
    }

    /// The bytes captured by a `capture()` context; empty for a native one.
    pub fn captured(&self) -> Vec<u8> {
        match &self.sink {
            Sink::Capture(buffer) => buffer
                .borrow()
                .clone(),
            Sink::Stdout { .. } => Vec::new(),
        }
    }
}
