//! Host capabilities available to native functions when they execute. For now
//! the only capability is passing output through to the user: output goes
//! straight to standard output, or — for tests — into an in-memory buffer. A
//! future GUI or web frontend would hold its own sink here, with
//! `native()` staying the terminal default and a separate constructor carrying
//! the real one.

use std::cell::RefCell;
use std::io::{self, Write};

pub struct Context {
    sink: Sink,
}

/// Which of a child's two output streams a run of bytes came from; `write_run`
/// reddens `Stderr` on a colour terminal so the user can tell them apart.
#[derive(Clone, Copy)]
pub enum Stream {
    Stdout,
    Stderr,
}

enum Sink {
    // The terminal. `colour` records whether ANSI escapes are wanted, decided
    // by the caller from the `raw_output || is_terminal()` gate.
    Stdout { colour: bool },
    // An in-memory buffer, used by tests. Kept a distinct variant from
    // `Stdout` so the terminal path carries no interior-mutability wrapper.
    // The `RefCell` lets `write(&self)` append through the shared reference
    // the `Context` is threaded by.
    Capture(RefCell<Vec<u8>>),
}

impl Context {
    /// The default context, writing through to standard output. `colour` says
    /// whether builtins that tee child output (e.g. `exec`) may tag it with
    /// ANSI escapes; pure builtins ignore the context entirely.
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

    /// Tee a run of child output to the user, making the foreground colour
    /// red for any `Stream::Stderr` output when run in a terminal. The reset
    /// every time is necessary in case the user interrupts with Ctrl+C,
    /// preventing the subsequent output from incorrectly being in red.
    pub fn write_run(&self, run: &[u8], stream: Stream) -> io::Result<()> {
        let red = match stream {
            Stream::Stderr => self.colour(),
            Stream::Stdout => false,
        };
        if red {
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
    pub fn emit(&self, message: &str) -> io::Result<()> {
        self.write(message.as_bytes())
    }

    /// Whether this sink is a colour-capable terminal; a capture buffer is not.
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
