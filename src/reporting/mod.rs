//! Present the trail of a recorded run. The PFFTT records are read back from
//! the store then rendered to terminal.

mod column;

pub use column::{
    CONSOLE_COLUMNS, Column, JSON_COLUMNS, PFFTT_COLUMNS, render_console, render_json, render_pfftt,
};

#[cfg(test)]
#[path = "checks/column.rs"]
mod check;
