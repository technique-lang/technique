//! Code formatter for the Technique language

use technique::language::*;

pub fn format<'i>(technique: &Technique) -> String {
    let mut buffer = String::new();

    if let Some(metadata) = &technique.header {
        buffer.push_str("% technique v1\n");
    }

    buffer
}

#[cfg(test)]
mod check {
    use super::*;
}
