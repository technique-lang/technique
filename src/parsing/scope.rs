#![allow(unused_variables)]
#![allow(dead_code)]

use regex::Regex;
use technique::language::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Layer {
    Blank,       // beginning of input, before any state is encountered
    Technique,   // within a technique file
    Metadata,    // header lines
    Procedure,   // procedure function block
    Declaration, // procedure function signature
    Description, // procedure description, as free form text
    StepItem,    // (sub)step within a procedure body
    CodeBlock,   // escape to a code mode
    Embedded,    // multi-line string of another language.
}
