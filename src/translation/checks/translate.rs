// Hand-written check suite for translation: surface AST -> IR.
//
// Modelled on `src/parsing/checks/parser.rs`. Tests grow alongside the
// translation passes; for now this file is a placeholder so the
// `mod check;` wiring in `mod.rs` resolves.

use super::*;

#[test]
fn empty_program_translates() {
    let program = Program::empty();
    assert!(program
        .procedures
        .is_empty());
}
