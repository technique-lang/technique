#![allow(unused_variables)]
#![allow(dead_code)]

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Scope {
    stack: Vec<Layer>,
}

impl Scope {
    pub(crate) fn new() -> Scope {
        Scope {
            stack: vec![Layer::Blank],
        }
    }

    pub(crate) fn current(&self) -> Layer {
        self.stack
            .last()
            .copied()
            .unwrap()
    }

    pub(crate) fn push(&mut self, layer: Layer) {
        self.stack
            .push(layer);
    }

    pub(crate) fn pop(&mut self) -> Layer {
        self.stack
            .pop()
            .unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_stack_operations() {
        let mut stack = Scope::new();

        assert_eq!(stack.current(), Layer::Blank);

        stack.push(Layer::Technique);

        assert_eq!(stack.current(), Layer::Technique);
    }
}
