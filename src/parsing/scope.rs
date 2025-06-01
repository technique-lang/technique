#![allow(unused_variables)]
#![allow(dead_code)]

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Layer {
    Technique,   // within a technique file, by definition the base state
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
        Scope { stack: vec![] }
    }

    pub(crate) fn current(&self) -> Layer {
        match self
            .stack
            .last()
        {
            Some(layer) => *layer,
            None => Layer::Technique,
        }
    }

    pub(crate) fn push(&mut self, layer: Layer) {
        self.stack
            .push(layer);
    }

    pub(crate) fn pop(&mut self) -> Layer {
        match self
            .stack
            .pop()
        {
            Some(layer) => layer,
            None => Layer::Technique,
        }
    }

    pub(crate) fn reset(&mut self) {
        self.stack
            .clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_stack_operations() {
        let mut stack = Scope::new();

        let current = stack.current();
        assert_eq!(current, Layer::Technique);

        stack.push(Layer::Metadata);

        let current = stack.current();
        assert_eq!(current, Layer::Metadata);

        let popped = stack.pop();
        assert_eq!(popped, Layer::Metadata);

        stack.push(Layer::Procedure);
        stack.push(Layer::Declaration);
        let current = stack.current();
        assert_eq!(current, Layer::Declaration);

        let popped = stack.pop();
        assert_eq!(popped, Layer::Declaration);

        let current = stack.current();
        assert_eq!(current, Layer::Procedure);

        let popped = stack.pop();
        assert_eq!(popped, Layer::Procedure);

        // and if we pop again, we're still in Technique
        let popped = stack.pop();
        assert_eq!(popped, Layer::Technique);

        stack.push(Layer::Description);
        stack.push(Layer::CodeBlock);

        // TODO get layers as tags in sorted order

        // now we try reset()

        stack.reset();
        let current = stack.current();
        assert_eq!(current, Layer::Technique);

        let popped = stack.pop();
        assert_eq!(popped, Layer::Technique);
    }
}
