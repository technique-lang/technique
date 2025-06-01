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
        Scope { stack: vec![] }
    }

    pub(crate) fn current(&self) -> Layer {
        match self
            .stack
            .last()
        {
            Some(layer) => *layer,
            None => Layer::Blank,
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

        let current1 = stack.current();
        assert_eq!(current1, Layer::Blank);

        stack.push(Layer::Technique);

        let current2 = stack.current();
        assert_eq!(current2, Layer::Technique);

        stack.push(Layer::Procedure);

        let current3 = stack.current();
        assert_eq!(current3, Layer::Procedure);

        let popped1 = stack.pop();
        assert_eq!(popped1, Layer::Procedure);

        let current4 = stack.current();
        assert_eq!(current4, Layer::Technique);

        let popped2 = stack.pop();
        assert_eq!(popped2, Layer::Technique);

        // and if we pop again, we're still in Technique
        let popped3 = stack.pop();
        assert_eq!(popped3, Layer::Technique);

        // now we try reset()

        stack.reset();
        let current5 = stack.current();
        assert_eq!(current5, Layer::Blank);

        // weird corner case; if you pop on a re-initialized stack you get
        // Technique back. This shouldn't be problematic because you'd only be
        // popping out after having parsed something and so you're in a file
        // by definition.
        let popped4 = stack.pop();
        assert_eq!(popped4, Layer::Technique);
    }
}
