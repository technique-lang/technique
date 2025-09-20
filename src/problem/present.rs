use technique::{
    formatting::{formatter, Render},
    language::*,
};

/// Trait for AST types that can present themselves via a renderer
pub trait Present {
    /// Present this AST node using the given renderer
    fn present(&self, renderer: &dyn Render) -> String;
}

// TODO: Implement Present for all AST types used in error examples:
// - Signature
// - Genus (Single, Tuple, Naked, List, Unit)
// - Forma
// - Identifier
// - Procedure
// - Response
// - Numeric (Integral, Scientific/Quantity)
// - Invocation
// - Function
// - Expression (for code blocks)
// - And others as needed

impl Present for Signature<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_signature(self, renderer)
    }
}

impl Present for Genus<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_genus(self, renderer)
    }
}

impl Present for Forma<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_forma(self, renderer)
    }
}

impl Present for Identifier<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_identifier(self, renderer)
    }
}

impl Present for Response<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_response(self, renderer)
    }
}

impl Present for Numeric<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_numeric(self, renderer)
    }
}

impl Present for Quantity<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_quantity(self, renderer)
    }
}

impl Present for Invocation<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_invocation(self, renderer)
    }
}

impl Present for Descriptive<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_descriptive(self, renderer)
    }
}

impl Present for Function<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_function(self, renderer)
    }
}

impl Present for Expression<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_expression(self, renderer)
    }
}

impl Present for Procedure<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_procedure_declaration(self, renderer)
    }
}

impl Present for Scope<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        formatter::render_scope(self, renderer)
    }
}
