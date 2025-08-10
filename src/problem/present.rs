use technique::{formatting::Render, language::*};

// Helper function to convert numbers to superscript
fn to_superscript(num: i8) -> String {
    num.to_string()
        .chars()
        .map(|c| match c {
            '0' => '⁰',
            '1' => '¹',
            '2' => '²',
            '3' => '³',
            '4' => '⁴',
            '5' => '⁵',
            '6' => '⁶',
            '7' => '⁷',
            '8' => '⁸',
            '9' => '⁹',
            '-' => '⁻',
            _ => c,
        })
        .collect()
}

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
        let mut result = String::new();
        result.push_str(
            &self
                .domain
                .present(renderer),
        );
        result.push_str(&renderer.style(crate::formatting::Syntax::Structure, " -> "));
        result.push_str(
            &self
                .range
                .present(renderer),
        );
        result
    }
}

impl Present for Genus<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        match self {
            Genus::Unit => renderer.style(crate::formatting::Syntax::Forma, "()"),
            Genus::Single(forma) => forma.present(renderer),
            Genus::Tuple(formas) => {
                let mut result = String::new();
                result.push_str(&renderer.style(crate::formatting::Syntax::Structure, "("));
                for (i, forma) in formas
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        result
                            .push_str(&renderer.style(crate::formatting::Syntax::Structure, ", "));
                    }
                    result.push_str(&forma.present(renderer));
                }
                result.push_str(&renderer.style(crate::formatting::Syntax::Structure, ")"));
                result
            }
            Genus::Naked(formas) => {
                let mut result = String::new();
                for (i, forma) in formas
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        result
                            .push_str(&renderer.style(crate::formatting::Syntax::Structure, ", "));
                    }
                    result.push_str(&forma.present(renderer));
                }
                result
            }
            Genus::List(forma) => {
                let mut result = String::new();
                result.push_str(&renderer.style(crate::formatting::Syntax::Structure, "["));
                result.push_str(&forma.present(renderer));
                result.push_str(&renderer.style(crate::formatting::Syntax::Structure, "]"));
                result
            }
        }
    }
}

impl Present for Forma<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        renderer.style(crate::formatting::Syntax::Forma, self.0)
    }
}

impl Present for Identifier<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        renderer.style(crate::formatting::Syntax::Declaration, self.0)
    }
}

impl Present for Response<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        let mut result = String::new();
        result.push_str(&renderer.style(crate::formatting::Syntax::Quote, "'"));
        result.push_str(&renderer.style(crate::formatting::Syntax::Response, self.value));
        result.push_str(&renderer.style(crate::formatting::Syntax::Quote, "'"));

        if let Some(condition) = self.condition {
            result.push(' ');
            result.push_str(&renderer.style(crate::formatting::Syntax::Neutral, condition));
        }

        result
    }
}

impl Present for Numeric<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        match self {
            Numeric::Integral(value) => {
                renderer.style(crate::formatting::Syntax::Numeric, &value.to_string())
            }
            Numeric::Scientific(quantity) => quantity.present(renderer),
        }
    }
}

impl Present for Quantity<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        let mut result = String::new();

        // Format the mantissa
        result.push_str(&renderer.style(
            crate::formatting::Syntax::Numeric,
            &format!("{}", self.mantissa),
        ));

        // Add uncertainty if present
        if let Some(uncertainty) = &self.uncertainty {
            result.push_str(&renderer.style(crate::formatting::Syntax::Numeric, " ± "));
            result.push_str(&renderer.style(
                crate::formatting::Syntax::Numeric,
                &format!("{}", uncertainty),
            ));
        }

        // Add magnitude if present
        if let Some(magnitude) = &self.magnitude {
            result.push_str(&renderer.style(crate::formatting::Syntax::Numeric, " × "));
            result.push_str(&renderer.style(crate::formatting::Syntax::Numeric, "10"));
            result.push_str(&renderer.style(
                crate::formatting::Syntax::Numeric,
                &to_superscript(*magnitude),
            ));
        }

        // Add unit symbol
        result.push(' ');
        result.push_str(&renderer.style(crate::formatting::Syntax::Numeric, self.symbol));

        result
    }
}

impl Present for Invocation<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        let mut result = String::new();
        result.push_str(&renderer.style(crate::formatting::Syntax::Structure, "<"));

        match &self.target {
            Target::Local(identifier) => {
                result
                    .push_str(&renderer.style(crate::formatting::Syntax::Invocation, identifier.0));
            }
            Target::Remote(external) => {
                result.push_str(&renderer.style(crate::formatting::Syntax::Invocation, external.0));
            }
        }

        result.push_str(&renderer.style(crate::formatting::Syntax::Structure, ">"));

        if let Some(params) = &self.parameters {
            result.push_str(&renderer.style(crate::formatting::Syntax::Structure, "("));
            for (i, param) in params
                .iter()
                .enumerate()
            {
                if i > 0 {
                    result.push_str(&renderer.style(crate::formatting::Syntax::Structure, ", "));
                }
                result.push_str(&param.present(renderer));
            }
            result.push_str(&renderer.style(crate::formatting::Syntax::Structure, ")"));
        }

        result
    }
}

impl Present for Function<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        let mut result = String::new();
        result.push_str(
            &renderer.style(
                crate::formatting::Syntax::Function,
                self.target
                    .0,
            ),
        );
        result.push_str(&renderer.style(crate::formatting::Syntax::Structure, "("));

        for (i, param) in self
            .parameters
            .iter()
            .enumerate()
        {
            if i > 0 {
                result.push_str(&renderer.style(crate::formatting::Syntax::Structure, ", "));
            }
            result.push_str(&param.present(renderer));
        }

        result.push_str(&renderer.style(crate::formatting::Syntax::Structure, ")"));
        result
    }
}

impl Present for Expression<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        match self {
            Expression::Variable(identifier) => identifier.present(renderer),
            Expression::String(pieces) => {
                let mut result = String::new();
                result.push_str(&renderer.style(crate::formatting::Syntax::Quote, "\""));
                for piece in pieces {
                    match piece {
                        Piece::Text(text) => {
                            result
                                .push_str(&renderer.style(crate::formatting::Syntax::String, text));
                        }
                        Piece::Interpolation(expr) => {
                            result.push_str(
                                &renderer.style(crate::formatting::Syntax::Structure, "${"),
                            );
                            result.push_str(&expr.present(renderer));
                            result.push_str(
                                &renderer.style(crate::formatting::Syntax::Structure, "}"),
                            );
                        }
                    }
                }
                result.push_str(&renderer.style(crate::formatting::Syntax::Quote, "\""));
                result
            }
            Expression::Number(numeric) => numeric.present(renderer),
            Expression::Repeat(expr) => {
                let mut result = String::new();
                result.push_str(&renderer.style(crate::formatting::Syntax::Keyword, "repeat"));
                result.push(' ');
                result.push_str(&expr.present(renderer));
                result
            }
            Expression::Foreach(vars, expr) => {
                let mut result = String::new();
                result.push_str(&renderer.style(crate::formatting::Syntax::Keyword, "foreach"));
                result.push(' ');

                if vars.len() == 1 {
                    result.push_str(&vars[0].present(renderer));
                } else {
                    result.push_str(&renderer.style(crate::formatting::Syntax::Structure, "("));
                    for (i, var) in vars
                        .iter()
                        .enumerate()
                    {
                        if i > 0 {
                            result.push_str(
                                &renderer.style(crate::formatting::Syntax::Structure, ", "),
                            );
                        }
                        result.push_str(&var.present(renderer));
                    }
                    result.push_str(&renderer.style(crate::formatting::Syntax::Structure, ")"));
                }

                result.push_str(&renderer.style(crate::formatting::Syntax::Keyword, " in "));
                result.push_str(&expr.present(renderer));
                result
            }
            Expression::Application(invocation) => invocation.present(renderer),
            Expression::Execution(function) => function.present(renderer),
            Expression::Binding(expr, vars) => {
                let mut result = String::new();
                result.push_str(&expr.present(renderer));
                result.push_str(&renderer.style(crate::formatting::Syntax::Operator, " ~ "));
                for (i, var) in vars
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        result
                            .push_str(&renderer.style(crate::formatting::Syntax::Structure, ", "));
                    }
                    result.push_str(&var.present(renderer));
                }
                result
            }
            Expression::Tablet(pairs) => {
                let mut result = String::new();
                result.push_str(&renderer.style(crate::formatting::Syntax::Structure, "["));
                for (i, pair) in pairs
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        result.push_str(&renderer.style(crate::formatting::Syntax::Structure, ","));
                    }
                    result.push('\n');
                    result.push_str("    ");
                    result.push_str(&renderer.style(crate::formatting::Syntax::Quote, "\""));
                    result.push_str(&renderer.style(crate::formatting::Syntax::Label, pair.label));
                    result.push_str(&renderer.style(crate::formatting::Syntax::Quote, "\""));
                    result.push_str(&renderer.style(crate::formatting::Syntax::Operator, " = "));
                    result.push_str(
                        &pair
                            .value
                            .present(renderer),
                    );
                }
                result.push('\n');
                result.push_str(&renderer.style(crate::formatting::Syntax::Structure, "]"));
                result
            }
            Expression::Multiline(lang, lines) => {
                let mut result = String::new();
                result.push_str(&renderer.style(crate::formatting::Syntax::Structure, "```"));
                if let Some(language) = lang {
                    result.push_str(&renderer.style(crate::formatting::Syntax::Language, language));
                }
                result.push('\n');
                for line in lines {
                    result.push_str(&renderer.style(crate::formatting::Syntax::Multiline, line));
                    result.push('\n');
                }
                result.push_str(&renderer.style(crate::formatting::Syntax::Structure, "```"));
                result
            }
        }
    }
}

impl Present for Procedure<'_> {
    fn present(&self, renderer: &dyn Render) -> String {
        let mut result = String::new();

        // Add procedure name
        result.push_str(
            &self
                .name
                .present(renderer),
        );

        // Add parameters if present
        if let Some(params) = &self.parameters {
            result.push_str(&renderer.style(crate::formatting::Syntax::Structure, "("));
            for (i, param) in params
                .iter()
                .enumerate()
            {
                if i > 0 {
                    result.push_str(&renderer.style(crate::formatting::Syntax::Structure, ", "));
                }
                result.push_str(&param.present(renderer));
            }
            result.push_str(&renderer.style(crate::formatting::Syntax::Structure, ")"));
        }

        // Add colon
        result.push_str(&renderer.style(crate::formatting::Syntax::Declaration, " :"));

        // Add signature if present
        if let Some(signature) = &self.signature {
            result.push(' ');
            result.push_str(&signature.present(renderer));
        }

        result
    }
}
