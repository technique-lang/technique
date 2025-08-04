//! Code formatter for the Technique language

use crate::formatting::*;
use crate::language::*;

pub fn format(renderer: &impl Render, technique: &Document, width: u8) -> String {
    let mut output = Formatter::new(renderer, width);

    if let Some(metadata) = &technique.header {
        output.format_header(metadata);
    }

    if let Some(body) = &technique.body {
        output.format_technique(body);
    }

    if !output
        .buffer
        .is_empty()
        && !output
            .buffer
            .ends_with('\n')
    {
        output.append_char('\n');
    }
    output.buffer
}

struct Formatter<'a, R> {
    renderer: &'a R,
    buffer: String,
    nesting: u8,
    width: u8,
}

impl<'a, R> Formatter<'a, R>
where
    R: Render,
{
    fn new(renderer: &'a R, width: u8) -> Formatter<'a, R> {
        Formatter {
            renderer,
            buffer: String::new(),
            nesting: 0,
            width,
        }
    }

    #[cfg(test)]
    fn reset(&mut self) {
        self.buffer
            .clear();
    }

    fn increase(&mut self, depth: u8) {
        self.nesting += depth;
    }

    fn decrease(&mut self, depth: u8) {
        self.nesting -= depth;
    }

    fn indent(&mut self) {
        for _ in 0..self.nesting {
            self.buffer
                .push(' ');
        }
    }

    fn append_str(&mut self, text: &str) {
        for c in text.chars() {
            self.append_char(c);
        }
    }

    fn subformatter(&self) -> Formatter<'a, R> {
        Formatter {
            buffer: String::new(),
            nesting: self.nesting,
            width: self.width,
            renderer: &self.renderer,
        }
    }

    fn builder(&mut self) -> Line<'_, 'a, R> {
        Line::new(self)
    }

    fn render_inline_code(&self, expr: &Expression) -> String {
        match expr {
            Expression::Tablet(_) | Expression::Multiline(_, _) => {
                // These are not inline, caller should handle specially
                String::new()
            }
            _ => {
                let mut sub = self.subformatter();
                sub.append_char('{');
                sub.append_char(' ');
                sub.append_expression(expr);
                sub.append_char(' ');
                sub.append_char('}');
                sub.buffer
            }
        }
    }

    fn render_application(&self, invocation: &Invocation) -> String {
        let mut sub = self.subformatter();
        sub.append_application(invocation);
        sub.buffer
    }

    fn render_binding(
        &self,
        inner_descriptive: &Descriptive,
        variables: &Vec<Identifier>,
    ) -> String {
        let mut sub = self.subformatter();

        match inner_descriptive {
            Descriptive::Text(text) => sub.append_str(text),
            Descriptive::CodeInline(expr) => {
                sub.append_char('{');
                sub.append_char(' ');
                sub.append_expression(expr);
                sub.append_char(' ');
                sub.append_char('}');
            }
            Descriptive::Application(invocation) => {
                sub.append_application(invocation);
            }
            Descriptive::Binding(_, _) => {
                sub.append_str("<<nested binding>>");
            }
        }

        sub.append_str(" ~ ");
        sub.append_variables(variables);
        sub.buffer
    }

    fn append_char(&mut self, c: char) {
        self.buffer
            .push(c);
    }

    fn is_empty(&self) -> bool {
        self.buffer
            .len()
            == 0
    }

    fn format_header(&mut self, metadata: &Metadata) {
        self.append_str("% technique v1\n");

        if let Some(license) = metadata.license {
            self.append_str("! ");
            self.append_str(license);

            if let Some(copyright) = metadata.copyright {
                self.append_str("; © ");
                self.append_str(copyright);
            }

            self.append_char('\n');
        }

        if let Some(template) = metadata.template {
            self.append_str("& ");
            self.append_str(template);
            self.append_char('\n');
        }
    }

    fn format_technique(&mut self, technique: &Technique) {
        match technique {
            Technique::Steps(steps) => {
                self.append_steps(steps);
            }
            Technique::Procedures(procedures) => {
                // Procedures always format at left margin
                let saved = self.nesting;
                self.nesting = 0;
                for procedure in procedures {
                    self.format_procedure(procedure);
                }
                // and restore
                self.nesting = saved;
            }
            Technique::Empty => {}
        }
    }

    fn format_procedure(&mut self, procedure: &Procedure) {
        // if a header or another procedure has already been added,
        // separate the upcoming one with a blank line.
        if !self.is_empty() {
            self.append_char('\n');
        }

        // declaration

        let name = &procedure.name;
        self.append_str(name.0);

        if let Some(parameters) = &procedure.parameters {
            // note that append_arguments() is for general expression
            // arguments and append_variables() is for the special case where
            // tuples of names have parenthesis but single identifiers are
            // naked. We use append_parameters() here which always encloses
            // with parenthesis.
            self.append_parameters(parameters);
        }

        self.append_char(' ');
        self.append_char(':');

        if let Some(signature) = &procedure.signature {
            self.append_char(' ');
            self.append_signature(signature);
        }

        self.append_char('\n');

        // elements

        for element in &procedure.elements {
            self.append_element(element);
        }
    }

    fn append_element(&mut self, element: &Element) {
        match element {
            Element::Title(title) => {
                self.append_char('\n');
                self.append_char('#');
                self.append_char(' ');
                self.append_str(title);
                self.append_char('\n');
            }
            Element::Description(paragraphs) => {
                self.append_char('\n');
                self.append_paragraphs(paragraphs);
            }
            Element::Steps(steps) => {
                self.append_char('\n');
                self.append_steps(steps);
            }
            Element::CodeBlock(expression) => {
                self.append_char('{');
                self.append_char('\n');

                self.increase(4);
                self.indent();
                self.append_expression(expression);
                self.append_char('\n');
                self.decrease(4);

                self.append_char('}');
            }
        }
    }

    fn append_signature(&mut self, signature: &Signature) {
        self.append_genus(&signature.domain);
        self.append_str(" -> ");
        self.append_genus(&signature.range);
    }

    fn append_genus(&mut self, genus: &Genus) {
        match genus {
            Genus::Unit => {
                self.append_char('(');
                self.append_char(')');
            }
            Genus::Single(forma) => self.append_forma(forma),
            Genus::Tuple(formas) => {
                self.append_char('(');
                for (i, forma) in formas
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        self.append_char(',');
                        self.append_char(' ');
                    }
                    self.append_forma(forma);
                }
                self.append_char(')');
            }
            Genus::Naked(formas) => {
                for (i, forma) in formas
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        self.append_char(',');
                        self.append_char(' ');
                    }
                    self.append_forma(forma);
                }
            }
            Genus::List(forma) => {
                self.append_char('[');
                self.append_forma(forma);
                self.append_char(']');
            }
        }
    }

    // Output names surrounded by parenthesis
    fn append_parameters(&mut self, variables: &Vec<Identifier>) {
        self.append_char('(');
        for (i, variable) in variables
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.append_char(',');
                self.append_char(' ');
            }
            self.append_identifier(variable);
        }
        self.append_char(')');
    }

    fn append_forma(&mut self, forma: &Forma) {
        self.append_str(forma.0)
    }

    fn append_paragraphs(&mut self, paragraphs: &Vec<Paragraph>) {
        for (i, paragraph) in paragraphs
            .iter()
            .enumerate()
        {
            if i > 0 {
                // Add blank line between paragraphs
                self.append_char('\n');
            }
            self.append_descriptives(&paragraph.0);
            self.append_char('\n');
        }
    }

    fn append_descriptives(&mut self, descriptives: &Vec<Descriptive>) {
        let mut line = self.builder();

        for descriptive in descriptives {
            match descriptive {
                Descriptive::Text(text) => {
                    line.add_text(text);
                }
                Descriptive::CodeInline(expr) => match expr {
                    Expression::Tablet(_) => {
                        line.flush();
                        self.append_char('{');
                        self.append_char('\n');
                        self.increase(4);
                        self.indent();
                        self.append_expression(expr);
                        self.append_char('\n');
                        self.decrease(4);
                        line = self.builder();
                        line.add_text("}");
                    }
                    Expression::Multiline(_, _) => {
                        line.flush();
                        self.append_char('{');
                        self.increase(4);
                        self.append_expression(expr);
                        self.decrease(4);
                        self.append_char('\n');
                        self.indent();
                        line = self.builder();
                        line.add_text("}");
                    }
                    _ => match expr {
                        Expression::Execution(func)
                            if func
                                .parameters
                                .iter()
                                .any(|p| matches!(p, Expression::Multiline(_, _))) =>
                        {
                            line.flush();
                            self.append_char(' ');
                            self.append_char('{');
                            self.append_char(' ');
                            self.append_expression(expr);
                            self.append_char(' ');
                            line = self.builder();
                            line.add_text("}");
                        }
                        _ => {
                            line.add_inline_code(expr);
                        }
                    },
                },
                Descriptive::Application(invocation) => {
                    line.add_application(invocation);
                }
                Descriptive::Binding(inner_descriptive, variables) => {
                    line.add_binding(inner_descriptive, variables);
                }
            }
        }

        line.flush();
    }

    fn append_steps(&mut self, steps: &Vec<Scope>) {
        self.increase(4);
        self.append_scopes(steps);
        self.decrease(4);
    }

    fn append_step(&mut self, step: &Scope) {
        match step {
            Scope::DependentBlock {
                ordinal,
                description: content,
                subscopes: scopes,
            } => {
                self.indent();
                self.append_str(ordinal);
                self.append_char('.');
                self.append_char(' ');
                if ordinal.len() == 1 {
                    self.append_char(' ');
                }

                self.increase(4);

                if content.len() > 0 {
                    self.append_paragraphs(content);
                }

                if scopes.len() > 0 {
                    self.append_scopes(scopes);
                }

                self.decrease(4);
            }
            Scope::ParallelBlock {
                bullet,
                description,
                subscopes,
            } => {
                self.indent();
                self.append_char(*bullet);
                self.append_char(' ');
                self.append_char(' ');
                self.append_char(' ');

                self.increase(4);

                if description.len() > 0 {
                    self.append_paragraphs(description);
                }

                if subscopes.len() > 0 {
                    self.append_scopes(subscopes);
                }

                self.decrease(4);
            }
            _ => panic!("Shouldn't be calling append_step() with a non-step Scope"),
        }
    }

    fn append_responses(&mut self, responses: &Vec<Response>) {
        for (i, response) in responses
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.append_str(" | ");
            }
            self.append_char('\'');
            self.append_str(response.value);
            self.append_char('\'');

            if let Some(text) = response.condition {
                self.append_char(' ');
                self.append_str(text);
            }
        }
        self.append_char('\n');
    }

    fn append_scope(&mut self, scope: &Scope) {
        match scope {
            Scope::DependentBlock { .. } | Scope::ParallelBlock { .. } => {
                self.append_step(scope);
            }
            Scope::AttributeBlock {
                attributes,
                subscopes,
            } => {
                self.append_attributes(attributes);
                self.append_char('\n');

                if subscopes.len() == 0 {
                    return;
                }

                let first = subscopes
                    .iter()
                    .next()
                    .unwrap();

                if let Scope::CodeBlock { .. } = first {
                    // do NOT increase indent
                    self.append_scopes(subscopes);
                } else {
                    self.increase(4);
                    self.append_scopes(subscopes);
                    self.decrease(4);
                }
            }
            Scope::CodeBlock {
                expression,
                subscopes: substeps,
            } => {
                match expression {
                    Expression::Tablet(_) => {
                        self.indent();
                        self.append_char('{');
                        self.append_char('\n');

                        self.increase(4);
                        self.indent();
                        self.append_expression(expression);
                        self.append_char('\n');
                        self.decrease(4);
                        self.indent();
                        self.append_char('}');
                    }
                    _ => {
                        self.indent();
                        self.append_char('{');
                        self.append_char(' ');
                        self.append_expression(expression);
                        self.append_char(' ');
                        self.append_char('}');
                    }
                }
                self.append_char('\n');

                // Format subscopes below this code block, if there are any.
                self.increase(4);
                self.append_scopes(substeps);
                self.decrease(4);
            }
            Scope::ResponseBlock { responses } => {
                self.increase(4);
                self.indent();
                self.append_responses(responses);
                self.decrease(4);
            }
            Scope::SectionChunk {
                numeral,
                title,
                body,
            } => {
                self.append_str(numeral);
                self.append_char('.');
                if let Some(paragraph) = title {
                    self.append_char(' ');
                    self.append_descriptives(&paragraph.0);
                }
                self.append_char('\n');

                // Sections headings always reset back to left margin
                let saved = self.nesting;
                self.nesting = 0;
                self.format_technique(body);
                self.nesting = saved;
            }
        }
    }

    fn append_scopes(&mut self, scopes: &Vec<Scope>) {
        for (i, scope) in scopes
            .iter()
            .enumerate()
        {
            if i > 0 {
                if let Scope::SectionChunk { .. } = scope {
                    self.append_char('\n');
                }
            }
            self.append_scope(scope);
        }
    }

    fn append_attributes(&mut self, attributes: &Vec<Attribute>) {
        self.indent();
        for (i, attribute) in attributes
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.append_str(" + ");
            }
            match attribute {
                Attribute::Role(name) => {
                    self.append_char('@');
                    self.append_identifier(name);
                }
                Attribute::Place(name) => {
                    self.append_char('#');
                    self.append_identifier(name);
                }
            }
        }
    }

    fn append_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Variable(identifier) => self.append_str(identifier.0),
            Expression::String(text) => {
                self.append_char('"');
                self.append_str(text);
                self.append_char('"');
            }
            Expression::Number(numeric) => self.append_numeric(numeric),
            Expression::Multiline(lang, lines) => {
                self.append_char('\n');

                self.indent();
                self.append_str("```");
                if let Some(which) = lang {
                    self.append_str(which);
                }
                self.append_char('\n');

                self.increase(4);
                for line in lines {
                    self.indent();
                    self.append_str(line);
                    self.append_char('\n');
                }
                self.decrease(4);

                self.indent();
                self.append_str("```");
                self.append_char('\n');
            }
            Expression::Repeat(expression) => {
                self.append_str("repeat ");
                self.append_expression(expression);
            }
            Expression::Foreach(variables, expression) => {
                self.append_str("foreach ");
                self.append_variables(variables);
                self.append_str(" in ");
                self.append_expression(expression);
            }
            Expression::Application(invocation) => self.append_application(invocation),
            Expression::Execution(function) => self.append_function(function),
            Expression::Binding(expression, variables) => {
                self.append_expression(expression);
                self.append_str(" ~ ");
                self.append_variables(variables);
            }
            Expression::Tablet(pairs) => self.append_tablet(pairs),
        }
    }

    // When doing binding we omit the parenthesis in the most common case of
    // there only being one name being bound to.
    fn append_variables(&mut self, variables: &Vec<Identifier>) {
        if variables.len() > 1 {
            self.append_char('(');
        }
        for (i, variable) in variables
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.append_char(',');
                self.append_char(' ');
            }
            self.append_identifier(variable);
        }
        if variables.len() > 1 {
            self.append_char(')');
        }
    }

    fn append_numeric(&mut self, numeric: &Numeric) {
        match numeric {
            Numeric::Integral(num) => self.append_str(&num.to_string()),
            Numeric::Scientific(_) => todo!(),
        }
    }

    fn append_application(&mut self, invocation: &Invocation) {
        self.append_char('<');
        match &invocation.target {
            Target::Local(identifier) => self.append_str(identifier.0),
            Target::Remote(external) => self.append_str(external.0),
        }
        self.append_char('>');
        if let Some(parameters) = &invocation.parameters {
            self.append_arguments(parameters);
        }
    }

    fn append_identifier(&mut self, identifier: &Identifier) {
        self.append_str(identifier.0);
    }

    // This is the one that is for the generalized case where the arguments to
    // a function can be Expressions themselves (though usually are just
    // variable names)
    fn append_arguments(&mut self, parameters: &Vec<Expression>) {
        self.append_char('(');

        for (i, parameter) in parameters
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.append_char(',');
                self.append_char(' ');
            }
            self.append_expression(parameter);
        }

        self.append_char(')');
    }

    fn append_function(&mut self, function: &Function) {
        self.append_identifier(&function.target);
        self.append_char('(');

        let mut has_multiline = false;
        for parameter in &function.parameters {
            if let Expression::Multiline(_, _) = parameter {
                has_multiline = true;
                break;
            }
        }

        for (i, parameter) in function
            .parameters
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.append_char(',');
                self.append_char(' ');
            }
            self.append_expression(parameter);
        }

        if has_multiline {
            self.indent();
        }
        self.append_char(')');
    }

    fn append_tablet(&mut self, pairs: &Vec<Pair>) {
        self.append_char('[');
        self.append_char('\n');

        self.increase(4);
        for pair in pairs {
            self.indent();
            self.append_char('"');
            self.append_str(pair.label);
            self.append_char('"');
            self.append_str(" = ");
            self.append_expression(&pair.value);
            self.append_char('\n');
        }
        self.decrease(4);

        self.indent();
        self.append_char(']');
    }
}

struct Line<'a, 'b, R> {
    output: &'a mut Formatter<'b, R>, // reference to parent
    current: String,
    position: u8,
}

impl<'a, 'b, R> Line<'a, 'b, R>
where
    R: Render,
{
    fn new(output: &'a mut Formatter<'b, R>) -> Self {
        Line {
            current: String::new(),
            position: output.nesting,
            output,
        }
    }

    fn add_text(&mut self, text: &str) {
        for word in text.split_ascii_whitespace() {
            self.add_word(word);
        }
    }

    fn add_word(&mut self, word: &str) {
        if !self
            .current
            .is_empty()
        {
            if self.position + 1 + word.len() as u8
                > self
                    .output
                    .width
            {
                self.wrap_line();
                self.current
                    .push_str(word);
                self.position = self
                    .output
                    .nesting
                    + word.len() as u8;
            } else {
                self.current
                    .push(' ');
                self.current
                    .push_str(word);
                self.position += 1 + word.len() as u8;
            }
        } else {
            self.current
                .push_str(word);
            self.position += word.len() as u8;
        }
    }

    fn add_inline_code(&mut self, expr: &Expression) {
        let code_text = self
            .output
            .render_inline_code(expr);
        self.add_token(&code_text);
    }

    fn add_application(&mut self, invocation: &Invocation) {
        let app_text = self
            .output
            .render_application(invocation);
        self.add_token(&app_text);
    }

    fn add_binding(&mut self, inner_descriptive: &Descriptive, variables: &Vec<Identifier>) {
        let binding_text = self
            .output
            .render_binding(inner_descriptive, variables);
        self.add_token(&binding_text);
    }

    fn add_token(&mut self, token: &str) {
        if self
            .current
            .is_empty()
        {
            self.current
                .push_str(token);
            self.position += token.len() as u8;
        } else {
            if self.position + 1 + token.len() as u8
                > self
                    .output
                    .width
            {
                self.wrap_line();
                self.current
                    .push_str(token);
                self.position = self
                    .output
                    .nesting
                    + token.len() as u8;
            } else {
                self.current
                    .push(' ');
                self.current
                    .push_str(token);
                self.position += 1 + token.len() as u8;
            }
        }
    }

    fn wrap_line(&mut self) {
        self.output
            .append_str(&self.current);
        self.output
            .append_char('\n');
        self.output
            .indent();
        self.current
            .clear();
        self.position = self
            .output
            .nesting;
    }

    fn flush(self) {
        if !self
            .current
            .is_empty()
        {
            self.output
                .append_str(&self.current);
        }
    }
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn genus() {
        let mut output = Formatter::new(&Identity, 78);

        output.append_forma(&Forma("Jedi"));
        assert_eq!(output.buffer, "Jedi");

        output.reset();
        output.append_genus(&Genus::Unit);
        assert_eq!(output.buffer, "()");

        output.reset();
        output.append_genus(&Genus::Single(Forma("Stormtrooper")));
        assert_eq!(output.buffer, "Stormtrooper");

        output.reset();
        output.append_genus(&Genus::List(Forma("Pilot")));
        assert_eq!(output.buffer, "[Pilot]");

        output.reset();
        output.append_genus(&Genus::Tuple(vec![
            Forma("Kid"),
            Forma("Pilot"),
            Forma("Scoundrel"),
            Forma("Princess"),
        ]));
        assert_eq!(output.buffer, "(Kid, Pilot, Scoundrel, Princess)");

        output.reset();
    }

    #[test]
    fn signatures() {
        let mut output = Formatter::new(&Identity, 78);

        output.append_signature(&Signature {
            domain: Genus::Single(Forma("Alderaan")),
            range: Genus::Single(Forma("AsteroidField")),
        });
        assert_eq!(output.buffer, "Alderaan -> AsteroidField");

        output.reset();
        output.append_signature(&Signature {
            domain: Genus::List(Forma("Clone")),
            range: Genus::Single(Forma("Army")),
        });
        assert_eq!(output.buffer, "[Clone] -> Army");

        output.reset();
        output.append_signature(&Signature {
            domain: Genus::Single(Forma("TaxationOfTradeRoutes")),
            range: Genus::Tuple(vec![Forma("Rebels"), Forma("Empire")]),
        });
        assert_eq!(output.buffer, "TaxationOfTradeRoutes -> (Rebels, Empire)");
    }

    #[test]
    fn numbers() {
        let mut output = Formatter::new(&Identity, 78);

        output.append_numeric(&Numeric::Integral(42));
        assert_eq!(output.buffer, "42");
    }
}
