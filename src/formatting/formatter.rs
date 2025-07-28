//! Code formatter for the Technique language

use crate::language::*;

pub fn format(technique: &Technique, width: u8) -> String {
    let mut output = Formatter::new(width);

    if let Some(metadata) = &technique.header {
        output.format_header(metadata);
    }

    if let Some(procedures) = &technique.body {
        for procedure in procedures {
            output.format_procedure(procedure);
        }
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

struct Formatter {
    buffer: String,
    nesting: u8,
    width: u8,
}

impl Formatter {
    fn new(width: u8) -> Formatter {
        Formatter {
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

    fn subformatter(&self) -> Formatter {
        Formatter {
            buffer: String::new(),
            nesting: self.nesting,
            width: self.width,
        }
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
        let mut current = String::new();
        let mut pos = self.nesting;

        for descriptive in descriptives {
            match descriptive {
                Descriptive::Text(text) => {
                    for word in text.split_ascii_whitespace() {
                        // Add space before word if we have content
                        if !current.is_empty() {
                            // Check if space + word would exceed width
                            if pos + 1 + word.len() as u8 > self.width {
                                // Wrap: output current line and start new one
                                self.append_str(&current);
                                self.append_char('\n');
                                for _ in 0..self.nesting {
                                    self.append_char(' ');
                                }
                                current.clear();
                                current.push_str(word);
                                pos = self.nesting + word.len() as u8;
                            } else {
                                // Fits: add space and word
                                current.push(' ');
                                current.push_str(word);
                                pos += 1 + word.len() as u8;
                            }
                        } else {
                            // First word on line
                            current.push_str(word);
                            pos += word.len() as u8;
                        }
                    }
                }
                Descriptive::CodeInline(expr) => {
                    match expr {
                        Expression::Tablet(_) => {
                            // Multi-line expressions break the line - output what we have first
                            if !current.is_empty() {
                                self.append_str(&current);
                                current.clear();
                                pos = self.nesting;
                            }
                            // Output tablet expression with newlines
                            self.append_char('{');
                            self.append_char('\n');
                            self.increase(4);
                            self.indent();
                            self.append_expression(expr);
                            self.append_char('\n');
                            self.decrease(4);
                            self.append_char('}');
                            continue;
                        }
                        Expression::Multiline(_, _) => {
                            // Multi-line expressions break the line - output what we have first
                            if !current.is_empty() {
                                self.append_str(&current);
                                current.clear();
                                pos = self.nesting;
                            }
                            // Output multiline expression preserving current indentation context
                            self.append_char('{');
                            self.increase(4);
                            self.append_expression(expr);
                            self.decrease(4);
                            self.append_char('\n');
                            self.indent();
                            self.append_char('}');
                            continue;
                        }
                        _ => {
                            // For expressions containing multilines, don't treat as atomic
                            match expr {
                                Expression::Execution(func)
                                    if func
                                        .parameters
                                        .iter()
                                        .any(|p| matches!(p, Expression::Multiline(_, _))) =>
                                {
                                    // Multi-line expressions break the line - output what we have first
                                    if !current.is_empty() {
                                        self.append_str(&current);
                                        self.append_char(' ');
                                        current.clear();
                                        pos = self.nesting;
                                    }
                                    // Output with proper line breaks
                                    self.append_char('{');
                                    self.append_char(' ');
                                    self.append_expression(expr);
                                    self.append_char(' ');
                                    self.append_char('}');
                                    continue;
                                }
                                _ => {
                                    // Inline code block - treat as atomic token
                                    let mut sub = self.subformatter();
                                    sub.append_char('{');
                                    sub.append_char(' ');
                                    sub.append_expression(expr);
                                    sub.append_char(' ');
                                    sub.append_char('}');
                                    let code_text = sub.buffer;

                                    if !current.is_empty() {
                                        if pos + 1 + code_text.len() as u8 > self.width {
                                            self.append_str(&current);
                                            self.append_char('\n');
                                            for _ in 0..self.nesting {
                                                self.append_char(' ');
                                            }
                                            current.clear();
                                            current.push_str(&code_text);
                                            pos = self.nesting + code_text.len() as u8;
                                        } else {
                                            current.push(' ');
                                            current.push_str(&code_text);
                                            pos += 1 + code_text.len() as u8;
                                        }
                                    } else {
                                        current.push_str(&code_text);
                                        pos += code_text.len() as u8;
                                    }
                                }
                            }
                        }
                    }
                }
                Descriptive::Application(invocation) => {
                    let mut sub = self.subformatter();
                    sub.nesting = self.nesting;
                    sub.append_application(invocation);
                    let app_text = sub.buffer;

                    // Treat as atomic token - either fits on line or wrap
                    if !current.is_empty() {
                        if pos + 1 + app_text.len() as u8 > self.width {
                            self.append_str(&current);
                            self.append_char('\n');
                            for _ in 0..self.nesting {
                                self.append_char(' ');
                            }
                            current.clear();
                            current.push_str(&app_text);
                            pos = self.nesting + app_text.len() as u8;
                        } else {
                            current.push(' ');
                            current.push_str(&app_text);
                            pos += 1 + app_text.len() as u8;
                        }
                    } else {
                        current.push_str(&app_text);
                        pos += app_text.len() as u8;
                    }
                }
                Descriptive::Binding(inner_descriptive, variables) => {
                    // Use subformatter to generate the complete binding text
                    let mut sub = self.subformatter();
                    sub.nesting = self.nesting;

                    // Render the inner descriptive
                    match inner_descriptive.as_ref() {
                        Descriptive::Text(text) => sub.append_str(text),
                        Descriptive::CodeInline(expr) => match expr {
                            Expression::Tablet(_) => {
                                sub.append_char('{');
                                sub.append_char('\n');
                                sub.increase(4);
                                sub.indent();
                                sub.append_expression(expr);
                                sub.append_char('\n');
                                sub.decrease(4);
                                sub.append_char('}');
                            }
                            _ => {
                                sub.append_char('{');
                                sub.append_char(' ');
                                sub.append_expression(expr);
                                sub.append_char(' ');
                                sub.append_char('}');
                            }
                        },
                        Descriptive::Application(invocation) => {
                            sub.append_application(invocation);
                        }
                        Descriptive::Binding(_, _) => {
                            // Nested bindings - for simplicity, just add placeholder
                            sub.append_str("<<nested binding>>");
                        }
                    }

                    // Add binding operator and variables
                    sub.append_str(" ~ ");
                    sub.append_variables(variables);

                    let binding_text = sub.buffer;

                    // Treat as atomic token
                    if !current.is_empty() {
                        if pos + 1 + binding_text.len() as u8 > self.width {
                            self.append_str(&current);
                            self.append_char('\n');
                            for _ in 0..self.nesting {
                                self.append_char(' ');
                            }
                            current.clear();
                            current.push_str(&binding_text);
                            pos = self.nesting + binding_text.len() as u8;
                        } else {
                            current.push(' ');
                            current.push_str(&binding_text);
                            pos += 1 + binding_text.len() as u8;
                        }
                    } else {
                        current.push_str(&binding_text);
                        pos += binding_text.len() as u8;
                    }
                }
            }
        }

        // Output any remaining text
        if !current.is_empty() {
            self.append_str(&current);
        }
    }

    fn append_steps(&mut self, steps: &Vec<Scope>) {
        self.increase(4);
        for step in steps {
            self.append_step(step);
        }
        self.decrease(4);
    }

    fn append_step(&mut self, step: &Scope) {
        match step {
            Scope::DependentBlock {
                ordinal,
                description: content,
                responses,
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
                if responses.len() > 0 {
                    self.indent();
                    self.append_responses(responses);
                }

                if scopes.len() > 0 {
                    self.append_scopes(scopes);
                }

                self.decrease(4);
            }
            Scope::ParallelBlock {
                bullet,
                description,
                responses,
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
                if responses.len() > 0 {
                    self.indent();
                    self.append_responses(responses);
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

    fn append_scopes(&mut self, scopes: &Vec<Scope>) {
        for scope in scopes {
            match scope {
                Scope::DependentBlock {
                    ordinal: _,
                    description: _,
                    responses: _,
                    subscopes: _,
                } => {
                    self.append_step(scope);
                }
                Scope::ParallelBlock {
                    bullet: _,
                    description: _,
                    responses: _,
                    subscopes: _,
                } => {
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

                    // Format subscopes within the code block scope
                    self.increase(4);
                    self.append_scopes(substeps);
                    self.decrease(4);
                }
            }
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

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn genus() {
        let mut output = Formatter::new(78);

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
        let mut output = Formatter::new(78);

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
        let mut output = Formatter::new(78);

        output.append_numeric(&Numeric::Integral(42));
        assert_eq!(output.buffer, "42");
    }
}
