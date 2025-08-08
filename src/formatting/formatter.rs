//! Code formatter for the Technique language

use crate::formatting::*;
use crate::language::*;

pub fn format_with_renderer(technique: &Document, width: u8) -> Vec<(Syntax, String)> {
    let mut output = Formatter::new(width);

    if let Some(metadata) = &technique.header {
        output.format_header(metadata);
    }

    if let Some(body) = &technique.body {
        output.format_technique(body);
    }

    // Flush any remaining content
    output.flush_current();

    // Add final newline if needed
    if !output
        .fragments
        .is_empty()
    {
        if let Some((_, last_content)) = output
            .fragments
            .last()
        {
            if !last_content.ends_with('\n') {
                output.append_fragment(Syntax::Description, "\n");
            }
        }
    }

    output.fragments
}

struct Formatter {
    fragments: Vec<(Syntax, String)>,
    nesting: u8,
    width: u8,
    current: Syntax,
    buffer: String,
}

impl Formatter {
    fn new(width: u8) -> Formatter {
        Formatter {
            fragments: Vec::new(),
            nesting: 0,
            width,
            current: Syntax::Neutral,
            buffer: String::new(),
        }
    }

    fn append_fragment(&mut self, syntax: Syntax, content: &str) {
        self.fragments
            .push((syntax, content.to_string()));
    }

    /// Append content with specific syntax tagging, maintaining order
    fn append(&mut self, syntax: Syntax, content: &str) {
        // Flush any pending buffer content first to maintain order
        self.flush_current();
        self.fragments
            .push((syntax, content.to_string()));
    }

    fn switch_syntax(&mut self, new_syntax: Syntax) {
        if !self
            .buffer
            .is_empty()
        {
            self.fragments
                .push((
                    self.current,
                    self.buffer
                        .clone(),
                ));
            self.buffer
                .clear();
        }
        self.current = new_syntax;
    }

    fn reset_syntax(&mut self) {
        self.switch_syntax(Syntax::Neutral);
    }

    fn flush_current(&mut self) {
        if !self
            .buffer
            .is_empty()
        {
            self.fragments
                .push((
                    self.current,
                    self.buffer
                        .clone(),
                ));
            self.buffer
                .clear();
        }
    }

    #[cfg(test)]
    fn reset(&mut self) {
        self.fragments
            .clear();
        self.buffer
            .clear();
        self.current = Syntax::Neutral;
    }

    fn increase(&mut self, depth: u8) {
        self.nesting += depth;
    }

    fn decrease(&mut self, depth: u8) {
        self.nesting -= depth;
    }

    fn indent(&mut self) {
        if self.nesting > 0 {
            // Flush any existing buffer before adding indentation
            self.flush_current();
            let spaces = " ".repeat(self.nesting as usize);
            self.append_fragment(Syntax::Indent, &spaces);
        }
    }

    fn append_str(&mut self, text: &str) {
        for c in text.chars() {
            self.append_char(c);
        }
    }

    fn append_breakable(&mut self, syntax: Syntax, text: &str) {
        for (i, word) in text
            .split_ascii_whitespace()
            .enumerate()
        {
            if i > 0 {
                self.append_char(' ');
            }
            self.append(syntax, word);
        }
    }

    fn subformatter(&self) -> Formatter {
        Formatter {
            fragments: Vec::new(),
            nesting: self.nesting,
            width: self.width,
            current: Syntax::Neutral,
            buffer: String::new(),
        }
    }

    fn builder(&mut self) -> Line {
        Line::new(self)
    }

    fn render_inline_code(&self, expr: &Expression) -> Vec<(Syntax, String)> {
        match expr {
            Expression::Tablet(_) | Expression::Multiline(_, _) => {
                // These are not inline, caller should handle specially
                Vec::new()
            }
            _ => {
                let mut sub = self.subformatter();
                sub.append(Syntax::Structure, "{");
                sub.append_char(' ');
                sub.append_expression(expr);
                sub.append_char(' ');
                sub.append(Syntax::Structure, "}");
                sub.flush_current();
                sub.fragments
            }
        }
    }

    fn render_application(&self, invocation: &Invocation) -> Vec<(Syntax, String)> {
        let mut sub = self.subformatter();
        sub.append_application(invocation);
        sub.flush_current();
        sub.fragments
    }

    fn render_binding(
        &self,
        inner_descriptive: &Descriptive,
        variables: &Vec<Identifier>,
    ) -> Vec<(Syntax, String)> {
        let mut sub = self.subformatter();

        match inner_descriptive {
            Descriptive::Text(text) => sub.append_breakable(Syntax::Description, text),
            Descriptive::CodeInline(expr) => {
                sub.append(Syntax::Structure, "{");
                sub.append_char(' ');
                sub.append_expression(expr);
                sub.append_char(' ');
                sub.append(Syntax::Structure, "}");
            }
            Descriptive::Application(invocation) => {
                sub.append_application(invocation);
            }
            Descriptive::Binding(_, _) => {
                sub.append_str("<<nested binding>>");
            }
        }

        sub.append(Syntax::Structure, " ~ ");
        sub.append_variables(variables);
        sub.flush_current();
        sub.fragments
    }

    fn append_char(&mut self, c: char) {
        if c == '\n' {
            // Flush any existing buffer before adding newline
            self.flush_current();
            self.append_fragment(Syntax::Newline, "\n");
        } else {
            self.buffer
                .push(c);
        }
    }

    fn is_empty(&self) -> bool {
        self.fragments
            .is_empty()
            && self
                .buffer
                .is_empty()
    }

    fn format_header(&mut self, metadata: &Metadata) {
        self.switch_syntax(Syntax::Header);
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
        self.reset_syntax();
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
        self.append(Syntax::Declaration, name.0);

        if let Some(parameters) = &procedure.parameters {
            // note that append_arguments() is for general expression
            // arguments and append_variables() is for the special case where
            // tuples of names have parenthesis but single identifiers are
            // naked. We use append_parameters() here which always encloses
            // with parenthesis.
            self.append_parameters(parameters);
        }

        self.append_char(' ');
        self.append(Syntax::Structure, ":");

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
                self.append(Syntax::Header, "# ");
                self.append(Syntax::Title, title);
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
                self.append(Syntax::Structure, "{");
                self.append_char('\n');

                self.increase(4);
                self.indent();
                self.append_expression(expression);
                self.append_char('\n');
                self.decrease(4);

                self.append(Syntax::Structure, "}");
            }
        }
    }

    fn append_signature(&mut self, signature: &Signature) {
        self.append_genus(&signature.domain);
        self.append(Syntax::Structure, " -> ");
        self.append_genus(&signature.range);
    }

    fn append_genus(&mut self, genus: &Genus) {
        match genus {
            Genus::Unit => {
                self.append(Syntax::Forma, "()");
            }
            Genus::Single(forma) => self.append_forma(forma),
            Genus::Tuple(formas) => {
                self.append(Syntax::Structure, "(");
                for (i, forma) in formas
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        self.append(Syntax::Quote, ",");
                        self.append_char(' ');
                    }
                    self.append_forma(forma);
                }
                self.append(Syntax::Structure, ")");
            }
            Genus::Naked(formas) => {
                for (i, forma) in formas
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        self.append(Syntax::Quote, ",");
                        self.append_char(' ');
                    }
                    self.append_forma(forma);
                }
            }
            Genus::List(forma) => {
                self.append(Syntax::Structure, "[");
                self.append_forma(forma);
                self.append(Syntax::Structure, "]");
            }
        }
    }

    // Output names surrounded by parenthesis
    fn append_parameters(&mut self, variables: &Vec<Identifier>) {
        self.append(Syntax::Structure, "(");
        for (i, variable) in variables
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.append(Syntax::Structure, ", ");
            }
            self.append(Syntax::Variable, variable.0);
        }
        self.append(Syntax::Structure, ")");
    }

    fn append_forma(&mut self, forma: &Forma) {
        self.append(Syntax::Forma, forma.0)
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
        let syntax = self.current;
        let mut line = self.builder();

        for descriptive in descriptives {
            match descriptive {
                Descriptive::Text(text) => {
                    line.add_breakable(syntax, text);
                }
                Descriptive::CodeInline(expr) => match expr {
                    Expression::Tablet(_) => {
                        line.flush();
                        self.append(Syntax::Structure, "{");
                        self.append_char('\n');
                        self.increase(4);
                        self.indent();
                        self.append_expression(expr);
                        self.append_char('\n');
                        self.decrease(4);
                        line = self.builder();
                        line.add_word(Syntax::Structure, "}");
                    }
                    Expression::Multiline(_, _) => {
                        line.flush();
                        self.append(Syntax::Structure, "{");
                        self.increase(4);
                        self.append_expression(expr);
                        self.decrease(4);
                        self.append_char('\n');
                        self.indent();
                        line = self.builder();
                        line.add_word(Syntax::Structure, "}");
                    }
                    _ => match expr {
                        Expression::Execution(func)
                            if func
                                .parameters
                                .iter()
                                .any(|p| matches!(p, Expression::Multiline(_, _))) =>
                        {
                            line.flush();
                            self.append(Syntax::Structure, " { ");
                            self.append_expression(expr);
                            self.append_char(' ');
                            line = self.builder();
                            line.add_word(Syntax::Structure, "}");
                        }
                        _ => {
                            // Add space before inline code if line is not empty
                            if !line
                                .current
                                .is_empty()
                            {
                                line.add_atomic(syntax, " ");
                            }
                            line.add_inline_code(expr);
                        }
                    },
                },
                Descriptive::Application(invocation) => {
                    // Add space before application if line is not empty
                    if !line
                        .current
                        .is_empty()
                    {
                        line.add_atomic(syntax, " ");
                    }
                    line.add_application(invocation);
                }
                Descriptive::Binding(inner_descriptive, variables) => {
                    // Add space before binding if line is not empty
                    if !line
                        .current
                        .is_empty()
                    {
                        line.add_atomic(syntax, " ");
                    }
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
                self.append(Syntax::StepItem, &format!("{}.", ordinal));
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
                self.append(Syntax::StepItem, &bullet.to_string());
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
                self.append(Syntax::Structure, " | ");
            }
            self.append(Syntax::Quote, "'");
            self.append(Syntax::Response, response.value);
            self.append(Syntax::Quote, "'");

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
                        self.append(Syntax::Structure, "{");
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
                        self.append(Syntax::Structure, "{");
                        self.append_char(' ');
                        self.append_expression(expression);
                        self.append_char(' ');
                        self.append(Syntax::Structure, "}");
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
                self.append(Syntax::StepItem, numeral);
                self.append_char('.');
                if let Some(paragraph) = title {
                    self.append_char(' ');
                    self.switch_syntax(Syntax::Section);
                    self.append_descriptives(&paragraph.0);
                    self.reset_syntax();
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
                    self.append(Syntax::Attribute, "@");
                    self.append(Syntax::Attribute, name.0);
                }
                Attribute::Place(name) => {
                    self.append(Syntax::Attribute, "#");
                    self.append(Syntax::Attribute, name.0);
                }
            }
        }
    }

    fn append_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Variable(identifier) => {
                self.append(Syntax::Variable, identifier.0);
            }
            Expression::String(pieces) => {
                self.append(Syntax::Quote, "\"");
                for piece in pieces {
                    match piece {
                        Piece::Text(text) => {
                            // Preserve user string content exactly as written
                            self.append(Syntax::String, text);
                        }
                        Piece::Interpolation(expr) => {
                            let fragments = self.render_inline_code(expr);
                            for (syntax, content) in fragments {
                                self.append(syntax, &content);
                            }
                        }
                    }
                }
                self.append(Syntax::Quote, "\"");
            }
            Expression::Number(numeric) => self.append_numeric(numeric),
            Expression::Multiline(lang, lines) => {
                self.append_char('\n');

                self.indent();
                self.append(Syntax::Quote, "```");
                if let Some(which) = lang {
                    self.append(Syntax::Language, which);
                }
                self.append_char('\n');

                self.increase(4);
                for line in lines {
                    self.indent();
                    // Break multiline content into words for wrapping
                    for (i, word) in line
                        .split_ascii_whitespace()
                        .enumerate()
                    {
                        if i > 0 {
                            self.append(Syntax::Multiline, " ");
                        }
                        self.append(Syntax::Multiline, word);
                    }
                    self.append_char('\n');
                }
                self.decrease(4);

                self.indent();
                self.append(Syntax::Quote, "```");
                self.append_char('\n');
            }
            Expression::Repeat(expression) => {
                self.append(Syntax::Keyword, "repeat ");
                self.append_expression(expression);
            }
            Expression::Foreach(variables, expression) => {
                self.append(Syntax::Keyword, "foreach ");
                self.append_variables(variables);
                self.append(Syntax::Keyword, " in ");
                self.append_expression(expression);
            }
            Expression::Application(invocation) => self.append_application(invocation),
            Expression::Execution(function) => self.append_function(function),
            Expression::Binding(expression, variables) => {
                self.append_expression(expression);
                self.append(Syntax::Structure, " ~ ");
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
            self.append(Syntax::Variable, variable.0);
        }
        if variables.len() > 1 {
            self.append_char(')');
        }
    }

    fn append_numeric(&mut self, numeric: &Numeric) {
        match numeric {
            Numeric::Integral(num) => self.append(Syntax::Numeric, &num.to_string()),
            Numeric::Scientific(quantity) => self.append(Syntax::Numeric, &quantity.to_string()),
        }
    }

    fn append_application(&mut self, invocation: &Invocation) {
        self.append(Syntax::Quote, "<");
        match &invocation.target {
            Target::Local(identifier) => self.append(Syntax::Invocation, identifier.0),
            Target::Remote(external) => self.append(Syntax::Invocation, external.0),
        }
        self.append(Syntax::Quote, ">");
        if let Some(parameters) = &invocation.parameters {
            self.append_arguments(parameters);
        }
    }

    // This is the one that is for the generalized case where the arguments to
    // a function can be Expressions themselves (though usually are just
    // variable names)
    fn append_arguments(&mut self, parameters: &Vec<Expression>) {
        self.append(Syntax::Structure, "(");

        for (i, parameter) in parameters
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.append(Syntax::Structure, ", ");
            }
            self.append_expression(parameter);
        }

        self.append(Syntax::Structure, ")");
    }

    fn append_function(&mut self, function: &Function) {
        self.append(
            Syntax::Function,
            &function
                .target
                .0,
        );
        self.append(Syntax::Structure, "(");

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
                self.append(Syntax::Structure, ", ");
            }
            self.append_expression(parameter);
        }

        if has_multiline {
            self.indent();
        }
        self.append(Syntax::Structure, ")");
    }

    fn append_tablet(&mut self, pairs: &Vec<Pair>) {
        self.append(Syntax::Structure, "[");
        self.append_char('\n');

        self.increase(4);
        for pair in pairs {
            self.indent();
            self.append(Syntax::Quote, "\"");
            self.append(Syntax::Label, pair.label);
            self.append(Syntax::Quote, "\"");
            self.append(Syntax::Structure, " = ");
            self.append_expression(&pair.value);
            self.append_char('\n');
        }
        self.decrease(4);

        self.indent();
        self.append(Syntax::Structure, "]");
    }
}

impl ToString for Formatter {
    fn to_string(&self) -> String {
        let mut result = String::new();

        // Start with all already accumulated fragments
        for (_, content) in &self.fragments {
            result.push_str(content);
        }

        // If there's unflushed content in the buffer, add it as well
        // This ensures we get a complete representation even if the formatter
        // hasn't been explicitly flushed
        if !self
            .buffer
            .is_empty()
        {
            result.push_str(&self.buffer);
        }

        result
    }
}

struct Line<'a> {
    output: &'a mut Formatter, // reference to parent
    current: Vec<(Syntax, String)>,
    position: u8,
}

impl<'a> Line<'a> {
    fn new(output: &'a mut Formatter) -> Self {
        Line {
            current: Vec::new(),
            position: output.nesting,
            output,
        }
    }

    fn add_atomic(&mut self, syntax: Syntax, content: &str) {
        // Treat as atomic units - don't split them further
        if !self
            .current
            .is_empty()
        {
            if self.position + content.len() as u8
                > self
                    .output
                    .width
            {
                self.wrap_line();
                self.current
                    .push((syntax, content.to_string()));
                self.position = self
                    .output
                    .nesting
                    + content.len() as u8;
            } else {
                self.current
                    .push((syntax, content.to_string()));
                self.position += content.len() as u8;
            }
        } else {
            self.current
                .push((syntax, content.to_string()));
            self.position += content.len() as u8;
        }
    }

    fn add_breakable(&mut self, syntax: Syntax, content: &str) {
        // Split content by whitespace for proper line wrapping
        for word in content.split_ascii_whitespace() {
            self.add_word(syntax, word);
        }
    }

    fn add_word(&mut self, syntax: Syntax, word: &str) {
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
                    .push((syntax, word.to_string()));
                self.position = self
                    .output
                    .nesting
                    + word.len() as u8;
            } else {
                self.current
                    .push((Syntax::Description, " ".to_string()));
                self.current
                    .push((syntax, word.to_string()));
                self.position += 1 + word.len() as u8;
            }
        } else {
            self.current
                .push((syntax, word.to_string()));
            self.position += word.len() as u8;
        }
    }

    fn add_inline_code(&mut self, expr: &Expression) {
        let fragments = self
            .output
            .render_inline_code(expr);
        self.add_fragments(fragments);
    }

    fn add_application(&mut self, invocation: &Invocation) {
        let fragments = self
            .output
            .render_application(invocation);
        self.add_fragments(fragments);
    }

    fn add_binding(&mut self, inner_descriptive: &Descriptive, variables: &Vec<Identifier>) {
        let fragments = self
            .output
            .render_binding(inner_descriptive, variables);
        self.add_fragments(fragments);
    }

    fn add_fragments(&mut self, fragments: Vec<(Syntax, String)>) {
        // All fragments should be atomic - the formatter is responsible for breaking up content
        for (syntax, content) in fragments {
            self.add_atomic(syntax, &content);
        }
    }

    fn wrap_line(&mut self) {
        // Emit all current fragments to the output
        for (syntax, content) in &self.current {
            self.output
                .append(*syntax, content);
        }
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
            // Emit all current fragments to the output
            for (syntax, content) in &self.current {
                self.output
                    .append(*syntax, content);
            }
        }
    }
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn genus() {
        let mut output = Formatter::new(78);

        output.append_forma(&Forma("Jedi"));
        assert_eq!(output.to_string(), "Jedi");

        output.reset();
        output.append_genus(&Genus::Unit);
        assert_eq!(output.to_string(), "()");

        output.reset();
        output.append_genus(&Genus::Single(Forma("Stormtrooper")));
        assert_eq!(output.to_string(), "Stormtrooper");

        output.reset();
        output.append_genus(&Genus::List(Forma("Pilot")));
        assert_eq!(output.to_string(), "[Pilot]");

        output.reset();
        output.append_genus(&Genus::Tuple(vec![
            Forma("Kid"),
            Forma("Pilot"),
            Forma("Scoundrel"),
            Forma("Princess"),
        ]));
        assert_eq!(output.to_string(), "(Kid, Pilot, Scoundrel, Princess)");

        output.reset();
    }

    #[test]
    fn signatures() {
        let mut output = Formatter::new(78);

        output.append_signature(&Signature {
            domain: Genus::Single(Forma("Alderaan")),
            range: Genus::Single(Forma("AsteroidField")),
        });
        assert_eq!(output.to_string(), "Alderaan -> AsteroidField");

        output.reset();
        output.append_signature(&Signature {
            domain: Genus::List(Forma("Clone")),
            range: Genus::Single(Forma("Army")),
        });
        assert_eq!(output.to_string(), "[Clone] -> Army");

        output.reset();
        output.append_signature(&Signature {
            domain: Genus::Single(Forma("TaxationOfTradeRoutes")),
            range: Genus::Tuple(vec![Forma("Rebels"), Forma("Empire")]),
        });
        assert_eq!(
            output.to_string(),
            "TaxationOfTradeRoutes -> (Rebels, Empire)"
        );
    }

    #[test]
    fn numbers() {
        let mut output = Formatter::new(78);

        output.append_numeric(&Numeric::Integral(42));
        assert_eq!(output.to_string(), "42");
    }

    #[test]
    fn to_string_handles_unflushed_content() {
        let mut output = Formatter::new(78);

        // Manually add content to buffer without flushing
        output
            .buffer
            .push_str("unflushed content");

        // to_string() should include both fragments and unflushed buffer
        let result = output.to_string();
        assert_eq!(result, "unflushed content");

        // Add some fragments and more buffer content
        output
            .fragments
            .push((Syntax::Declaration, "flushed content".to_string()));
        output
            .buffer
            .push_str(" more unflushed");

        let result2 = output.to_string();
        assert_eq!(result2, "flushed contentunflushed content more unflushed");
    }

    #[test]
    fn append_methods_flush_state() {
        let mut output = Formatter::new(78);

        // Check state after append_numeric
        output.append_numeric(&Numeric::Integral(42));
        println!("After append_numeric:");
        println!("  Fragments: {:?}", output.fragments);
        println!("  Buffer: '{}'", output.buffer);
        println!("  Current syntax: {:?}", output.current);

        // The number should be in fragments, buffer should be empty
        assert!(!output
            .fragments
            .is_empty());
        assert!(output
            .buffer
            .is_empty());
        assert_eq!(output.current, Syntax::Neutral);
    }
}
