//! Code formatter for the Technique language

use crate::formatting::*;
use crate::language::*;
use std::borrow::Cow;

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

// Pre-allocated common indent strings to avoid repeated allocations
const INDENT_CACHE: &[&str] = &[
    "",
    "    ",
    "        ",
    "            ",
    "                ",
    "                    ",
    "                        ",
    "                            ",
    "                                ",
    "                                    ",
    "                                        ",
];

pub fn format_with_renderer<'i>(technique: &'i Document, width: u8) -> Vec<(Syntax, Cow<'i, str>)> {
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
                output.add_fragment_reference(Syntax::Description, "\n");
            }
        }
    }

    output.fragments
}

/// Utility functions for rendering individual AST types for Present trait implementations
/// These functions create a sub-formatter, render the specific type, and return a styled string

pub fn render_signature<'i>(signature: &'i Signature, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.append_signature(signature);
    render_fragments(&sub.fragments, renderer)
}

pub fn render_genus<'i>(genus: &'i Genus, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.append_genus(genus);
    render_fragments(&sub.fragments, renderer)
}

pub fn render_forma<'i>(forma: &'i Forma, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.append_forma(forma);
    render_fragments(&sub.fragments, renderer)
}

pub fn render_identifier<'i>(identifier: &'i Identifier, renderer: &dyn Render) -> String {
    renderer.style(Syntax::Declaration, identifier.0)
}

pub fn render_response<'i>(response: &'i Response, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.add_fragment_reference(Syntax::Quote, "'");
    sub.add_fragment_reference(Syntax::Response, response.value);
    sub.add_fragment_reference(Syntax::Quote, "'");
    if let Some(condition) = response.condition {
        sub.add_fragment_reference(Syntax::Neutral, " ");
        sub.add_fragment_reference(Syntax::Description, condition);
    }
    render_fragments(&sub.fragments, renderer)
}

pub fn render_numeric<'i>(numeric: &'i Numeric, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.append_numeric(numeric);
    render_fragments(&sub.fragments, renderer)
}

pub fn render_quantity<'i>(quantity: &'i Quantity, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.append_quantity(quantity);
    render_fragments(&sub.fragments, renderer)
}

pub fn render_invocation<'i>(invocation: &'i Invocation, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.append_application(invocation);
    render_fragments(&sub.fragments, renderer)
}

pub fn render_descriptive<'i>(descriptive: &'i Descriptive, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.append_descriptive(descriptive);
    render_fragments(&sub.fragments, renderer)
}

pub fn render_function<'i>(function: &'i Function, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.append_function(function);
    render_fragments(&sub.fragments, renderer)
}

pub fn render_expression<'i>(expression: &'i Expression, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.append_expression(expression);
    render_fragments(&sub.fragments, renderer)
}

pub fn render_procedure_declaration<'i>(procedure: &'i Procedure, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.append(
        Syntax::Declaration,
        procedure
            .name
            .0,
    );
    if let Some(parameters) = &procedure.parameters {
        sub.append_parameters(parameters);
    }
    sub.add_fragment_reference(Syntax::Neutral, " ");
    sub.add_fragment_reference(Syntax::Structure, ":");
    if let Some(signature) = &procedure.signature {
        sub.add_fragment_reference(Syntax::Neutral, " ");
        sub.append_signature(signature);
    }
    render_fragments(&sub.fragments, renderer)
}

/// Helper function to convert fragments to a styled string using a renderer
fn render_fragments<'i>(fragments: &[(Syntax, Cow<'i, str>)], renderer: &dyn Render) -> String {
    let mut result = String::new();
    for (syntax, content) in fragments {
        result.push_str(&renderer.style(*syntax, content));
    }
    result
}

struct Formatter<'i> {
    fragments: Vec<(Syntax, Cow<'i, str>)>,
    nesting: u8,
    width: u8,
    current: Syntax,
    buffer: String,
}

impl<'i> Formatter<'i> {
    fn new(width: u8) -> Formatter<'i> {
        Formatter {
            fragments: Vec::new(),
            nesting: 0,
            width,
            current: Syntax::Neutral,
            buffer: String::new(),
        }
    }

    /// Add a fragment from a reference to the input string. This is
    /// extensively used for adding whitespace and structural characters so we
    /// avoid repeatedly allocating entire Strings just to hold a single known
    /// character.
    pub fn add_fragment_reference(&mut self, syntax: Syntax, content: &'i str) {
        self.fragments
            .push((syntax, Cow::Borrowed(content)));
    }

    /// Add a fragment, taking ownership of the supplied String. This is
    /// useful when we have just allocated a temporary string immediately
    /// prior to passing it in.
    pub fn add_fragment_string(&mut self, syntax: Syntax, content: String) {
        self.fragments
            .push((syntax, Cow::Owned(content)));
    }

    /// Add a fragment, already wrapped as a Copy-on-write value.
    pub fn add_fragment(&mut self, syntax: Syntax, content: Cow<'i, str>) {
        match content {
            Cow::Borrowed(s) => self.add_fragment_reference(syntax, s),
            Cow::Owned(s) => self.add_fragment_string(syntax, s),
        }
    }

    /// Append content with a specific syntax tagging
    fn append(&mut self, syntax: Syntax, content: &'i str) {
        // Flush any pending buffer content first to maintain order
        self.flush_current();
        self.add_fragment_reference(syntax, content);
    }

    fn switch_syntax(&mut self, new_syntax: Syntax) {
        if !self
            .buffer
            .is_empty()
        {
            self.fragments
                .push((self.current, Cow::Owned(std::mem::take(&mut self.buffer))));
        }
        self.current = new_syntax;
    }

    fn reset_syntax(&mut self) {
        self.switch_syntax(Syntax::Neutral);
    }

    pub fn flush_current(&mut self) {
        if !self
            .buffer
            .is_empty()
        {
            self.fragments
                .push((self.current, Cow::Owned(std::mem::take(&mut self.buffer))));
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
            let indent_level = (self.nesting as usize) / 4;
            if indent_level < INDENT_CACHE.len() {
                self.add_fragment_reference(Syntax::Indent, INDENT_CACHE[indent_level]);
            } else {
                // Fallback for deep nesting
                let spaces = " ".repeat(self.nesting as usize);
                self.add_fragment_string(Syntax::Indent, spaces);
            }
        }
    }

    pub fn append_str(&mut self, text: &str) {
        for c in text.chars() {
            self.append_char(c);
        }
    }

    fn append_breakable(&mut self, syntax: Syntax, text: &'i str) {
        for (i, word) in text
            .split_ascii_whitespace()
            .enumerate()
        {
            if i > 0 {
                self.add_fragment_reference(Syntax::Neutral, " ");
            }
            self.add_fragment_reference(syntax, word);
        }
    }

    fn subformatter(&self) -> Formatter<'i> {
        Formatter {
            fragments: Vec::new(),
            nesting: self.nesting,
            width: self.width,
            current: Syntax::Neutral,
            buffer: String::new(),
        }
    }

    fn builder(&mut self) -> Line<'_, 'i> {
        Line::new(self)
    }

    fn render_inline_code(&self, expr: &'i Expression) -> Vec<(Syntax, Cow<'i, str>)> {
        match expr {
            Expression::Tablet(_) | Expression::Multiline(_, _) => {
                // These are not inline, caller should handle specially
                Vec::new()
            }
            _ => {
                let mut sub = self.subformatter();
                sub.add_fragment_reference(Syntax::Structure, "{");
                sub.add_fragment_reference(Syntax::Neutral, " ");
                sub.append_expression(expr);
                sub.add_fragment_reference(Syntax::Neutral, " ");
                sub.add_fragment_reference(Syntax::Structure, "}");
                sub.flush_current();

                let mut combined = String::new();
                for (_syntax, content) in &sub.fragments {
                    combined.push_str(&content);
                }

                vec![(Syntax::Structure, Cow::Owned(combined))]
            }
        }
    }

    fn render_string_interpolation(&self, expr: &'i Expression) -> Vec<(Syntax, Cow<'i, str>)> {
        let mut sub = self.subformatter();
        sub.add_fragment_reference(Syntax::Structure, "{");
        sub.add_fragment_reference(Syntax::Neutral, " ");
        sub.append_expression(expr);
        sub.add_fragment_reference(Syntax::Neutral, " ");
        sub.add_fragment_reference(Syntax::Structure, "}");
        sub.flush_current();
        sub.fragments
    }

    fn render_application(&self, invocation: &'i Invocation) -> Vec<(Syntax, Cow<'i, str>)> {
        let mut sub = self.subformatter();
        sub.append_application(invocation);
        sub.flush_current();

        // Combine all fragments into a single atomic fragment to prevent wrapping
        let mut combined = String::new();
        for (_syntax, content) in &sub.fragments {
            combined.push_str(&content);
        }

        // Return as a single fragment
        vec![(Syntax::Invocation, Cow::Owned(combined))]
    }

    fn render_binding(
        &self,
        inner_descriptive: &'i Descriptive,
        variables: &'i Vec<Identifier>,
    ) -> Vec<(Syntax, Cow<'i, str>)> {
        let mut sub = self.subformatter();

        match inner_descriptive {
            Descriptive::Text(text) => sub.append_breakable(Syntax::Description, text),
            Descriptive::CodeInline(expr) => {
                sub.add_fragment_reference(Syntax::Structure, "{");
                sub.add_fragment_reference(Syntax::Neutral, " ");
                sub.append_expression(expr);
                sub.add_fragment_reference(Syntax::Neutral, " ");
                sub.add_fragment_reference(Syntax::Structure, "}");
            }
            Descriptive::Application(invocation) => {
                sub.append_application(invocation);
            }
            Descriptive::Binding(_, _) => {
                sub.append_str("<<nested binding>>");
            }
        }

        sub.add_fragment_reference(Syntax::Neutral, " ");
        sub.add_fragment_reference(Syntax::Structure, "~");
        sub.add_fragment_reference(Syntax::Neutral, " ");
        sub.append_variables(variables);
        sub.flush_current();

        // Combine all fragments into a single atomic fragment to prevent wrapping
        let mut combined = String::new();
        for (_syntax, content) in &sub.fragments {
            combined.push_str(&content);
        }

        vec![(Syntax::Structure, Cow::Owned(combined))]
    }

    pub fn append_char(&mut self, c: char) {
        if c == '\n' {
            // Flush any existing buffer before adding newline
            self.flush_current();
            self.add_fragment_reference(Syntax::Newline, "\n");
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

    fn format_header(&mut self, metadata: &'i Metadata) {
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

    fn format_technique(&mut self, technique: &'i Technique) {
        match technique {
            Technique::Steps(steps) => {
                // if a header has already been added,
                // separate the upcoming steps with a blank line.
                if !self.is_empty() {
                    self.append_char('\n');
                }
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

    fn format_procedure(&mut self, procedure: &'i Procedure) {
        // if a header or another procedure has already been added,
        // separate the upcoming one with a blank line.
        if !self.is_empty() {
            self.append_char('\n');
        }

        // declaration

        let name = &procedure.name;
        self.add_fragment_reference(Syntax::Declaration, name.0);

        if let Some(parameters) = &procedure.parameters {
            // note that append_arguments() is for general expression
            // arguments and append_variables() is for the special case where
            // tuples of names have parenthesis but single identifiers are
            // naked. We use append_parameters() here which always encloses
            // with parenthesis.
            self.append_parameters(parameters);
        }

        self.add_fragment_reference(Syntax::Neutral, " ");
        self.add_fragment_reference(Syntax::Structure, ":");

        if let Some(signature) = &procedure.signature {
            self.add_fragment_reference(Syntax::Neutral, " ");
            self.append_signature(signature);
        }

        self.append_char('\n');

        // elements

        for element in &procedure.elements {
            self.append_element(element);
        }
    }

    fn append_element(&mut self, element: &'i Element) {
        match element {
            Element::Title(title) => {
                self.add_fragment_reference(Syntax::Newline, "\n");
                self.add_fragment_reference(Syntax::Header, "# ");
                self.add_fragment_reference(Syntax::Title, title);
                self.add_fragment_reference(Syntax::Newline, "\n");
            }
            Element::Description(paragraphs) => {
                self.add_fragment_reference(Syntax::Newline, "\n");
                self.append_paragraphs(paragraphs);
            }
            Element::Steps(steps) => {
                self.add_fragment_reference(Syntax::Newline, "\n");
                self.append_steps(steps);
            }
            Element::CodeBlock(expression) => {
                self.add_fragment_reference(Syntax::Structure, "{");
                self.add_fragment_reference(Syntax::Newline, "\n");

                self.increase(4);
                self.indent();
                self.append_expression(expression);
                self.add_fragment_reference(Syntax::Newline, "\n");
                self.decrease(4);

                self.add_fragment_reference(Syntax::Structure, "}");
            }
        }
    }

    pub fn append_signature(&mut self, signature: &'i Signature) {
        self.append_genus(&signature.domain);
        self.add_fragment_reference(Syntax::Neutral, " ");
        self.add_fragment_reference(Syntax::Structure, "->");
        self.add_fragment_reference(Syntax::Neutral, " ");
        self.append_genus(&signature.range);
    }

    pub fn append_genus(&mut self, genus: &'i Genus) {
        match genus {
            Genus::Unit => {
                self.add_fragment_reference(Syntax::Forma, "()");
            }
            Genus::Single(forma) => self.append_forma(forma),
            Genus::Tuple(formas) => {
                self.add_fragment_reference(Syntax::Structure, "(");
                for (i, forma) in formas
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        self.add_fragment_reference(Syntax::Structure, ",");
                        self.add_fragment_reference(Syntax::Neutral, " ");
                    }
                    self.append_forma(forma);
                }
                self.add_fragment_reference(Syntax::Structure, ")");
            }
            Genus::Naked(formas) => {
                for (i, forma) in formas
                    .iter()
                    .enumerate()
                {
                    if i > 0 {
                        self.add_fragment_reference(Syntax::Structure, ",");
                        self.add_fragment_reference(Syntax::Neutral, " ");
                    }
                    self.append_forma(forma);
                }
            }
            Genus::List(forma) => {
                self.add_fragment_reference(Syntax::Structure, "[");
                self.append_forma(forma);
                self.add_fragment_reference(Syntax::Structure, "]");
            }
        }
    }

    // Output names surrounded by parenthesis
    pub fn append_parameters(&mut self, variables: &'i Vec<Identifier>) {
        self.add_fragment_reference(Syntax::Structure, "(");
        for (i, variable) in variables
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.add_fragment_reference(Syntax::Structure, ",");
                self.add_fragment_reference(Syntax::Neutral, " ");
            }
            self.add_fragment_reference(Syntax::Variable, variable.0);
        }
        self.add_fragment_reference(Syntax::Structure, ")");
    }

    pub fn append_forma(&mut self, forma: &'i Forma) {
        self.add_fragment_reference(Syntax::Forma, forma.0)
    }

    fn append_paragraphs(&mut self, paragraphs: &'i Vec<Paragraph>) {
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

    // This is a helper for rendering a single descriptives in error messages.
    // The real method is append_decriptives() below; this method simply
    // creates a single element slice that can be passed to it.
    fn append_descriptive(&mut self, descriptive: &'i Descriptive) {
        use std::slice;
        let slice = slice::from_ref(descriptive);
        self.append_descriptives(slice);
    }

    fn append_descriptives(&mut self, descriptives: &'i [Descriptive<'i>]) {
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
                        self.add_fragment_reference(Syntax::Structure, "{");
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
                        self.add_fragment_reference(Syntax::Structure, "{");
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
                            self.add_fragment_reference(Syntax::Neutral, " ");
                            self.add_fragment_reference(Syntax::Structure, "{");
                            self.add_fragment_reference(Syntax::Neutral, " ");
                            self.append_expression(expr);
                            self.add_fragment_reference(Syntax::Neutral, " ");
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

    fn append_steps(&mut self, steps: &'i Vec<Scope>) {
        self.increase(4);
        self.append_scopes(steps);
        self.decrease(4);
    }

    fn append_step(&mut self, step: &'i Scope) {
        match step {
            Scope::DependentBlock {
                ordinal,
                description: content,
                subscopes: scopes,
            } => {
                self.indent();
                self.add_fragment_string(Syntax::StepItem, format!("{}.", ordinal));
                self.add_fragment_reference(Syntax::Neutral, " ");
                if ordinal.len() == 1 {
                    self.add_fragment_reference(Syntax::Neutral, " ");
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
                self.add_fragment_string(Syntax::StepItem, bullet.to_string());
                self.add_fragment_reference(Syntax::Neutral, "   ");

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

    fn append_responses(&mut self, responses: &'i Vec<Response>) {
        for (i, response) in responses
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.add_fragment_reference(Syntax::Structure, "|");
                self.add_fragment_reference(Syntax::Neutral, " ");
            }
            self.add_fragment_reference(Syntax::Quote, "'");
            self.add_fragment_reference(Syntax::Response, response.value);
            self.add_fragment_reference(Syntax::Quote, "'");

            if let Some(text) = response.condition {
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.add_fragment_reference(Syntax::Description, text);
            }
        }
        self.append_char('\n');
    }

    fn append_scope(&mut self, scope: &'i Scope) {
        match scope {
            Scope::DependentBlock { .. } | Scope::ParallelBlock { .. } => {
                self.append_step(scope);
            }
            Scope::AttributeBlock {
                attributes,
                subscopes,
            } => {
                self.append_attributes(attributes);
                self.add_fragment_reference(Syntax::Newline, "\n");

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
                        self.add_fragment_reference(Syntax::Structure, "{");
                        self.add_fragment_reference(Syntax::Newline, "\n");

                        self.increase(4);
                        self.indent();
                        self.append_expression(expression);
                        self.add_fragment_reference(Syntax::Newline, "\n");
                        self.decrease(4);
                        self.indent();
                        self.add_fragment_reference(Syntax::Structure, "}");
                    }
                    _ => {
                        self.indent();
                        self.add_fragment_reference(Syntax::Structure, "{");
                        self.add_fragment_reference(Syntax::Neutral, " ");
                        self.append_expression(expression);
                        self.add_fragment_reference(Syntax::Neutral, " ");
                        self.add_fragment_reference(Syntax::Structure, "}");
                    }
                }
                self.add_fragment_reference(Syntax::Newline, "\n");

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
                self.add_fragment_reference(Syntax::StepItem, numeral);
                self.add_fragment_reference(Syntax::Structure, ".");
                if let Some(paragraph) = title {
                    self.add_fragment_reference(Syntax::Neutral, " ");
                    self.switch_syntax(Syntax::Section);
                    self.append_descriptives(&paragraph.0);
                    self.reset_syntax();
                }
                self.add_fragment_reference(Syntax::Newline, "\n");

                // Sections headings always reset back to left margin
                let saved = self.nesting;
                self.nesting = 0;
                self.format_technique(body);
                self.nesting = saved;
            }
        }
    }

    fn append_scopes(&mut self, scopes: &'i Vec<Scope>) {
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

    fn append_attributes(&mut self, attributes: &'i Vec<Attribute>) {
        self.indent();
        for (i, attribute) in attributes
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.add_fragment_reference(Syntax::Neutral, " + ");
            }
            match attribute {
                Attribute::Role(name) => {
                    self.add_fragment_reference(Syntax::Attribute, "@");
                    self.add_fragment_reference(Syntax::Attribute, name.0);
                }
                Attribute::Place(name) => {
                    self.add_fragment_reference(Syntax::Attribute, "^");
                    self.add_fragment_reference(Syntax::Attribute, name.0);
                }
            }
        }
    }

    pub fn append_expression(&mut self, expression: &'i Expression) {
        match expression {
            Expression::Variable(identifier) => {
                self.add_fragment_reference(Syntax::Variable, identifier.0);
            }
            Expression::String(pieces) => {
                self.add_fragment_reference(Syntax::Quote, "\"");
                for piece in pieces {
                    match piece {
                        Piece::Text(text) => {
                            // Preserve user string content exactly as written
                            self.add_fragment_reference(Syntax::String, text);
                        }
                        Piece::Interpolation(expr) => {
                            let fragments = self.render_string_interpolation(expr);
                            for (syntax, content) in fragments {
                                self.add_fragment(syntax, content);
                            }
                        }
                    }
                }
                self.add_fragment_reference(Syntax::Quote, "\"");
            }
            Expression::Number(numeric) => self.append_numeric(numeric),
            Expression::Multiline(lang, lines) => {
                self.append_char('\n');

                self.indent();
                self.add_fragment_reference(Syntax::Quote, "```");
                if let Some(which) = lang {
                    self.add_fragment_reference(Syntax::Language, which);
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
                            self.add_fragment_reference(Syntax::Multiline, " ");
                        }
                        self.add_fragment_reference(Syntax::Multiline, word);
                    }
                    self.append_char('\n');
                }
                self.decrease(4);

                self.indent();
                self.add_fragment_reference(Syntax::Quote, "```");
                self.append_char('\n');
            }
            Expression::Repeat(expression) => {
                self.add_fragment_reference(Syntax::Keyword, "repeat");
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.append_expression(expression);
            }
            Expression::Foreach(variables, expression) => {
                self.add_fragment_reference(Syntax::Keyword, "foreach");
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.append_variables(variables);
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.add_fragment_reference(Syntax::Keyword, "in");
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.append_expression(expression);
            }
            Expression::Application(invocation) => self.append_application(invocation),
            Expression::Execution(function) => self.append_function(function),
            Expression::Binding(expression, variables) => {
                self.append_expression(expression);
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.add_fragment_reference(Syntax::Structure, "~");
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.append_variables(variables);
            }
            Expression::Tablet(pairs) => self.append_tablet(pairs),
        }
    }

    // When doing binding we omit the parenthesis in the most common case of
    // there only being one name being bound to.
    fn append_variables(&mut self, variables: &'i Vec<Identifier>) {
        if variables.len() > 1 {
            self.add_fragment_reference(Syntax::Structure, "(");
        }
        for (i, variable) in variables
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.add_fragment_reference(Syntax::Structure, ",");
                self.add_fragment_reference(Syntax::Neutral, " ");
            }
            self.add_fragment_reference(Syntax::Variable, variable.0);
        }
        if variables.len() > 1 {
            self.add_fragment_reference(Syntax::Structure, ")");
        }
    }

    pub fn append_numeric(&mut self, numeric: &'i Numeric) {
        match numeric {
            Numeric::Integral(num) => self.add_fragment_string(Syntax::Numeric, num.to_string()),
            Numeric::Scientific(quantity) => self.append_quantity(quantity),
        }
    }

    pub fn append_quantity(&mut self, quantity: &'i Quantity) {
        // Format the mantissa
        self.add_fragment_string(Syntax::Numeric, format!("{}", quantity.mantissa));

        // Add uncertainty if present
        if let Some(uncertainty) = &quantity.uncertainty {
            self.add_fragment_reference(Syntax::Neutral, " ");
            self.add_fragment_reference(Syntax::Numeric, "±");
            self.add_fragment_reference(Syntax::Neutral, " ");
            self.add_fragment_string(Syntax::Numeric, format!("{}", uncertainty));
        }

        // Add magnitude if present
        if let Some(magnitude) = &quantity.magnitude {
            self.add_fragment_reference(Syntax::Neutral, " ");
            self.add_fragment_reference(Syntax::Numeric, "×");
            self.add_fragment_reference(Syntax::Neutral, " ");
            self.add_fragment_reference(Syntax::Numeric, "10");
            self.add_fragment_string(Syntax::Numeric, to_superscript(*magnitude));
        }

        // Add unit symbol
        self.add_fragment_reference(Syntax::Neutral, " ");
        self.add_fragment_reference(Syntax::Numeric, quantity.symbol);
    }

    pub fn append_application(&mut self, invocation: &'i Invocation) {
        self.add_fragment_reference(Syntax::Quote, "<");
        match &invocation.target {
            Target::Local(identifier) => {
                self.add_fragment_reference(Syntax::Invocation, identifier.0)
            }
            Target::Remote(external) => self.add_fragment_reference(Syntax::Invocation, external.0),
        }
        self.add_fragment_reference(Syntax::Quote, ">");
        if let Some(parameters) = &invocation.parameters {
            self.append_arguments(parameters);
        }
    }

    // This is the one that is for the generalized case where the arguments to
    // a function can be Expressions themselves (though usually are just
    // variable names)
    fn append_arguments(&mut self, parameters: &'i Vec<Expression>) {
        self.add_fragment_reference(Syntax::Structure, "(");

        for (i, parameter) in parameters
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.add_fragment_reference(Syntax::Structure, ",");
                self.add_fragment_reference(Syntax::Neutral, " ");
            }
            self.append_expression(parameter);
        }

        self.add_fragment_reference(Syntax::Structure, ")");
    }

    pub fn append_function(&mut self, function: &'i Function) {
        self.add_fragment_reference(
            Syntax::Function,
            function
                .target
                .0,
        );
        self.add_fragment_reference(Syntax::Structure, "(");

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
                self.add_fragment_reference(Syntax::Structure, ",");
                self.add_fragment_reference(Syntax::Neutral, " ");
            }
            self.append_expression(parameter);
        }

        if has_multiline {
            self.indent();
        }
        self.add_fragment_reference(Syntax::Structure, ")");
    }

    fn append_tablet(&mut self, pairs: &'i Vec<Pair>) {
        self.add_fragment_reference(Syntax::Structure, "[");
        self.append_char('\n');

        self.increase(4);
        for pair in pairs {
            self.indent();
            self.add_fragment_reference(Syntax::Quote, "\"");
            self.add_fragment_reference(Syntax::Label, pair.label);
            self.add_fragment_reference(Syntax::Quote, "\"");
            self.add_fragment_reference(Syntax::Neutral, " ");
            self.add_fragment_reference(Syntax::Structure, "=");
            self.add_fragment_reference(Syntax::Neutral, " ");
            self.append_expression(&pair.value);
            self.append_char('\n');
        }
        self.decrease(4);

        self.indent();
        self.add_fragment_reference(Syntax::Structure, "]");
    }
}

impl<'i> ToString for Formatter<'i> {
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

struct Line<'a, 'i> {
    output: &'a mut Formatter<'i>, // reference to parent
    current: Vec<(Syntax, Cow<'i, str>)>,
    position: u8,
}

impl<'a, 'i> Line<'a, 'i> {
    fn new(output: &'a mut Formatter<'i>) -> Self {
        Line {
            current: Vec::new(),
            position: output.nesting,
            output,
        }
    }

    fn add_atomic(&mut self, syntax: Syntax, content: &'i str) {
        self.add_atomic_cow(syntax, Cow::Borrowed(content));
    }

    fn add_atomic_cow(&mut self, syntax: Syntax, content: Cow<'i, str>) {
        // Treat as atomic units - don't split them further
        let len = content.len() as u8;
        if !self
            .current
            .is_empty()
        {
            if self.position + len
                > self
                    .output
                    .width
            {
                self.wrap_line();
                self.current
                    .push((syntax, content));
                self.position = self
                    .output
                    .nesting
                    + len;
            } else {
                self.current
                    .push((syntax, content));
                self.position += len;
            }
        } else {
            self.current
                .push((syntax, content));
            self.position += len;
        }
    }

    fn add_breakable(&mut self, syntax: Syntax, content: &'i str) {
        // Split content by whitespace for proper line wrapping
        for word in content.split_ascii_whitespace() {
            self.add_word(syntax, word);
        }
    }

    fn add_word(&mut self, syntax: Syntax, word: &'i str) {
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
                    .push((syntax, Cow::Borrowed(word)));
                self.position = self
                    .output
                    .nesting
                    + word.len() as u8;
            } else {
                self.current
                    .push((Syntax::Description, Cow::Borrowed(" ")));
                self.current
                    .push((syntax, Cow::Borrowed(word)));
                self.position += 1 + word.len() as u8;
            }
        } else {
            self.current
                .push((syntax, Cow::Borrowed(word)));
            self.position += word.len() as u8;
        }
    }

    fn add_inline_code(&mut self, expr: &'i Expression) {
        let fragments = self
            .output
            .render_inline_code(expr);
        for (syntax, content) in fragments {
            self.add_no_wrap(syntax, content);
        }
    }

    fn add_application(&mut self, invocation: &'i Invocation) {
        let fragments = self
            .output
            .render_application(invocation);
        for (syntax, content) in fragments {
            self.add_no_wrap(syntax, content);
        }
    }

    fn add_binding(&mut self, inner_descriptive: &'i Descriptive, variables: &'i Vec<Identifier>) {
        let fragments = self
            .output
            .render_binding(inner_descriptive, variables);
        // Bindings should not wrap - add as a single non-wrapping unit
        for (syntax, content) in fragments {
            self.add_no_wrap(syntax, content);
        }
    }

    fn add_fragments(&mut self, fragments: Vec<(Syntax, Cow<'i, str>)>) {
        // All fragments should be atomic - the formatter is responsible for breaking up content
        for (syntax, content) in fragments {
            self.add_atomic_cow(syntax, content);
        }
    }

    fn add_no_wrap(&mut self, syntax: Syntax, content: Cow<'i, str>) {
        // Add content that must never wrap mid-construct (inline code,
        // applications, bindings) Unlike add_atomic_cow(), this bypasses
        // width checking entirely to preserve the integrity of these language
        // constructs on single lines
        let len = content.len() as u8;
        self.current
            .push((syntax, content));
        self.position += len;
    }

    fn wrap_line(&mut self) {
        // Emit all current fragments to the output
        for (syntax, content) in self
            .current
            .drain(..)
        {
            self.output
                .add_fragment(syntax, content);
        }
        self.output
            .append_char('\n');
        self.output
            .indent();
        self.position = self
            .output
            .nesting;
    }

    fn flush(mut self) {
        if !self
            .current
            .is_empty()
        {
            // Emit all current fragments to the output
            for (syntax, content) in self
                .current
                .drain(..)
            {
                self.output
                    .add_fragment(syntax, content);
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
        let genus = Genus::Tuple(vec![
            Forma("Kid"),
            Forma("Pilot"),
            Forma("Scoundrel"),
            Forma("Princess"),
        ]);
        output.append_genus(&genus);
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
        let signature = Signature {
            domain: Genus::Single(Forma("TaxationOfTradeRoutes")),
            range: Genus::Tuple(vec![Forma("Rebels"), Forma("Empire")]),
        };
        output.append_signature(&signature);
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
            .push((
                Syntax::Declaration,
                Cow::Owned("flushed content".to_string()),
            ));
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
