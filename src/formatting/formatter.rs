//! Code formatter for the Technique language

use crate::formatting::*;
use crate::language::*;
use std::borrow::Cow;
use std::collections::HashMap;

/// Runtime values for the variables a step's prose interpolates, so the
/// runner can splice the values the user supplied in place of the `{ name }`
/// reference. Each entry is the pre-styled fragments for one variable.
/// Non-runtime rendering (formatting, error messages) uses an empty set, so
/// interpolations render as their source `{ name }`.
#[derive(Default)]
pub struct Substitutions {
    bindings: HashMap<String, Vec<(Syntax, Cow<'static, str>)>>,
}

impl Substitutions {
    pub fn new() -> Self {
        Substitutions {
            bindings: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, fragments: Vec<(Syntax, Cow<'static, str>)>) {
        self.bindings
            .insert(name, fragments);
    }

    fn lookup(&self, name: &str) -> Option<&[(Syntax, Cow<'static, str>)]> {
        self.bindings
            .get(name)
            .map(|fragments| fragments.as_slice())
    }
}

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
            if !last_content.is_empty() && !last_content.ends_with('\n') {
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
    renderer.style(Syntax::Declaration, identifier.value)
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

/// Render the document's metadata header, coloured, to a String.
pub fn render_header<'i>(metadata: &'i Metadata, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.format_header(metadata);
    render_fragments(&sub.fragments, renderer)
        .trim_end()
        .to_string()
}

/// Render a procedure or section title with its leading `#` marker.
pub fn render_title(title: &str, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.add_fragment_reference(Syntax::Header, "# ");
    sub.add_fragment_reference(Syntax::Title, title);
    sub.flush_current();
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
    render_declaration(
        procedure
            .name
            .value,
        procedure
            .parameters
            .as_ref()
            .map(|v| v.as_slice()),
        procedure
            .signature
            .as_ref(),
        renderer,
    )
}

pub fn render_declaration<'i>(
    name: &'i str,
    parameters: Option<&'i [Identifier<'i>]>,
    signature: Option<&'i Signature<'i>>,
    renderer: &dyn Render,
) -> String {
    let mut sub = Formatter::new(78);
    sub.append(Syntax::Declaration, name);
    if let Some(parameters) = parameters {
        sub.append_parameters(parameters);
    }
    sub.add_fragment_reference(Syntax::Neutral, " ");
    sub.add_fragment_reference(Syntax::Structure, ":");
    if let Some(signature) = signature {
        sub.add_fragment_reference(Syntax::Neutral, " ");
        sub.append_signature(signature);
    }
    render_fragments(&sub.fragments, renderer)
}

pub fn render_scope<'i>(scope: &'i Scope, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    match scope {
        Scope::AttributeBlock { attributes, .. } => {
            sub.append_attributes(attributes);
        }
        Scope::DependentBlock { .. } | Scope::ParallelBlock { .. } => {
            sub.append_scope(scope);
        }
        _ => unreachable!(),
    }
    render_fragments(&sub.fragments, renderer)
        .trim_end()
        .to_string()
}

/// Render a procedure's description: its prose paragraphs, blank-line
/// separated, exactly as written in the source.
pub fn render_description<'i>(paragraphs: &'i [Paragraph<'i>], renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.append_paragraphs(paragraphs);
    render_fragments(&sub.fragments, renderer)
        .trim_end()
        .to_string()
}

/// Render step's without descending into nested subscopes.
pub fn render_step<'i>(scope: &'i Scope, subs: &'i Substitutions, renderer: &dyn Render) -> String {
    let mut sub = Formatter::new(78);
    sub.substitutions = Some(subs);
    match scope {
        Scope::DependentBlock { .. } | Scope::ParallelBlock { .. } => {
            sub.append_step(scope);
        }
        _ => unreachable!(),
    }
    render_fragments(&sub.fragments, renderer)
        .trim_end()
        .to_string()
}

/// Helper function to convert fragments to a styled string using a renderer
fn render_fragments<'i>(fragments: &[(Syntax, Cow<'i, str>)], renderer: &dyn Render) -> String {
    let mut result = String::new();
    for (syntax, content) in fragments {
        result.push_str(&renderer.style(*syntax, content));
    }
    result
}

/// Whether a word opens with sentence punctuation that should sit flush
/// against the preceding token rather than after a separating space. Lets the
/// formatter preserve `{ code },` written without an intervening space.
fn hugs_left(word: &str) -> bool {
    match word
        .chars()
        .next()
    {
        Some(c) => c == ',' || c == '.' || c == ';' || c == ':' || c == '!' || c == '?',
        None => false,
    }
}

/// A list reads as a tablet when it is non-empty and every element is a
/// labelled value. Such lists are laid out and treated as blocks rather than
/// inline.
fn is_tablet_list(elements: &[Expression]) -> bool {
    !elements.is_empty()
        && elements
            .iter()
            .all(|element| {
                if let Expression::Pair(_, _) = element {
                    true
                } else {
                    false
                }
            })
}

/// True when an expression is a tablet-shaped list (see `is_tablet_list`).
fn is_tablet_list_expr(expr: &Expression) -> bool {
    if let Expression::List(elements, _) = expr {
        is_tablet_list(elements)
    } else {
        false
    }
}

struct Formatter<'i> {
    fragments: Vec<(Syntax, Cow<'i, str>)>,
    nesting: u8,
    width: u8,
    current: Syntax,
    buffer: String,
    substitutions: Option<&'i Substitutions>,
}

impl<'i> Formatter<'i> {
    fn new(width: u8) -> Formatter<'i> {
        Formatter {
            fragments: Vec::new(),
            nesting: 0,
            width,
            current: Syntax::Neutral,
            buffer: String::new(),
            substitutions: None,
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
            substitutions: self.substitutions,
        }
    }

    fn builder(&mut self) -> Line<'_, 'i> {
        Line::new(self)
    }

    fn render_inline_code(&self, expr: &'i Expression) -> Vec<(Syntax, Cow<'i, str>)> {
        if is_tablet_list_expr(expr) {
            // Not inline; caller handles the block layout specially.
            return Vec::new();
        }
        match expr {
            Expression::Multiline(_, _, _) => {
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
                sub.fragments
            }
        }
    }

    /// Render a `;`-separated code block inline: `{ a; b }`, with `;` for
    /// each of the separators.
    fn render_inline_block(&self, exprs: &'i [Expression]) -> Vec<(Syntax, Cow<'i, str>)> {
        let mut sub = self.subformatter();
        sub.add_fragment_reference(Syntax::Structure, "{");
        sub.add_fragment_reference(Syntax::Neutral, " ");
        for expr in exprs {
            if let Expression::Separator = expr {
                sub.add_fragment_reference(Syntax::Structure, ";");
                sub.add_fragment_reference(Syntax::Neutral, " ");
            } else {
                sub.append_expression(expr);
            }
        }
        sub.add_fragment_reference(Syntax::Neutral, " ");
        sub.add_fragment_reference(Syntax::Structure, "}");
        sub.flush_current();
        sub.fragments
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

        sub.fragments
    }

    fn render_binding(
        &self,
        inner_descriptive: &'i Descriptive,
        variables: &'i Vec<Identifier>,
    ) -> Vec<(Syntax, Cow<'i, str>)> {
        let mut sub = self.subformatter();

        match inner_descriptive {
            Descriptive::Text(text) => sub.append_breakable(Syntax::Description, text),
            Descriptive::CodeInline(exprs) => {
                sub.add_fragment_reference(Syntax::Structure, "{");
                if let [expr] = exprs.as_slice() {
                    sub.add_fragment_reference(Syntax::Neutral, " ");
                    sub.append_expression(expr);
                    sub.add_fragment_reference(Syntax::Neutral, " ");
                } else {
                    sub.add_fragment_reference(Syntax::Newline, "\n");
                    sub.increase(4);
                    for expr in exprs {
                        sub.indent();
                        sub.append_expression(expr);
                        sub.add_fragment_reference(Syntax::Newline, "\n");
                    }
                    sub.decrease(4);
                    sub.indent();
                }
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

        sub.fragments
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

        if let Some(domain) = metadata.domain {
            self.append_str("& ");
            self.append_str(domain);
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

        // declaration and title kept together

        self.add_fragment_reference(Syntax::BlockBegin, "");

        let name = &procedure.name;
        self.add_fragment_reference(Syntax::Declaration, name.value);

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

        // include title in block to keep it with the declaration
        let mut elements = procedure
            .elements
            .iter();
        let mut preceded = false;
        if let Some(Element::Title(_, _)) = procedure
            .elements
            .first()
        {
            self.append_element(
                elements
                    .next()
                    .unwrap(),
            );
            preceded = true;
        }

        self.add_fragment_reference(Syntax::BlockEnd, "");

        // remaining elements

        for element in elements {
            // A code block separates from a preceding title or description with
            // a blank line, but collapses onto the declaration as the first element.
            if let Element::CodeBlock(..) = element {
                if preceded {
                    self.add_fragment_reference(Syntax::Newline, "\n");
                }
            }
            self.append_element(element);
            preceded = true;
        }
    }

    fn append_element(&mut self, element: &'i Element) {
        match element {
            Element::Title(title, _) => {
                self.add_fragment_reference(Syntax::Newline, "\n");
                self.add_fragment_reference(Syntax::Header, "# ");
                self.add_fragment_reference(Syntax::Title, title);
                self.add_fragment_reference(Syntax::Newline, "\n");
            }
            Element::Description(paragraphs, _) => {
                self.add_fragment_reference(Syntax::Newline, "\n");
                self.append_paragraphs(paragraphs);
            }
            Element::Steps(steps, _) => {
                self.add_fragment_reference(Syntax::Newline, "\n");
                self.append_steps(steps);
            }
            Element::CodeBlock(expressions, subscopes, _) => {
                self.add_fragment_reference(Syntax::Structure, "{");
                self.add_fragment_reference(Syntax::Newline, "\n");

                self.increase(4);
                for expression in expressions {
                    self.indent();
                    self.append_expression(expression);
                    self.add_fragment_reference(Syntax::Newline, "\n");
                }
                self.decrease(4);

                self.add_fragment_reference(Syntax::Structure, "}");

                if !subscopes.is_empty() {
                    self.add_fragment_reference(Syntax::Newline, "\n");
                    self.increase(4);
                    self.append_scopes(subscopes);
                    self.decrease(4);
                }
            }
        }
    }

    pub fn append_signature(&mut self, signature: &'i Signature) {
        self.append_genus(&signature.requires);
        self.add_fragment_reference(Syntax::Neutral, " ");
        self.add_fragment_reference(Syntax::Structure, "->");
        self.add_fragment_reference(Syntax::Neutral, " ");
        self.append_genus(&signature.provides);
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
    pub fn append_parameters(&mut self, variables: &'i [Identifier]) {
        self.add_fragment_reference(Syntax::Structure, "(");
        for (i, variable) in variables
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.add_fragment_reference(Syntax::Structure, ",");
                self.add_fragment_reference(Syntax::Neutral, " ");
            }
            self.add_fragment_reference(Syntax::Variable, variable.value);
        }
        self.add_fragment_reference(Syntax::Structure, ")");
    }

    pub fn append_forma(&mut self, forma: &'i Forma) {
        self.add_fragment_reference(Syntax::Forma, forma.value)
    }

    fn append_paragraphs(&mut self, paragraphs: &'i [Paragraph<'i>]) {
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
    // The real method is append_descriptives() below; this method simply
    // creates a single element slice that can be passed to it.
    fn append_descriptive(&mut self, descriptive: &'i Descriptive) {
        use std::slice;
        let slice = slice::from_ref(descriptive);
        self.append_descriptives(slice);
    }

    fn append_descriptives(&mut self, descriptives: &'i [Descriptive<'i>]) {
        let syntax = self.current;
        let subs = self.substitutions;
        let mut line = self.builder();
        // Whether the previous descriptive emitted an inline construct (code,
        // application, binding). Sentence punctuation opening the next text
        // then hugs it, preserving `{ code },` written without a space.
        let mut after_construct = false;

        for descriptive in descriptives {
            match descriptive {
                Descriptive::Text(text) => {
                    if after_construct {
                        line.add_breakable_hugging(syntax, text);
                    } else {
                        line.add_breakable(syntax, text);
                    }
                    after_construct = false;
                    continue;
                }
                Descriptive::CodeInline(exprs) if exprs.len() == 1 => {
                    let expr = &exprs[0];
                    // A bare `{ variable }` for which the runner has a bound
                    // value renders as that value in place of the source name.
                    if let Expression::Variable(name, _) = expr {
                        if let Some(fragments) = subs.and_then(|subs| subs.lookup(name.value)) {
                            line.add_value(fragments);
                            continue;
                        }
                    }
                    match expr {
                        _ if is_tablet_list_expr(expr) => {
                            line.flush();
                            self.append_char('\n');
                            self.indent();
                            self.add_fragment_reference(Syntax::Structure, "{");
                            self.append_char('\n');
                            self.increase(4);
                            self.indent();
                            self.append_expression(expr);
                            self.append_char('\n');
                            self.decrease(4);
                            self.indent();
                            // Put `}` into the fresh line builder (not as a raw
                            // fragment) so any trailing prose gets its separating space.
                            line = self.builder();
                            line.add_word(Syntax::Structure, "}");
                        }
                        Expression::Multiline(_, _, _) => {
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
                            Expression::Execution(func, _)
                                if func
                                    .parameters
                                    .iter()
                                    .any(|p| {
                                        if let Expression::Multiline(_, _, _) = p {
                                            true
                                        } else {
                                            false
                                        }
                                    }) =>
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
                    }
                }
                Descriptive::CodeInline(exprs) => {
                    let has_separator = exprs
                        .iter()
                        .any(|e| {
                            if let Expression::Separator = e {
                                true
                            } else {
                                false
                            }
                        });
                    if has_separator {
                        // `;`-separated statements stay inline — `{ a; b }` —
                        // preserving the separators the author wrote, the way
                        // the `Scope::CodeBlock` inline form does.
                        if !line
                            .current
                            .is_empty()
                        {
                            line.add_atomic(syntax, " ");
                        }
                        line.add_inline_block(exprs);
                    } else {
                        // Newline-separated statements render over several
                        // lines, mirroring the multi-line `Scope::CodeBlock`
                        // form (but remaining a `Descriptive::CodeInline`).
                        line.flush();
                        self.append_char('\n');
                        self.indent();
                        self.add_fragment_reference(Syntax::Structure, "{");
                        self.append_char('\n');
                        self.increase(4);
                        for expr in exprs {
                            self.indent();
                            self.append_expression(expr);
                            self.append_char('\n');
                        }
                        self.decrease(4);
                        self.indent();
                        // Put `}` into the fresh line builder (not as a raw
                        // fragment) so any trailing prose gets its separating
                        // space.
                        line = self.builder();
                        line.add_word(Syntax::Structure, "}");
                    }
                }
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
            after_construct = true;
        }

        line.flush();
    }

    fn append_steps(&mut self, steps: &'i Vec<Scope>) {
        self.increase(4);
        self.append_scopes(steps);
        self.decrease(4);
    }

    /// Render a step's its ordinal or bullet and its description paragraphs.
    /// Nested subscopes are walked separately by append_scope().
    fn append_step(&mut self, step: &'i Scope) {
        match step {
            Scope::DependentBlock {
                ordinal,
                description: content,
                ..
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
                self.decrease(4);
            }
            Scope::ParallelBlock {
                bullet,
                description,
                ..
            } => {
                self.indent();
                self.add_fragment_string(Syntax::StepItem, bullet.to_string());
                self.add_fragment_reference(Syntax::Neutral, "   ");

                self.increase(4);
                if description.len() > 0 {
                    self.append_paragraphs(description);
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
            Scope::DependentBlock { subscopes, .. } | Scope::ParallelBlock { subscopes, .. } => {
                self.add_fragment_reference(Syntax::BlockBegin, "");
                self.append_step(scope);
                if subscopes.len() > 0 {
                    self.increase(4);
                    self.append_scopes(subscopes);
                    self.decrease(4);
                }
                self.add_fragment_reference(Syntax::BlockEnd, "");
            }
            Scope::AttributeBlock {
                attributes,
                subscopes,
                ..
            } => {
                if subscopes.len() == 0 {
                    self.indent();
                    self.append_attributes(attributes);
                    self.add_fragment_reference(Syntax::Newline, "\n");
                    return;
                }

                let is_code = if let Scope::CodeBlock { .. } = subscopes[0] {
                    true
                } else {
                    false
                };

                // Keep attribute with its first subscope
                self.add_fragment_reference(Syntax::BlockBegin, "");
                self.indent();
                self.append_attributes(attributes);
                self.add_fragment_reference(Syntax::Newline, "\n");

                if !is_code {
                    self.increase(4);
                }
                self.append_scope(&subscopes[0]);
                self.add_fragment_reference(Syntax::BlockEnd, "");

                for scope in &subscopes[1..] {
                    self.append_scope(scope);
                }

                if !is_code {
                    self.decrease(4);
                }
            }
            Scope::CodeBlock {
                expressions,
                subscopes: substeps,
                ..
            } => {
                let has_separator = expressions
                    .iter()
                    .any(|e| {
                        if let Expression::Separator = e {
                            true
                        } else {
                            false
                        }
                    });
                let inline = if has_separator {
                    true
                } else if expressions.len() == 1 {
                    !is_tablet_list_expr(&expressions[0])
                } else {
                    false
                };

                if inline {
                    self.indent();
                    self.add_fragment_reference(Syntax::Structure, "{");
                    self.add_fragment_reference(Syntax::Neutral, " ");
                    for expression in expressions {
                        if let Expression::Separator = expression {
                            self.add_fragment_reference(Syntax::Structure, ";");
                            self.add_fragment_reference(Syntax::Neutral, " ");
                        } else {
                            self.append_expression(expression);
                        }
                    }
                    self.add_fragment_reference(Syntax::Neutral, " ");
                    self.add_fragment_reference(Syntax::Structure, "}");
                } else {
                    self.indent();
                    self.add_fragment_reference(Syntax::Structure, "{");
                    self.add_fragment_reference(Syntax::Newline, "\n");
                    self.increase(4);
                    for expression in expressions {
                        if let Expression::Separator = expression {
                            continue;
                        }
                        self.indent();
                        self.append_expression(expression);
                        self.add_fragment_reference(Syntax::Newline, "\n");
                    }
                    self.decrease(4);
                    self.indent();
                    self.add_fragment_reference(Syntax::Structure, "}");
                }
                self.add_fragment_reference(Syntax::Newline, "\n");

                // Format subscopes below this code block, if there are any.
                self.increase(4);
                self.append_scopes(substeps);
                self.decrease(4);
            }
            Scope::ResponseBlock { responses, .. } => {
                self.increase(4);
                self.indent();
                self.append_responses(responses);
                self.decrease(4);
            }
            Scope::SectionChunk {
                numeral,
                title,
                body,
                ..
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
        for (i, attribute) in attributes
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.add_fragment_reference(Syntax::Neutral, " + ");
            }
            match attribute {
                Attribute::Role(name, _) => {
                    self.add_fragment_reference(Syntax::Attribute, "@");
                    self.add_fragment_reference(Syntax::Attribute, name.value);
                }
                Attribute::Place(name, _) => {
                    self.add_fragment_reference(Syntax::Attribute, "^");
                    self.add_fragment_reference(Syntax::Attribute, name.value);
                }
            }
        }
    }

    pub fn append_expression(&mut self, expression: &'i Expression) {
        match expression {
            Expression::Variable(identifier, _) => {
                self.add_fragment_reference(Syntax::Variable, identifier.value);
            }
            Expression::String(pieces, _) => {
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
            Expression::Number(numeric, _) => self.append_numeric(numeric),
            Expression::Multiline(lang, lines, _) => {
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
            Expression::Repeat(expression, _) => {
                self.add_fragment_reference(Syntax::Keyword, "repeat");
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.append_expression(expression);
            }
            Expression::Foreach(variables, expression, _) => {
                self.add_fragment_reference(Syntax::Keyword, "foreach");
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.append_variables(variables);
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.add_fragment_reference(Syntax::Keyword, "in");
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.append_expression(expression);
            }
            Expression::Application(invocation, _) => self.append_application(invocation),
            Expression::Execution(function, _) => self.append_function(function),
            Expression::Binding(expression, variables, _) => {
                self.append_expression(expression);
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.add_fragment_reference(Syntax::Structure, "~");
                self.add_fragment_reference(Syntax::Neutral, " ");
                self.append_variables(variables);
            }
            Expression::Pair(pair, _) => self.append_pair(pair),
            Expression::List(elements, _) => self.append_list(elements),
            Expression::Hole(_) => {
                self.add_fragment_reference(Syntax::Hole, "?");
            }
            Expression::Unit(_) => {
                self.add_fragment_reference(Syntax::Unit, "()");
            }
            Expression::Separator => {}
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
            self.add_fragment_reference(Syntax::Variable, variable.value);
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
                self.add_fragment_reference(Syntax::Invocation, identifier.value)
            }
            Target::Remote(external) => {
                self.add_fragment_reference(Syntax::Invocation, external.value)
            }
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
                .value,
        );
        self.add_fragment_reference(Syntax::Structure, "(");

        let mut has_multiline = false;
        for parameter in &function.parameters {
            if let Expression::Multiline(_, _, _) = parameter {
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

    fn append_pair(&mut self, pair: &'i Pair) {
        self.add_fragment_reference(Syntax::Quote, "\"");
        self.add_fragment_reference(Syntax::Label, pair.label);
        self.add_fragment_reference(Syntax::Quote, "\"");
        self.add_fragment_reference(Syntax::Neutral, " ");
        self.add_fragment_reference(Syntax::Structure, "=");
        self.add_fragment_reference(Syntax::Neutral, " ");
        self.append_expression(&pair.value);
    }

    /// A list whose elements are all labelled (a tablet) is laid out one
    /// element per line; any other list, and the empty list, is inline.
    fn append_list(&mut self, elements: &'i Vec<Expression>) {
        if elements.is_empty() {
            self.add_fragment_reference(Syntax::Structure, "[]");
            return;
        }

        if is_tablet_list(elements) {
            self.add_fragment_reference(Syntax::Structure, "[");
            self.append_char('\n');

            self.increase(4);
            for element in elements {
                self.indent();
                self.append_expression(element);
                self.append_char('\n');
            }
            self.decrease(4);

            self.indent();
            self.add_fragment_reference(Syntax::Structure, "]");
            return;
        }

        self.add_fragment_reference(Syntax::Structure, "[");
        for (i, element) in elements
            .iter()
            .enumerate()
        {
            if i > 0 {
                self.add_fragment_reference(Syntax::Structure, ",");
            }
            self.add_fragment_reference(Syntax::Neutral, " ");
            self.append_expression(element);
        }
        self.add_fragment_reference(Syntax::Neutral, " ");
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

    /// Like `add_breakable`, but a leading sentence-punctuation word hugs the
    /// preceding token with no separating space, so `{ code },` round-trips.
    fn add_breakable_hugging(&mut self, syntax: Syntax, content: &'i str) {
        let mut words = content.split_ascii_whitespace();
        if let Some(first) = words.next() {
            if !self
                .current
                .is_empty()
                && hugs_left(first)
            {
                self.add_hugging(syntax, first);
            } else {
                self.add_word(syntax, first);
            }
            for word in words {
                self.add_word(syntax, word);
            }
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
                    .push((syntax, Cow::Borrowed(" ")));
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

    /// Append a fragment that must hug the preceding token with no separating
    /// space. Used for sentence punctuation written flush against an inline
    /// construct, e.g. the `,` in `{ code },`.
    fn add_hugging(&mut self, syntax: Syntax, word: &'i str) {
        self.add_no_wrap(syntax, Cow::Borrowed(word));
    }

    /// Splice a substituted variable's pre-styled fragments into the line as
    /// one non-breaking unit, wrapping before it if it would overflow.
    fn add_value(&mut self, fragments: &[(Syntax, Cow<'static, str>)]) {
        let len: u8 = fragments
            .iter()
            .map(|(_, content)| content.len() as u8)
            .sum();
        if !self
            .current
            .is_empty()
        {
            if self.position + 1 + len
                > self
                    .output
                    .width
            {
                self.wrap_line();
            } else {
                self.current
                    .push((Syntax::Neutral, Cow::Borrowed(" ")));
                self.position += 1;
            }
        }
        for (syntax, content) in fragments {
            self.current
                .push((*syntax, content.clone()));
        }
        self.position += len;
    }

    fn add_inline_code(&mut self, expr: &'i Expression) {
        let fragments = self
            .output
            .render_inline_code(expr);
        for (syntax, content) in fragments {
            self.add_no_wrap(syntax, content);
        }
    }

    fn add_inline_block(&mut self, exprs: &'i [Expression]) {
        let fragments = self
            .output
            .render_inline_block(exprs);
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

        let forma = Forma::new("Jedi");
        output.append_forma(&forma);
        assert_eq!(output.to_string(), "Jedi");

        output.reset();
        output.append_genus(&Genus::Unit);
        assert_eq!(output.to_string(), "()");

        output.reset();
        let single = Genus::Single(Forma::new("Stormtrooper"));
        output.append_genus(&single);
        assert_eq!(output.to_string(), "Stormtrooper");

        output.reset();
        let list = Genus::List(Forma::new("Pilot"));
        output.append_genus(&list);
        assert_eq!(output.to_string(), "[Pilot]");

        output.reset();
        let genus = Genus::Tuple(vec![
            Forma::new("Kid"),
            Forma::new("Pilot"),
            Forma::new("Scoundrel"),
            Forma::new("Princess"),
        ]);
        output.append_genus(&genus);
        assert_eq!(output.to_string(), "(Kid, Pilot, Scoundrel, Princess)");

        output.reset();
    }

    #[test]
    fn signatures() {
        let mut output = Formatter::new(78);

        let sig = Signature {
            requires: Genus::Single(Forma::new("Alderaan")),
            provides: Genus::Single(Forma::new("AsteroidField")),
        };
        output.append_signature(&sig);
        assert_eq!(output.to_string(), "Alderaan -> AsteroidField");

        output.reset();
        let sig = Signature {
            requires: Genus::List(Forma::new("Clone")),
            provides: Genus::Single(Forma::new("Army")),
        };
        output.append_signature(&sig);
        assert_eq!(output.to_string(), "[Clone] -> Army");

        output.reset();
        let signature = Signature {
            requires: Genus::Single(Forma::new("TaxationOfTradeRoutes")),
            provides: Genus::Tuple(vec![Forma::new("Rebels"), Forma::new("Empire")]),
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
        assert!(
            !output
                .fragments
                .is_empty()
        );
        assert!(
            output
                .buffer
                .is_empty()
        );
        assert_eq!(output.current, Syntax::Neutral);
    }
}
