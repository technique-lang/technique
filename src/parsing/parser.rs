#![allow(dead_code)]

use regex::Regex;
use technique::language::*;

use super::scope::*;

pub fn parse_via_scopes(content: &str) {
    let mut input = Parser::new();
    input.initialize(content);

    let result = input.read_technique_header();
    println!("{:?}", result);
    println!("{:?}", input);

    std::process::exit(0);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ParsingError {
    IllegalParserState,
    Unimplemented,
    ZeroLengthToken,
    Unrecognized, // improve this
    Expected(&'static str),
    InvalidHeader,
    ValidationFailure(ValidationError),
    InvalidCharacter(char),
    UnexpectedEndOfInput,
    InvalidIdentifier,
    InvalidForma,
    InvalidGenus,
    InvalidSignature,
    InvalidDeclaration,
    InvalidInvocation,
    InvalidFunction,
    InvalidCodeBlock,
    InvalidStep,
}

impl From<ValidationError> for ParsingError {
    fn from(error: ValidationError) -> Self {
        ParsingError::ValidationFailure(error)
    }
}

#[derive(Debug)]
struct Parser<'i> {
    scope: Scope,
    source: &'i str,
    offset: usize,
    count: usize,
}

/// Wrap parse results with the width consumed.
#[derive(Debug, PartialEq)]
struct Parsed<'i>(&'i str);

impl<'i> Parser<'i> {
    fn new() -> Parser<'i> {
        Parser {
            scope: Scope::new(),
            source: "",
            offset: 0,
            count: 0,
        }
    }

    fn initialize(&mut self, content: &'i str) {
        self.scope = Scope::new();
        self.source = content;
        self.count = 0;
        self.offset = 0;
    }

    fn advance(&mut self, width: usize) {
        // advance the parser position
        self.source = &self.source[width..];
        self.offset += width;
    }

    fn parse_from_start(&mut self) -> Result<(), ParsingError> {
        let layer = self
            .scope
            .current();

        match layer {
            Layer::Technique => (), // this is where we should be
            _ => return Err(ParsingError::IllegalParserState),
        }

        let _header = self.read_technique_header()?;
        Ok(()) // FIXME
    }

    /// consume up to but not including newline (or end)
    fn take_line<A, F>(&mut self, f: F) -> Result<A, ParsingError>
    where
        F: Fn(&'i str) -> Result<A, ParsingError>,
    {
        match self
            .source
            .split_once('\n')
        {
            Some((before, after)) => {
                let result = f(before)?;

                self.source = after;
                self.offset += before.len() + 1;
                self.count += 1;
                Ok(result)
            }
            None => {
                let before = self.source;
                let result = f(before)?;

                self.source = "";
                self.offset += before.len() + 1;
                Ok(result)
            }
        }
    }

    fn entire(&self) -> &'i str {
        self.source
    }

    fn is_finished(&self) -> bool {
        self.source
            .is_empty()
    }

    fn take_block_lines<A, F, P1, P2>(
        &mut self,
        start_predicate: P1,
        end_predicate: P2,
        function: F,
    ) -> Result<A, ParsingError>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError>,
        P1: Fn(&str) -> bool,
        P2: Fn(&str) -> bool,
    {
        let mut i = 0;
        let mut begun = false;

        for line in self
            .source
            .lines()
        {
            if !begun && start_predicate(line) {
                begun = true;
                i += line.len();
                continue;
            } else if begun && end_predicate(line) {
                // don't include this line
                break;
            }

            i += line.len() + 1;
        }

        if i > self
            .source
            .len()
        {
            i -= 1;
        }

        // Extract the substring from start to the found position
        let block = &self.source[..i];

        let mut parser = self.subparser(0, block);

        // Pass to closure for processing
        let result = function(&mut parser)?;

        // Advance parser state
        self.source = &self.source[i..];
        self.offset += i;

        Ok(result)
    }

    fn take_block_chars<A, F>(
        &mut self,
        start_char: char,
        end_char: char,
        function: F,
    ) -> Result<A, ParsingError>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError>,
    {
        let mut l = 0;
        let mut begun = false;

        for (i, c) in self
            .source
            .char_indices()
        {
            if !begun && c == start_char {
                begun = true;
            } else if begun && c == end_char {
                l = i + 1; // add end character
                break;
            }
        }
        if !begun {
            return Err(ParsingError::Expected("the start character"));
        }
        if l == 0 {
            return Err(ParsingError::Expected("the end character"));
        }

        let block = &self.source[1..l - 1];

        let mut parser = self.subparser(1, block);

        // Pass to closure for processing
        let result = function(&mut parser)?;

        // Advance parser state
        self.source = &self.source[l..];
        self.offset += l;

        Ok(result)
    }

    fn take_block_delimited<A, F>(
        &mut self,
        delimiter: &str,
        function: F,
    ) -> Result<A, ParsingError>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError>,
    {
        let width = delimiter.len();

        // Find the start delimiter
        let start = self
            .source
            .find(delimiter)
            .ok_or(ParsingError::Expected("a starting delimiter"))?;

        // Look for the end delimiter after correcting for the starting one
        let start = start + width;
        let end = self.source[start..]
            .find(delimiter)
            .ok_or(ParsingError::Expected("the corresponding end delimiter"))?;

        // Correct actual positions in input
        let end = start + end;

        // Extract the content between delimiters
        let block = &self.source[start..end];

        let mut parser = self.subparser(start, block);

        // Pass to closure for processing
        let result = function(&mut parser)?;

        // Advance parser state past the entire delimited block
        let end = end + width;
        self.source = &self.source[end..];
        self.offset += end;

        Ok(result)
    }

    fn take_until<A, F>(&mut self, pattern: &[char], function: F) -> Result<A, ParsingError>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError>,
    {
        let content = self.source;
        let end_pos = content
            .find(pattern)
            .unwrap_or(content.len());

        let block = &content[..end_pos];
        let mut parser = self.subparser(0, block);

        // Pass to closure for processing
        let result = function(&mut parser)?;

        // Advance parser state
        self.source = &self.source[end_pos..];
        self.offset += end_pos;

        Ok(result)
    }

    fn take_split_by<A, F>(&mut self, delimiter: char, function: F) -> Result<Vec<A>, ParsingError>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError>,
    {
        let content = self.entire();
        let mut results = Vec::new();

        for chunk in content.split(delimiter) {
            let trimmed = chunk.trim();
            if trimmed.is_empty() {
                return Err(ParsingError::Expected(
                    "non-empty content between delimiters",
                ));
            }
            let mut parser = self.subparser(0, trimmed);
            results.push(function(&mut parser)?);
        }

        // Advance parser past all consumed content
        self.advance(content.len());

        Ok(results)
    }

    fn peek_next_char(&self) -> Option<char> {
        self.source
            .chars()
            .next()
    }

    /// Given a string, fork a copy of the parser state and run a nested
    /// parser on that string. Does NOT advance the parent's parser state;
    /// the caller needs to do that via one of the take_*() methods.
    fn subparser(&self, indent: usize, content: &'i str) -> Parser<'i> {
        let parser = Parser {
            scope: self
                .scope
                .clone(),
            source: content,
            count: self.count,
            offset: indent + self.offset,
        };

        // and return
        parser
    }

    fn read_newline(&mut self) -> Result<(), ParsingError> {
        for (i, c) in self
            .source
            .char_indices()
        {
            let l = i + 1;

            if c == '\n' {
                self.source = &self.source[l..];
                self.count += 1;
                self.offset += l;
                return Ok(());
            } else if c.is_ascii_whitespace() {
                continue;
            } else {
                return Err(ParsingError::InvalidCharacter(c));
            }
        }

        // We don't actually require a newline to end the file.

        self.source = "";
        self.offset += self
            .source
            .len();
        Ok(())
    }

    fn read_technique_header(&mut self) -> Result<Metadata<'i>, ParsingError> {
        // Process magic line
        let version = self.take_line(|content| {
            if is_magic_line(content) {
                Ok(parse_magic_line(content)?)
            } else {
                Err(ParsingError::Expected("The % symbol"))
            }
        })?;

        // Process SPDX line
        let (license, copyright) = self.take_line(|content| {
            if is_spdx_line(content) {
                Ok(parse_spdx_line(content)?)
            } else {
                Ok((None, None))
            }
        })?;

        // Process template line
        let template = self.take_line(|content| {
            if is_template_line(content) {
                Ok(parse_template_line(content)?)
            } else {
                Ok(None)
            }
        })?;

        Ok(Metadata {
            version,
            license,
            copyright,
            template,
        })
    }

    fn read_procedure_title(&mut self) -> Result<&'i str, ParsingError> {
        self.trim_whitespace();
        if self.peek_next_char() == Some('#') {
            let title = self.take_line(|content| Ok(content[1..].trim()))?;
            Ok(title)
        } else {
            // we shouldn't have invoked this unless we have a title to parse!
            Err(ParsingError::IllegalParserState)
        }
    }

    fn read_procedure(&mut self) -> Result<Procedure<'i>, ParsingError> {
        let procedure = self.take_block_lines(
            is_procedure_declaration,
            is_procedure_declaration,
            |outer| {
                // Extract the declaration, and parse it
                let declaration = outer.take_block_lines(
                    is_procedure_declaration,
                    |line| !is_procedure_declaration(line),
                    |inner| {
                        let content = inner.entire();
                        Ok(parse_procedure_declaration(content)?)
                    },
                )?;
                outer.read_newline()?;

                // Extract content after declaration until a step is encountered
                let (title, description) = outer.take_block_lines(
                    |_| true,
                    |line| is_step(line),
                    |inner| {
                        let mut title = None;
                        let mut description = vec![];

                        let content = inner.entire();
                        if is_procedure_title(content) {
                            let text = inner.read_procedure_title()?;
                            inner.read_newline()?;
                            title = Some(text);
                        }

                        let content = inner.entire();
                        if !content.is_empty() {
                            description = inner.read_descriptive_content()?;
                        }

                        Ok((title, description))
                    },
                )?;

                // Parse remaining content as steps
                let mut steps = vec![];
                while !outer.is_finished() {
                    let content = outer.entire();
                    if is_step(content) {
                        let step = outer.read_step()?;
                        steps.push(step);
                    } else {
                        return Err(ParsingError::Unrecognized);
                    }
                }

                Ok(Procedure {
                    name: declaration.0,
                    signature: declaration.1,
                    title,
                    description,
                    attribute: vec![], // TODO: parse attributes
                    steps,
                })
            },
        )?;

        Ok(procedure)
    }

    fn read_code_block(&mut self) -> Result<Expression<'i>, ParsingError> {
        let expression = self.take_block_chars('{', '}', |outer| {
            outer.trim_whitespace();
            let content = outer.entire();

            if content.starts_with("repeat") {
                // TODO: implement outer.read_repeat_expression()
                Err(ParsingError::Unimplemented)
            } else if content.starts_with("foreach") {
                // TODO: implement outer.read_foreach_expression()
                Err(ParsingError::Unimplemented)
            } else if is_invocation(content) {
                let invocation = outer.read_invocation()?;
                Ok(Expression::Application(invocation))
            } else if is_function(content) {
                let target = outer.read_identifier()?;
                let parameters = outer.read_parameters()?;

                let function = Function { target, parameters };
                Ok(Expression::Execution(function))
            } else {
                let identifier = outer.read_identifier()?;
                Ok(Expression::Value(identifier))
            }
        })?;

        Ok(expression)
    }

    /// Consume an identifier. As with the other smaller read methods, we do a
    /// general scan of the range here to get the relevant, then call the more
    /// detailed validation function to actually determine if it's a match.
    fn read_identifier(&mut self) -> Result<Identifier<'i>, ParsingError> {
        self.trim_whitespace();

        let content = self.entire();

        let possible = match content.find([' ', '\t', '\n', '(', '{', ',']) {
            None => content,
            Some(i) => &content[0..i],
        };

        let identifier = validate_identifier(possible)?;

        self.advance(possible.len());

        Ok(identifier)
    }

    /// Parse a target like <procedure_name> or <https://example.com/proc>
    fn read_target(&mut self) -> Result<Target<'i>, ParsingError> {
        self.take_block_chars('<', '>', |inner| {
            let content = inner.entire();
            if content.starts_with("https://") {
                Ok(Target::Remote(External(content)))
            } else {
                let identifier = inner.read_identifier()?;
                Ok(Target::Local(identifier))
            }
        })
    }

    /// Parse a complete invocation like <procedure>(params)
    fn read_invocation(&mut self) -> Result<Invocation<'i>, ParsingError> {
        let target = self.read_target()?;
        let parameters = if self.peek_next_char() == Some('(') {
            Some(self.read_parameters()?)
        } else {
            None
        };
        Ok(Invocation { target, parameters })
    }

    /// Parse a step (main steps are always dependent, substeps can be dependent or parallel)
    fn read_step(&mut self) -> Result<Step<'i>, ParsingError> {
        self.take_block_lines(is_step, is_step, |outer| {
            outer.trim_whitespace();
            let content = outer.entire();

            if content.is_empty() {
                // FIXME do we even need this check?
                return Err(ParsingError::ZeroLengthToken);
            }

            // Parse ordinal

            let re = Regex::new(r"^\s*(\d+)\.\s+").unwrap();
            let cap = re
                .captures(content)
                .ok_or(ParsingError::InvalidStep)?;

            let number = cap
                .get(1)
                .ok_or(ParsingError::Expected("the ordinal Step number"))?
                .as_str();

            let l = cap
                .get(0)
                .unwrap()
                .len();

            outer.advance(l);

            let text = outer.read_descriptive_content()?;

            // Parse substeps if present. They're either a set of dependent
            // substeps or parallel substeps (but not a mix!).

            let mut substeps = vec![];

            // Only check for substeps if there's remaining content
            if !outer.is_finished() {
                let content = outer.entire();

                if is_substep_dependent(content) {
                    loop {
                        outer.trim_whitespace();
                        if outer.is_finished() {
                            break;
                        }
                        let substep = outer.read_substep_dependent()?;
                        substeps.push(substep);
                    }
                } else if is_substep_parallel(content) {
                    loop {
                        outer.trim_whitespace();
                        if outer.is_finished() {
                            break;
                        }
                        let substep = outer.read_substep_parallel()?;
                        substeps.push(substep);
                    }
                } else {
                    return Err(ParsingError::IllegalParserState);
                }
            }

            // TODO: Parse attributes
            let attributes = vec![];

            return Ok(Step::Dependent {
                ordinal: number,
                content: text,
                attribute: attributes,
                substeps,
            });
        })
    }

    /// Parse a dependent substep (a., b., c., etc.)
    fn read_substep_dependent(&mut self) -> Result<Step<'i>, ParsingError> {
        self.take_block_lines(is_substep_dependent, is_substep_dependent, |outer| {
            let content = outer.entire();
            let re = Regex::new(r"^\s*([a-hj-uw-z])\.\s+").unwrap();
            let cap = re
                .captures(content)
                .ok_or(ParsingError::InvalidStep)?;

            let letter = cap
                .get(1)
                .ok_or(ParsingError::Expected("the ordinal Sub-Step letter"))?
                .as_str();

            // Skip past the letter, dot, and space
            let l = cap
                .get(0)
                .unwrap()
                .len();

            outer.advance(l);

            // Parse the remaining content
            let text = outer.read_descriptive_content()?;

            // Parse nested sub-sub-steps if present.
            let mut substeps = vec![];
            loop {
                outer.trim_whitespace();
                let content = outer.entire();

                if content.is_empty() {
                    break;
                }

                if is_subsubstep_dependent(content) {
                    substeps = vec![]; // TODO
                } else {
                    break;
                }
            }

            // TODO: Parse attributes
            let attributes = vec![];

            Ok(Step::Dependent {
                ordinal: letter,
                content: text,
                attribute: attributes,
                substeps,
            })
        })
    }

    /// Parse a parallel substep (-)
    fn read_substep_parallel(&mut self) -> Result<Step<'i>, ParsingError> {
        self.take_block_lines(is_substep_dependent, is_substep_dependent, |outer| {
            let content = outer.entire();
            let re = Regex::new(r"^\s*-\s+").unwrap();
            let cap = re
                .captures(content)
                .ok_or(ParsingError::InvalidStep)?;

            let letter = cap
                .get(1)
                .ok_or(ParsingError::Expected("the ordinal Sub-Step letter"))?
                .as_str();

            // Skip past the letter, dot, and space
            let l = cap
                .get(0)
                .unwrap()
                .len();

            outer.advance(l);

            // Parse the remaining content
            let text = outer.read_descriptive_content()?;

            // Parse nested sub-sub-steps if present.
            let mut substeps = vec![];
            loop {
                outer.trim_whitespace();
                let content = outer.entire();

                if content.is_empty() {
                    break;
                }

                if is_subsubstep_dependent(content) {
                    substeps = vec![]; // TODO
                } else {
                    break;
                }
            }

            // TODO: Parse attributes
            let attributes = vec![];

            Ok(Step::Dependent {
                ordinal: letter,
                content: text,
                attribute: attributes,
                substeps,
            })
        })
    }

    /// Parse descriptive content within a step
    fn read_descriptive_content(&mut self) -> Result<Vec<Descriptive<'i>>, ParsingError> {
        let mut results = vec![];

        while let Some(ch) = self.peek_next_char() {
            self.trim_whitespace();
            if self
                .entire()
                .is_empty()
            {
                break;
            }

            if ch == '{' {
                let expression = self.read_code_block()?;
                results.push(Descriptive::CodeBlock(expression));
            } else if ch == '<' {
                let invocation = self.read_invocation()?;
                if self.peek_next_char() == Some('~') {
                    self.advance(1); // consume '~'
                    self.trim_whitespace();
                    let variable = self.read_identifier()?;
                    results.push(Descriptive::Binding(invocation, variable));
                } else {
                    results.push(Descriptive::Application(invocation));
                }
            } else if ch == '\'' {
                let responses = self.read_responses()?;
                results.push(Descriptive::Responses(responses));
            } else {
                // Parse regular text until we hit special characters
                let text = self.take_until(&['{', '<', '\''], |inner| {
                    Ok(inner
                        .entire()
                        .trim())
                })?;
                if !text.is_empty() {
                    results.push(Descriptive::Text(text));
                }
            }
        }

        Ok(results)
    }

    /// Parse enum responses like 'Yes' | 'No' | 'Not Applicable'
    fn read_responses(&mut self) -> Result<Vec<Response<'i>>, ParsingError> {
        self.take_split_by('|', |inner| {
            let content = inner.entire();
            Ok(validate_response(content)?)
        })
    }

    /// Consume parameters to an invocation or function. Specifically, look
    /// for the form
    ///
    /// ( one, 2, "three", ```bash echo "four"``` )
    ///
    /// and return a Vec with an Expression for each parameter in the list. Most however,
    /// will either be
    ///
    /// ( a, b, c )
    ///
    /// or
    ///
    /// ( ```lang some content``` )
    ///
    fn read_parameters(&mut self) -> Result<Vec<Expression<'i>>, ParsingError> {
        self.take_block_chars('(', ')', |outer| {
            let mut params = Vec::new();

            loop {
                outer.trim_whitespace();

                if outer
                    .entire()
                    .is_empty()
                {
                    break;
                }

                let content = outer.entire();

                if content.starts_with("```") {
                    let raw = outer.take_block_delimited("```", |inner| Ok(inner.entire()))?;
                    params.push(Expression::Multiline(raw));
                } else if content.starts_with("\"") {
                    let raw = outer.take_block_chars('"', '"', |inner| Ok(inner.entire()))?;
                    params.push(Expression::String(raw));
                } else {
                    let name = outer.read_identifier()?;
                    params.push(Expression::Value(name));
                }

                // Handle comma separation
                outer.trim_whitespace();
                if outer
                    .entire()
                    .starts_with(',')
                {
                    outer.advance(1);
                } else {
                    break;
                }
            }

            Ok(params)
        })
    }

    fn ensure_nonempty(&mut self) -> Result<(), ParsingError> {
        if self
            .source
            .len()
            == 0
        {
            return Err(ParsingError::UnexpectedEndOfInput);
        }
        Ok(())
    }

    /// Trim any leading whitespace (space, tab, newline) from the front of
    /// the current parser text.
    fn trim_whitespace(&mut self) {
        let mut l = 0;
        let mut n = 0;

        if self
            .source
            .is_empty()
        {
            return;
        }

        for c in self
            .source
            .chars()
        {
            if c == '\n' {
                n += 1;
                l += 1;
                continue;
            } else if c.is_ascii_whitespace() {
                l += 1;
                continue;
            } else {
                break;
            }
        }

        self.source = &self.source[l..];
        self.count += n;
        self.offset += l;
    }
}

fn is_magic_line(content: &str) -> bool {
    let re = Regex::new(r"%\s*technique").unwrap();

    re.is_match(content)
}

// hard wire the version for now. If we ever grow to supporting multiple major
// versions then this will be a lot more complicated than just dealing with a
// different natural number here.
fn parse_magic_line(content: &str) -> Result<u8, ParsingError> {
    let re = Regex::new(r"%\s*technique\s+v1").unwrap();

    if re.is_match(content) {
        Ok(1)
    } else {
        Err(ParsingError::InvalidHeader)
    }
}

fn is_spdx_line(content: &str) -> bool {
    let re = Regex::new(r"!\s*[^;]+(?:;\s*.+)?").unwrap();

    re.is_match(content)
}

// This one is awkward because if a SPDX line is present, then it really needs
// to have a license, whereas the copyright part is optional.
fn parse_spdx_line(content: &str) -> Result<(Option<&str>, Option<&str>), ParsingError> {
    let re = Regex::new(r"^!\s*([^;]+)(?:;\s*(?:\(c\)|\(C\)|©)\s*(.+))?$").unwrap();

    let cap = re
        .captures(content)
        .ok_or(ParsingError::InvalidHeader)?;

    // Now to extracting the values we need. We get the license code from
    // the first capture. It must be present otherwise we don't have a
    // valid SPDX line (and we declared that we're on an SPDX line by the
    // presence of the '!' character at the beginning of the line).

    let one = cap
        .get(1)
        .ok_or(ParsingError::Expected("the license name"))?;

    let result = validate_license(one.as_str())?;
    let license = Some(result);

    // Now dig out the copyright, if present:

    let copyright = match cap.get(2) {
        Some(two) => {
            let result = validate_copyright(two.as_str())?;
            Some(result)
        }
        None => None,
    };

    Ok((license, copyright))
}

fn is_template_line(content: &str) -> bool {
    let re = Regex::new(r"&\s*.+").unwrap();

    re.is_match(content)
}

fn parse_template_line(content: &str) -> Result<Option<&str>, ParsingError> {
    let re = Regex::new(r"^&\s*(.+)$").unwrap();

    let cap = re
        .captures(content)
        .ok_or(ParsingError::InvalidHeader)?;

    let one = cap
        .get(1)
        .ok_or(ParsingError::Expected("a template name"))?;

    let result = validate_template(one.as_str())?;

    Ok(Some(result))
}

fn parse_identifier(content: &str) -> Result<Identifier, ParsingError> {
    let result = validate_identifier(content)?;
    Ok(result)
}

fn parse_forma(content: &str) -> Result<Forma, ParsingError> {
    let result = validate_forma(content)?;
    Ok(result)
}

fn parse_genus(content: &str) -> Result<Genus, ParsingError> {
    let result = validate_genus(content)?;
    Ok(result)
}

/// A signature is of the form
///
/// genus -> genus
///
/// terminated by an end of line.

fn is_signature(content: &str) -> bool {
    let re = Regex::new(r"\s*.+?\s*->\s*.+?\s*$").unwrap();

    re.is_match(content)
}

fn parse_signature(content: &str) -> Result<Signature, ParsingError> {
    let re = Regex::new(r"\s*(.+?)\s*->\s*(.+?)\s*$").unwrap();

    let cap = match re.captures(content) {
        Some(c) => c,
        None => return Err(ParsingError::InvalidSignature),
    };

    let one = cap
        .get(1)
        .ok_or(ParsingError::Expected("a Genus for the domain"))?;

    let two = cap
        .get(2)
        .ok_or(ParsingError::Expected("a Genus for the range"))?;

    let domain = validate_genus(one.as_str())?;
    let range = validate_genus(two.as_str())?;

    Ok(Signature { domain, range })
}

/// declarations are of the form
///
///     identifier : signature
///
/// where the optional signature is
///
///     genus -> genus
///
/// as above.

fn is_procedure_declaration(content: &str) -> bool {
    let re = Regex::new(r"^\s*(?:.+?)\s*:\s*(?:.+?)?\s*$").unwrap();

    re.is_match(content)
}

fn parse_procedure_declaration(
    content: &str,
) -> Result<(Identifier, Option<Signature>), ParsingError> {
    // These capture groups use .+? to make "match more than one, but
    // lazily" so that the subsequent grabs of whitespace and the all
    // important ':' character are not absorbed.
    let re = Regex::new(r"^\s*(.+?)\s*:\s*(.+?)?\s*$").unwrap();

    let cap = re
        .captures(content)
        .ok_or(ParsingError::InvalidDeclaration)?;

    let one = cap
        .get(1)
        .ok_or(ParsingError::Expected(
            "an Identifier for the procedure declaration",
        ))?;

    let name = validate_identifier(one.as_str())?;

    let signature = match cap.get(2) {
        Some(two) => {
            let result = parse_signature(two.as_str())?;
            Some(result)
        }
        None => None,
    };

    Ok((name, signature))
}

fn is_procedure_title(content: &str) -> bool {
    content
        .trim_start()
        .starts_with('#')
}

// I'm not sure about anchoring this one on start and end, seeing as how it
// will be used when scanning.
fn is_invocation(content: &str) -> bool {
    let re = Regex::new(r"^\s*(<.+?>\s*(?:\(.*?\))?)\s*$").unwrap();

    re.is_match(content)
}

fn is_code_block(content: &str) -> bool {
    let re = Regex::new(r"\s*{.*?}").unwrap();

    re.is_match(content)
}

fn is_function(content: &str) -> bool {
    let re = Regex::new(r"^\s*.+?\(").unwrap();

    re.is_match(content)
}

fn is_step(input: &str) -> bool {
    let re = Regex::new(r"^\s*\d+\.\s+").unwrap();
    re.is_match(input)
}

/// Recognize
///
///    a. First
///    b. Second
///    c. Third
///
/// as sub-steps. This discriminator excludes the characters that would be
/// used to compose a number below 40 in roman numerals, as those are
/// sub-sub-steps.
fn is_substep_dependent(input: &str) -> bool {
    let re = Regex::new(r"^\s*[a-hj-uw-z]\.\s+").unwrap();
    re.is_match(input)
}

fn is_substep_parallel(input: &str) -> bool {
    let re = Regex::new(r"^\s*-\s+").unwrap();
    re.is_match(input)
}

fn is_subsubstep_dependent(input: &str) -> bool {
    let re = Regex::new(r"^\s*[ivx]+\.\s+").unwrap();
    re.is_match(input)
}

fn is_role_assignment(input: &str) -> bool {
    let re = Regex::new(r"^\s*@[a-z][a-z0-9_]*").unwrap();
    re.is_match(input)
}

fn is_enum_response(input: &str) -> bool {
    let re = Regex::new(r"^\s*'.+?'").unwrap();
    re.is_match(input)
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn magic_line() {
        let content = "% technique v1";
        assert!(is_magic_line(content));

        let result = parse_magic_line(content);
        assert_eq!(result, Ok(1));

        let content = "%technique v1";
        assert!(is_magic_line(content));

        let result = parse_magic_line(content);
        assert_eq!(result, Ok(1));

        let content = "%techniquev1";
        assert!(is_magic_line(content));

        // this is rejected because the technique keyword isn't present.
        let result = parse_magic_line(content);
        assert!(result.is_err());
    }

    #[test]
    fn header_spdx() {
        let content = "! PD";
        assert!(is_spdx_line(content));

        let result = parse_spdx_line(content);
        assert_eq!(result, Ok((Some("PD"), None)));

        let content = "! MIT; (c) ACME, Inc.";
        assert!(is_spdx_line(content));

        let result = parse_spdx_line(content);
        assert_eq!(result, Ok((Some("MIT"), Some("ACME, Inc."))));

        let content = "! MIT; (C) 2024 ACME, Inc.";
        assert!(is_spdx_line(content));

        let result = parse_spdx_line(content);
        assert_eq!(result, Ok((Some("MIT"), Some("2024 ACME, Inc."))));

        let content = "! CC BY-SA 3.0 [IGO]; (c) 2024 ACME, Inc.";
        assert!(is_spdx_line(content));

        let result = parse_spdx_line(content);
        assert_eq!(
            result,
            Ok((Some("CC BY-SA 3.0 [IGO]"), Some("2024 ACME, Inc.")))
        );
    }

    #[test]
    fn header_template() {
        let content = "& checklist";
        assert!(is_template_line(content));

        let result = parse_template_line(content);
        assert_eq!(result, Ok(Some("checklist")));

        let content = "& nasa-flight-plan,v4.0";
        assert!(is_template_line(content));

        let result = parse_template_line(content);
        assert_eq!(result, Ok(Some("nasa-flight-plan,v4.0")));
    }

    // now we test incremental parsing

    #[test]
    fn check_not_eof() {
        let mut input = Parser::new();
        input.initialize("Hello World");
        assert_eq!(input.ensure_nonempty(), Ok(()));

        input.initialize("");
        assert_eq!(
            input.ensure_nonempty(),
            Err(ParsingError::UnexpectedEndOfInput)
        );
    }

    #[test]
    fn consume_whitespace() {
        let mut input = Parser::new();
        input.initialize("  hello");
        input.trim_whitespace();
        assert_eq!(input.source, "hello");

        input.initialize("\n \nthere");
        input.trim_whitespace();
        assert_eq!(input.source, "there");
        assert_eq!(input.count, 2);
        assert_eq!(input.offset, 3);
    }

    // It is not clear that we will ever actually need parse_identifier(),
    // parse_forma(), parse_genus(), or parse_signature() as they are not
    // called directly, but even though they are not used in composition of
    // the parse_procedure_declaration() parser, it is highly likely that
    // someday we will need to be able to parse them individually, perhaps for
    // a future language server or code highlighter. So we test them properly
    // here; in any event it exercises the underlying validate_*() codepaths.

    #[test]
    fn identifier_rules() {
        let input = "p";
        let result = parse_identifier(input);
        assert_eq!(result, Ok(Identifier("p")));

        let input = "pizza";
        let result = parse_identifier(input);
        assert_eq!(result, Ok(Identifier("pizza")));

        let input = "pizza0";
        let result = parse_identifier(input);
        assert_eq!(result, Ok(Identifier("pizza0")));

        let input = "0pizza";
        let result = parse_forma(input);
        assert!(result.is_err());

        let input = "cook_pizza";
        let result = parse_identifier(input);
        assert_eq!(result, Ok(Identifier("cook_pizza")));

        let input = "cook-pizza";
        let result = parse_forma(input);
        assert!(result.is_err());
    }

    #[test]
    fn forma_rules() {
        let input = "A";
        let result = parse_forma(input);
        assert_eq!(result, Ok(Forma("A")));

        let input = "Apple";
        let result = parse_forma(input);
        assert_eq!(result, Ok(Forma("Apple")));

        let input = "apple";
        let result = parse_forma(input);
        assert_eq!(
            result,
            Err(ParsingError::ValidationFailure(
                ValidationError::InvalidForma
            ))
        );
    }

    #[test]
    fn single_genus_definitions() {
        let input = "A";
        let result = parse_genus(input);
        assert_eq!(result, Ok(Genus::Single(Forma("A"))));

        let input = "Apple";
        let result = parse_genus(input);
        assert_eq!(result, Ok(Genus::Single(Forma("Apple"))));
    }

    #[test]
    fn list_genus_definitions() {
        let input = "[A]";
        let result = parse_genus(input);
        assert_eq!(result, Ok(Genus::List(Forma("A"))))
    }

    #[test]
    fn tuple_genus_definitions() {
        let input = "(A, B)";
        let result = parse_genus(input);
        assert_eq!(result, Ok(Genus::Tuple(vec![Forma("A"), Forma("B")])));

        // not actually sure whether we should be normalizing this? Probably
        // not, because formatting and linting is a separate concern.
        let input = "(A)";
        let result = parse_genus(input);
        assert_eq!(result, Ok(Genus::Tuple(vec![Forma("A")])));
    }

    #[test]
    fn unit_genus_definitions() {
        // and now the special case of the unit type
        let input = "()";
        let result = parse_genus(input);
        assert_eq!(result, Ok(Genus::Unit));
    }

    #[test]
    fn signatures() {
        let input = "A -> B";
        let result = parse_signature(input);
        assert_eq!(
            result,
            Ok(Signature {
                domain: Genus::Single(Forma("A")),
                range: Genus::Single(Forma("B"))
            })
        );

        let input = "Beans -> Coffee";
        let result = parse_signature(input);
        assert_eq!(
            result,
            Ok(Signature {
                domain: Genus::Single(Forma("Beans")),
                range: Genus::Single(Forma("Coffee"))
            })
        );

        let input = "[Bits] -> Bob";
        let result = parse_signature(input);
        assert_eq!(
            result,
            Ok(Signature {
                domain: Genus::List(Forma("Bits")),
                range: Genus::Single(Forma("Bob"))
            })
        );

        let input = "Complex -> (Real, Imaginary)";
        let result = parse_signature(input);
        assert_eq!(
            result,
            Ok(Signature {
                domain: Genus::Single(Forma("Complex")),
                range: Genus::Tuple(vec![Forma("Real"), Forma("Imaginary")])
            })
        );
    }

    #[test]
    fn declaration_simple() {
        let content = "making_coffee :";

        assert!(is_procedure_declaration(content));

        let result = parse_procedure_declaration(content);
        assert_eq!(result, Ok((Identifier("making_coffee"), None)));
    }

    #[test]
    fn declaration_full() {
        let content = "f : A -> B";
        assert!(is_procedure_declaration(content));

        let result = parse_procedure_declaration(content);
        assert_eq!(
            result,
            Ok((
                Identifier("f"),
                Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                })
            ))
        );

        let content = "making_coffee : (Beans, Milk) -> [Coffee]";
        assert!(is_procedure_declaration(content));

        let result = parse_procedure_declaration(content);
        assert_eq!(
            result,
            Ok((
                Identifier("making_coffee"),
                Some(Signature {
                    domain: Genus::Tuple(vec![Forma("Beans"), Forma("Milk")]),
                    range: Genus::List(Forma("Coffee"))
                })
            ))
        );

        let content = "f : B";
        // it should detect a procedure is being declared
        assert!(is_procedure_declaration(content));

        // but it is invalid
        let result = parse_procedure_declaration(content);
        assert!(result.is_err());
    }

    #[test]
    fn character_delimited_blocks() {
        let mut input = Parser::new();
        input.initialize("{ todo() }");

        let result = input.take_block_chars('{', '}', |parser| {
            let text = parser.entire();
            assert_eq!(text, " todo() ");
            Ok(true)
        });
        assert_eq!(result, Ok(true));

        // this is somewhat contrived as we would not be using this to parse
        // strings (We will need to preserve whitespace inside strings when
        // we find ourselves parsing them, so subparser() won't work.
        input.initialize("XhelloX world");

        let result = input.take_block_chars('X', 'X', |parser| {
            let text = parser.entire();
            assert_eq!(text, "hello");
            Ok(true)
        });
        assert_eq!(result, Ok(true));
    }

    #[test]
    fn string_delimited_blocks() {
        let mut input = Parser::new();
        input.initialize("```bash\nls -l\necho hello```");
        assert_eq!(input.offset, 0);

        let result = input.take_block_delimited("```", |parser| {
            let text = parser.entire();
            assert_eq!(text, "bash\nls -l\necho hello");
            Ok(true)
        });
        assert_eq!(result, Ok(true));
        assert_eq!(input.source, "");
        assert_eq!(input.offset, 27);

        // Test with different delimiter
        input.initialize("---start\ncontent here\nmore content---end");

        let result = input.take_block_delimited("---", |parser| {
            let text = parser.entire();
            assert_eq!(text, "start\ncontent here\nmore content");
            Ok(true)
        });
        assert_eq!(result, Ok(true));

        // Test with whitespace around delimiters
        input.initialize("```  hello world  ``` and now goodbye");

        let result = input.take_block_delimited("```", |parser| {
            let text = parser.entire();
            assert_eq!(text, "  hello world  ");
            Ok(true)
        });
        assert_eq!(result, Ok(true));
        assert_eq!(input.source, " and now goodbye");
        assert_eq!(input.offset, 21);
    }

    #[test]
    fn taking_until() {
        let mut input = Parser::new();

        // Test take_until() with an identifier up to a limiting character
        input.initialize("hello,world");
        let result = input.take_until(&[','], |inner| inner.read_identifier());
        assert_eq!(result, Ok(Identifier("hello")));
        assert_eq!(input.source, ",world");

        // Test take_until() with whitespace delimiters
        input.initialize("test \t\nmore");
        let result = input.take_until(&[' ', '\t', '\n'], |inner| inner.read_identifier());
        assert_eq!(result, Ok(Identifier("test")));
        assert_eq!(input.source, " \t\nmore");

        // Test take_until() when no delimiter found (it should take everything)
        input.initialize("onlytext");
        let result = input.take_until(&[',', ';'], |inner| inner.read_identifier());
        assert_eq!(result, Ok(Identifier("onlytext")));
        assert_eq!(input.source, "");
    }

    #[test]
    fn reading_invocations() {
        let mut input = Parser::new();

        // Test simple invocation without parameters
        input.initialize("<hello>");
        let result = input.read_invocation();
        assert_eq!(
            result,
            Ok(Invocation {
                target: Target::Local(Identifier("hello")),
                parameters: None
            })
        );

        // Test invocation with empty parameters
        input.initialize("<hello_world>()");
        let result = input.read_invocation();
        assert_eq!(
            result,
            Ok(Invocation {
                target: Target::Local(Identifier("hello_world")),
                parameters: Some(vec![])
            })
        );

        // Test invocation with multiple parameters
        input.initialize("<greetings>(name, title, occupation)");
        let result = input.read_invocation();
        assert_eq!(
            result,
            Ok(Invocation {
                target: Target::Local(Identifier("greetings")),
                parameters: Some(vec![
                    Expression::Value(Identifier("name")),
                    Expression::Value(Identifier("title")),
                    Expression::Value(Identifier("occupation"))
                ])
            })
        );

        // We don't have real support for this yet, but syntactically we will
        // support the idea of invoking a procedure at an external URL, so we
        // have this case as a placeholder.
        input.initialize("<https://example.com/proc>");
        let result = input.read_invocation();
        assert_eq!(
            result,
            Ok(Invocation {
                target: Target::Remote(External("https://example.com/proc")),
                parameters: None
            })
        );
    }

    #[test]
    fn read_steps() {
        let mut input = Parser::new();

        // Test simple dependent step
        input.initialize("1. First step");
        let result = input.read_step();
        assert_eq!(
            result,
            Ok(Step::Dependent {
                ordinal: "1",
                content: vec![Descriptive::Text("First step")],
                attribute: vec![],
                substeps: vec![],
            })
        );

        // Test simple parallel step
        input.initialize("- Parallel task");
        let result = input.read_step();
        assert_eq!(
            result,
            Ok(Step::Parallel {
                content: vec![Descriptive::Text("Parallel task")],
                attribute: vec![],
                substeps: vec![],
            })
        );

        // Test multi-line dependent step
        input.initialize("2. Check system status\nand verify connectivity");
        let result = input.read_step();
        assert_eq!(
            result,
            Ok(Step::Dependent {
                ordinal: "2",
                content: vec![Descriptive::Text(
                    "Check system status\nand verify connectivity"
                )],
                attribute: vec![],
                substeps: vec![],
            })
        );

        // Test invalid step
        input.initialize("Not a step");
        let result = input.read_step();
        assert_eq!(result, Err(ParsingError::InvalidStep));
    }

    #[test]
    fn step_detection() {
        // Test main dependent steps (whitespace agnostic)
        assert!(is_step("1. First step"));
        assert!(is_step("  1. Indented step"));
        assert!(is_step("10. Tenth step"));
        assert!(!is_step("a. Letter step"));
        assert!(!is_step("1.No space"));

        // Test dependent substeps (whitespace agnostic)
        assert!(is_substep_dependent("a. Substep"));
        assert!(is_substep_dependent("  a. Indented substep"));
        assert!(!is_substep_dependent("2. Substep can't have number"));
        assert!(!is_substep_dependent("   1. Even if it is indented"));

        // Test parallel substeps (whitespace agnostic, no main parallel steps)
        assert!(is_substep_parallel("- Parallel substep"));
        assert!(is_substep_parallel("  - Indented parallel"));
        assert!(is_substep_parallel("    - Deeper indented"));
        assert!(!is_substep_parallel("-No space")); // it's possible we may allow this in the future
        assert!(!is_substep_parallel("* Different bullet"));

        // Test recognition of sub-sub-steps
        assert!(!is_subsubstep_dependent("i. One"));
        assert!(!is_subsubstep_dependent(" ii. Two"));
        assert!(!is_subsubstep_dependent("v. Five"));
        assert!(!is_subsubstep_dependent("vi. Six"));
        assert!(!is_subsubstep_dependent("ix. Nine"));
        assert!(!is_subsubstep_dependent("x. Ten"));
        assert!(!is_subsubstep_dependent("xi. Eleven"));
        assert!(!is_subsubstep_dependent("xxxix. Thirty-nine"));

        // Test role assignments
        assert!(is_role_assignment("@surgeon"));
        assert!(is_role_assignment("  @nursing_team"));
        assert!(!is_role_assignment("surgeon"));
        assert!(!is_role_assignment("@123invalid"));

        // Test enum responses
        assert!(is_enum_response("'Yes'"));
        assert!(is_enum_response("  'No'"));
        assert!(is_enum_response("'Not Applicable'"));
        assert!(!is_enum_response("Yes"));
        assert!(!is_enum_response("'unclosed"));
    }

    #[test]
    fn code_blocks() {
        let mut input = Parser::new();

        // Test simple identifier in code block
        input.initialize("{ count }");
        let result = input.read_code_block();
        assert_eq!(result, Ok(Expression::Value(Identifier("count"))));

        // Test function with simple parameter
        input.initialize("{ sum(count) }");
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Execution(Function {
                target: Identifier("sum"),
                parameters: vec![Expression::Value(Identifier("count"))]
            }))
        );

        // Test function with multiple parameters
        input.initialize("{ consume(apple, banana, chocolate) }");
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Execution(Function {
                target: Identifier("consume"),
                parameters: vec![
                    Expression::Value(Identifier("apple")),
                    Expression::Value(Identifier("banana")),
                    Expression::Value(Identifier("chocolate"))
                ]
            }))
        );

        // Test function with text parameter
        input.initialize("{ exec(\"Hello, World\") }");
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Execution(Function {
                target: Identifier("exec"),
                parameters: vec![Expression::String("Hello, World")]
            }))
        );

        // Test function with multiline string parameter
        input.initialize("{ exec(```bash\nls -l\necho \"Done\"```) }");
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Execution(Function {
                target: Identifier("exec"),
                parameters: vec![Expression::Multiline("bash\nls -l\necho \"Done\"")]
            }))
        );
    }

    #[test]
    fn reading_identifiers() {
        let mut input = Parser::new();

        // Parse a basic identifier
        input.initialize("hello");
        let result = input.read_identifier();
        assert_eq!(result, Ok(Identifier("hello")));
        assert_eq!(input.source, "");

        // Parse an identifier with trailing content
        input.initialize("count more");
        let result = input.read_identifier();
        assert_eq!(result, Ok(Identifier("count")));
        assert_eq!(input.source, " more");

        // Parse an identifier with leading whitespace and trailing content
        input.initialize("  \t  test_name  after");
        let result = input.read_identifier();
        assert_eq!(result, Ok(Identifier("test_name")));
        assert_eq!(input.source, "  after");

        // Parse an identifier with various delimiters
        input.initialize("name(param)");
        let result = input.read_identifier();
        assert_eq!(result, Ok(Identifier("name")));
        assert_eq!(input.source, "(param)");
    }

    #[test]
    fn splitting_by() {
        let mut input = Parser::new();

        // Test splitting simple comma-separated identifiers
        input.initialize("apple, banana, cherry");
        let result = input.take_split_by(',', |inner| inner.read_identifier());
        assert_eq!(
            result,
            Ok(vec![
                Identifier("apple"),
                Identifier("banana"),
                Identifier("cherry")
            ])
        );
        assert_eq!(input.source, "");

        // Test splitting with extra whitespace
        input.initialize("  un  |  deux  |  trois  ");
        let result = input.take_split_by('|', |inner| inner.read_identifier());
        assert_eq!(
            result,
            Ok(vec![
                Identifier("un"),
                Identifier("deux"),
                Identifier("trois")
            ])
        );

        // Ensure a single item (no delimiter present in input) works
        input.initialize("seulement");
        let result = input.take_split_by(',', |inner| inner.read_identifier());
        assert_eq!(result, Ok(vec![Identifier("seulement")]));

        // an empty chunk causes an error
        input.initialize("un,,trois");
        let result = input.take_split_by(',', |inner| inner.read_identifier());
        assert!(result.is_err());

        // empty trailing chunk causes an error
        input.initialize("un,deux,");
        let result = input.take_split_by(',', |inner| inner.read_identifier());
        assert!(result.is_err());

        // different split character
        input.initialize("'Yes'|'No'|'Maybe'");
        let result = input.take_split_by('|', |inner| {
            let content = inner.entire();
            Ok(validate_response(content)?)
        });
        assert_eq!(
            result,
            Ok(vec![
                Response {
                    value: "Yes",
                    condition: None
                },
                Response {
                    value: "No",
                    condition: None
                },
                Response {
                    value: "Maybe",
                    condition: None
                }
            ])
        );
    }

    #[test]
    fn reading_responses() {
        let mut input = Parser::new();

        // Test single response
        input.initialize("'Yes'");
        let result = input.read_responses();
        assert_eq!(
            result,
            Ok(vec![Response {
                value: "Yes",
                condition: None
            }])
        );

        // Test multiple responses
        input.initialize("'Yes' | 'No'");
        let result = input.read_responses();
        assert_eq!(
            result,
            Ok(vec![
                Response {
                    value: "Yes",
                    condition: None
                },
                Response {
                    value: "No",
                    condition: None
                }
            ])
        );

        // Test three responses
        input.initialize("'Yes' | 'No' | 'Not Applicable'");
        let result = input.read_responses();
        assert_eq!(
            result,
            Ok(vec![
                Response {
                    value: "Yes",
                    condition: None
                },
                Response {
                    value: "No",
                    condition: None
                },
                Response {
                    value: "Not Applicable",
                    condition: None
                }
            ])
        );

        // Test response with condition
        input.initialize("'Yes' and equipment available");
        let result = input.read_responses();
        assert_eq!(
            result,
            Ok(vec![Response {
                value: "Yes",
                condition: Some("and equipment available")
            }])
        );

        // Test responses with whitespace
        input.initialize("  'Option A'  |  'Option B'  ");
        let result = input.read_responses();
        assert_eq!(
            result,
            Ok(vec![
                Response {
                    value: "Option A",
                    condition: None
                },
                Response {
                    value: "Option B",
                    condition: None
                }
            ])
        );
    }
}

#[cfg(test)]
mod verify {
    use super::*;

    fn trim(s: &str) -> &str {
        s.strip_prefix('\n')
            .unwrap_or(s)
    }

    #[test]
    fn detect_declarations() {
        let content = trim(
            r#"
making_coffee :
            "#,
        );

        assert!(is_procedure_declaration(content));
    }

    #[test]
    fn technique_header() {
        let mut input = Parser::new();
        input.initialize("% technique v1");

        let metadata = input.read_technique_header();
        assert_eq!(
            metadata,
            Ok(Metadata {
                version: 1,
                license: None,
                copyright: None,
                template: None
            })
        );

        input.initialize(trim(
            r#"
% technique v1
! MIT; (c) ACME, Inc
& checklist
            "#,
        ));

        let metadata = input.read_technique_header();
        assert_eq!(
            metadata,
            Ok(Metadata {
                version: 1,
                license: Some("MIT"),
                copyright: Some("ACME, Inc"),
                template: Some("checklist")
            })
        );
    }

    #[test]
    fn procedure_declaration_one() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
making_coffee : (Beans, Milk) -> Coffee

            "#,
        ));

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("making_coffee"),
                signature: Some(Signature {
                    domain: Genus::Tuple(vec![Forma("Beans"), Forma("Milk")]),
                    range: Genus::Single(Forma("Coffee"))
                }),
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![],
            })
        );
    }

    #[test]
    fn procedure_declaration_two() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
first : A -> B

second : C -> D

            "#,
        ));

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("first"),
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                }),
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![],
            })
        );

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("second"),
                signature: Some(Signature {
                    domain: Genus::Single(Forma("C")),
                    range: Genus::Single(Forma("D"))
                }),
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![],
            })
        );
    }

    #[test]
    fn example_procedure() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
first : A -> B

# The First

This is the first one.

1. Do the first thing in the first one.
2. Do the second thing in the first one.

            "#,
        ));

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("first"),
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                }),
                title: Some("The First"),
                description: vec![Descriptive::Text("This is the first one.")],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![Descriptive::Text("Do the first thing in the first one.")],
                        attribute: vec![],
                        substeps: vec![],
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![Descriptive::Text("Do the second thing in the first one.")],
                        attribute: vec![],
                        substeps: vec![],
                    }
                ],
            })
        );
    }

    #[test]
    fn example_with_responses() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
first : A -> B

# The First

This is the first one.

1. Have you done the first thing in the first one?
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
        ));

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("first"),
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                }),
                title: Some("The First"),
                description: vec![Descriptive::Text("This is the first one.")],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![
                            Descriptive::Text("Have you done the first thing in the first one?"),
                            Descriptive::Responses(vec![
                                Response {
                                    value: "Yes",
                                    condition: None
                                },
                                Response {
                                    value: "No",
                                    condition: Some("but I have an excuse")
                                }
                            ])
                        ],
                        attribute: vec![],
                        substeps: vec![],
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![Descriptive::Text("Do the second thing in the first one.")],
                        attribute: vec![],
                        substeps: vec![],
                    }
                ],
            })
        );
    }

    #[test]
    fn example_with_substeps() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
first : A -> B

# The First

This is the first one.

1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
        ));

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("first"),
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                }),
                title: Some("The First"),
                description: vec![Descriptive::Text("This is the first one.")],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![Descriptive::Text(
                            "Have you done the first thing in the first one?"
                        )],
                        attribute: vec![],
                        substeps: vec![Step::Dependent {
                            ordinal: "a",
                            content: vec![
                                Descriptive::Text(
                                    "Do the first thing. Then ask yourself if you are done:"
                                ),
                                Descriptive::Responses(vec![
                                    Response {
                                        value: "Yes",
                                        condition: None
                                    },
                                    Response {
                                        value: "No",
                                        condition: Some("but I have an excuse")
                                    }
                                ])
                            ],
                            attribute: vec![],
                            substeps: vec![]
                        }]
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![Descriptive::Text("Do the second thing in the first one.")],
                        attribute: vec![],
                        substeps: vec![],
                    }
                ],
            })
        );
    }

    #[test]
    fn realistic_procedure() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
            before_anesthesia :

            # Before induction of anaesthesia

                1.  Has the patient confirmed his/her identity, site, procedure,
                    and consent?
                        'Yes'
                2.  Is the site marked?
                        'Yes' | 'Not Applicable'
                3.  Is the anaesthesia machine and medication check complete?
                        'Yes'
                4.  Is the pulse oximeter on the patient and functioning?
                        'Yes'
                5.  Does the patient have a:
                    - Known allergy?
                            'No' | 'Yes'
                    - Difficult airway or aspiration risk?
                            'No' | 'Yes' and equipment/assistance available
                    - Risk of blood loss > 500 mL?
                            'No' | 'Yes' and two IVs planned and fluids available
            "#,
        ));
        let procedure = input.read_procedure();

        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("before_anesthesia"),
                signature: None,
                title: Some("Before induction of anaesthesia"),
                description: vec![],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![
                            Descriptive::Text("Has the patient confirmed his/her identity, site, procedure,\n                    and consent?"),
                            Descriptive::Responses(vec![Response { value: "Yes", condition: None }])
                        ],
                        attribute: vec![],
                        substeps: vec![],
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![
                            Descriptive::Text("Is the site marked?"),
                            Descriptive::Responses(vec![
                                Response { value: "Yes", condition: None },
                                Response { value: "Not Applicable", condition: None }
                            ])
                        ],
                        attribute: vec![],
                        substeps: vec![],
                    },
                    Step::Dependent {
                        ordinal: "3",
                        content: vec![
                            Descriptive::Text("Is the anaesthesia machine and medication check complete?"),
                            Descriptive::Responses(vec![Response { value: "Yes", condition: None }])
                        ],
                        attribute: vec![],
                        substeps: vec![],
                    },
                    Step::Dependent {
                        ordinal: "4",
                        content: vec![
                            Descriptive::Text("Is the pulse oximeter on the patient and functioning?"),
                            Descriptive::Responses(vec![Response { value: "Yes", condition: None }])
                        ],
                        attribute: vec![],
                        substeps: vec![],
                    },
                    Step::Dependent {
                        ordinal: "5",
                        content: vec![
                            Descriptive::Text("Does the patient have a:")
                        ],
                        attribute: vec![],
                        substeps: vec![
                            Step::Parallel {
                                content: vec![
                                    Descriptive::Text("Known allergy?"),
                                    Descriptive::Responses(vec![
                                        Response { value: "No", condition: None },
                                        Response { value: "Yes", condition: None }
                                    ])
                                ],
                                attribute: vec![],
                                substeps: vec![],
                            },
                            Step::Parallel {
                                content: vec![
                                    Descriptive::Text("Difficult airway or aspiration risk?"),
                                    Descriptive::Responses(vec![
                                        Response { value: "No", condition: None },
                                        Response { value: "Yes", condition: Some("and equipment/assistance available") }
                                    ])
                                ],
                                attribute: vec![],
                                substeps: vec![],
                            },
                            Step::Parallel {
                                content: vec![
                                    Descriptive::Text("Risk of blood loss > 500 mL?"),
                                    Descriptive::Responses(vec![
                                        Response { value: "No", condition: None },
                                        Response { value: "Yes", condition: Some("and two IVs planned and fluids available") }
                                    ])
                                ],
                                attribute: vec![],
                                substeps: vec![],
                            }
                        ],
                    }
                ],
            })
        );
    }
}

/*
    #[test]
    fn check_attribute_role() {
        let a = grammar::attributeParser::new();

        assert_eq!(
            a.parse("@chef"),
            Ok(Attribute {
                name: "chef".to_owned()
            })
        );

        let p = grammar::attribute_lineParser::new();

        assert_eq!(
            p.parse("@chef"),
            Ok(vec![Attribute {
                name: "chef".to_owned()
            }])
        );
        assert_eq!(
            p.parse("@chef + @sous"),
            Ok(vec![
                Attribute {
                    name: "chef".to_owned()
                },
                Attribute {
                    name: "sous".to_owned()
                }
            ])
        );
    }
*/
