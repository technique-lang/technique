#![allow(dead_code)]

use regex::Regex;
use technique::error::*;
use technique::language::*;

macro_rules! regex {
    ($pattern:expr) => {{
        use std::sync::OnceLock;
        static REGEX: OnceLock<regex::Regex> = OnceLock::new();
        REGEX.get_or_init(|| regex::Regex::new($pattern).unwrap_or_else(|e| panic!("{}", e)))
    }};
}

pub fn parse_via_taking(content: &str) -> Result<Technique, TechniqueError> {
    let mut input = Parser::new();
    input.initialize(content);

    let result = input.parse_from_start();
    match result {
        Ok(technique) => Ok(technique),
        Err(error) => Err(make_error(input, error)),
    }
}

fn make_error<'i>(parser: Parser<'i>, error: ParsingError<'i>) -> TechniqueError<'i> {
    TechniqueError {
        problem: error.message(),
        source: parser.original,
        offset: error.offset(),
        width: None,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsingError<'i> {
    IllegalParserState(usize),
    Unimplemented(usize),
    ZeroLengthToken(usize),
    Unrecognized(usize), // improve this
    Expected(usize, &'static str),
    InvalidHeader(usize),
    InvalidCharacter(usize, char),
    UnexpectedEndOfInput(usize),
    InvalidIdentifier(usize, &'i str),
    InvalidForma(usize),
    InvalidGenus(usize),
    InvalidSignature(usize),
    InvalidDeclaration(usize),
    InvalidInvocation(usize),
    InvalidFunction(usize),
    InvalidCodeBlock(usize),
    InvalidStep(usize),
    InvalidForeach(usize),
    InvalidResponse(usize),
}

impl<'i> ParsingError<'i> {
    fn offset(&self) -> usize {
        match self {
            ParsingError::IllegalParserState(offset) => *offset,
            ParsingError::Unimplemented(offset) => *offset,
            ParsingError::ZeroLengthToken(offset) => *offset,
            ParsingError::Unrecognized(offset) => *offset,
            ParsingError::Expected(offset, _) => *offset,
            ParsingError::InvalidHeader(offset) => *offset,
            ParsingError::InvalidCharacter(offset, _) => *offset,
            ParsingError::UnexpectedEndOfInput(offset) => *offset,
            ParsingError::InvalidIdentifier(offset, _) => *offset,
            ParsingError::InvalidForma(offset) => *offset,
            ParsingError::InvalidGenus(offset) => *offset,
            ParsingError::InvalidSignature(offset) => *offset,
            ParsingError::InvalidDeclaration(offset) => *offset,
            ParsingError::InvalidInvocation(offset) => *offset,
            ParsingError::InvalidFunction(offset) => *offset,
            ParsingError::InvalidCodeBlock(offset) => *offset,
            ParsingError::InvalidStep(offset) => *offset,
            ParsingError::InvalidForeach(offset) => *offset,
            ParsingError::InvalidResponse(offset) => *offset,
        }
    }

    fn message(&self) -> String {
        match self {
            ParsingError::IllegalParserState(_) => "illegal parser state".to_string(),
            ParsingError::Unimplemented(_) => "as yet unimplemented!".to_string(),
            ParsingError::ZeroLengthToken(_) => "zero length input".to_string(),
            ParsingError::Unrecognized(_) => "unrecognized".to_string(),
            ParsingError::Expected(_, value) => format!("expected {}", value),
            ParsingError::InvalidHeader(_) => "invalid header".to_string(),
            ParsingError::InvalidCharacter(_, c) => format!("invalid character '{}'", c),
            ParsingError::UnexpectedEndOfInput(_) => "unexpected end of input".to_string(),
            ParsingError::InvalidIdentifier(_, _) => "invalid identifier".to_string(),
            ParsingError::InvalidForma(_) => "invalid forma".to_string(),
            ParsingError::InvalidGenus(_) => "invalid genus".to_string(),
            ParsingError::InvalidSignature(_) => "invalid signature".to_string(),
            ParsingError::InvalidDeclaration(_) => "invalid procedure declaration".to_string(),
            ParsingError::InvalidInvocation(_) => "invalid procedure invocation".to_string(),
            ParsingError::InvalidFunction(_) => "invalid function call".to_string(),
            ParsingError::InvalidCodeBlock(_) => "invalid code block".to_string(),
            ParsingError::InvalidStep(_) => "invalid step".to_string(),
            ParsingError::InvalidForeach(_) => "invalid foreach loop".to_string(),
            ParsingError::InvalidResponse(_) => "invalid response literal".to_string(),
        }
    }
}

#[derive(Debug)]
struct Parser<'i> {
    original: &'i str,
    source: &'i str,
    offset: usize,
}

impl<'i> Parser<'i> {
    fn new() -> Parser<'i> {
        Parser {
            original: "",
            source: "",
            offset: 0,
        }
    }

    fn initialize(&mut self, content: &'i str) {
        self.original = content;
        self.source = content;
        self.offset = 0;
    }

    fn advance(&mut self, width: usize) {
        // advance the parser position
        self.source = &self.source[width..];
        self.offset += width;
    }

    fn parse_from_start(&mut self) -> Result<Technique<'i>, ParsingError<'i>> {
        // Check if header is present by looking for magic line
        let header = if is_magic_line(self.entire()) {
            Some(self.read_technique_header()?)
        } else {
            None
        };

        // Parse zero or more procedures
        let mut procedures = Vec::new();
        while !self.is_finished() {
            self.trim_whitespace();

            if self.is_finished() {
                break;
            }

            // Check if current position starts with a procedure declaration
            let content = self.entire();
            if is_procedure_declaration(content) {
                let procedure = self.read_procedure()?;
                procedures.push(procedure);
            } else {
                // TODO: Handle unexpected content properly
                return Err(ParsingError::Unrecognized(self.offset));
            }
        }

        let body = if procedures.is_empty() {
            None
        } else {
            Some(procedures)
        };

        Ok(Technique { header, body })
    }

    /// consume up to but not including newline (or end)
    fn take_line<A, F>(&mut self, f: F) -> Result<A, ParsingError<'i>>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError<'i>>,
    {
        let result = self.take_until(&['\n'], f);
        self.require_newline()?;
        result
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
    ) -> Result<A, ParsingError<'i>>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError<'i>>,
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
                i += line.len() + 1;
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
    ) -> Result<A, ParsingError<'i>>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError<'i>>,
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
            return Err(ParsingError::Expected(self.offset, "the start character"));
        }
        if l == 0 {
            return Err(ParsingError::Expected(self.offset, "the end character"));
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
    ) -> Result<A, ParsingError<'i>>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError<'i>>,
    {
        let width = delimiter.len();

        // Find the start delimiter
        let start = self
            .source
            .find(delimiter)
            .ok_or(ParsingError::Expected(self.offset, "a starting delimiter"))?;

        // Look for the end delimiter after correcting for the starting one
        let start = start + width;
        let end = self.source[start..]
            .find(delimiter)
            .ok_or(ParsingError::Expected(
                self.offset,
                "the corresponding end delimiter",
            ))?;

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

    fn take_until<A, F>(&mut self, pattern: &[char], function: F) -> Result<A, ParsingError<'i>>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError<'i>>,
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

    fn take_split_by<A, F>(
        &mut self,
        delimiter: char,
        function: F,
    ) -> Result<Vec<A>, ParsingError<'i>>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError<'i>>,
    {
        let content = self.entire();
        let mut results = Vec::new();

        for chunk in content.split(delimiter) {
            let trimmed = chunk.trim();
            if trimmed.is_empty() {
                return Err(ParsingError::Expected(
                    self.offset,
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

    fn take_paragraph<A, F>(&mut self, function: F) -> Result<A, ParsingError<'i>>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError<'i>>,
    {
        // Find the end of this paragraph (\n\n or end of input)
        let content = self.source;
        let mut i = content
            .find("\n\n")
            .unwrap_or(content.len());
        let paragraph = &content[..i];

        let mut parser = self.subparser(0, paragraph);
        let result = function(&mut parser)?;

        // Advance past this paragraph and the \n\n delimiter if present
        if i < content.len() {
            i += 2;
        }
        self.advance(i);

        Ok(result)
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
            original: self.original,
            source: content,
            offset: indent + self.offset,
        };

        // and return
        parser
    }

    // because test cases and trivial single-line examples might omit an
    // ending newline, this also returns Ok if end of input is reached.
    fn require_newline(&mut self) -> Result<(), ParsingError<'i>> {
        for (i, c) in self
            .source
            .char_indices()
        {
            let l = i + 1;

            if c == '\n' {
                self.source = &self.source[l..];
                self.offset += l;
                return Ok(());
            } else if c.is_ascii_whitespace() {
                continue;
            } else {
                return Err(ParsingError::InvalidCharacter(self.offset, c));
            }
        }

        // We don't actually require a newline to end the file.

        self.source = "";
        self.offset += self
            .source
            .len();
        Ok(())
    }

    // hard wire the version for now. If we ever grow to supporting multiple major
    // versions then this will be a lot more complicated than just dealing with a
    // different natural number here.
    fn read_magic_line(&mut self) -> Result<u8, ParsingError<'i>> {
        self.take_line(|inner| {
            let re = regex!(r"%\s*technique\s+v1");

            if re.is_match(inner.source) {
                Ok(1)
            } else {
                Err(ParsingError::InvalidHeader(0))
            }
        })
    }

    // This one is awkward because if a SPDX line is present, then it really needs
    // to have a license, whereas the copyright part is optional.
    fn read_spdx_line(&mut self) -> Result<(Option<&'i str>, Option<&'i str>), ParsingError<'i>> {
        self.take_line(|inner| {
            let re = regex!(r"^!\s*([^;]+)(?:;\s*(?:\(c\)|\(C\)|©)\s*(.+))?$");

            let cap = re
                .captures(inner.source)
                .ok_or(ParsingError::InvalidHeader(0))?;

            // Now to extracting the values we need. We get the license code from
            // the first capture. It must be present otherwise we don't have a
            // valid SPDX line (and we declared that we're on an SPDX line by the
            // presence of the '!' character at the beginning of the line).

            let one = cap
                .get(1)
                .ok_or(ParsingError::Expected(inner.offset, "the license name"))?;

            let result =
                validate_license(one.as_str()).ok_or(ParsingError::InvalidHeader(inner.offset))?;
            let license = Some(result);

            // Now dig out the copyright, if present:

            let copyright = match cap.get(2) {
                Some(two) => {
                    let result = validate_copyright(two.as_str())
                        .ok_or(ParsingError::InvalidHeader(inner.offset))?;
                    Some(result)
                }
                None => None,
            };

            Ok((license, copyright))
        })
    }

    fn read_template_line(&mut self) -> Result<Option<&'i str>, ParsingError<'i>> {
        self.take_line(|inner| {
            let re = regex!(r"^&\s*(.+)$");

            let cap = re
                .captures(inner.source)
                .ok_or(ParsingError::InvalidHeader(inner.offset))?;

            let one = cap
                .get(1)
                .ok_or(ParsingError::Expected(inner.offset, "a template name"))?;

            let result =
                validate_template(one.as_str()).ok_or(ParsingError::InvalidHeader(inner.offset))?;
            Ok(Some(result))
        })
    }

    fn read_technique_header(&mut self) -> Result<Metadata<'i>, ParsingError<'i>> {
        // Process magic line
        let version = if is_magic_line(self.source) {
            self.read_magic_line()?
        } else {
            Err(ParsingError::Expected(0, "The % symbol"))?
        };

        // Process SPDX line

        let (license, copyright) = if is_spdx_line(self.source) {
            self.read_spdx_line()?
        } else {
            (None, None)
        };

        // Process template line
        let template = if is_template_line(self.source) {
            self.read_template_line()?
        } else {
            None
        };

        Ok(Metadata {
            version,
            license,
            copyright,
            template,
        })
    }

    fn read_signature(&mut self) -> Result<Signature<'i>, ParsingError<'i>> {
        let content = self.entire();

        let re = regex!(r"\s*(.+?)\s*->\s*(.+?)\s*$");

        let cap = match re.captures(content) {
            Some(c) => c,
            None => return Err(ParsingError::InvalidSignature(self.offset)),
        };

        let one = cap
            .get(1)
            .ok_or(ParsingError::Expected(
                self.offset,
                "a Genus for the domain",
            ))?;

        let two = cap
            .get(2)
            .ok_or(ParsingError::Expected(self.offset, "a Genus for the range"))?;

        let domain = validate_genus(one.as_str()).ok_or(ParsingError::InvalidGenus(self.offset))?;
        let range = validate_genus(two.as_str()).ok_or(ParsingError::InvalidGenus(self.offset))?;

        Ok(Signature { domain, range })
    }

    fn parse_procedure_declaration(
        &mut self,
    ) -> Result<
        (
            Identifier<'i>,
            Option<Vec<Identifier<'i>>>,
            Option<Signature<'i>>,
        ),
        ParsingError<'i>,
    > {
        // These capture groups use .+? to make "match more than one, but
        // lazily" so that the subsequent grabs of whitespace and the all
        // important ':' character are not absorbed.
        let re = regex!(r"^\s*(.+?)\s*:\s*(.+?)?\s*$");

        let cap = re
            .captures(self.source)
            .ok_or(ParsingError::InvalidDeclaration(self.offset))?;

        let one = cap
            .get(1)
            .ok_or(ParsingError::Expected(
                self.offset,
                "an Identifier for the procedure declaration",
            ))?;

        let text = one.as_str();
        let (name, parameters) = if let Some((before, list)) = text.split_once('(') {
            let name = validate_identifier(before)
                .ok_or(ParsingError::InvalidIdentifier(self.offset, before))?;

            // Extract parameters from parentheses
            if !list.ends_with(')') {
                return Err(ParsingError::InvalidDeclaration(self.offset));
            }
            let list = &list[..list.len() - 1].trim();

            let parameters = if list.is_empty() {
                None
            } else {
                let mut params = Vec::new();
                for item in list.split(',') {
                    let param = validate_identifier(item.trim())
                        .ok_or(ParsingError::Unrecognized(self.offset))?;
                    params.push(param);
                }
                Some(params)
            };

            (name, parameters)
        } else {
            let name = validate_identifier(text)
                .ok_or(ParsingError::InvalidIdentifier(self.offset, text))?;
            (name, None)
        };

        let signature = match cap.get(2) {
            Some(two) => {
                let mut inner = self.subparser(0, two.as_str());
                let result = inner.read_signature()?;
                Some(result)
            }
            None => None,
        };

        Ok((name, parameters, signature))
    }

    // assumes we've set up the precondition that indeed there is a # present.
    fn parse_procedure_title(&mut self) -> Result<&'i str, ParsingError<'i>> {
        self.trim_whitespace();

        if self.peek_next_char() == Some('#') {
            let title = self.source[1..].trim();
            Ok(title)
        } else {
            // we shouldn't have invoked this unless we have a title to parse!
            Err(ParsingError::IllegalParserState(self.offset))
        }
    }

    fn read_procedure(&mut self) -> Result<Procedure<'i>, ParsingError<'i>> {
        let procedure = self.take_block_lines(
            is_procedure_declaration,
            is_procedure_declaration,
            |outer| {
                // Extract the declaration, and parse it
                let declaration = outer.take_block_lines(
                    is_procedure_declaration,
                    |line| !is_procedure_declaration(line),
                    |inner| inner.parse_procedure_declaration(),
                )?;

                // Read title, if present

                let content = outer.entire();
                let title = if is_procedure_title(content) {
                    let title = outer.take_block_lines(
                        |line| {
                            line.trim_start()
                                .starts_with('#')
                        },
                        |line| !line.starts_with('#'),
                        |inner| {
                            let text = inner.parse_procedure_title()?;
                            Ok(Some(text))
                        },
                    )?;

                    title
                } else {
                    None
                };

                // Extract content after declaration until a step is encountered

                let content = outer.entire();
                let description = if !is_step(content) {
                    outer.take_block_lines(
                        |line| !is_step(line),
                        |line| is_step(line),
                        |inner| {
                            let mut description = vec![];

                            let content = inner.entire();
                            if !content.is_empty() {
                                description = inner.read_descriptive()?;
                            }

                            Ok(description)
                        },
                    )?
                } else {
                    vec![]
                };

                // Parse remaining content as steps
                let mut steps = vec![];
                while !outer.is_finished() {
                    let content = outer.entire();
                    if is_step(content) {
                        let step = outer.read_step()?;
                        steps.push(step);
                    } else {
                        return Err(ParsingError::Unrecognized(outer.offset));
                    }
                }

                Ok(Procedure {
                    name: declaration.0,
                    parameters: declaration.1,
                    signature: declaration.2,
                    title,
                    description,
                    attribute: vec![], // TODO: parse attributes
                    steps,
                })
            },
        )?;

        Ok(procedure)
    }

    fn read_code_block(&mut self) -> Result<Expression<'i>, ParsingError<'i>> {
        self.take_block_chars('{', '}', |outer| outer.read_expression())
    }

    fn read_expression(&mut self) -> Result<Expression<'i>, ParsingError<'i>> {
        self.trim_whitespace();
        let content = self.entire();

        if is_binding(content) {
            self.read_binding_expression()
        } else if is_repeat_keyword(content) {
            self.read_repeat_expression()
        } else if is_foreach_keyword(content) {
            self.read_foreach_expression()
        } else if content
            .trim()
            .starts_with("foreach ")
        {
            // Malformed foreach expression
            return Err(ParsingError::InvalidForeach(self.offset));
        } else if content
            .trim()
            .starts_with('[')
        {
            // Data structure syntax - not yet implemented
            return Err(ParsingError::Unimplemented(self.offset));
        } else if is_invocation(content) {
            let invocation = self.read_invocation()?;
            Ok(Expression::Application(invocation))
        } else if is_function(content) {
            let target = self.read_identifier()?;
            let parameters = self.read_parameters()?;

            let function = Function { target, parameters };
            Ok(Expression::Execution(function))
        } else {
            let identifier = self.read_identifier()?;
            Ok(Expression::Value(identifier))
        }
    }

    fn read_foreach_expression(&mut self) -> Result<Expression<'i>, ParsingError<'i>> {
        // Parse "foreach <pattern> in <expression>" where pattern is either:
        // - identifier
        // - (identifier, identifier, ...)

        // Skip "foreach" keyword - we already know it's there from starts_with check
        self.advance(7);
        self.trim_whitespace();

        let identifiers = self.read_identifiers()?;

        self.trim_whitespace();

        // Skip the "in" keyword
        self.advance(2);
        if !self
            .peek_next_char()
            .unwrap()
            .is_ascii_whitespace()
        {
            return Err(ParsingError::InvalidForeach(self.offset));
        }
        self.trim_whitespace();

        let expression = self.read_expression()?;

        Ok(Expression::Foreach(identifiers, Box::new(expression)))
    }

    fn read_identifiers(&mut self) -> Result<Vec<Identifier<'i>>, ParsingError<'i>> {
        if self
            .entire()
            .starts_with('(')
        {
            // Parse parenthesized list: (id1, id2, ...)
            self.take_block_chars('(', ')', |outer| {
                let mut identifiers = Vec::new();

                loop {
                    outer.trim_whitespace();

                    if outer
                        .entire()
                        .is_empty()
                    {
                        break;
                    }

                    let name = outer.read_identifier()?;
                    identifiers.push(name);

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

                if identifiers.is_empty() {
                    return Err(ParsingError::InvalidForeach(outer.offset));
                }

                Ok(identifiers)
            })
        } else {
            // Parse single identifier
            let name = self.read_identifier()?;
            Ok(vec![name])
        }
    }

    fn read_repeat_expression(&mut self) -> Result<Expression<'i>, ParsingError<'i>> {
        // Parse "repeat <expression>"
        self.advance(6);
        self.trim_whitespace();

        // obviously we don't want to ultimately find nested "repeat repeat"
        // here but the compiler can sort that out later; this is still just
        // parsing.
        let expression = self.read_expression()?;

        Ok(Expression::Repeat(Box::new(expression)))
    }

    fn read_binding_expression(&mut self) -> Result<Expression<'i>, ParsingError<'i>> {
        // Parse the expression before the ~ operator
        let expression = self.take_until(&['~'], |inner| inner.read_expression())?;

        // Consume the ~ operator
        self.advance(1); // consume '~'
        self.trim_whitespace();

        let identifiers = self.read_identifiers()?;

        Ok(Expression::Binding(Box::new(expression), identifiers))
    }

    /// Consume an identifier. As with the other smaller read methods, we do a
    /// general scan of the range here to get the relevant, then call the more
    /// detailed validation function to actually determine if it's a match.
    fn read_identifier(&mut self) -> Result<Identifier<'i>, ParsingError<'i>> {
        self.trim_whitespace();

        let content = self.entire();

        let possible = match content.find([' ', '\t', '\n', '(', '{', ',']) {
            None => content,
            Some(i) => &content[0..i],
        };

        let identifier = validate_identifier(possible)
            .ok_or(ParsingError::InvalidIdentifier(self.offset, possible))?;

        self.advance(possible.len());

        Ok(identifier)
    }

    /// Parse a target like <procedure_name> or <https://example.com/proc>
    fn read_target(&mut self) -> Result<Target<'i>, ParsingError<'i>> {
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
    fn read_invocation(&mut self) -> Result<Invocation<'i>, ParsingError<'i>> {
        let target = self.read_target()?;
        let parameters = if self.peek_next_char() == Some('(') {
            Some(self.read_parameters()?)
        } else {
            None
        };
        Ok(Invocation { target, parameters })
    }

    /// Parse a step (main steps are always dependent, substeps can be dependent or parallel)
    fn read_step(&mut self) -> Result<Step<'i>, ParsingError<'i>> {
        self.take_block_lines(is_step, is_step, |outer| {
            outer.trim_whitespace();
            let content = outer.entire();

            if content.is_empty() {
                // FIXME do we even need this check?
                return Err(ParsingError::ZeroLengthToken(outer.offset));
            }

            // Parse ordinal

            let re = regex!(r"^\s*(\d+)\.\s+");
            let cap = re
                .captures(content)
                .ok_or(ParsingError::InvalidStep(outer.offset))?;

            let number = cap
                .get(1)
                .ok_or(ParsingError::Expected(
                    outer.offset,
                    "the ordinal Step number",
                ))?
                .as_str();

            let l = cap
                .get(0)
                .unwrap()
                .len();

            outer.advance(l);

            let text = outer.read_descriptive()?;

            // Parse responses if present
            let mut responses = vec![];
            if !outer.is_finished() {
                let content = outer.entire();
                if is_enum_response(content) {
                    responses = outer.read_responses()?;
                }
            }

            // Parse scopes (role assignments and substeps)
            let scopes = outer.read_scopes()?;

            return Ok(Step::Dependent {
                ordinal: number,
                content: text,
                responses,
                scopes,
            });
        })
    }

    /// Parse a dependent substep (a., b., c., etc.)
    fn read_substep_dependent(&mut self) -> Result<Step<'i>, ParsingError<'i>> {
        self.take_block_lines(
            is_substep_dependent,
            |line| is_substep_dependent(line) || is_role_assignment(line),
            |outer| {
                let content = outer.entire();
                let re = regex!(r"^\s*([a-hj-uw-z])\.\s+");
                let cap = re
                    .captures(content)
                    .ok_or(ParsingError::InvalidStep(outer.offset))?;

                let letter = cap
                    .get(1)
                    .ok_or(ParsingError::Expected(
                        outer.offset,
                        "the ordinal Sub-Step letter",
                    ))?
                    .as_str();

                // Skip past the letter, dot, and space
                let l = cap
                    .get(0)
                    .unwrap()
                    .len();

                outer.advance(l);

                // Parse the remaining content
                let text = outer.read_descriptive()?;

                // Parse responses if present
                let mut responses = vec![];
                if !outer.is_finished() {
                    let content = outer.entire();
                    if is_enum_response(content) {
                        responses = outer.read_responses()?;
                    }
                }

                // Parse scopes (role assignments and substeps)
                let scopes = outer.read_scopes()?;

                Ok(Step::Dependent {
                    ordinal: letter,
                    content: text,
                    responses,
                    scopes,
                })
            },
        )
    }

    /// Parse a parallel substep (-)
    fn read_substep_parallel(&mut self) -> Result<Step<'i>, ParsingError<'i>> {
        self.take_block_lines(
            is_substep_parallel,
            |line| {
                is_substep_dependent(line) || is_substep_parallel(line) || is_role_assignment(line)
            },
            |outer| {
                let content = outer.entire();
                let re = regex!(r"^\s*-\s+");
                let zero = re
                    .find(content)
                    .ok_or(ParsingError::InvalidStep(outer.offset))?;

                // Skip past the dash and space
                let l = zero.len();

                outer.advance(l);

                // Parse the remaining content
                let text = outer.read_descriptive()?;

                // Parse responses if present
                let mut responses = vec![];
                if !outer.is_finished() {
                    let content = outer.entire();
                    if is_enum_response(content) {
                        responses = outer.read_responses()?;
                    }
                }

                // Parse scopes (role assignments and substeps)
                let scopes = outer.read_scopes()?;

                Ok(Step::Parallel {
                    content: text,
                    responses,
                    scopes,
                })
            },
        )
    }

    fn read_descriptive(&mut self) -> Result<Vec<Descriptive<'i>>, ParsingError<'i>> {
        self.take_block_lines(
            |_| true,
            |line| {
                is_step(line)
                    || is_substep_dependent(line)
                    || is_substep_parallel(line)
                    || is_subsubstep_dependent(line)
                    || is_role_assignment(line)
                    || is_enum_response(line)
            },
            |outer| {
                let mut results = vec![];

                while !outer.is_finished() {
                    outer.trim_whitespace();
                    if outer.is_finished() {
                        break;
                    }

                    // Decide container type based on first character
                    if outer.peek_next_char() == Some('{') {
                        // Standalone CodeBlock
                        let code_block = outer.take_paragraph(|parser| parser.read_code_block())?;
                        results.push(Descriptive::CodeBlock(code_block));
                    } else {
                        // Paragraph container
                        let paragraph_content = outer.take_paragraph(|parser| {
                            let mut content = vec![];

                            while let Some(c) = parser.peek_next_char() {
                                parser.trim_whitespace();
                                if parser.is_finished() {
                                    break;
                                }

                                if c == '{' {
                                    let expression = parser.read_code_block()?;
                                    content.push(Descriptive::CodeBlock(expression));
                                } else if c == '<' {
                                    let invocation = parser.read_invocation()?;
                                    parser.trim_whitespace();
                                    if parser.peek_next_char() == Some('~') {
                                        parser.advance(1);
                                        parser.trim_whitespace();
                                        let variable = parser.read_identifier()?;
                                        content.push(Descriptive::Binding(
                                            Box::new(Descriptive::Application(invocation)),
                                            vec![variable],
                                        ));
                                    } else {
                                        content.push(Descriptive::Application(invocation));
                                    }
                                } else {
                                    let text =
                                        parser.take_until(&['{', '<', '~', '\n'], |inner| {
                                            Ok(inner
                                                .source
                                                .trim())
                                        })?;
                                    if text.is_empty() {
                                        continue;
                                    } else if parser.peek_next_char() == Some('~') {
                                        parser.advance(1);
                                        parser.trim_whitespace();
                                        let variable = parser.read_identifier()?;
                                        content.push(Descriptive::Binding(
                                            Box::new(Descriptive::Text(text)),
                                            vec![variable],
                                        ));
                                    } else {
                                        content.push(Descriptive::Text(text));
                                    }
                                }
                            }

                            Ok(content)
                        })?;

                        if !paragraph_content.is_empty() {
                            results.push(Descriptive::Paragraph(paragraph_content));
                        }
                    }
                }

                Ok(results)
            },
        )
    }

    /// Parse enum responses like 'Yes' | 'No' | 'Not Applicable'
    fn read_responses(&mut self) -> Result<Vec<Response<'i>>, ParsingError<'i>> {
        self.take_split_by('|', |inner| {
            let content = inner.entire();
            validate_response(content).ok_or(ParsingError::InvalidResponse(inner.offset))
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
    fn read_parameters(&mut self) -> Result<Vec<Expression<'i>>, ParsingError<'i>> {
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

    fn ensure_nonempty(&mut self) -> Result<(), ParsingError<'i>> {
        if self
            .source
            .len()
            == 0
        {
            return Err(ParsingError::UnexpectedEndOfInput(self.offset));
        }
        Ok(())
    }

    /// Trim any leading whitespace (space, tab, newline) from the front of
    /// the current parser text.
    fn trim_whitespace(&mut self) {
        let mut l = 0;

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
        self.offset += l;
    }

    /// Parse role assignments like @surgeon, @nurse, or @marketing + @sales
    fn read_role_assignments(&mut self) -> Result<Vec<Attribute<'i>>, ParsingError<'i>> {
        self.take_line(|inner| {
            let mut attributes = Vec::new();

            let line = inner.source;

            // Handle multiple roles separated by +
            let role_parts: Vec<&str> = line
                .split('+')
                .collect();

            for part in role_parts {
                let re = Regex::new(r"^\s*@([a-z][a-z0-9_]*)\s*$").unwrap();
                let cap = re
                    .captures(part.trim())
                    .ok_or(ParsingError::InvalidStep(inner.offset))?;

                let role_name = cap
                    .get(1)
                    .ok_or(ParsingError::Expected(inner.offset, "role name after @"))?
                    .as_str();

                let identifier = validate_identifier(role_name)
                    .ok_or(ParsingError::InvalidIdentifier(inner.offset, role_name))?;
                attributes.push(Attribute::Role(identifier));
            }

            Ok(attributes)
        })
    }

    /// Parse scopes - role assignments with their substeps
    fn read_scopes(&mut self) -> Result<Vec<Scope<'i>>, ParsingError<'i>> {
        let mut scopes = Vec::new();
        let mut current_roles = Vec::new();
        let mut current_substeps = Vec::new();

        while !self.is_finished() {
            self.trim_whitespace();
            if self.is_finished() {
                break;
            }

            let content = self.entire();

            if is_role_assignment(content) {
                // If we have accumulated substeps without roles, create a scope for them
                if !current_substeps.is_empty() && current_roles.is_empty() {
                    scopes.push(Scope {
                        roles: Vec::new(),
                        substeps: current_substeps,
                    });
                    current_substeps = Vec::new();
                }

                // If we have accumulated roles and substeps, create a scope for them
                if !current_roles.is_empty() {
                    scopes.push(Scope {
                        roles: current_roles,
                        substeps: current_substeps,
                    });
                    current_substeps = Vec::new();
                }

                // Parse the new role assignment
                current_roles = self.read_role_assignments()?;
            } else if is_substep_dependent(content) {
                let substep = self.read_substep_dependent()?;
                current_substeps.push(substep);
            } else if is_substep_parallel(content) {
                let substep = self.read_substep_parallel()?;
                current_substeps.push(substep);
            } else {
                break;
            }
        }

        // Handle any remaining roles and substeps
        if !current_roles.is_empty() || !current_substeps.is_empty() {
            scopes.push(Scope {
                roles: current_roles,
                substeps: current_substeps,
            });
        }

        Ok(scopes)
    }
}

fn is_magic_line(content: &str) -> bool {
    let re = regex!(r"%\s*technique");

    re.is_match(content)
}

fn is_spdx_line(content: &str) -> bool {
    let re = regex!(r"!\s*[^;]+(?:;\s*.+)?");

    re.is_match(content)
}

fn is_template_line(content: &str) -> bool {
    let re = regex!(r"&\s*.+");

    re.is_match(content)
}

fn is_identifier(content: &str) -> bool {
    let re = regex!(r"^[a-z][a-z0-9_]*$");
    re.is_match(content)
}

/// A signature is of the form
///
/// genus -> genus
///
/// terminated by an end of line.

fn is_signature(content: &str) -> bool {
    let re = regex!(r"\s*.+?\s*->\s*.+?\s*$");

    re.is_match(content)
}

/// Lightweight detection function for Genus patterns. This is necessary as an
/// adjunct to is_procedure_declaration() in order to support recognizing
/// multi-line procedure declarations. Each of these regexes unfortunately has
/// the full validation template for Forma but we're only matching, not
/// capturing, so it is an acceptable duplication.
fn is_genus(content: &str) -> bool {
    let content = content.trim();
    if content.is_empty() {
        return false;
    }

    let mut chars = content.chars();
    let first = chars
        .next()
        .unwrap();

    match first {
        '[' => {
            // List pattern: [Forma] where Forma starts with uppercase
            let re = regex!(r"^\[\s*[A-Z][A-Za-z0-9]*\s*\]$");
            re.is_match(content)
        }
        '(' => {
            // Unit Forma: ()
            if let Some(c) = chars.next() {
                if c == ')' {
                    return true;
                }
            }
            // Tuple pattern: (Forma, Forma, ...)
            let re = regex!(r"^\(\s*[A-Z][A-Za-z0-9]*(\s*,\s*[A-Z][A-Za-z0-9]*)*\s*\)$");
            re.is_match(content)
        }
        _ => {
            // Single Forma pattern
            let re = regex!(r"^[A-Z][A-Za-z0-9]*$");
            re.is_match(content)
        }
    }
}

/// declarations are of the form
///
///     name : signature
///
/// where the name is either
///
///     identifier
///
/// or
///
///     identifier(parameters)
///
/// and where the optional signature is
///
///     genus -> genus
///
/// as above. Crucially, it must not match within a procedure body, for
/// example it must not match " a. And now: do something" or "b. Proceed
/// with:".

fn is_procedure_declaration(content: &str) -> bool {
    match content.split_once(':') {
        Some((before, after)) => {
            let before = before.trim();
            let after = after.trim();

            // Check if the name part is valid
            let has_valid_name = if let Some((name, params)) = before.split_once('(') {
                // Has parameters: check name is identifier and params end with ')'
                is_identifier(name.trim()) && params.ends_with(')')
            } else {
                // No parameters: just check if it's an identifier
                is_identifier(before)
            };

            // Check content after ':' - either empty, procedure content, or valid signature
            let has_valid_signature = if after.is_empty() {
                true // No signature, just procedure content
            } else {
                // Check if the first token looks like a Genus
                let token = after
                    .split_whitespace()
                    .next()
                    .unwrap_or("");

                if !is_genus(token) {
                    // First token is not a Genus, so this is procedure content (title, description, etc.)
                    true
                } else {
                    // First token is a Genus, so we expect a complete signature: Genus -> Genus
                    // Find the arrow and extract domain and range
                    if let Some(i) = after.find("->") {
                        let domain = after[..i].trim();
                        let range = after[i + 2..].trim();

                        let range = if let Some(j) = range.find('\n') {
                            &range[..j]
                        } else {
                            range
                        };

                        // Both parts must be valid Genus
                        is_genus(domain) && is_genus(range)
                    } else {
                        // Has Genus but no arrow - malformed signature
                        false
                    }
                }
            };

            has_valid_name && has_valid_signature
        }
        None => false,
    }
}

fn is_procedure_title(content: &str) -> bool {
    content
        .trim_start()
        .starts_with('#')
}

// I'm not sure about anchoring this one on start and end, seeing as how it
// will be used when scanning.
fn is_invocation(content: &str) -> bool {
    let re = regex!(r"^\s*(<.+?>\s*(?:\(.*?\))?)\s*$");

    re.is_match(content)
}

fn is_code_block(content: &str) -> bool {
    let re = regex!(r"\s*{.*?}");

    re.is_match(content)
}

fn is_foreach_keyword(content: &str) -> bool {
    let re = regex!(
        r"^\s*foreach\s+([a-z][a-z0-9_]*|\([a-z][a-z0-9_]*(?:\s*,\s*[a-z][a-z0-9_]*)*\))\s+in\s+"
    );

    re.is_match(content)
}

fn is_repeat_keyword(content: &str) -> bool {
    let re = regex!(r"^\s*repeat\s+");

    re.is_match(content)
}

fn is_function(content: &str) -> bool {
    let re = regex!(r"^\s*.+?\(");

    re.is_match(content)
}

fn is_binding(content: &str) -> bool {
    let re = regex!(r"~\s+([a-z][a-z0-9_]*|\([a-z][a-z0-9_]*(?:\s*,\s*[a-z][a-z0-9_]*)*\))\s*$");

    re.is_match(content)
}

fn is_step(content: &str) -> bool {
    let re = regex!(r"^\s*\d+\.\s+");
    re.is_match(content)
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
fn is_substep_dependent(content: &str) -> bool {
    let re = regex!(r"^\s*[a-hj-uw-z]\.\s+");
    re.is_match(content)
}

fn is_substep_parallel(content: &str) -> bool {
    let re = regex!(r"^\s*-\s+");
    re.is_match(content)
}

fn is_subsubstep_dependent(content: &str) -> bool {
    let re = regex!(r"^\s*[ivx]+\.\s+");
    re.is_match(content)
}

fn is_role_assignment(content: &str) -> bool {
    let re = regex!(r"^\s*@[a-z][a-z0-9_]*(\s*\+\s*@[a-z][a-z0-9_]*)*");
    re.is_match(content)
}

fn is_enum_response(content: &str) -> bool {
    let re = regex!(r"^\s*'.+?'");
    re.is_match(content)
}

#[cfg(test)]
mod check {
    use super::*;

    #[test]
    fn magic_line() {
        let mut input = Parser::new();
        input.initialize("% technique v1");
        assert!(is_magic_line(input.source));

        let result = input.read_magic_line();
        assert_eq!(result, Ok(1));

        input.initialize("%technique v1");
        assert!(is_magic_line(input.source));

        let result = input.read_magic_line();
        assert_eq!(result, Ok(1));

        input.initialize("%techniquev1");
        assert!(is_magic_line(input.source));

        // this is rejected because the technique keyword isn't present.
        let result = input.read_magic_line();
        assert!(result.is_err());
    }

    #[test]
    fn header_spdx() {
        let mut input = Parser::new();
        input.initialize("! PD");
        assert!(is_spdx_line(input.source));

        let result = input.read_spdx_line();
        assert_eq!(result, Ok((Some("PD"), None)));

        input.initialize("! MIT; (c) ACME, Inc.");
        assert!(is_spdx_line(input.source));

        let result = input.read_spdx_line();
        assert_eq!(result, Ok((Some("MIT"), Some("ACME, Inc."))));

        input.initialize("! MIT; (C) 2024 ACME, Inc.");
        assert!(is_spdx_line(input.source));

        let result = input.read_spdx_line();
        assert_eq!(result, Ok((Some("MIT"), Some("2024 ACME, Inc."))));

        input.initialize("! CC BY-SA 3.0 [IGO]; (c) 2024 ACME, Inc.");
        assert!(is_spdx_line(input.source));

        let result = input.read_spdx_line();
        assert_eq!(
            result,
            Ok((Some("CC BY-SA 3.0 [IGO]"), Some("2024 ACME, Inc.")))
        );
    }

    #[test]
    fn header_template() {
        let mut input = Parser::new();
        input.initialize("& checklist");
        assert!(is_template_line(input.source));

        let result = input.read_template_line();
        assert_eq!(result, Ok(Some("checklist")));

        input.initialize("& nasa-flight-plan,v4.0");
        assert!(is_template_line(input.source));

        let result = input.read_template_line();
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
            Err(ParsingError::UnexpectedEndOfInput(input.offset))
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
        let mut input = Parser::new();
        input.initialize("p");
        let result = input.read_identifier();
        assert_eq!(result, Ok(Identifier("p")));

        input.initialize("cook_pizza");
        let result = input.read_identifier();
        assert_eq!(result, Ok(Identifier("cook_pizza")));

        input.initialize("cook-pizza");
        let result = input.read_identifier();
        assert!(result.is_err());
    }

    #[test]
    fn signatures() {
        let mut input = Parser::new();
        input.initialize("A -> B");
        let result = input.read_signature();
        assert_eq!(
            result,
            Ok(Signature {
                domain: Genus::Single(Forma("A")),
                range: Genus::Single(Forma("B"))
            })
        );

        input.initialize("Beans -> Coffee");
        let result = input.read_signature();
        assert_eq!(
            result,
            Ok(Signature {
                domain: Genus::Single(Forma("Beans")),
                range: Genus::Single(Forma("Coffee"))
            })
        );

        input.initialize("[Bits] -> Bob");
        let result = input.read_signature();
        assert_eq!(
            result,
            Ok(Signature {
                domain: Genus::List(Forma("Bits")),
                range: Genus::Single(Forma("Bob"))
            })
        );

        input.initialize("Complex -> (Real, Imaginary)");
        let result = input.read_signature();
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
        let mut input = Parser::new();
        input.initialize("making_coffee :");

        assert!(is_procedure_declaration(input.source));

        let result = input.parse_procedure_declaration();
        assert_eq!(result, Ok((Identifier("making_coffee"), None, None)));
    }

    #[test]
    fn declaration_full() {
        let mut input = Parser::new();
        input.initialize("f : A -> B");
        assert!(is_procedure_declaration(input.source));

        let result = input.parse_procedure_declaration();
        assert_eq!(
            result,
            Ok((
                Identifier("f"),
                None,
                Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                })
            ))
        );

        input.initialize("making_coffee : (Beans, Milk) -> [Coffee]");
        assert!(is_procedure_declaration(input.source));

        let result = input.parse_procedure_declaration();
        assert_eq!(
            result,
            Ok((
                Identifier("making_coffee"),
                None,
                Some(Signature {
                    domain: Genus::Tuple(vec![Forma("Beans"), Forma("Milk")]),
                    range: Genus::List(Forma("Coffee"))
                })
            ))
        );

        let content = "f : B";
        assert!(!is_procedure_declaration(content));
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
        assert!(is_subsubstep_dependent("i. One"));
        assert!(is_subsubstep_dependent(" ii. Two"));
        assert!(is_subsubstep_dependent("v. Five"));
        assert!(is_subsubstep_dependent("vi. Six"));
        assert!(is_subsubstep_dependent("ix. Nine"));
        assert!(is_subsubstep_dependent("x. Ten"));
        assert!(is_subsubstep_dependent("xi. Eleven"));
        assert!(is_subsubstep_dependent("xxxix. Thirty-nine"));

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
    fn read_steps() {
        let mut input = Parser::new();

        // Test simple dependent step
        input.initialize("1. First step");
        let result = input.read_step();
        assert_eq!(
            result,
            Ok(Step::Dependent {
                ordinal: "1",
                content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "First step"
                )])],
                responses: vec![],
                scopes: vec![],
            })
        );

        // Test multi-line dependent step
        input.initialize(
            r#"
    1.  Have you done the first thing in the first one?
            "#,
        );
        let result = input.read_step();
        assert_eq!(
            result,
            Ok(Step::Dependent {
                ordinal: "1",
                content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "Have you done the first thing in the first one?"
                )])],
                responses: vec![],
                scopes: vec![],
            })
        );

        // Test invalid step
        input.initialize("Not a step");
        let result = input.read_step();
        assert_eq!(result, Err(ParsingError::InvalidStep(0)));
    }

    #[test]
    fn reading_substeps_basic() {
        let mut input = Parser::new();

        // Test simple dependent sub-step
        input.initialize("a. First subordinate task");
        let result = input.read_substep_dependent();
        assert_eq!(
            result,
            Ok(Step::Dependent {
                ordinal: "a",
                content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "First subordinate task"
                )])],
                responses: vec![],
                scopes: vec![],
            })
        );

        // Test simple parallel sub-step
        input.initialize("- Parallel task");
        let result = input.read_substep_parallel();
        assert_eq!(
            result,
            Ok(Step::Parallel {
                content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "Parallel task"
                )])],
                responses: vec![],
                scopes: vec![],
            })
        );
    }

    #[test]
    fn single_step_with_dependent_substeps() {
        let mut input = Parser::new();

        input.initialize(
            r#"
1. Main step
    a. First substep
    b. Second substep
            "#,
        );
        let result = input.read_step();

        assert_eq!(
            result,
            Ok(Step::Dependent {
                ordinal: "1",
                content: vec![Descriptive::Paragraph(vec![Descriptive::Text("Main step")])],
                responses: vec![],
                scopes: vec![Scope {
                    roles: vec![],
                    substeps: vec![
                        Step::Dependent {
                            ordinal: "a",
                            content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                "First substep"
                            )])],
                            responses: vec![],
                            scopes: vec![],
                        },
                        Step::Dependent {
                            ordinal: "b",
                            content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                "Second substep"
                            )])],
                            responses: vec![],
                            scopes: vec![],
                        },
                    ],
                },],
            })
        );
    }

    #[test]
    fn single_step_with_parallel_substeps() {
        let mut input = Parser::new();

        input.initialize(
            r#"
1. Main step
    - First substep
    - Second substep
            "#,
        );
        let result = input.read_step();

        assert_eq!(
            result,
            Ok(Step::Dependent {
                ordinal: "1",
                content: vec![Descriptive::Paragraph(vec![Descriptive::Text("Main step")])],
                responses: vec![],
                scopes: vec![Scope {
                    roles: vec![],
                    substeps: vec![
                        Step::Parallel {
                            content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                "First substep"
                            )])],
                            responses: vec![],
                            scopes: vec![],
                        },
                        Step::Parallel {
                            content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                "Second substep"
                            )])],
                            responses: vec![],
                            scopes: vec![],
                        },
                    ],
                }],
            })
        );
    }

    #[test]
    fn multiple_steps_with_substeps() {
        let mut input = Parser::new();

        input.initialize(
            r#"
1. First step
    a. Substep
2. Second step
            "#,
        );
        let first_result = input.read_step();
        let second_result = input.read_step();

        assert_eq!(
            first_result,
            Ok(Step::Dependent {
                ordinal: "1",
                content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "First step"
                )])],
                responses: vec![],
                scopes: vec![Scope {
                    roles: vec![],
                    substeps: vec![Step::Dependent {
                        ordinal: "a",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text("Substep")])],
                        responses: vec![],
                        scopes: vec![],
                    }],
                }],
            })
        );

        assert_eq!(
            second_result,
            Ok(Step::Dependent {
                ordinal: "2",
                content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "Second step"
                )])],
                responses: vec![],
                scopes: vec![],
            })
        );
    }

    #[test]
    fn substeps_with_responses() {
        let mut input = Parser::new();

        input.initialize(
            r#"
1. Main step
    a. Substep with response
        'Yes' | 'No'
            "#,
        );
        let result = input.read_step();

        assert_eq!(
            result,
            Ok(Step::Dependent {
                ordinal: "1",
                content: vec![Descriptive::Paragraph(vec![Descriptive::Text("Main step")])],
                responses: vec![],
                scopes: vec![Scope {
                    roles: vec![],
                    substeps: vec![Step::Dependent {
                        ordinal: "a",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Substep with response"
                        )])],
                        responses: vec![
                            Response {
                                value: "Yes",
                                condition: None,
                            },
                            Response {
                                value: "No",
                                condition: None,
                            },
                        ],
                        scopes: vec![],
                    }],
                }],
            })
        );
    }

    #[test]
    fn is_step_with_failing_input() {
        let test_input = "1. Have you done the first thing in the first one?\n    a. Do the first thing. Then ask yourself if you are done:\n        'Yes' | 'No' but I have an excuse\n2. Do the second thing in the first one.";

        // Test each line that should be a step
        assert!(is_step(
            "1. Have you done the first thing in the first one?"
        ));
        assert!(is_step("2. Do the second thing in the first one."));

        // Test lines that should NOT be steps
        assert!(!is_step(
            "    a. Do the first thing. Then ask yourself if you are done:"
        ));
        assert!(!is_step("        'Yes' | 'No' but I have an excuse"));

        // Finally, test content over multiple lines
        assert!(is_step(test_input));
    }

    #[test]
    fn read_step_with_complete_content() {
        let mut input = Parser::new();

        input.initialize(
            r#"
1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
        );

        let result = input.read_step();

        // Should parse the complete first step with substeps
        assert_eq!(
            result,
            Ok(Step::Dependent {
                ordinal: "1",
                content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "Have you done the first thing in the first one?"
                )])],
                responses: vec![],
                scopes: vec![Scope {
                    roles: vec![],
                    substeps: vec![Step::Dependent {
                        ordinal: "a",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Do the first thing. Then ask yourself if you are done:"
                        )])],
                        responses: vec![
                            Response {
                                value: "Yes",
                                condition: None
                            },
                            Response {
                                value: "No",
                                condition: Some("but I have an excuse")
                            }
                        ],
                        scopes: vec![]
                    }],
                }],
            })
        );

        assert_eq!(
            input.entire(),
            "2. Do the second thing in the first one.\n            "
        );
    }

    #[test]
    fn read_step_with_incomplete_content() {
        let mut input = Parser::new();

        input.initialize("1. Have you done the first thing in the first one?");

        let result = input.read_step();

        // Should parse only the step line, no substeps
        assert_eq!(
            result,
            Ok(Step::Dependent {
                ordinal: "1",
                content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "Have you done the first thing in the first one?"
                )])],
                responses: vec![],
                scopes: vec![],
            })
        );

        assert_eq!(input.entire(), "");
    }

    #[test]
    fn read_procedure_step_isolation() {
        let mut input = Parser::new();

        input.initialize(
            r#"
first : A -> B

# The First

This is the first one.

1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
        );

        let result = input.read_procedure();

        // This should pass if read_procedure correctly isolates step content
        match result {
            Ok(procedure) => {
                assert_eq!(
                    procedure
                        .steps
                        .len(),
                    2
                );
            }
            Err(_e) => {
                panic!("read_procedure failed");
            }
        }
    }

    #[test]
    fn take_block_lines_with_is_step() {
        let mut input = Parser::new();

        input.initialize(
            r#"
1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
        );

        let result = input.take_block_lines(is_step, is_step, |inner| Ok(inner.entire()));

        match result {
            Ok(content) => {
                // Should isolate first step including substeps, stop at second step
                assert!(content.contains("1. Have you done"));
                assert!(content.contains("a. Do the first thing"));
                assert!(!content.contains("2. Do the second thing"));

                // Remaining should be the second step
                assert_eq!(
                    input.entire(),
                    "2. Do the second thing in the first one.\n            "
                );
            }
            Err(_) => {
                panic!("take_block_lines() failed");
            }
        }
    }

    #[test]
    fn is_step_line_by_line() {
        // Test is_step on each line of our test content
        let lines = [
            "1. Have you done the first thing in the first one?",
            "    a. Do the first thing. Then ask yourself if you are done:",
            "        'Yes' | 'No' but I have an excuse",
            "2. Do the second thing in the first one.",
        ];

        for (i, line) in lines
            .iter()
            .enumerate()
        {
            let is_step_result = is_step(line);

            match i {
                0 => assert!(is_step_result, "First step line should match is_step"),
                1 | 2 => assert!(
                    !is_step_result,
                    "Substep/response lines should NOT match is_step"
                ),
                3 => assert!(is_step_result, "Second step line should match is_step"),
                _ => {}
            }
        }
    }

    #[test]
    fn take_block_lines_title_description_pattern() {
        let mut input = Parser::new();

        // Test the exact pattern used in read_procedure for title/description extraction
        input.initialize(
            r#"
# The First

This is the first one.

1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
        );

        let result = input.take_block_lines(
            |_| true,             // start predicate (always true)
            |line| is_step(line), // end predicate (stop at first step)
            |inner| Ok(inner.entire()),
        );

        match result {
            Ok(content) => {
                // The isolated content should be title + description, stopping at first step
                assert!(content.contains("# The First"));
                assert!(content.contains("This is the first one."));
                assert!(!content.contains("1. Have you done"));

                // The remaining content should include ALL steps and substeps
                let remaining = input
                    .entire()
                    .trim_start();
                assert!(remaining.starts_with("1. Have you done"));
                assert!(remaining.contains("a. Do the first thing"));
                assert!(remaining.contains("2. Do the second thing"));
            }
            Err(_e) => {
                panic!("take_block_lines failed");
            }
        }
    }

    #[test]
    fn test_take_block_lines_procedure_wrapper() {
        let mut input = Parser::new();

        // Test the outer take_block_lines call that wraps read_procedure
        input.initialize(
            r#"
first : A -> B

# The First

This is the first one.

1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
        );

        let result = input.take_block_lines(
            is_procedure_declaration,
            is_procedure_declaration,
            |outer| Ok(outer.entire()),
        );

        match result {
            Ok(isolated_content) => {
                // Since there's only one procedure, the outer take_block_lines should capture everything
                assert!(isolated_content.contains("first : A -> B"));
                assert!(isolated_content.contains("# The First"));
                assert!(isolated_content.contains("This is the first one."));
                assert!(
                    isolated_content.contains("1. Have you done the first thing in the first one?")
                );
                assert!(isolated_content.contains("a. Do the first thing"));
                assert!(isolated_content.contains("2. Do the second thing"));
            }
            Err(_e) => {
                panic!("take_block_lines failed");
            }
        }
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
        input.initialize(
            r#"{ exec(```bash
ls -l
echo "Done"```) }"#,
        );
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
    fn test_foreach_expression() {
        let mut input = Parser::new();
        input.initialize("{ foreach item in items }");

        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Foreach(
                vec![Identifier("item")],
                Box::new(Expression::Value(Identifier("items")))
            ))
        );
    }

    #[test]
    fn foreach_tuple_pattern() {
        let mut input = Parser::new();
        input.initialize("{ foreach (design, component) in zip(designs, components) }");

        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Foreach(
                vec![Identifier("design"), Identifier("component")],
                Box::new(Expression::Execution(Function {
                    target: Identifier("zip"),
                    parameters: vec![
                        Expression::Value(Identifier("designs")),
                        Expression::Value(Identifier("components"))
                    ]
                }))
            ))
        );

        input.initialize("{ foreach (a, b, c) in zip(list1, list2, list3) }");

        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Foreach(
                vec![Identifier("a"), Identifier("b"), Identifier("c")],
                Box::new(Expression::Execution(Function {
                    target: Identifier("zip"),
                    parameters: vec![
                        Expression::Value(Identifier("list1")),
                        Expression::Value(Identifier("list2")),
                        Expression::Value(Identifier("list3"))
                    ]
                }))
            ))
        );
    }

    #[test]
    fn tuple_binding_expression() {
        let mut input = Parser::new();
        input.initialize("{ <get_coordinates>() ~ (x, y) }");

        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Binding(
                Box::new(Expression::Application(Invocation {
                    target: Target::Local(Identifier("get_coordinates")),
                    parameters: Some(vec![])
                })),
                vec![Identifier("x"), Identifier("y")]
            ))
        );
    }

    #[test]
    fn test_repeat_expression() {
        let mut input = Parser::new();
        input.initialize("{ repeat count }");

        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Repeat(Box::new(Expression::Value(Identifier(
                "count"
            )))))
        );
    }

    #[test]
    fn test_foreach_keyword_boundary() {
        // Test that "foreach" must be a complete word
        let mut input = Parser::new();
        input.initialize("{ foreachitem in items }");

        let result = input.read_code_block();
        // Should parse as identifier, not foreach
        assert_eq!(result, Ok(Expression::Value(Identifier("foreachitem"))));
    }

    #[test]
    fn test_repeat_keyword_boundary() {
        // Test that "repeat" must be a complete word
        let mut input = Parser::new();
        input.initialize("{ repeater }");

        let result = input.read_code_block();
        // Should parse as identifier, not repeat
        assert_eq!(result, Ok(Expression::Value(Identifier("repeater"))));
    }

    #[test]
    fn test_foreach_in_keyword_boundary() {
        // Test that "in" must be a complete word in foreach
        let mut input = Parser::new();
        input.initialize("{ foreach item instead items }");

        let result = input.read_code_block();
        // Should fail because "instead" doesn't match "in"
        assert!(result.is_err());
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
            validate_response(inner.source).ok_or(ParsingError::IllegalParserState(inner.offset))
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

    #[test]
    fn reading_role_assignments() {
        let mut input = Parser::new();

        // Test simple role assignment
        input.initialize("@surgeon");
        let result = input.read_role_assignments();
        assert_eq!(result, Ok(vec![Attribute::Role(Identifier("surgeon"))]));

        // Test role assignment with whitespace
        input.initialize("  @nurse  ");
        let result = input.read_role_assignments();
        assert_eq!(result, Ok(vec![Attribute::Role(Identifier("nurse"))]));

        // Test role assignment with underscores
        input.initialize("@nursing_team");
        let result = input.read_role_assignments();
        assert_eq!(
            result,
            Ok(vec![Attribute::Role(Identifier("nursing_team"))])
        );

        // Test role assignment with numbers
        input.initialize("@team1");
        let result = input.read_role_assignments();
        assert_eq!(result, Ok(vec![Attribute::Role(Identifier("team1"))]));

        // Test multiple roles with +
        input.initialize("@marketing + @sales");
        let result = input.read_role_assignments();
        assert_eq!(
            result,
            Ok(vec![
                Attribute::Role(Identifier("marketing")),
                Attribute::Role(Identifier("sales"))
            ])
        );

        // Test multiple roles with + and extra whitespace
        input.initialize("@operators + @users + @management");
        let result = input.read_role_assignments();
        assert_eq!(
            result,
            Ok(vec![
                Attribute::Role(Identifier("operators")),
                Attribute::Role(Identifier("users")),
                Attribute::Role(Identifier("management"))
            ])
        );

        // Test invalid role assignment - uppercase
        input.initialize("@Surgeon");
        let result = input.read_role_assignments();
        assert!(result.is_err());

        // Test invalid role assignment - missing @
        input.initialize("surgeon");
        let result = input.read_role_assignments();
        assert!(result.is_err());

        // Test invalid role assignment - empty
        input.initialize("@");
        let result = input.read_role_assignments();
        assert!(result.is_err());
    }

    #[test]
    fn step_with_role_assignment() {
        let mut input = Parser::new();

        // Test step with role assignment
        input.initialize(
            r#"
1. Check the patient's vital signs
        @nurse
            "#,
        );
        let result = input.read_step();

        match result {
            Ok(Step::Dependent {
                ordinal,
                content,
                responses,
                scopes,
            }) => {
                assert_eq!(ordinal, "1");
                assert_eq!(
                    content,
                    &[Descriptive::Paragraph(vec![Descriptive::Text(
                        "Check the patient's vital signs"
                    )])]
                );
                assert_eq!(responses, vec![]);
                assert_eq!(
                    scopes,
                    vec![Scope {
                        roles: vec![Attribute::Role(Identifier("nurse"))],
                        substeps: vec![],
                    }]
                );
            }
            _ => panic!("Expected dependent step with role assignment"),
        }
    }

    #[test]
    fn substep_with_role_assignment() {
        let mut input = Parser::new();

        // Test step with role assignment and substep
        input.initialize(
            r#"
1. Verify patient identity
        @surgeon
            a. Check ID
            "#,
        );
        let result = input.read_step();

        match result {
            Ok(Step::Dependent {
                ordinal,
                content,
                responses,
                scopes,
            }) => {
                assert_eq!(ordinal, "1");
                assert_eq!(
                    content,
                    vec![Descriptive::Paragraph(vec![Descriptive::Text(
                        "Verify patient identity"
                    )])]
                );
                assert_eq!(responses, vec![]);
                assert_eq!(scopes.len(), 1);
                assert_eq!(
                    scopes[0].roles,
                    vec![Attribute::Role(Identifier("surgeon"))]
                );
                assert_eq!(
                    scopes[0]
                        .substeps
                        .len(),
                    1
                );
            }
            _ => panic!("Expected dependent step with role assignment"),
        }
    }

    #[test]
    fn parallel_step_with_role_assignment() {
        let mut input = Parser::new();

        // Test step with role assignment and parallel substep
        input.initialize(
            r#"
1. Monitor patient vitals
        @nursing_team
            - Check readings
            "#,
        );
        let result = input.read_step();

        match result {
            Ok(Step::Dependent {
                ordinal,
                content,
                responses,
                scopes,
            }) => {
                assert_eq!(ordinal, "1");
                assert_eq!(
                    content,
                    vec![Descriptive::Paragraph(vec![Descriptive::Text(
                        "Monitor patient vitals"
                    )])]
                );
                assert_eq!(responses, vec![]);
                assert_eq!(scopes.len(), 1);
                assert_eq!(
                    scopes[0].roles,
                    vec![Attribute::Role(Identifier("nursing_team"))]
                );
                assert_eq!(
                    scopes[0]
                        .substeps
                        .len(),
                    1
                );
            }
            _ => panic!("Expected dependent step with role assignment"),
        }
    }

    #[test]
    fn simple_step_with_role_and_substeps() {
        let mut input = Parser::new();

        // Test a simpler case first
        input.initialize(
            r#"
1. Review events.
        @surgeon
            a. What are the steps?
            "#,
        );

        let result = input.read_step();

        match result {
            Ok(step) => {
                if let Step::Dependent { scopes, .. } = step {
                    assert_eq!(scopes.len(), 1);
                    assert_eq!(
                        scopes[0].roles,
                        vec![Attribute::Role(Identifier("surgeon"))]
                    );
                }
            }
            Err(e) => panic!(
                "Failed to parse simple step with role and substeps: {:?}",
                e
            ),
        }
    }

    #[test]
    fn two_roles_with_substeps() {
        let mut input = Parser::new();

        // Test two roles each with one substep
        input.initialize(
            r#"
1. Review events.
        @surgeon
            a. What are the steps?
        @nurse
            b. What are the concerns?
            "#,
        );

        let result = input.read_step();

        match result {
            Ok(step) => {
                if let Step::Dependent { scopes, .. } = step {
                    assert_eq!(scopes.len(), 2);

                    // First scope should have surgeon role
                    assert_eq!(
                        scopes[0].roles,
                        vec![Attribute::Role(Identifier("surgeon"))]
                    );
                    assert_eq!(
                        scopes[0]
                            .substeps
                            .len(),
                        1
                    );

                    // Second scope should have nurse role
                    assert_eq!(scopes[1].roles, vec![Attribute::Role(Identifier("nurse"))]);
                    assert_eq!(
                        scopes[1]
                            .substeps
                            .len(),
                        1
                    );
                }
            }
            Err(e) => panic!("Failed to parse two roles with substeps: {:?}", e),
        }
    }

    #[test]
    fn surgical_checklist_style_role_assignments() {
        let mut input = Parser::new();

        // Test a step that mirrors the surgical safety checklist pattern
        input.initialize(
            r#"
5. Review anticipated critical events.
        @surgeon
            a. What are the critical or non-routine steps?
            b. How long will the case take?
            c. What is the blood loss expected?
        @anaesthetist
            d. Are there any patient-specific concerns?
        @nursing_team
            e. Has sterility been confirmed?
            f. Has the equipment issues been addressed?
            "#,
        );

        let result = input.read_step();

        match result {
            Ok(Step::Dependent {
                ordinal,
                content,
                responses,
                scopes,
            }) => {
                assert_eq!(ordinal, "5");
                assert_eq!(
                    content,
                    vec![Descriptive::Paragraph(vec![Descriptive::Text(
                        "Review anticipated critical events."
                    )])]
                );
                assert_eq!(responses, vec![]);
                // Should have 3 scopes: one for each role with their substeps
                assert_eq!(scopes.len(), 3);

                // Check that the first scope has surgeon role
                assert_eq!(
                    scopes[0].roles,
                    vec![Attribute::Role(Identifier("surgeon"))]
                );
                assert_eq!(
                    scopes[0]
                        .substeps
                        .len(),
                    3
                ); // a, b, c

                // Check that the second scope has anaesthetist role
                assert_eq!(
                    scopes[1].roles,
                    vec![Attribute::Role(Identifier("anaesthetist"))]
                );
                assert_eq!(
                    scopes[1]
                        .substeps
                        .len(),
                    1
                ); // d

                // Check that the third scope has nursing_team role
                assert_eq!(
                    scopes[2].roles,
                    vec![Attribute::Role(Identifier("nursing_team"))]
                );
                assert_eq!(
                    scopes[2]
                        .substeps
                        .len(),
                    2
                ); // e, f
            }
            _ => panic!("Expected dependent step with role assignment"),
        }
    }

    #[test]
    fn simple_step_with_role_debug() {
        let mut input = Parser::new();

        // Test a simple step with role assignment
        input.initialize(
            r#"
1. Check patient vitals
        @nurse
            "#,
        );
        let result = input.read_step();

        match result {
            Ok(step) => {
                if let Step::Dependent { scopes, .. } = step {
                    assert_eq!(scopes.len(), 1);
                    assert_eq!(scopes[0].roles, vec![Attribute::Role(Identifier("nurse"))]);
                }
            }
            Err(e) => panic!("Failed to parse simple step with role: {:?}", e),
        }
    }

    #[test]
    fn role_with_dependent_substeps() {
        let mut input = Parser::new();

        // Test role assignment with multiple dependent substeps that execute in series
        input.initialize(
            r#"
1. Perform procedure
        @surgeon
            a. Make initial incision
            b. Locate target area
            c. Complete procedure
            "#,
        );

        let result = input.read_step();

        match result {
            Ok(Step::Dependent {
                ordinal,
                content,
                responses,
                scopes,
            }) => {
                assert_eq!(ordinal, "1");
                assert_eq!(
                    content,
                    vec![Descriptive::Paragraph(vec![Descriptive::Text(
                        "Perform procedure"
                    )])]
                );
                assert_eq!(responses, vec![]);
                assert_eq!(scopes.len(), 1);

                // Check that the scope has the surgeon role
                assert_eq!(
                    scopes[0].roles,
                    vec![Attribute::Role(Identifier("surgeon"))]
                );

                // Check that the scope has 3 dependent substeps in order
                assert_eq!(
                    scopes[0]
                        .substeps
                        .len(),
                    3
                );

                if let Step::Dependent {
                    ordinal, content, ..
                } = &scopes[0].substeps[0]
                {
                    assert_eq!(ordinal, &"a");
                    assert_eq!(
                        content,
                        &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Make initial incision"
                        )])]
                    );
                }

                if let Step::Dependent {
                    ordinal, content, ..
                } = &scopes[0].substeps[1]
                {
                    assert_eq!(ordinal, &"b");
                    assert_eq!(
                        content,
                        &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Locate target area"
                        )])]
                    );
                }

                if let Step::Dependent {
                    ordinal, content, ..
                } = &scopes[0].substeps[2]
                {
                    assert_eq!(ordinal, &"c");
                    assert_eq!(
                        content,
                        &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Complete procedure"
                        )])]
                    );
                }
            }
            _ => panic!("Expected dependent step with role assignment and substeps"),
        }
    }

    #[test]
    fn multiple_roles_with_dependent_substeps() {
        let mut input = Parser::new();

        // Test multiple roles each with their own dependent substeps
        input.initialize(
            r#"
1. Review surgical procedure
        @surgeon
            a. Review patient chart
            b. Verify surgical site
            c. Confirm procedure type
        @anaesthetist
            a. Check patient allergies
            b. Review medication history
        @nursing_team
            a. Prepare instruments
            b. Verify sterility
            c. Confirm patient positioning
            "#,
        );

        let result = input.read_step();

        match result {
            Ok(Step::Dependent {
                ordinal,
                content,
                responses,
                scopes,
            }) => {
                assert_eq!(ordinal, "1");
                assert_eq!(
                    content,
                    vec![Descriptive::Paragraph(vec![Descriptive::Text(
                        "Review surgical procedure"
                    )])]
                );
                assert_eq!(responses, vec![]);
                assert_eq!(scopes.len(), 3);

                // Check surgeon scope (3 dependent substeps)
                assert_eq!(
                    scopes[0].roles,
                    vec![Attribute::Role(Identifier("surgeon"))]
                );
                assert_eq!(
                    scopes[0]
                        .substeps
                        .len(),
                    3
                );

                // Check anaesthetist scope (2 dependent substeps)
                assert_eq!(
                    scopes[1].roles,
                    vec![Attribute::Role(Identifier("anaesthetist"))]
                );
                assert_eq!(
                    scopes[1]
                        .substeps
                        .len(),
                    2
                );

                // Check nursing_team scope (3 dependent substeps)
                assert_eq!(
                    scopes[2].roles,
                    vec![Attribute::Role(Identifier("nursing_team"))]
                );
                assert_eq!(
                    scopes[2]
                        .substeps
                        .len(),
                    3
                );

                // Verify all substeps are dependent (ordered) steps
                for scope in &scopes {
                    for substep in &scope.substeps {
                        assert!(matches!(substep, Step::Dependent { .. }));
                    }
                }
            }
            _ => panic!("Expected dependent step with multiple role assignments"),
        }
    }

    #[test]
    fn mixed_dependent_and_parallel_substeps_in_roles() {
        let mut input = Parser::new();

        input.initialize(
            r#"
1. Emergency response
    @team_lead
        a. Assess situation
        b. Coordinate response
            - Monitor communications
            - Track resources
        c. File report
            "#,
        );
        let result = input.read_step();

        match result {
            Ok(Step::Dependent {
                ordinal,
                content,
                responses,
                scopes,
            }) => {
                assert_eq!(ordinal, "1");
                assert_eq!(
                    content,
                    vec![Descriptive::Paragraph(vec![Descriptive::Text(
                        "Emergency response"
                    )])]
                );
                assert_eq!(responses, vec![]);
                assert_eq!(scopes.len(), 1);

                // Check team_lead scope
                assert_eq!(
                    scopes[0].roles,
                    vec![Attribute::Role(Identifier("team_lead"))]
                );
                assert_eq!(
                    scopes[0]
                        .substeps
                        .len(),
                    3
                );

                // Verify the sequence: dependent (a), dependent (b with nested parallel), dependent (c)
                assert!(matches!(scopes[0].substeps[0], Step::Dependent { .. }));
                assert!(matches!(scopes[0].substeps[1], Step::Dependent { .. }));
                assert!(matches!(scopes[0].substeps[2], Step::Dependent { .. }));

                // Check substep a
                if let Step::Dependent {
                    ordinal,
                    content,
                    scopes,
                    ..
                } = &scopes[0].substeps[0]
                {
                    assert_eq!(ordinal, &"a");
                    assert_eq!(
                        content,
                        &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Assess situation"
                        )])]
                    );
                    assert_eq!(scopes.len(), 0); // No nested scopes
                }

                // Check substep b - should have nested parallel steps
                if let Step::Dependent {
                    ordinal,
                    content,
                    scopes,
                    ..
                } = &scopes[0].substeps[1]
                {
                    assert_eq!(ordinal, &"b");
                    assert_eq!(
                        content,
                        &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Coordinate response"
                        )])]
                    );
                    assert_eq!(scopes.len(), 1); // Should have nested scope with parallel steps

                    // Check the nested parallel steps
                    assert_eq!(
                        scopes[0]
                            .substeps
                            .len(),
                        2
                    );
                    assert!(matches!(scopes[0].substeps[0], Step::Parallel { .. }));
                    assert!(matches!(scopes[0].substeps[1], Step::Parallel { .. }));

                    if let Step::Parallel { content, .. } = &scopes[0].substeps[0] {
                        assert_eq!(
                            content,
                            &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                "Monitor communications"
                            )])]
                        );
                    }

                    if let Step::Parallel { content, .. } = &scopes[0].substeps[1] {
                        assert_eq!(
                            content,
                            &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                "Track resources"
                            )])]
                        );
                    }
                }

                // Check substep c
                if let Step::Dependent {
                    ordinal,
                    content,
                    scopes,
                    ..
                } = &scopes[0].substeps[2]
                {
                    assert_eq!(ordinal, &"c");
                    assert_eq!(
                        content,
                        &vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "File report"
                        )])]
                    );
                    assert_eq!(scopes.len(), 0); // No nested scopes
                }
            }
            _ => panic!("Expected step with mixed substep types"),
        }
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

        // Test procedure declaration with parameter names
        let content_with_params = trim(
            r#"
making_coffee(e) : Ingredients -> Coffee
            "#,
        );

        assert!(is_procedure_declaration(content_with_params));

        let content_multiple_params = trim(
            r#"
connectivity_check(e,s) : LocalEnvironment, TargetService -> NetworkHealth
            "#,
        );

        assert!(is_procedure_declaration(content_multiple_params));
    }

    #[test]
    fn multiline_signature_declaration() {
        let content = trim(
            r#"
making_coffee (b, m) :
   (Beans, Milk)
     -> Coffee

And now we will make coffee as follows...

    1. Add the beans to the machine
    2. Pour in the milk
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
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Tuple(vec![Forma("Beans"), Forma("Milk")]),
                    range: Genus::Single(Forma("Coffee"))
                }),
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![]
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
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                }),
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![]
            })
        );

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("second"),
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("C")),
                    range: Genus::Single(Forma("D"))
                }),
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![]
            })
        );
    }

    #[test]
    fn procedure_declaration_with_parameters() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
making_coffee(e) : Ingredients -> Coffee

            "#,
        ));

        let procedure = input.read_procedure();
        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("making_coffee"),
                parameters: Some(vec![Identifier("e")]),
                signature: Some(Signature {
                    domain: Genus::Single(Forma("Ingredients")),
                    range: Genus::Single(Forma("Coffee"))
                }),
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![]
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
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                }),
                title: Some("The First"),
                description: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "This is the first one."
                )])],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Do the first thing in the first one."
                        )])],
                        responses: vec![],
                        scopes: vec![]
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Do the second thing in the first one."
                        )])],
                        responses: vec![],
                        scopes: vec![]
                    }
                ]
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
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                }),
                title: Some("The First"),
                description: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "This is the first one."
                )])],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Have you done the first thing in the first one?"
                        )])],
                        responses: vec![
                            Response {
                                value: "Yes",
                                condition: None
                            },
                            Response {
                                value: "No",
                                condition: Some("but I have an excuse")
                            }
                        ],
                        scopes: vec![],
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Do the second thing in the first one."
                        )])],
                        responses: vec![],
                        scopes: vec![],
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
                parameters: None,
                signature: Some(Signature {
                    domain: Genus::Single(Forma("A")),
                    range: Genus::Single(Forma("B"))
                }),
                title: Some("The First"),
                description: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                    "This is the first one."
                )])],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Have you done the first thing in the first one?"
                        )])],
                        responses: vec![],
                        scopes: vec![Scope {
                            roles: vec![],
                            substeps: vec![Step::Dependent {
                                ordinal: "a",
                                content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                    "Do the first thing. Then ask yourself if you are done:"
                                )])],
                                responses: vec![
                                    Response {
                                        value: "Yes",
                                        condition: None
                                    },
                                    Response {
                                        value: "No",
                                        condition: Some("but I have an excuse")
                                    }
                                ],
                                scopes: vec![]
                            }]
                        }]
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Do the second thing in the first one."
                        )])],
                        responses: vec![],
                        scopes: vec![],
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
                parameters: None,
                signature: None,
                title: Some("Before induction of anaesthesia"),
                description: vec![],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![Descriptive::Paragraph(vec![
                            Descriptive::Text(
                                "Has the patient confirmed his/her identity, site, procedure,"
                            ),
                            Descriptive::Text("and consent?")
                        ])],
                        responses: vec![Response {
                            value: "Yes",
                            condition: None
                        }],
                        scopes: vec![],
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Is the site marked?"
                        )])],
                        responses: vec![
                            Response {
                                value: "Yes",
                                condition: None
                            },
                            Response {
                                value: "Not Applicable",
                                condition: None
                            }
                        ],
                        scopes: vec![],
                    },
                    Step::Dependent {
                        ordinal: "3",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Is the anaesthesia machine and medication check complete?"
                        )])],
                        responses: vec![Response {
                            value: "Yes",
                            condition: None
                        }],
                        scopes: vec![],
                    },
                    Step::Dependent {
                        ordinal: "4",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Is the pulse oximeter on the patient and functioning?"
                        )])],
                        responses: vec![Response {
                            value: "Yes",
                            condition: None
                        }],
                        scopes: vec![],
                    },
                    Step::Dependent {
                        ordinal: "5",
                        content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                            "Does the patient have a:"
                        )])],
                        responses: vec![],
                        scopes: vec![Scope {
                            roles: vec![],
                            substeps: vec![
                                Step::Parallel {
                                    content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                        "Known allergy?"
                                    )])],
                                    responses: vec![
                                        Response {
                                            value: "No",
                                            condition: None
                                        },
                                        Response {
                                            value: "Yes",
                                            condition: None
                                        }
                                    ],
                                    scopes: vec![],
                                },
                                Step::Parallel {
                                    content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                        "Difficult airway or aspiration risk?"
                                    )])],
                                    responses: vec![
                                        Response {
                                            value: "No",
                                            condition: None
                                        },
                                        Response {
                                            value: "Yes",
                                            condition: Some("and equipment/assistance available")
                                        }
                                    ],
                                    scopes: vec![],
                                },
                                Step::Parallel {
                                    content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                        "Risk of blood loss > 500 mL?"
                                    )])],
                                    responses: vec![
                                        Response {
                                            value: "No",
                                            condition: None
                                        },
                                        Response {
                                            value: "Yes",
                                            condition: Some(
                                                "and two IVs planned and fluids available"
                                            )
                                        }
                                    ],
                                    scopes: vec![],
                                }
                            ]
                        }],
                    }
                ],
            })
        );
    }

    #[test]
    fn realistic_procedure_part2() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
label_the_specimens :

    1.  Specimen labelling
                @nursing_team
                    - Label blood tests
                    - Label tissue samples
                @admin_staff
                    a. Prepare the envelopes
            "#,
        ));
        let procedure = input.read_procedure();

        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("label_the_specimens"),
                parameters: None,
                signature: None,
                title: None,
                description: vec![],
                attribute: vec![],
                steps: vec![Step::Dependent {
                    ordinal: "1",
                    content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                        "Specimen labelling"
                    )])],
                    responses: vec![],
                    scopes: vec![
                        Scope {
                            roles: vec![Attribute::Role(Identifier("nursing_team"))],
                            substeps: vec![
                                Step::Parallel {
                                    content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                        "Label blood tests"
                                    )])],
                                    responses: vec![],
                                    scopes: vec![],
                                },
                                Step::Parallel {
                                    content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                        "Label tissue samples"
                                    )])],
                                    responses: vec![],
                                    scopes: vec![],
                                }
                            ]
                        },
                        Scope {
                            roles: vec![Attribute::Role(Identifier("admin_staff"))],
                            substeps: vec![Step::Dependent {
                                ordinal: "a",
                                content: vec![Descriptive::Paragraph(vec![Descriptive::Text(
                                    "Prepare the envelopes"
                                )])],
                                responses: vec![],
                                scopes: vec![],
                            }]
                        }
                    ],
                }],
            })
        );
    }

    #[test]
    fn realistic_procedure_part3() {
        let mut input = Parser::new();
        input.initialize(trim(
            r#"
before_leaving :

# Before patient leaves operating room

    1.  Verbally confirm:
        -   The name of the surgical procedure(s).
        -   Completion of instrument, sponge, and needle counts.
        -   Specimen labelling
            { foreach specimen in specimens }
                @nursing_team
                    a.  Read specimen labels aloud, including patient
                        name.
        -   Whether there are any equipment problems to be addressed.
    2.  Post-operative care:
        @surgeon
            a.  What are the key concerns for recovery and management
                of this patient?
        @anesthetist
            b.  What are the key concerns for recovery and management
                of this patient?
        @nursing_team
            c.  What are the key concerns for recovery and management
                of this patient?
            "#,
        ));
        let procedure = input.read_procedure();

        assert_eq!(
            procedure,
            Ok(Procedure {
                name: Identifier("before_leaving"),
                parameters: None,
                signature: None,
                title: Some("Before patient leaves operating room"),
                description: vec![],
                attribute: vec![],
                steps: vec![
                    Step::Dependent {
                        ordinal: "1",
                        content: vec![
                            Descriptive::Paragraph(vec![Descriptive::Text("Verbally confirm:")])
                        ],
                        responses: vec![],
                        scopes: vec![
                            Scope {
                                roles: vec![],
                                substeps: vec![
                                    Step::Parallel {
                                        content: vec![
                                            Descriptive::Paragraph(vec![Descriptive::Text("The name of the surgical procedure(s).")])
                                        ],
                                        responses: vec![],
                                        scopes: vec![],
                                    },
                                    Step::Parallel {
                                        content: vec![
                                            Descriptive::Paragraph(vec![Descriptive::Text("Completion of instrument, sponge, and needle counts.")])
                                        ],
                                        responses: vec![],
                                        scopes: vec![],
                                    },
                                    Step::Parallel {
                                        content: vec![Descriptive::Paragraph(vec![
                                            Descriptive::Text("Specimen labelling"),
                                            Descriptive::CodeBlock(Expression::Foreach(
                                                vec![Identifier("specimen")],
                                                Box::new(Expression::Value(Identifier("specimens")))
                                            )),
                                        ])],
                                        responses: vec![],
                                        scopes: vec![],
                                    }
                                ]
                            },
                            Scope {
                                roles: vec![Attribute::Role(Identifier("nursing_team"))],
                                substeps: vec![
                                    Step::Dependent {
                                        ordinal: "a",
                                        content: vec![
                                            Descriptive::Paragraph(vec![
                                                Descriptive::Text("Read specimen labels aloud, including patient"),
                                                Descriptive::Text("name.")
                                            ])
                                        ],
                                        responses: vec![],
                                        scopes: vec![Scope {
                                            roles: vec![],
                                            substeps: vec![
                                                Step::Parallel {
                                                    content: vec![
                                                        Descriptive::Paragraph(vec![Descriptive::Text("Whether there are any equipment problems to be addressed.")])
                                                    ],
                                                    responses: vec![],
                                                    scopes: vec![],
                                                }
                                            ]
                                        }],
                                    }
                                ]
                            }
                        ],
                    },
                    Step::Dependent {
                        ordinal: "2",
                        content: vec![
                            Descriptive::Paragraph(vec![Descriptive::Text("Post-operative care:")])
                        ],
                        responses: vec![],
                        scopes: vec![
                            Scope {
                                roles: vec![Attribute::Role(Identifier("surgeon"))],
                                substeps: vec![
                                    Step::Dependent {
                                        ordinal: "a",
                                        content: vec![
                                            Descriptive::Paragraph(vec![
                                                Descriptive::Text("What are the key concerns for recovery and management"),
                                                Descriptive::Text("of this patient?")
                                            ])
                                        ],
                                        responses: vec![],
                                        scopes: vec![],
                                    }
                                ]
                            },
                            Scope {
                                roles: vec![Attribute::Role(Identifier("anesthetist"))],
                                substeps: vec![
                                    Step::Dependent {
                                        ordinal: "b",
                                        content: vec![
                                            Descriptive::Paragraph(vec![
                                                Descriptive::Text("What are the key concerns for recovery and management"),
                                                Descriptive::Text("of this patient?")
                                            ])
                                        ],
                                        responses: vec![],
                                        scopes: vec![],
                                    }
                                ]
                            },
                            Scope {
                                roles: vec![Attribute::Role(Identifier("nursing_team"))],
                                substeps: vec![
                                    Step::Dependent {
                                        ordinal: "c",
                                        content: vec![
                                            Descriptive::Paragraph(vec![
                                                Descriptive::Text("What are the key concerns for recovery and management"),
                                                Descriptive::Text("of this patient?")
                                            ])
                                        ],
                                        responses: vec![],
                                        scopes: vec![],
                                    }
                                ]
                            }
                        ],
                    }
                ],
            })
        );
    }

    #[test]
    fn naked_bindings() {
        let mut input = Parser::new();

        // Test simple naked binding: text ~ variable
        input.initialize("What is the result? ~ answer");
        let descriptive = input.read_descriptive();
        assert_eq!(
            descriptive,
            Ok(vec![Descriptive::Paragraph(vec![Descriptive::Binding(
                Box::new(Descriptive::Text("What is the result?")),
                vec![Identifier("answer")]
            )])])
        );

        // Test naked binding followed by more text. This is probably not a
        // valid usage, but it's good that it parses cleanly.
        input.initialize("Enter your name ~ name\nContinue with next step");
        let descriptive = input.read_descriptive();
        assert_eq!(
            descriptive,
            Ok(vec![Descriptive::Paragraph(vec![
                Descriptive::Binding(
                    Box::new(Descriptive::Text("Enter your name")),
                    vec![Identifier("name")]
                ),
                Descriptive::Text("Continue with next step")
            ])])
        );

        // Test mixed content with function call binding and naked binding.
        // This likewise may turn out to be something that fails compilation,
        // but it's important that it parses right so that the users gets
        // appropriate feedback.
        input.initialize("First <do_something> ~ result then describe the outcome ~ description");
        let descriptive = input.read_descriptive();
        assert_eq!(
            descriptive,
            Ok(vec![Descriptive::Paragraph(vec![
                Descriptive::Text("First"),
                Descriptive::Binding(
                    Box::new(Descriptive::Application(Invocation {
                        target: Target::Local(Identifier("do_something")),
                        parameters: None,
                    })),
                    vec![Identifier("result")]
                ),
                Descriptive::Binding(
                    Box::new(Descriptive::Text("then describe the outcome")),
                    vec![Identifier("description")]
                )
            ])])
        );
    }
}
