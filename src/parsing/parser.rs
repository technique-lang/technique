#![allow(dead_code)]

use crate::error::*;
use crate::language::*;
use crate::regex::*;

pub fn parse_via_taking(content: &str) -> Result<Document, TechniqueError> {
    let mut input = Parser::new();
    input.initialize(content);

    let result = input.parse_from_start();
    match result {
        Ok(technique) => Ok(technique),
        Err(error) => Err(make_error(input, error)),
    }
}

fn make_error<'i>(parser: Parser<'i>, error: ParsingError<'i>) -> TechniqueError<'i> {
    let (problem, details) = error.message();
    TechniqueError {
        problem,
        details,
        source: parser.original,
        offset: error.offset(),
        width: None,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsingError<'i> {
    IllegalParserState(usize),
    Unimplemented(usize),
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
    InvalidSection(usize),
    InvalidInvocation(usize),
    InvalidFunction(usize),
    InvalidCodeBlock(usize),
    InvalidMultiline(usize),
    InvalidStep(usize),
    InvalidForeach(usize),
    InvalidResponse(usize),
    InvalidNumeric(usize),
}

impl<'i> ParsingError<'i> {
    fn offset(&self) -> usize {
        match self {
            ParsingError::IllegalParserState(offset) => *offset,
            ParsingError::Unimplemented(offset) => *offset,
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
            ParsingError::InvalidSection(offset) => *offset,
            ParsingError::InvalidInvocation(offset) => *offset,
            ParsingError::InvalidFunction(offset) => *offset,
            ParsingError::InvalidCodeBlock(offset) => *offset,
            ParsingError::InvalidMultiline(offset) => *offset,
            ParsingError::InvalidStep(offset) => *offset,
            ParsingError::InvalidForeach(offset) => *offset,
            ParsingError::InvalidResponse(offset) => *offset,
            ParsingError::InvalidNumeric(offset) => *offset,
        }
    }

    fn message(&self) -> String {
        match self {
            ParsingError::IllegalParserState(_) => "illegal parser state".to_string(),
            ParsingError::Unimplemented(_) => "as yet unimplemented!".to_string(),
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
            ParsingError::InvalidSection(_) => "invalid section heading".to_string(),
            ParsingError::InvalidInvocation(_) => "invalid procedure invocation".to_string(),
            ParsingError::InvalidFunction(_) => "invalid function call".to_string(),
            ParsingError::InvalidCodeBlock(_) => "invalid code block".to_string(),
            ParsingError::InvalidMultiline(_) => "invalid multi-line string".to_string(),
            ParsingError::InvalidStep(_) => "invalid step".to_string(),
            ParsingError::InvalidForeach(_) => "invalid foreach loop".to_string(),
            ParsingError::InvalidResponse(_) => "invalid response literal".to_string(),
            ParsingError::InvalidNumeric(_) => "invalid numeric literal".to_string(),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'i> {
    original: &'i str,
    source: &'i str,
    offset: usize,
}

impl<'i> Parser<'i> {
    pub fn new() -> Parser<'i> {
        Parser {
            original: "",
            source: "",
            offset: 0,
        }
    }

    pub fn initialize(&mut self, content: &'i str) {
        self.original = content;
        self.source = content;
        self.offset = 0;
    }

    fn advance(&mut self, width: usize) {
        // advance the parser position
        self.source = &self.source[width..];
        self.offset += width;
    }

    fn parse_from_start(&mut self) -> Result<Document<'i>, ParsingError<'i>> {
        // Check if header is present by looking for magic line
        let header = if is_magic_line(self.source) {
            Some(self.read_technique_header()?)
        } else {
            None
        };

        // Parse zero or more procedures, handling sections if they exist
        let mut procedures = Vec::new();

        while !self.is_finished() {
            self.trim_whitespace();

            if self.is_finished() {
                break;
            }

            if is_procedure_declaration(self.source) {
                let mut procedure = self.take_block_lines(
                    is_procedure_declaration,
                    |line| is_section(line) || is_procedure_declaration(line),
                    |inner| inner.read_procedure(),
                )?;

                // Check if there are sections following this procedure
                while !self.is_finished() {
                    self.trim_whitespace();
                    if self.is_finished() {
                        break;
                    }

                    if is_section(self.source) {
                        let section = self.read_section()?;
                        if let Some(Element::Steps(ref mut steps)) = procedure
                            .elements
                            .last_mut()
                        {
                            steps.push(section);
                        } else {
                            // Create a new Steps element if one doesn't exist
                            procedure
                                .elements
                                .push(Element::Steps(vec![section]));
                        }
                    } else {
                        // If we hit something that's not a section, stop parsing sections
                        break;
                    }
                }

                procedures.push(procedure);
            } else {
                // TODO: Handle unexpected content properly
                return Err(ParsingError::Unrecognized(self.offset));
            }
        }

        let body = if procedures.is_empty() {
            None
        } else {
            Some(Technique::Procedures(procedures))
        };

        Ok(Document { header, body })
    }

    /// consume up to but not including newline (or end), then take newline
    fn take_line<A, F>(&mut self, f: F) -> Result<A, ParsingError<'i>>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError<'i>>,
    {
        let result = self.take_until(&['\n'], f);
        self.require_newline()?;
        result
    }

    #[deprecated]
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

        if start_char == end_char {
            // Simple case: same character for start and end (like X...X)
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
        } else {
            // Nesting case: different characters for start and end (like (...))
            let mut depth = 0;

            for (i, c) in self
                .source
                .char_indices()
            {
                if !begun && c == start_char {
                    begun = true;
                    depth = 1;
                } else if begun {
                    if c == start_char {
                        depth += 1;
                    } else if c == end_char {
                        depth -= 1;
                        if depth == 0 {
                            l = i + 1; // add end character
                            break;
                        }
                    }
                }
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
        let content = self.source;
        let mut results = Vec::new();

        for chunk in content.split(delimiter) {
            let trimmed = chunk.trim_ascii();
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
        self.take_until(&['\n'], |inner| {
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
        self.take_until(&['\n'], |inner| {
            let re = regex!(r"^!\s*([^;]+)(?:;\s*(?:\(c\)|\(C\)|©)\s*(.+))?$");

            let cap = re
                .captures(inner.source)
                .ok_or(ParsingError::InvalidHeader(inner.offset))?;

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
        self.take_until(&['\n'], |inner| {
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

    pub fn read_technique_header(&mut self) -> Result<Metadata<'i>, ParsingError<'i>> {
        // Process magic line
        let version = if is_magic_line(self.source) {
            let result = self.read_magic_line()?;
            self.require_newline()?;
            result
        } else {
            Err(ParsingError::Expected(0, "The % symbol"))?
        };

        // Process SPDX line
        let (license, copyright) = if is_spdx_line(self.source) {
            let result = self.read_spdx_line()?;
            self.require_newline()?;
            result
        } else {
            (None, None)
        };

        // Process template line
        let template = if is_template_line(self.source) {
            let result = self.read_template_line()?;
            self.require_newline()?;
            result
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
        let re = regex!(r"\s*(.+?)\s*->\s*(.+?)\s*$");

        let cap = match re.captures(self.source) {
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
            let list = &list[..list.len() - 1].trim_ascii();

            let parameters = if list.is_empty() {
                None
            } else {
                let mut params = Vec::new();
                for item in list.split(',') {
                    let param = validate_identifier(item.trim_ascii())
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
            let title = self.source[1..].trim_ascii();
            Ok(title)
        } else {
            // we shouldn't have invoked this unless we have a title to parse!
            Err(ParsingError::IllegalParserState(self.offset))
        }
    }

    pub fn read_procedure(&mut self) -> Result<Procedure<'i>, ParsingError<'i>> {
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

                // Parse procedure elements in order
                let mut elements = vec![];

                while !outer.is_finished() {
                    let content = outer.source;

                    if is_procedure_title(content) {
                        let title = outer.take_block_lines(
                            |line| {
                                line.trim_ascii_start()
                                    .starts_with('#')
                            },
                            |line| !line.starts_with('#'),
                            |inner| {
                                let text = inner.parse_procedure_title()?;
                                Ok(text)
                            },
                        )?;
                        elements.push(Element::Title(title));
                    } else if is_code_block(content) {
                        let expression = outer.read_code_block()?;
                        elements.push(Element::CodeBlock(expression));
                    } else if is_step(content) {
                        let mut steps = vec![];
                        while !outer.is_finished() && is_step(outer.source) {
                            let content = outer.source;
                            if is_step_dependent(content) {
                                let step = outer.read_step_dependent()?;
                                steps.push(step);
                            } else if is_step_parallel(content) {
                                let step = outer.read_step_parallel()?;
                                steps.push(step);
                            } else {
                                break;
                            }
                        }
                        if !steps.is_empty() {
                            elements.push(Element::Steps(steps));
                        }
                    } else {
                        // Handle descriptive text
                        let description = outer.take_block_lines(
                            |line| {
                                !is_step(line) && !is_procedure_title(line) && !is_code_block(line)
                            },
                            |line| is_step(line) || is_procedure_title(line) || is_code_block(line),
                            |inner| {
                                let content = inner.source;
                                if !content.is_empty() {
                                    inner.read_descriptive()
                                } else {
                                    Ok(vec![])
                                }
                            },
                        )?;
                        if !description.is_empty() {
                            elements.push(Element::Description(description));
                        }
                    }
                }

                Ok(Procedure {
                    name: declaration.0,
                    parameters: declaration.1,
                    signature: declaration.2,
                    elements,
                })
            },
        )?;

        Ok(procedure)
    }

    fn read_section(&mut self) -> Result<Scope<'i>, ParsingError<'i>> {
        self.take_block_lines(is_section, is_section, |outer| {
            // Parse the section header first
            let (numeral, title) = outer.parse_section_header()?;
            outer.require_newline()?;

            // Determine if this section contains procedures or steps
            outer.trim_whitespace();
            if outer.is_finished() {
                // Section is empty (fairly common, especially in early
                // drafting of a Technique)
                Ok(Scope::SectionChunk {
                    numeral,
                    title,
                    body: Technique::Empty,
                })
            } else if is_procedure_declaration(outer.source) {
                // Section contains procedures
                let mut procedures = Vec::new();
                while !outer.is_finished() {
                    outer.trim_whitespace();
                    if outer.is_finished() {
                        break;
                    }
                    if is_procedure_declaration(outer.source) {
                        let procedure = outer.read_procedure()?;
                        procedures.push(procedure);
                    } else {
                        // Skip non-procedure content line by line
                        if let Some(newline_pos) = outer
                            .source
                            .find('\n')
                        {
                            outer.advance(newline_pos + 1);
                        } else {
                            outer.advance(
                                outer
                                    .source
                                    .len(),
                            );
                        }
                    }
                }
                Ok(Scope::SectionChunk {
                    numeral,
                    title,
                    body: Technique::Procedures(procedures),
                })
            } else {
                // Section contains steps - parse as steps
                let mut steps = Vec::new();
                while !outer.is_finished() {
                    outer.trim_whitespace();
                    if outer.is_finished() {
                        break;
                    }

                    // Try to parse steps
                    if is_substep_dependent(outer.source) {
                        let step = outer.read_substep_dependent()?;
                        steps.push(step);
                    } else if is_substep_parallel(outer.source) {
                        let step = outer.read_substep_parallel()?;
                        steps.push(step);
                    } else {
                        // Skip unrecognized content line by line
                        if let Some(newline_pos) = outer
                            .source
                            .find('\n')
                        {
                            outer.advance(newline_pos + 1);
                        } else {
                            outer.advance(
                                outer
                                    .source
                                    .len(),
                            );
                        }
                    }
                }
                Ok(Scope::SectionChunk {
                    numeral,
                    title,
                    body: Technique::Steps(steps),
                })
            }
        })
    }

    fn parse_section_header(
        &mut self,
    ) -> Result<(&'i str, Option<Paragraph<'i>>), ParsingError<'i>> {
        self.trim_whitespace();

        // Get the current line (up to newline or end)
        let line_end = self
            .source
            .find('\n')
            .unwrap_or(
                self.source
                    .len(),
            );
        let line = &self.source[..line_end];

        // Extract roman numeral and optional title
        let re = regex!(r"^\s*([IVX]+)\.\s*(.*)$");
        let cap = re
            .captures(line)
            .ok_or(ParsingError::InvalidSection(self.offset))?;

        let numeral = match cap.get(1) {
            Some(one) => one.as_str(),
            None => return Err(ParsingError::Expected(self.offset, "section header")),
        };

        // Though section text appear as titles, they are in fact steps and so
        // their text can support the various things you can put in a
        // Descriptive. Section titles should, however, only be single line,
        // so we take the first paragraph found and error otherwise.
        let title = match cap.get(2) {
            Some(two) => {
                let text = two
                    .as_str()
                    .trim();
                if text.is_empty() {
                    Ok(None)
                } else {
                    let mut parser = self.subparser(two.start(), text);
                    let paragraphs = parser.read_descriptive()?;

                    if paragraphs.len() != 1 {
                        return Err(ParsingError::InvalidSection(self.offset));
                    }
                    let paragraph = paragraphs
                        .into_iter()
                        .next();
                    Ok(paragraph)
                }
            }
            None => Ok(None),
        }?;

        // Advance past the header line
        self.advance(line_end);
        Ok((numeral, title))
    }

    fn read_code_block(&mut self) -> Result<Expression<'i>, ParsingError<'i>> {
        self.take_block_chars('{', '}', |outer| outer.read_expression())
    }

    fn read_expression(&mut self) -> Result<Expression<'i>, ParsingError<'i>> {
        self.trim_whitespace();
        let content = self
            .source
            .trim_ascii_start();

        if is_binding(content) {
            self.read_binding_expression()
        } else if is_repeat_keyword(content) {
            self.read_repeat_expression()
        } else if is_foreach_keyword(content) {
            self.read_foreach_expression()
        } else if content.starts_with("foreach ") {
            // Malformed foreach expression
            return Err(ParsingError::InvalidForeach(self.offset));
        } else if content.starts_with('[') {
            self.read_tablet_expression()
        } else if is_numeric(content) {
            let numeric = self.read_numeric()?;
            Ok(Expression::Number(numeric))
        } else if is_string_literal(content) {
            let raw = self.take_block_chars('"', '"', |inner| Ok(inner.source))?;
            Ok(Expression::String(raw))
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
            Ok(Expression::Variable(identifier))
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
            .source
            .starts_with('(')
        {
            // Parse parenthesized list: (id1, id2, ...)
            self.take_block_chars('(', ')', |outer| {
                let mut identifiers = Vec::new();

                loop {
                    outer.trim_whitespace();

                    if outer
                        .source
                        .is_empty()
                    {
                        break;
                    }

                    let name = outer.read_identifier()?;
                    identifiers.push(name);

                    // Handle comma separation
                    outer.trim_whitespace();
                    if outer
                        .source
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

    fn read_tablet_expression(&mut self) -> Result<Expression<'i>, ParsingError<'i>> {
        self.take_block_chars('[', ']', |outer| {
            let mut pairs = Vec::new();

            loop {
                outer.trim_whitespace();

                if outer
                    .source
                    .is_empty()
                {
                    break;
                }

                // Parse quoted key
                if !outer
                    .source
                    .starts_with('"')
                {
                    return Err(ParsingError::Expected(
                        outer.offset,
                        "a string label for the field, in double-quotes",
                    ));
                }

                let label = outer.take_block_chars('"', '"', |inner| Ok(inner.source))?;

                // Skip whitespace and expect '='
                outer.trim_whitespace();
                if !outer
                    .source
                    .starts_with('=')
                {
                    return Err(ParsingError::Expected(
                        outer.offset,
                        "a '=' after the field name to indicate what value is to be assigned to it",
                    ));
                }
                outer.advance(1); // consume '='
                outer.trim_whitespace();

                // Parse value - take everything up to newline or end
                let value = outer.take_line(|inner| {
                    inner.trim_whitespace();

                    let content = inner.source;
                    if content.is_empty() {
                        return Err(ParsingError::Expected(inner.offset, "value expression"));
                    };

                    inner.read_expression()
                })?;

                pairs.push(Pair { label, value });

                // Skip any remaining whitespace/newlines
                outer.trim_whitespace();
            }

            Ok(Expression::Tablet(pairs))
        })
    }

    /// Consume an identifier. As with the other smaller read methods, we do a
    /// general scan of the range here to get the relevant, then call the more
    /// detailed validation function to actually determine if it's a match.
    fn read_identifier(&mut self) -> Result<Identifier<'i>, ParsingError<'i>> {
        self.trim_whitespace();

        let content = self.source;

        let possible = match content.find([' ', '\t', '\n', '(', '{', ',']) {
            None => content,
            Some(i) => &content[0..i],
        };

        let identifier = validate_identifier(possible)
            .ok_or(ParsingError::InvalidIdentifier(self.offset, possible))?;

        self.advance(possible.len());

        Ok(identifier)
    }

    /// Parse a numeric literal (integer or quantity)
    fn read_numeric(&mut self) -> Result<Numeric<'i>, ParsingError<'i>> {
        self.trim_whitespace();

        let content = self.source;

        // Parser is whitespace agnostic - consume entire remaining content
        // The outer take_*() methods have already isolated the numeric content
        let numeric = validate_numeric(content).ok_or(ParsingError::InvalidNumeric(self.offset))?;

        self.advance(content.len());

        Ok(numeric)
    }

    /// Parse a target like <procedure_name> or <https://example.com/proc>
    fn read_target(&mut self) -> Result<Target<'i>, ParsingError<'i>> {
        self.take_block_chars('<', '>', |inner| {
            let content = inner.source;
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

    /// Parse top-level ordered step
    pub fn read_step_dependent(&mut self) -> Result<Scope<'i>, ParsingError<'i>> {
        self.take_block_lines(is_step_dependent, is_step_dependent, |outer| {
            outer.trim_whitespace();

            // Parse ordinal
            let re = regex!(r"^\s*(\d+)\.\s+");
            let cap = re
                .captures(outer.source)
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

            // Parse scopes (role assignments and substeps)
            let scopes = outer.read_scopes()?;

            return Ok(Scope::DependentBlock {
                ordinal: number,
                description: text,
                subscopes: scopes,
            });
        })
    }

    /// Parse a top-level concurrent step
    pub fn read_step_parallel(&mut self) -> Result<Scope<'i>, ParsingError<'i>> {
        self.take_block_lines(is_step_parallel, is_step_parallel, |outer| {
            outer.trim_whitespace();

            // Parse bullet
            if !outer
                .source
                .starts_with('-')
            {
                return Err(ParsingError::IllegalParserState(outer.offset));
            }
            outer.advance(1); // skip over '-'
            outer.trim_whitespace();

            let text = outer.read_descriptive()?;

            // Parse scopes (role assignments and substeps)
            let scopes = outer.read_scopes()?;

            return Ok(Scope::ParallelBlock {
                bullet: '-',
                description: text,
                subscopes: scopes,
            });
        })
    }

    /// Parse a dependent substep (a., b., c., etc.)
    fn read_substep_dependent(&mut self) -> Result<Scope<'i>, ParsingError<'i>> {
        self.take_block_lines(
            is_substep_dependent,
            |line| is_substep_dependent(line),
            |outer| {
                let content = outer.source;
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

                // Parse scopes (role assignments and substeps)
                let scopes = outer.read_scopes()?;

                Ok(Scope::DependentBlock {
                    ordinal: letter,
                    description: text,
                    subscopes: scopes,
                })
            },
        )
    }

    /// Parse a parallel substep (-)
    fn read_substep_parallel(&mut self) -> Result<Scope<'i>, ParsingError<'i>> {
        self.take_block_lines(
            is_substep_parallel,
            |line| is_substep_parallel(line),
            |outer| {
                let re = regex!(r"^\s*-\s+");
                let zero = re
                    .find(outer.source)
                    .ok_or(ParsingError::InvalidStep(outer.offset))?;

                // Skip past the dash and space
                let l = zero.len();

                outer.advance(l);

                // Parse the remaining content
                let text = outer.read_descriptive()?;

                // Parse scopes (role assignments and substeps)
                let scopes = outer.read_scopes()?;

                Ok(Scope::ParallelBlock {
                    bullet: '-',
                    description: text,
                    subscopes: scopes,
                })
            },
        )
    }

    pub fn read_descriptive(&mut self) -> Result<Vec<Paragraph<'i>>, ParsingError<'i>> {
        self.take_block_lines(
            |_| true,
            |line| {
                is_step_dependent(line)
                    || is_substep_dependent(line)
                    || is_substep_parallel(line)
                    || is_subsubstep_dependent(line)
                    || is_role_assignment(line)
                    || is_enum_response(line)
                    || is_code_block(line)
            },
            |outer| {
                let mut results = vec![];

                while !outer.is_finished() {
                    outer.trim_whitespace();
                    if outer.is_finished() {
                        break;
                    }

                    if is_code_block(outer.source) {
                        // standalone CodeBlock wrapped in a Paragraph

                        // FIXME this needs to be promoted to a Scope::CodeBlock? Or better yet shouldnt' be here?
                        let code_block = outer.read_code_block()?;
                        results.push(Paragraph(vec![Descriptive::CodeInline(code_block)]));
                    } else {
                        // Paragraph container
                        let descriptives = outer.take_paragraph(|parser| {
                            let mut content = vec![];

                            while let Some(c) = parser.peek_next_char() {
                                parser.trim_whitespace();
                                if parser.is_finished() {
                                    break;
                                }

                                if c == '{' {
                                    let expression = parser.read_code_block()?;
                                    content.push(Descriptive::CodeInline(expression));
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
                                                .trim_ascii())
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

                        if !descriptives.is_empty() {
                            results.push(Paragraph(descriptives));
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
            validate_response(inner.source).ok_or(ParsingError::InvalidResponse(inner.offset))
        })
    }

    fn parse_multiline_content(
        &mut self,
    ) -> Result<(Option<&'i str>, Vec<&'i str>), ParsingError<'i>> {
        let mut lines: Vec<&str> = self
            .source
            .lines()
            .collect();

        if lines.is_empty() {
            return Ok((None, vec![]));
        }

        // Extract language hint from first line if present
        let first = lines[0].trim_ascii();
        let lang = if !first.is_empty() { Some(first) } else { None };
        lines.remove(0);

        let second = lines[0];

        // We let the indentation of the first line govern the rest of the block
        let indent = second.len()
            - second
                .trim_ascii_start()
                .len();

        // Trim consistent leading whitespace while preserving internal indentation
        let mut result = Vec::with_capacity(lines.len());

        for line in lines {
            // the final line with ``` will be likely shorter, irrespective of
            // anything else going on.
            let i = indent.min(line.len());

            // now grab the text after the designated indent point. We check
            // to make sure there's nothing before that point, otherwise we
            // would have truncated the user's text. That's not allowed!
            let (before, after) = line.split_at(i);
            if !before
                .trim_ascii()
                .is_empty()
            {
                return Err(ParsingError::InvalidMultiline(self.offset));
            }

            result.push(after)
        }

        // Remove trailing empty line if it's just from the closing ``` delimiter
        if !result.is_empty() && result[result.len() - 1].is_empty() {
            result.pop();
        }

        Ok((lang, result))
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

                let content = outer.source;
                if content.is_empty() {
                    break;
                }

                if content.starts_with("```") {
                    let (lang, lines) = outer
                        .take_block_delimited("```", |inner| inner.parse_multiline_content())?;
                    params.push(Expression::Multiline(lang, lines));
                } else if content.starts_with("\"") {
                    let raw = outer.take_block_chars('"', '"', |inner| Ok(inner.source))?;
                    params.push(Expression::String(raw));
                } else {
                    let name = outer.read_identifier()?;
                    params.push(Expression::Variable(name));
                }

                // Handle comma separation
                outer.trim_whitespace();
                if outer
                    .source
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
                let re = regex!(r"^\s*@([a-z][a-z0-9_]*)\s*$");
                let cap = re
                    .captures(part.trim_ascii())
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

    /// Parse role assignments, substeps, and code blocks, crucially with all
    /// of their subscopes also parsed.
    fn read_scopes(&mut self) -> Result<Vec<Scope<'i>>, ParsingError<'i>> {
        let mut scopes = Vec::new();

        while !self.is_finished() {
            self.trim_whitespace();
            if self.is_finished() {
                break;
            }

            let content = self.source;

            if is_role_assignment(content) {
                let block = self.read_attribute_scope()?;
                scopes.push(block);
            } else if is_substep_dependent(content) {
                let block = self.read_substep_dependent()?;
                scopes.push(block);
            } else if is_substep_parallel(content) {
                let block = self.read_substep_parallel()?;
                scopes.push(block);
            } else if is_step_dependent(content) {
                let block = self.read_step_dependent()?;
                scopes.push(block);
            } else if is_step_parallel(content) {
                let block = self.read_step_parallel()?;
                scopes.push(block);
            } else if is_code_block(content) {
                let block = self.read_code_scope()?;
                scopes.push(block);
            } else if is_enum_response(content) {
                let responses = self.read_responses()?;
                scopes.push(Scope::ResponseBlock { responses });
            } else {
                break;
            }
        }
        Ok(scopes)
    }

    /// Parse an attribute block (role assignment) with its subscopes
    fn read_attribute_scope(&mut self) -> Result<Scope<'i>, ParsingError<'i>> {
        self.take_block_lines(is_role_assignment, is_role_assignment, |outer| {
            let attributes = outer.read_role_assignments()?;
            let subscopes = outer.read_scopes()?;

            Ok(Scope::AttributeBlock {
                attributes,
                subscopes,
            })
        })
    }

    /// Parse a code block scope with its subscopes (if any)
    fn read_code_scope(&mut self) -> Result<Scope<'i>, ParsingError<'i>> {
        self.take_block_lines(
            is_code_block,
            |_line| {
                // Code blocks consume everything until there's no more content
                // Since they're already isolated by the parent's take_block_lines,
                // we should process all remaining content as part of this code block
                false // Never stop - consume all remaining content
            },
            |outer| {
                let code = outer.read_code_block()?;
                let subscopes = outer.read_scopes()?;

                Ok(Scope::CodeBlock {
                    expression: code,
                    subscopes,
                })
            },
        )
    }
}

fn is_magic_line(content: &str) -> bool {
    let re = regex!(r"%\s*technique");

    re.is_match(content)
}

fn is_spdx_line(content: &str) -> bool {
    content
        .trim_ascii_start()
        .starts_with('!')
}

fn is_template_line(content: &str) -> bool {
    content
        .trim_ascii_start()
        .starts_with('&')
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
    let content = content.trim_ascii();
    if content.is_empty() {
        return false;
    }

    let mut chars = content.chars();
    let first = chars
        .next()
        .unwrap();

    match first {
        '[' => {
            // List pattern? [Forma] where Forma starts with uppercase
            let re = regex!(r"^\[\s*[A-Z][A-Za-z0-9]*\s*\]$");
            re.is_match(content)
        }
        '(' => {
            // Unit Forma? ()
            if let Some(c) = chars.next() {
                if c == ')' {
                    return true;
                }
            }
            // Tuple pattern? (Forma, Forma, ...)
            let re = regex!(r"^\(\s*[A-Z][A-Za-z0-9]*(\s*,\s*[A-Z][A-Za-z0-9]*)*\s*\)$");
            re.is_match(content)
        }
        _ => {
            if content.contains(',') {
                // could be a Naked tuple? Forma, Forma, ...
                let re = regex!(r"^[A-Z][A-Za-z0-9]*(\s*,\s*[A-Z][A-Za-z0-9]*)+$");
                re.is_match(content)
            } else {
                // nope, check if it's just a simple Single Forma. Great if
                // so, otherwise the caller is going to make some choices!
                let re = regex!(r"^[A-Z][A-Za-z0-9]*$");
                re.is_match(content)
            }
        }
    }
}

/// declarations are of the form
///
/// ```text
/// name : signature
/// ```
///
/// where the name is either
///
/// ```text
/// identifier
/// ```
///
/// or
///
/// ```text
/// identifier(parameters)
/// ```
///
/// and where the optional signature is
///
/// ```text
/// genus -> genus
/// ```
///
/// as above. Crucially, it must not match within a procedure body, for
/// example it must not match " a. And now: do something" or "b. Proceed
/// with:".

fn is_procedure_declaration(content: &str) -> bool {
    match content.split_once(':') {
        Some((before, after)) => {
            let before = before.trim_ascii();
            let after = after.trim_ascii();

            // Check if the name part is valid
            let has_valid_name = if let Some((name, params)) = before.split_once('(') {
                // Has parameters: check name is identifier and params end with ')'
                is_identifier(name.trim_ascii()) && params.ends_with(')')
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
                        let domain = after[..i].trim_ascii();
                        let range = after[i + 2..].trim_ascii();

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
        .trim_ascii_start()
        .starts_with('#')
}

// I'm not sure about anchoring this one on start and end, seeing as how it
// will be used when scanning.
fn is_invocation(content: &str) -> bool {
    let re = regex!(r"^\s*(<.+?>\s*(?:\(.*?\))?)\s*$");

    re.is_match(content)
}

fn is_code_block(content: &str) -> bool {
    let re = regex!(r"^\s*\{");

    re.is_match(content)
}

fn is_code_inline(content: &str) -> bool {
    let content = content.trim_ascii_start();
    content.starts_with('{')
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

fn is_step_dependent(content: &str) -> bool {
    let re = regex!(r"^\s*\d+\.\s+");
    re.is_match(content)
}

fn is_step_parallel(content: &str) -> bool {
    let re = regex!(r"^\s*-\s+");
    re.is_match(content)
}

fn is_step(content: &str) -> bool {
    is_step_dependent(content) || is_step_parallel(content)
}

fn is_section(content: &str) -> bool {
    let re = regex!(r"^\s*([IVX]+)\.\s+");
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

fn is_numeric(content: &str) -> bool {
    let integral = regex!(r"^\s*-?[0-9]+(\.[0-9]+)?\s*$");
    let scientific = regex!(r"^\s*-?[0-9]+(\.[0-9]+)?(\s*[a-zA-Z°μ]|\s*±|\s*×|\s*x\s*10)");

    integral.is_match(content) || scientific.is_match(content)
}

fn is_string_literal(content: &str) -> bool {
    let re = regex!(r#"^\s*".*"\s*$"#);
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

        let content = r#"
    connectivity_check(e,s) : LocalEnvironment, TargetService -> NetworkHealth
                "#;

        assert!(is_procedure_declaration(content));
    }

    // At one point we had a bug where parsing was racing ahead and taking too
    // much content, which was only uncovered when we expanded to be agnostic
    // about whitespace in procedure declarations.
    #[test]
    fn multiline_declaration() {
        let content = r#"
    making_coffee (b, m) :
       (Beans, Milk)
         -> Coffee

    And now we will make coffee as follows...

        1. Add the beans to the machine
        2. Pour in the milk
                "#;

        assert!(is_procedure_declaration(content));
    }

    #[test]
    fn character_delimited_blocks() {
        let mut input = Parser::new();
        input.initialize("{ todo() }");

        let result = input.take_block_chars('{', '}', |parser| {
            let text = parser.source;
            assert_eq!(text, " todo() ");
            Ok(true)
        });
        assert_eq!(result, Ok(true));

        // this is somewhat contrived as we would not be using this to parse
        // strings (We will need to preserve whitespace inside strings when
        // we find ourselves parsing them, so subparser() won't work.
        input.initialize("XhelloX world");

        let result = input.take_block_chars('X', 'X', |parser| {
            let text = parser.source;
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
            let text = parser.source;
            assert_eq!(text, "bash\nls -l\necho hello");
            Ok(true)
        });
        assert_eq!(result, Ok(true));
        assert_eq!(input.source, "");
        assert_eq!(input.offset, 27);

        // Test with different delimiter
        input.initialize("---start\ncontent here\nmore content---end");

        let result = input.take_block_delimited("---", |parser| {
            let text = parser.source;
            assert_eq!(text, "start\ncontent here\nmore content");
            Ok(true)
        });
        assert_eq!(result, Ok(true));

        // Test with whitespace around delimiters
        input.initialize("```  hello world  ``` and now goodbye");

        let result = input.take_block_delimited("```", |parser| {
            let text = parser.source;
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
                    Expression::Variable(Identifier("name")),
                    Expression::Variable(Identifier("title")),
                    Expression::Variable(Identifier("occupation"))
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
        assert!(is_step_dependent("1. First step"));
        assert!(is_step_dependent("  1. Indented step"));
        assert!(is_step_dependent("10. Tenth step"));
        assert!(!is_step_dependent("a. Letter step"));
        assert!(!is_step_dependent("1.No space"));

        // Test dependent substeps (whitespace agnostic)
        assert!(is_substep_dependent("a. Substep"));
        assert!(is_substep_dependent("  a. Indented substep"));
        assert!(!is_substep_dependent("2. Substep can't have number"));
        assert!(!is_substep_dependent("   1. Even if it is indented"));

        // Test parallel substeps (whitespace agnostic)
        assert!(is_substep_parallel("- Parallel substep"));
        assert!(is_substep_parallel("  - Indented parallel"));
        assert!(is_substep_parallel("    - Deeper indented"));
        assert!(!is_substep_parallel("-No space")); // it's possible we may allow this in the future
        assert!(!is_substep_parallel("* Different bullet"));

        // Test top-level parallel steps
        assert!(is_step_parallel("- Top level parallel"));
        assert!(is_step_parallel("  - Indented parallel"));
        assert!(is_step("- Top level parallel")); // general step detection
        assert!(is_step("1. Numbered step"));

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
    fn read_toplevel_steps() {
        let mut input = Parser::new();

        // Test simple dependent step
        input.initialize("1. First step");
        let result = input.read_step_dependent();
        assert_eq!(
            result,
            Ok(Scope::DependentBlock {
                ordinal: "1",
                description: vec![Paragraph(vec![Descriptive::Text("First step")])],
                subscopes: vec![],
            })
        );

        // Test simple parallel step
        input.initialize(
            r#"
 - a top-level task to be one in parallel with
 - another top-level task
       "#,
        );
        let result = input.read_step_parallel();
        assert_eq!(
            result,
            Ok(Scope::ParallelBlock {
                bullet: '-',
                description: vec![Paragraph(vec![Descriptive::Text(
                    "a top-level task to be one in parallel with"
                )]),],
                subscopes: vec![],
            })
        );
        let result = input.read_step_parallel();
        assert_eq!(
            result,
            Ok(Scope::ParallelBlock {
                bullet: '-',
                description: vec![Paragraph(vec![Descriptive::Text("another top-level task")]),],
                subscopes: vec![],
            })
        );

        // Test multi-line dependent step
        input.initialize(
            r#"
    1.  Have you done the first thing in the first one?
            "#,
        );
        let result = input.read_step_dependent();
        assert_eq!(
            result,
            Ok(Scope::DependentBlock {
                ordinal: "1",
                description: vec![Paragraph(vec![Descriptive::Text(
                    "Have you done the first thing in the first one?"
                )])],
                subscopes: vec![],
            })
        );

        // Test invalid step
        input.initialize("Not a step");
        let result = input.read_step_dependent();
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
            Ok(Scope::DependentBlock {
                ordinal: "a",
                description: vec![Paragraph(vec![Descriptive::Text("First subordinate task")])],
                subscopes: vec![],
            })
        );

        // Test simple parallel sub-step
        input.initialize("- Parallel task");
        let result = input.read_substep_parallel();
        assert_eq!(
            result,
            Ok(Scope::ParallelBlock {
                bullet: '-',
                description: vec![Paragraph(vec![Descriptive::Text("Parallel task")])],
                subscopes: vec![],
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
        let result = input.read_step_dependent();

        assert_eq!(
            result,
            Ok(Scope::DependentBlock {
                ordinal: "1",
                description: vec![Paragraph(vec![Descriptive::Text("Main step")])],
                subscopes: vec![
                    Scope::DependentBlock {
                        ordinal: "a",
                        description: vec![Paragraph(vec![Descriptive::Text("First substep")])],
                        subscopes: vec![],
                    },
                    Scope::DependentBlock {
                        ordinal: "b",
                        description: vec![Paragraph(vec![Descriptive::Text("Second substep")])],
                        subscopes: vec![],
                    },
                ],
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
        let result = input.read_step_dependent();

        assert_eq!(
            result,
            Ok(Scope::DependentBlock {
                ordinal: "1",
                description: vec![Paragraph(vec![Descriptive::Text("Main step")])],
                subscopes: vec![
                    Scope::ParallelBlock {
                        bullet: '-',
                        description: vec![Paragraph(vec![Descriptive::Text("First substep")])],
                        subscopes: vec![],
                    },
                    Scope::ParallelBlock {
                        bullet: '-',
                        description: vec![Paragraph(vec![Descriptive::Text("Second substep")])],
                        subscopes: vec![],
                    },
                ],
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
        let first_result = input.read_step_dependent();
        let second_result = input.read_step_dependent();

        assert_eq!(
            first_result,
            Ok(Scope::DependentBlock {
                ordinal: "1",
                description: vec![Paragraph(vec![Descriptive::Text("First step")])],
                subscopes: vec![Scope::DependentBlock {
                    ordinal: "a",
                    description: vec![Paragraph(vec![Descriptive::Text("Substep")])],
                    subscopes: vec![],
                }],
            })
        );

        assert_eq!(
            second_result,
            Ok(Scope::DependentBlock {
                ordinal: "2",
                description: vec![Paragraph(vec![Descriptive::Text("Second step")])],
                subscopes: vec![],
            })
        );
    }

    #[test]
    fn is_step_with_failing_input() {
        let test_input = "1. Have you done the first thing in the first one?\n    a. Do the first thing. Then ask yourself if you are done:\n        'Yes' | 'No' but I have an excuse\n2. Do the second thing in the first one.";

        // Test each line that should be a step
        assert!(is_step_dependent(
            "1. Have you done the first thing in the first one?"
        ));
        assert!(is_step_dependent(
            "2. Do the second thing in the first one."
        ));

        // Test lines that should NOT be steps
        assert!(!is_step_dependent(
            "    a. Do the first thing. Then ask yourself if you are done:"
        ));
        assert!(!is_step_dependent(
            "        'Yes' | 'No' but I have an excuse"
        ));

        // Finally, test content over multiple lines
        assert!(is_step_dependent(test_input));
    }

    #[test]
    fn read_step_with_content() {
        let mut input = Parser::new();

        input.initialize(
            r#"
1. Have you done the first thing in the first one?
    a. Do the first thing. Then ask yourself if you are done:
        'Yes' | 'No' but I have an excuse
2. Do the second thing in the first one.
            "#,
        );

        let result = input.read_step_dependent();

        // Should parse the complete first step with substeps
        assert_eq!(
            result,
            Ok(Scope::DependentBlock {
                ordinal: "1",
                description: vec![Paragraph(vec![Descriptive::Text(
                    "Have you done the first thing in the first one?"
                )])],
                subscopes: vec![Scope::DependentBlock {
                    ordinal: "a",
                    description: vec![Paragraph(vec![Descriptive::Text(
                        "Do the first thing. Then ask yourself if you are done:"
                    )])],
                    subscopes: vec![Scope::ResponseBlock {
                        responses: vec![
                            Response {
                                value: "Yes",
                                condition: None
                            },
                            Response {
                                value: "No",
                                condition: Some("but I have an excuse")
                            }
                        ]
                    }]
                }],
            })
        );

        assert_eq!(
            input.source,
            "2. Do the second thing in the first one.\n            "
        );
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
                let steps = procedure
                    .elements
                    .iter()
                    .find_map(|element| match element {
                        Element::Steps(steps) => Some(steps),
                        _ => None,
                    });
                assert_eq!(
                    steps
                        .unwrap()
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

        let result = input.take_block_lines(is_step_dependent, is_step_dependent, |inner| {
            Ok(inner.source)
        });

        match result {
            Ok(content) => {
                // Should isolate first step including substeps, stop at second step
                assert!(content.contains("1. Have you done"));
                assert!(content.contains("a. Do the first thing"));
                assert!(!content.contains("2. Do the second thing"));

                // Remaining should be the second step
                assert_eq!(
                    input.source,
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
            let is_step_result = is_step_dependent(line);

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
            |_| true,                       // start predicate (always true)
            |line| is_step_dependent(line), // end predicate (stop at first step)
            |inner| Ok(inner.source),
        );

        match result {
            Ok(content) => {
                // The isolated content should be title + description, stopping at first step
                assert!(content.contains("# The First"));
                assert!(content.contains("This is the first one."));
                assert!(!content.contains("1. Have you done"));

                // The remaining content should include ALL steps and substeps
                let remaining = input
                    .source
                    .trim_ascii_start();
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
            |outer| Ok(outer.source),
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
        assert_eq!(result, Ok(Expression::Variable(Identifier("count"))));

        // Test function with simple parameter
        input.initialize("{ sum(count) }");
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Execution(Function {
                target: Identifier("sum"),
                parameters: vec![Expression::Variable(Identifier("count"))]
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
                    Expression::Variable(Identifier("apple")),
                    Expression::Variable(Identifier("banana")),
                    Expression::Variable(Identifier("chocolate"))
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
                parameters: vec![Expression::Multiline(
                    Some("bash"),
                    vec!["ls -l", "echo \"Done\""]
                )]
            }))
        );
    }

    #[test]
    fn multiline() {
        let mut input = Parser::new();

        // Test multiline with consistent indentation that should be trimmed
        input.initialize(
            r#"{ exec(```bash
        ./stuff

        if [ true ]
        then
            ./other args
        fi```) }"#,
        );
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Execution(Function {
                target: Identifier("exec"),
                parameters: vec![Expression::Multiline(
                    Some("bash"),
                    vec![
                        "./stuff",
                        "",
                        "if [ true ]",
                        "then",
                        "    ./other args",
                        "fi"
                    ]
                )]
            }))
        );

        // Test multiline without language tag
        input.initialize(
            r#"{ exec(```
ls -l
echo "Done"```) }"#,
        );
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Execution(Function {
                target: Identifier("exec"),
                parameters: vec![Expression::Multiline(None, vec!["ls -l", "echo \"Done\""])]
            }))
        );

        // Test multiline with intentional empty lines in the middle
        input.initialize(
            r#"{ exec(```shell
echo "Starting"

echo "Middle section"


echo "Ending"```) }"#,
        );
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Execution(Function {
                target: Identifier("exec"),
                parameters: vec![Expression::Multiline(
                    Some("shell"),
                    vec![
                        "echo \"Starting\"",
                        "",
                        "echo \"Middle section\"",
                        "",
                        "",
                        "echo \"Ending\""
                    ]
                )]
            }))
        );

        // Test that internal indentation relative to the base is preserved,
        // and also that nested parenthesis don't break the enclosing
        // take_block_chars() used to capture the input to the function.
        input.initialize(
            r#"{ exec(```python
    def hello():
        print("Hello")
        if True:
            print("World")

    hello()```) }"#,
        );
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Execution(Function {
                target: Identifier("exec"),
                parameters: vec![Expression::Multiline(
                    Some("python"),
                    vec![
                        "def hello():",
                        "    print(\"Hello\")",
                        "    if True:",
                        "        print(\"World\")",
                        "",
                        "hello()"
                    ]
                )]
            }))
        );

        // Test that a trailing empty line from the closing delimiter is removed
        input.initialize(
            r#"{ exec(```
echo test
```) }"#,
        );
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Execution(Function {
                target: Identifier("exec"),
                parameters: vec![Expression::Multiline(None, vec!["echo test"])]
            }))
        );

        // Test various indentation edge cases
        input.initialize(
            r#"{ exec(```yaml
  name: test
  items:
    - item1
    - item2
  config:
    enabled: true```) }"#,
        );
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Execution(Function {
                target: Identifier("exec"),
                parameters: vec![Expression::Multiline(
                    Some("yaml"),
                    vec![
                        "name: test",
                        "items:",
                        "  - item1",
                        "  - item2",
                        "config:",
                        "  enabled: true"
                    ]
                )]
            }))
        );
    }

    #[test]
    fn tablets() {
        let mut input = Parser::new();

        // Test simple single-entry tablet
        input.initialize(r#"{ ["name" = "Johannes Grammerly"] }"#);
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Tablet(vec![Pair {
                label: "name",
                value: Expression::String("Johannes Grammerly")
            }]))
        );

        // Test multiline tablet with string values
        input.initialize(
            r#"{ [
    "name" = "Alice of Chains"
    "age" = "29"
] }"#,
        );
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Tablet(vec![
                Pair {
                    label: "name",
                    value: Expression::String("Alice of Chains")
                },
                Pair {
                    label: "age",
                    value: Expression::String("29")
                }
            ]))
        );

        // Test tablet with mixed value types
        input.initialize(
            r#"{ [
    "answer" = 42
    "message" = msg
    "timestamp" = now()
] }"#,
        );
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Tablet(vec![
                Pair {
                    label: "answer",
                    value: Expression::Number(Numeric::Integral(42))
                },
                Pair {
                    label: "message",
                    value: Expression::Variable(Identifier("msg"))
                },
                Pair {
                    label: "timestamp",
                    value: Expression::Execution(Function {
                        target: Identifier("now"),
                        parameters: vec![]
                    })
                }
            ]))
        );

        // Test empty tablet
        input.initialize("{ [ ] }");
        let result = input.read_code_block();
        assert_eq!(result, Ok(Expression::Tablet(vec![])));

        // Test tablet with interpolated string values
        input.initialize(
            r#"{ [
    "context" = "Details about the thing"
    "status" = active
] }"#,
        );
        let result = input.read_code_block();
        assert_eq!(
            result,
            Ok(Expression::Tablet(vec![
                Pair {
                    label: "context",
                    value: Expression::String("Details about the thing")
                },
                Pair {
                    label: "status",
                    value: Expression::Variable(Identifier("active"))
                }
            ]))
        );
    }

    #[test]
    fn numeric_literals() {
        let mut input = Parser::new();

        // Test simple integer
        input.initialize("{ 42 }");
        let result = input.read_code_block();
        assert_eq!(result, Ok(Expression::Number(Numeric::Integral(42))));

        // Test negative integer
        input.initialize("{ -123 }");
        let result = input.read_code_block();
        assert_eq!(result, Ok(Expression::Number(Numeric::Integral(-123))));

        // Test zero
        input.initialize("{ 0 }");
        let result = input.read_code_block();
        assert_eq!(result, Ok(Expression::Number(Numeric::Integral(0))));
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
                Box::new(Expression::Variable(Identifier("items")))
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
                        Expression::Variable(Identifier("designs")),
                        Expression::Variable(Identifier("components"))
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
                        Expression::Variable(Identifier("list1")),
                        Expression::Variable(Identifier("list2")),
                        Expression::Variable(Identifier("list3"))
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
            Ok(Expression::Repeat(Box::new(Expression::Variable(
                Identifier("count")
            ))))
        );
    }

    #[test]
    fn test_foreach_keyword_boundary() {
        // Test that "foreach" must be a complete word
        let mut input = Parser::new();
        input.initialize("{ foreachitem in items }");

        let result = input.read_code_block();
        // Should parse as identifier, not foreach
        assert_eq!(result, Ok(Expression::Variable(Identifier("foreachitem"))));
    }

    #[test]
    fn test_repeat_keyword_boundary() {
        // Test that "repeat" must be a complete word
        let mut input = Parser::new();
        input.initialize("{ repeater }");

        let result = input.read_code_block();
        // Should parse as identifier, not repeat
        assert_eq!(result, Ok(Expression::Variable(Identifier("repeater"))));
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
        let result = input.read_step_dependent();

        let scope = result.expect("Expected dependent step with role assignment");

        assert_eq!(
            scope,
            Scope::DependentBlock {
                ordinal: "1",

                description: vec![Paragraph(vec![Descriptive::Text(
                    "Check the patient's vital signs"
                )])],
                subscopes: vec![Scope::AttributeBlock {
                    attributes: vec![Attribute::Role(Identifier("nurse"))],
                    subscopes: vec![],
                }]
            }
        );
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
        let result = input.read_step_dependent();

        let scope = result.expect("Expected dependent step with role assignment");

        assert_eq!(
            scope,
            Scope::DependentBlock {
                ordinal: "1",
                description: vec![Paragraph(vec![Descriptive::Text(
                    "Verify patient identity"
                )])],
                subscopes: vec![Scope::AttributeBlock {
                    attributes: vec![Attribute::Role(Identifier("surgeon"))],
                    subscopes: vec![Scope::DependentBlock {
                        ordinal: "a",
                        description: vec![Paragraph(vec![Descriptive::Text("Check ID")])],
                        subscopes: vec![]
                    }]
                }]
            }
        );
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
        let result = input.read_step_dependent();

        let scope = result.expect("Expected dependent step with role assignment");

        assert_eq!(
            scope,
            Scope::DependentBlock {
                ordinal: "1",
                description: vec![Paragraph(vec![Descriptive::Text("Monitor patient vitals")])],
                subscopes: vec![Scope::AttributeBlock {
                    attributes: vec![Attribute::Role(Identifier("nursing_team"))],
                    subscopes: vec![Scope::ParallelBlock {
                        bullet: '-',
                        description: vec![Paragraph(vec![Descriptive::Text("Check readings")])],
                        subscopes: vec![]
                    }]
                }]
            }
        );
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

        let result = input.read_step_dependent();

        let scope = result.expect("Failed to parse two roles with substeps");

        assert_eq!(
            scope,
            Scope::DependentBlock {
                ordinal: "1",
                description: vec![Paragraph(vec![Descriptive::Text("Review events.")])],
                subscopes: vec![
                    Scope::AttributeBlock {
                        attributes: vec![Attribute::Role(Identifier("surgeon"))],
                        subscopes: vec![Scope::DependentBlock {
                            ordinal: "a",
                            description: vec![Paragraph(vec![Descriptive::Text(
                                "What are the steps?"
                            )])],
                            subscopes: vec![]
                        }]
                    },
                    Scope::AttributeBlock {
                        attributes: vec![Attribute::Role(Identifier("nurse"))],
                        subscopes: vec![Scope::DependentBlock {
                            ordinal: "b",
                            description: vec![Paragraph(vec![Descriptive::Text(
                                "What are the concerns?"
                            )])],
                            subscopes: vec![]
                        }]
                    }
                ]
            }
        );
    }
}
