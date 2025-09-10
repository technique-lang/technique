use std::path::Path;

use crate::language::*;
use crate::regex::*;

// This could be adapted to return both the partial document and the errors.
// But for our purposes if the parse fails then there's no point trying to do
// deeper validation or analysis; the input syntax is broken and the user
// needs to fix it. Should a partial parse turn out have meaning then the
// return type of this can change to ParseResult<'i> but for now it is fine
// to use Result.
pub fn parse_with_recovery<'i>(
    path: &'i Path,
    content: &'i str,
) -> Result<Document<'i>, Vec<ParsingError>> {
    let mut input = Parser::new();
    input.filename(path);
    input.initialize(content);

    input.parse_collecting_errors()
}

// Most general errors first, most specific last (when removing redundant
// errors, we prefer being able to give a more specific error message to the
// user)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParsingError {
    // lowest priority
    IllegalParserState(usize),
    Unimplemented(usize),
    Unrecognized(usize), // improve this
    UnexpectedEndOfInput(usize),
    Expected(usize, &'static str),
    ExpectedMatchingChar(usize, &'static str, char, char),
    // more specific errors
    InvalidCharacter(usize, char),
    InvalidHeader(usize),
    InvalidIdentifier(usize, String),
    InvalidForma(usize),
    InvalidGenus(usize),
    InvalidSignature(usize),
    InvalidParameters(usize),
    InvalidDeclaration(usize),
    InvalidSection(usize),
    InvalidInvocation(usize),
    InvalidFunction(usize),
    InvalidCodeBlock(usize),
    InvalidStep(usize),
    InvalidSubstep(usize),
    InvalidResponse(usize),
    InvalidMultiline(usize),
    InvalidForeach(usize),
    InvalidIntegral(usize),
    InvalidQuantity(usize),
    InvalidQuantityDecimal(usize),
    InvalidQuantityUncertainty(usize),
    InvalidQuantityMagnitude(usize),
    InvalidQuantitySymbol(usize),
    // highest priority
    UnclosedInterpolation(usize),
}

impl ParsingError {
    pub fn offset(&self) -> usize {
        match self {
            ParsingError::IllegalParserState(offset) => *offset,
            ParsingError::Unimplemented(offset) => *offset,
            ParsingError::Unrecognized(offset) => *offset,
            ParsingError::Expected(offset, _) => *offset,
            ParsingError::ExpectedMatchingChar(offset, _, _, _) => *offset,
            ParsingError::UnclosedInterpolation(offset) => *offset,
            ParsingError::InvalidHeader(offset) => *offset,
            ParsingError::InvalidCharacter(offset, _) => *offset,
            ParsingError::UnexpectedEndOfInput(offset) => *offset,
            ParsingError::InvalidIdentifier(offset, _) => *offset,
            ParsingError::InvalidForma(offset) => *offset,
            ParsingError::InvalidGenus(offset) => *offset,
            ParsingError::InvalidSignature(offset) => *offset,
            ParsingError::InvalidDeclaration(offset) => *offset,
            ParsingError::InvalidParameters(offset) => *offset,
            ParsingError::InvalidSection(offset) => *offset,
            ParsingError::InvalidInvocation(offset) => *offset,
            ParsingError::InvalidFunction(offset) => *offset,
            ParsingError::InvalidCodeBlock(offset) => *offset,
            ParsingError::InvalidMultiline(offset) => *offset,
            ParsingError::InvalidStep(offset) => *offset,
            ParsingError::InvalidSubstep(offset) => *offset,
            ParsingError::InvalidForeach(offset) => *offset,
            ParsingError::InvalidResponse(offset) => *offset,
            ParsingError::InvalidIntegral(offset) => *offset,
            ParsingError::InvalidQuantity(offset) => *offset,
            ParsingError::InvalidQuantityDecimal(offset) => *offset,
            ParsingError::InvalidQuantityUncertainty(offset) => *offset,
            ParsingError::InvalidQuantityMagnitude(offset) => *offset,
            ParsingError::InvalidQuantitySymbol(offset) => *offset,
        }
    }
}

/// Remove redundant errors, keeping only the most specific error at each offset.
/// When multiple errors occur at the same offset, keep only the most specific one.
/// Since ParsingError derives Ord with general errors first and specific errors last,
/// we use > to prefer the higher Ord value (more specific) errors.
fn remove_redundant_errors(errors: Vec<ParsingError>) -> Vec<ParsingError> {
    let mut deduped = Vec::new();

    for error in errors {
        let error_offset = error.offset();

        // Check if we have an existing error at this offset
        if let Some(existing_idx) = deduped
            .iter()
            .position(|e: &ParsingError| e.offset() == error_offset)
        {
            // Keep the more specific error
            if error > deduped[existing_idx] {
                deduped[existing_idx] = error;
            }
            // Otherwise, keep the existing error
        } else {
            // No error at this offset yet, add it
            deduped.push(error);
        }
    }

    deduped
}

#[derive(Debug)]
pub struct Parser<'i> {
    filename: &'i Path,
    original: &'i str,
    source: &'i str,
    offset: usize,
    problems: Vec<ParsingError>,
}

impl<'i> Parser<'i> {
    fn new() -> Parser<'i> {
        Parser {
            filename: Path::new("-"),
            original: "",
            source: "",
            offset: 0,
            problems: Vec::new(),
        }
    }

    fn filename(&mut self, filename: &'i Path) {
        self.filename = filename;
    }
    fn initialize(&mut self, content: &'i str) {
        self.original = content;
        self.source = content;
        self.offset = 0;
        self.problems
            .clear();
    }

    fn advance(&mut self, width: usize) {
        // advance the parser position
        self.source = &self.source[width..];
        self.offset += width;
    }

    /// Skip to the beginning of the next line (or end of input). This is used
    /// when an error is encountered; we attempt to recover back to a newline
    /// as that may well be in a parent scope and we can continue.
    fn skip_to_next_line(&mut self) {
        if let Some(pos) = self
            .source
            .find('\n')
        {
            self.advance(pos + 1);
        } else {
            self.advance(
                self.source
                    .len(),
            );
        }
    }

    fn parse_collecting_errors(&mut self) -> Result<Document<'i>, Vec<ParsingError>> {
        // Clear any existing errors
        self.problems
            .clear();

        // Parse header, collecting errors if encountered
        let header = if is_magic_line(self.source) {
            match self.read_technique_header() {
                Ok(header) => Some(header),
                Err(error) => {
                    self.problems
                        .push(error);
                    None
                }
            }
        } else {
            None
        };

        // Parse zero or more procedures, handling sections if they exist
        let mut procedures = Vec::new();
        let mut sections = Vec::new();

        while !self.is_finished() {
            self.trim_whitespace();

            if self.is_finished() {
                break;
            }

            // Check if this Technique is a single set of one or more
            // top-level Scope::SectionChunk
            if is_section(self.source) && procedures.is_empty() {
                while !self.is_finished() {
                    self.trim_whitespace();
                    if self.is_finished() {
                        break;
                    }

                    if is_section(self.source) {
                        match self.read_section() {
                            Ok(section) => sections.push(section),
                            Err(error) => {
                                self.problems
                                    .push(error);
                                self.skip_to_next_line();
                            }
                        }
                    } else {
                        self.problems
                            .push(ParsingError::Unrecognized(self.offset));
                        self.skip_to_next_line();
                    }
                }
                break;
            } else if is_procedure_declaration(self.source) {
                match self.take_block_lines(
                    is_procedure_declaration,
                    |line| is_section(line) || potential_procedure_declaration(line),
                    |inner| inner.read_procedure(),
                ) {
                    Ok(mut procedure) => {
                        // Check if there are sections following this procedure
                        while !self.is_finished() {
                            self.trim_whitespace();
                            if self.is_finished() {
                                break;
                            }

                            if is_section(self.source) {
                                match self.read_section() {
                                    Ok(section) => {
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
                                    }
                                    Err(error) => {
                                        self.problems
                                            .push(error);
                                        break;
                                    }
                                }
                            } else {
                                // If we hit something that's not a section, stop parsing sections
                                break;
                            }
                        }

                        procedures.push(procedure);
                    }
                    Err(error) => {
                        self.problems
                            .push(error);
                    }
                }
            } else if potential_procedure_declaration(self.source) {
                // It might be that we've encountered a malformed procedure
                // declaration, so we try parsing it anyway to get a more
                // specific error message.
                match self.take_block_lines(
                    potential_procedure_declaration,
                    |line| is_section(line) || potential_procedure_declaration(line),
                    |inner| inner.read_procedure(),
                ) {
                    Ok(procedure) => {
                        procedures.push(procedure);
                    }
                    Err(error) => {
                        self.problems
                            .push(error);
                    }
                }
            } else {
                self.problems
                    .push(ParsingError::Unrecognized(self.offset));
                self.skip_to_next_line();
            }
        }

        let body = if !sections.is_empty() {
            Some(Technique::Steps(sections))
        } else if !procedures.is_empty() {
            Some(Technique::Procedures(procedures))
        } else {
            None
        };

        let document = Document { header, body };
        let errors = std::mem::take(&mut self.problems);

        if errors.is_empty() {
            Ok(document)
        } else {
            // Remove redundant errors, keeping only the most specific error
            // at each offset
            let errors = remove_redundant_errors(errors);
            Err(errors)
        }
    }

    /// consume up to but not including newline (or end), then take newline
    fn take_line<A, F>(&mut self, f: F) -> Result<A, ParsingError>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError>,
    {
        let result = self.take_until(&['\n'], f)?;
        self.require_newline()?;
        Ok(result)
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
        let result = function(&mut parser);

        self.problems
            .extend(parser.problems);

        // Advance parser state
        self.source = &self.source[i..];
        self.offset += i;

        result
    }

    fn take_block_chars<A, F>(
        &mut self,
        subject: &'static str,
        start_char: char,
        end_char: char,
        skip_string_content: bool,
        function: F,
    ) -> Result<A, ParsingError>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError>,
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
            let mut in_string = false;

            for (i, c) in self
                .source
                .char_indices()
            {
                if !begun && c == start_char {
                    begun = true;
                    depth = 1;
                } else if begun {
                    if skip_string_content && c == '"' {
                        in_string = !in_string;
                    } else if !skip_string_content || !in_string {
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
        }

        if !begun {
            return Err(ParsingError::Expected(self.offset, "the start character"));
        }
        if l == 0 {
            return Err(ParsingError::ExpectedMatchingChar(
                self.offset,
                subject,
                start_char,
                end_char,
            ));
        }

        let block = &self.source[1..l - 1];

        let mut parser = self.subparser(1, block);

        // Pass to closure for processing
        let result = function(&mut parser)?;

        self.problems
            .extend(parser.problems);

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

        self.problems
            .extend(parser.problems);

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

        self.problems
            .extend(parser.problems);

        // Advance parser state
        self.source = &self.source[end_pos..];
        self.offset += end_pos;

        Ok(result)
    }

    fn take_split_by<A, F>(&mut self, delimiter: char, function: F) -> Result<Vec<A>, ParsingError>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError>,
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
            self.problems
                .extend(parser.problems);
        }

        // Advance parser past all consumed content
        self.advance(content.len());

        Ok(results)
    }

    fn take_paragraph<A, F>(&mut self, function: F) -> Result<A, ParsingError>
    where
        F: Fn(&mut Parser<'i>) -> Result<A, ParsingError>,
    {
        // Find the end of this paragraph (\n\n or end of input)
        let content = self.source;
        let mut i = content
            .find("\n\n")
            .unwrap_or(content.len());
        let paragraph = &content[..i];

        let mut parser = self.subparser(0, paragraph);
        let result = function(&mut parser)?;

        self.problems
            .extend(parser.problems);

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
            filename: self.filename,
            original: self.original,
            source: content,
            offset: indent + self.offset,
            problems: Vec::new(),
        };

        // and return
        parser
    }

    // because test cases and trivial single-line examples might omit an
    // ending newline, this also returns Ok if end of input is reached.
    fn require_newline(&mut self) -> Result<(), ParsingError> {
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
    fn read_magic_line(&mut self) -> Result<u8, ParsingError> {
        self.take_until(&['\n'], |inner| {
            let re = regex!(r"%\s*technique\s+v1\s*$");

            if re.is_match(inner.source) {
                Ok(1)
            } else {
                let error_offset = analyze_magic_line(inner.source);
                Err(ParsingError::InvalidHeader(inner.offset + error_offset))
            }
        })
    }

    // This one is awkward because if a SPDX line is present, then it really needs
    // to have a license, whereas the copyright part is optional.
    fn read_spdx_line(&mut self) -> Result<(Option<&'i str>, Option<&'i str>), ParsingError> {
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

    fn read_template_line(&mut self) -> Result<Option<&'i str>, ParsingError> {
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

    fn read_technique_header(&mut self) -> Result<Metadata<'i>, ParsingError> {
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

    fn read_signature(&mut self) -> Result<Signature<'i>, ParsingError> {
        let re = regex!(r"\s*(.+?)\s*->\s*(.+?)\s*$");

        let cap = match re.captures(self.source) {
            Some(c) => c,
            None => {
                let arrow_offset = analyze_malformed_signature(self.source);
                return Err(ParsingError::InvalidSignature(self.offset + arrow_offset));
            }
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
        ParsingError,
    > {
        // These capture groups use .+? to make "match more than one, but
        // lazily" so that the subsequent grabs of whitespace and the all
        // important ':' character are not absorbed.
        let re = regex!(r"(?s)^\s*(.+?)\s*:\s*(.+?)?\s*$");

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
            let before = before.trim();
            let name = validate_identifier(before).ok_or(ParsingError::InvalidIdentifier(
                self.offset,
                before.to_string(),
            ))?;

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
                    let param = validate_identifier(item.trim_ascii()).ok_or(
                        ParsingError::InvalidIdentifier(
                            self.offset,
                            item.trim_ascii()
                                .to_string(),
                        ),
                    )?;
                    params.push(param);
                }
                Some(params)
            };

            (name, parameters)
        } else {
            // Check if there are multiple words (procedure name + anything
            // else) which would indicates parameters without parentheses
            let words: Vec<&str> = text
                .trim()
                .split_whitespace()
                .collect();
            if words.len() > 1 {
                // Calculate position of first mistaken parameter-ish thing
                let first_space_pos = text
                    .find(' ')
                    .unwrap_or(0);
                let first_param_pos = text[first_space_pos..]
                    .trim_start()
                    .as_ptr() as isize
                    - text.as_ptr() as isize;
                let error_offset = self.offset + one.start() + first_param_pos as usize;
                return Err(ParsingError::InvalidParameters(error_offset));
            }

            let name = validate_identifier(text).ok_or(ParsingError::InvalidIdentifier(
                self.offset,
                text.to_string(),
            ))?;
            (name, None)
        };

        let signature = match cap.get(2) {
            Some(two) => {
                let mut inner = self.subparser(two.start(), two.as_str());
                let result = inner.read_signature()?;
                Some(result)
            }
            None => None,
        };

        Ok((name, parameters, signature))
    }

    // assumes we've set up the precondition that indeed there is a # present.
    fn parse_procedure_title(&mut self) -> Result<&'i str, ParsingError> {
        self.trim_whitespace();

        if self.peek_next_char() == Some('#') {
            let title = self.source[1..].trim_ascii();
            Ok(title)
        } else {
            // we shouldn't have invoked this unless we have a title to parse!
            Err(ParsingError::IllegalParserState(self.offset))
        }
    }

    /// Parse a procedure with error recovery - collects multiple errors instead of stopping at the first one
    fn read_procedure(&mut self) -> Result<Procedure<'i>, ParsingError> {
        // Find the procedure block boundaries
        let mut i = 0;
        let mut begun = false;

        for line in self
            .source
            .lines()
        {
            if !begun && is_procedure_declaration(line) {
                begun = true;
                i += line.len() + 1;
                continue;
            } else if begun && is_procedure_declaration(line) {
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

        // Extract the procedure block
        let block = &self.source[..i];
        let mut parser = self.subparser(0, block);

        // Parse the procedure with recovery
        let result = self.parse_procedure_block(&mut parser);

        // Collect errors from the subparser
        self.problems
            .extend(std::mem::take(&mut parser.problems));

        // Advance main parser state
        self.source = &self.source[i..];
        self.offset += i;

        result
    }

    fn parse_procedure_block(
        &mut self,
        parser: &mut Parser<'i>,
    ) -> Result<Procedure<'i>, ParsingError> {
        // Extract the declaration with recovery
        let declaration = match self.parse_declaration(parser) {
            Ok(decl) => decl,
            Err(err) => return Err(err),
        };

        // Parse procedure elements in order with recovery
        let mut elements = vec![];

        while !parser.is_finished() {
            parser.trim_whitespace();

            if parser.is_finished() {
                break;
            }

            let content = parser.source;

            if is_procedure_title(content) {
                match parser.take_block_lines(
                    |line| {
                        line.trim_ascii_start()
                            .starts_with('#')
                    },
                    |line| !line.starts_with('#'),
                    |inner| {
                        let text = inner.parse_procedure_title()?;
                        Ok(text)
                    },
                ) {
                    Ok(title) => elements.push(Element::Title(title)),
                    Err(error) => {
                        self.problems
                            .push(error);
                        parser.skip_to_next_line();
                    }
                }
            } else if is_code_block(content) {
                match parser.read_code_block() {
                    Ok(expression) => elements.push(Element::CodeBlock(expression)),
                    Err(error) => {
                        self.problems
                            .push(error);
                        parser.skip_to_next_line();
                    }
                }
            } else if is_attribute_assignment(content) {
                match parser.read_attribute_scope() {
                    Ok(attribute_block) => elements.push(Element::Steps(vec![attribute_block])),
                    Err(error) => {
                        self.problems
                            .push(error);
                        parser.skip_to_next_line();
                    }
                }
            } else if is_step(content) {
                let mut steps = vec![];
                while !parser.is_finished() && is_step(parser.source) {
                    let content = parser.source;
                    if is_step_dependent(content) {
                        match parser.read_step_dependent() {
                            Ok(step) => steps.push(step),
                            Err(error) => {
                                self.problems
                                    .push(error);
                                parser.skip_to_next_line();
                                break;
                            }
                        }
                    } else if is_step_parallel(content) {
                        match parser.read_step_parallel() {
                            Ok(step) => steps.push(step),
                            Err(error) => {
                                self.problems
                                    .push(error);
                                parser.skip_to_next_line();
                                break;
                            }
                        }
                    } else {
                        break;
                    }
                }
                if !steps.is_empty() {
                    elements.push(Element::Steps(steps));
                }
            } else if malformed_step_pattern(content) {
                // Store error but continue parsing
                self.problems
                    .push(ParsingError::InvalidStep(parser.offset));
                parser.skip_to_next_line();
            } else {
                match parser.take_block_lines(
                    |line| {
                        !is_step(line)
                            && !is_procedure_title(line)
                            && !is_code_block(line)
                            && !malformed_step_pattern(line)
                            && !is_attribute_assignment(line)
                    },
                    |line| {
                        is_step(line)
                            || is_procedure_title(line)
                            || is_code_block(line)
                            || malformed_step_pattern(line)
                            || is_attribute_assignment(line)
                    },
                    |inner| {
                        let content = inner.source;
                        if !content.is_empty() {
                            inner.read_descriptive()
                        } else {
                            Ok(vec![])
                        }
                    },
                ) {
                    Ok(description) => {
                        if !description.is_empty() {
                            elements.push(Element::Description(description));
                        }
                    }
                    Err(error) => {
                        self.problems
                            .push(error);
                        parser.skip_to_next_line();
                    }
                }
            }
        }

        Ok(Procedure {
            name: declaration.0,
            parameters: declaration.1,
            signature: declaration.2,
            elements,
        })
    }

    fn parse_declaration(
        &mut self,
        parser: &mut Parser<'i>,
    ) -> Result<
        (
            Identifier<'i>,
            Option<Vec<Identifier<'i>>>,
            Option<Signature<'i>>,
        ),
        ParsingError,
    > {
        // Find declaration block boundaries
        let mut i = 0;
        let mut begun = false;

        for line in parser
            .source
            .lines()
        {
            if !begun && is_procedure_declaration(line) {
                begun = true;
                i += line.len() + 1;
                continue;
            } else if begun && is_procedure_body(line) {
                // don't include this line
                break;
            }

            i += line.len() + 1;
        }

        if i > parser
            .source
            .len()
        {
            i -= 1;
        }

        // Extract declaration block
        let block = &parser.source[..i];
        let mut inner = parser.subparser(0, block);

        // Try to parse declaration
        match inner.parse_procedure_declaration() {
            Ok(decl) => {
                // Advance parser past declaration
                parser.source = &parser.source[i..];
                parser.offset += i;
                Ok(decl)
            }
            Err(err) => {
                // Advance parser past declaration anyway
                parser.source = &parser.source[i..];
                parser.offset += i;
                // Return the error
                Err(err)
            }
        }
    }

    fn read_section(&mut self) -> Result<Scope<'i>, ParsingError> {
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
                        match outer.read_procedure() {
                            Ok(procedure) => procedures.push(procedure),
                            Err(_err) => {
                                // Error is already collected in outer.problems
                                // Just skip adding this procedure and continue
                            }
                        }
                    } else {
                        // Skip non-procedure content line by line
                        outer.skip_to_next_line();
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
                    if is_step_dependent(outer.source) {
                        let step = outer.read_step_dependent()?;
                        steps.push(step);
                    } else if is_step_parallel(outer.source) {
                        let step = outer.read_step_parallel()?;
                        steps.push(step);
                    } else {
                        // Skip unrecognized content line by line
                        outer.skip_to_next_line();
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

    fn parse_section_header(&mut self) -> Result<(&'i str, Option<Paragraph<'i>>), ParsingError> {
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

    fn read_code_block(&mut self) -> Result<Expression<'i>, ParsingError> {
        self.take_block_chars("a code block", '{', '}', true, |outer| {
            outer.read_expression()
        })
    }

    fn read_expression(&mut self) -> Result<Expression<'i>, ParsingError> {
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
            let parts = self.take_block_chars("a string literal", '"', '"', false, |inner| {
                inner.parse_string_pieces(inner.source)
            })?;
            Ok(Expression::String(parts))
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

    fn read_foreach_expression(&mut self) -> Result<Expression<'i>, ParsingError> {
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

    fn read_identifiers(&mut self) -> Result<Vec<Identifier<'i>>, ParsingError> {
        if self
            .source
            .starts_with('(')
        {
            // Parse parenthesized list: (id1, id2, ...)
            self.take_block_chars("a list of identifiers", '(', ')', true, |outer| {
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

    fn read_repeat_expression(&mut self) -> Result<Expression<'i>, ParsingError> {
        // Parse "repeat <expression>"
        self.advance(6);
        self.trim_whitespace();

        // obviously we don't want to ultimately find nested "repeat repeat"
        // here but the compiler can sort that out later; this is still just
        // parsing.
        let expression = self.read_expression()?;

        Ok(Expression::Repeat(Box::new(expression)))
    }

    fn read_binding_expression(&mut self) -> Result<Expression<'i>, ParsingError> {
        // Parse the expression before the ~ operator
        let expression = self.take_until(&['~'], |inner| inner.read_expression())?;

        // Consume the ~ operator
        self.advance(1); // consume '~'
        self.trim_whitespace();

        let identifiers = self.read_identifiers()?;

        Ok(Expression::Binding(Box::new(expression), identifiers))
    }

    fn read_tablet_expression(&mut self) -> Result<Expression<'i>, ParsingError> {
        self.take_block_chars("a tablet", '[', ']', true, |outer| {
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

                let label =
                    outer.take_block_chars("a label", '"', '"', false, |inner| Ok(inner.source))?;

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

    fn parse_string_pieces(&mut self, raw: &'i str) -> Result<Vec<Piece<'i>>, ParsingError> {
        // Quick check: if no braces, just return a single text piece
        if !raw.contains('{') {
            return Ok(vec![Piece::Text(raw)]);
        }

        let mut pieces = Vec::new();
        let mut current_pos = 0;

        while current_pos < raw.len() {
            // Look for the start of an interpolation
            if let Some(brace_start) = raw[current_pos..].find('{') {
                let absolute_brace_start = current_pos + brace_start;

                // Add text before the brace if any
                if brace_start > 0 {
                    pieces.push(Piece::Text(&raw[current_pos..absolute_brace_start]));
                }

                // Find the matching closing brace
                let mut brace_depth = 0;
                let mut brace_end = None;

                for (i, c) in raw[absolute_brace_start..].char_indices() {
                    if c == '{' {
                        brace_depth += 1;
                    } else if c == '}' {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            brace_end = Some(absolute_brace_start + i);
                            break;
                        }
                    }
                }

                match brace_end {
                    Some(end_pos) => {
                        // Extract the content between braces
                        let expr_content = &raw[absolute_brace_start + 1..end_pos];

                        // Parse the expression using existing machinery
                        let mut parser = self.subparser(absolute_brace_start + 1, expr_content);
                        let expression = parser.read_expression()?;
                        pieces.push(Piece::Interpolation(expression));

                        current_pos = end_pos + 1;
                    }
                    None => {
                        // Unmatched brace - point to the opening brace position
                        return Err(ParsingError::UnclosedInterpolation(
                            self.offset + absolute_brace_start,
                        ));
                    }
                }
            } else {
                // No more braces - add the rest as text
                if current_pos < raw.len() {
                    pieces.push(Piece::Text(&raw[current_pos..]));
                }
                break;
            }
        }

        Ok(pieces)
    }

    /// Consume an identifier. As with the other smaller read methods, we do a
    /// general scan of the range here to get the relevant, then call the more
    /// detailed validation function to actually determine if it's a match.
    fn read_identifier(&mut self) -> Result<Identifier<'i>, ParsingError> {
        self.trim_whitespace();

        let content = self.source;

        let possible = match content.find([' ', '\t', '\n', '(', '{', ',']) {
            None => content,
            Some(i) => &content[0..i],
        };

        let identifier = validate_identifier(possible).ok_or(ParsingError::InvalidIdentifier(
            self.offset,
            possible.to_string(),
        ))?;

        self.advance(possible.len());

        Ok(identifier)
    }

    /// Parse a numeric literal (integer or quantity)
    fn read_numeric(&mut self) -> Result<Numeric<'i>, ParsingError> {
        self.trim_whitespace();

        let content = self.source;

        if is_numeric_integral(content) {
            self.read_numeric_integral()
        } else if is_numeric_quantity(content) {
            self.read_numeric_quantity()
        } else {
            Err(ParsingError::InvalidQuantity(self.offset))
        }
    }

    /// Parse a simple integral number
    fn read_numeric_integral(&mut self) -> Result<Numeric<'i>, ParsingError> {
        let content = self.source;

        if let Ok(amount) = content
            .trim_ascii()
            .parse::<i64>()
        {
            self.advance(content.len());
            Ok(Numeric::Integral(amount))
        } else {
            Err(ParsingError::InvalidIntegral(self.offset))
        }
    }

    /// Parse a scientific quantity with units
    fn read_numeric_quantity(&mut self) -> Result<Numeric<'i>, ParsingError> {
        self.trim_whitespace();

        // Parse mantissa (required)
        let mantissa = self.read_decimal_part()?;
        self.trim_whitespace();

        // Parse optional uncertainty
        let uncertainty = if self
            .source
            .starts_with('±')
            || self
                .source
                .starts_with("+/-")
        {
            if self
                .source
                .starts_with("+/-")
            {
                self.advance(3); // Skip +/- (3 bytes)
            } else {
                self.advance(2); // Skip ± (2 bytes in UTF-8)
            }
            self.trim_whitespace();
            Some(self.read_uncertainty_part()?)
        } else {
            None
        };
        self.trim_whitespace();

        // Parse optional magnitude
        let magnitude = if self
            .source
            .starts_with('×')
            || self
                .source
                .starts_with('x')
            || self
                .source
                .starts_with('*')
        {
            if self
                .source
                .starts_with('×')
            {
                self.advance(2); // Skip × (2 bytes in UTF-8)
            } else {
                self.advance(1); // Skip x or * (1 byte each)
            }
            self.trim_whitespace();
            if !self
                .source
                .starts_with("10")
            {
                return Err(ParsingError::InvalidQuantityMagnitude(self.offset));
            }
            self.advance(2); // Skip "10"

            if self
                .source
                .starts_with('^')
            {
                self.advance(1); // Skip ^
                Some(self.read_exponent_ascii()?)
            } else if let Some(exp) = self.read_exponent_superscript() {
                Some(exp)
            } else {
                return Err(ParsingError::InvalidQuantityMagnitude(self.offset));
            }
        } else {
            None
        };
        self.trim_whitespace();

        // Parse unit symbol (required) - consume everything remaining
        let symbol = self.read_units_symbol()?;

        let quantity = Quantity {
            mantissa,
            uncertainty,
            magnitude,
            symbol,
        };

        Ok(Numeric::Scientific(quantity))
    }

    fn read_decimal_part(&mut self) -> Result<crate::language::Decimal, ParsingError> {
        use crate::regex::*;
        let re = regex!(r"^-?[0-9]+(\.[0-9]+)?");

        if let Some(mat) = re.find(self.source) {
            let decimal_str = mat.as_str();
            if let Some(decimal) = crate::language::parse_decimal(decimal_str) {
                self.advance(decimal_str.len());
                Ok(decimal)
            } else {
                Err(ParsingError::InvalidQuantityDecimal(self.offset))
            }
        } else {
            Err(ParsingError::InvalidQuantityDecimal(self.offset))
        }
    }

    fn read_uncertainty_part(&mut self) -> Result<crate::language::Decimal, ParsingError> {
        use crate::regex::*;
        let re = regex!(r"^-?[0-9]+(\.[0-9]+)?");

        if let Some(mat) = re.find(self.source) {
            let decimal_str = mat.as_str();
            if let Some(decimal) = crate::language::parse_decimal(decimal_str) {
                self.advance(decimal_str.len());
                Ok(decimal)
            } else {
                Err(ParsingError::InvalidQuantityUncertainty(self.offset))
            }
        } else {
            Err(ParsingError::InvalidQuantityUncertainty(self.offset))
        }
    }

    fn read_exponent_ascii(&mut self) -> Result<i8, ParsingError> {
        use crate::regex::*;
        let re = regex!(r"^-?[0-9]+");

        if let Some(mat) = re.find(self.source) {
            let exp_str = mat.as_str();
            if let Ok(exp) = exp_str.parse::<i8>() {
                self.advance(exp_str.len());
                Ok(exp)
            } else {
                Err(ParsingError::InvalidQuantityMagnitude(self.offset))
            }
        } else {
            Err(ParsingError::InvalidQuantityMagnitude(self.offset))
        }
    }

    fn read_exponent_superscript(&mut self) -> Option<i8> {
        use crate::regex::*;
        let re = regex!(r"^[⁰¹²³⁴⁵⁶⁷⁸⁹⁻]+");

        if let Some(mat) = re.find(self.source) {
            let super_str = mat.as_str();
            let converted = crate::language::convert_superscript(super_str);
            if let Ok(exp) = converted.parse::<i8>() {
                self.advance(super_str.len());
                Some(exp)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn read_units_symbol(&mut self) -> Result<&'i str, ParsingError> {
        // Scan through each character and find the first invalid one
        let mut valid_end = 0;

        for (byte_offset, ch) in self
            .source
            .char_indices()
        {
            if ch.is_whitespace() || ch == ',' || ch == ')' {
                // Stop at whitespace, comma, or closing parameter boundary
                break;
            } else if ch.is_ascii_alphabetic() || ch == '°' || ch == '/' || ch == 'μ' {
                // Valid character
                valid_end = byte_offset + ch.len_utf8();
            } else {
                // Invalid character found - point directly at it
                return Err(ParsingError::InvalidQuantitySymbol(
                    self.offset + byte_offset,
                ));
            }
        }

        if valid_end == 0 {
            return Err(ParsingError::InvalidQuantitySymbol(self.offset));
        }

        let symbol = &self.source[..valid_end];
        self.advance(valid_end);
        Ok(symbol)
    }

    /// Parse a target like <procedure_name> or <https://example.com/proc>
    fn read_target(&mut self) -> Result<Target<'i>, ParsingError> {
        self.take_block_chars("an invocation", '<', '>', true, |inner| {
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
    fn read_invocation(&mut self) -> Result<Invocation<'i>, ParsingError> {
        let target = self.read_target()?;
        let parameters = if self.peek_next_char() == Some('(') {
            Some(self.read_parameters()?)
        } else {
            None
        };
        Ok(Invocation { target, parameters })
    }

    /// Parse top-level ordered step
    fn read_step_dependent(&mut self) -> Result<Scope<'i>, ParsingError> {
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
    fn read_step_parallel(&mut self) -> Result<Scope<'i>, ParsingError> {
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
    fn read_substep_dependent(&mut self) -> Result<Scope<'i>, ParsingError> {
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
    fn read_substep_parallel(&mut self) -> Result<Scope<'i>, ParsingError> {
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

    fn read_descriptive(&mut self) -> Result<Vec<Paragraph<'i>>, ParsingError> {
        self.take_block_lines(
            |_| true,
            |line| {
                is_step_dependent(line)
                    || is_substep_dependent(line)
                    || is_substep_parallel(line)
                    || is_subsubstep_dependent(line)
                    || is_attribute_assignment(line)
                    || is_enum_response(line)
                    || malformed_step_pattern(line)
                    || malformed_response_pattern(line)
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
                                } else if parser
                                    .source
                                    .starts_with("```")
                                {
                                    // Multiline blocks are not allowed in descriptive text
                                    return Err(ParsingError::InvalidMultiline(parser.offset));
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
                                            let content = inner
                                                .source
                                                .trim_ascii();
                                            // Check for invalid multiline patterns in text
                                            if content.contains("```") {
                                                return Err(ParsingError::InvalidMultiline(
                                                    inner.offset,
                                                ));
                                            }
                                            Ok(content)
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
    fn read_responses(&mut self) -> Result<Vec<Response<'i>>, ParsingError> {
        self.take_split_by('|', |inner| {
            validate_response(inner.source).ok_or(ParsingError::InvalidResponse(inner.offset))
        })
    }

    fn parse_multiline_content(&mut self) -> Result<(Option<&'i str>, Vec<&'i str>), ParsingError> {
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
    fn read_parameters(&mut self) -> Result<Vec<Expression<'i>>, ParsingError> {
        self.take_block_chars("parameters for a function", '(', ')', true, |outer| {
            let mut params = Vec::new();

            loop {
                outer.trim_whitespace();

                let content = outer.source;
                if content.is_empty() {
                    break;
                }

                if content.starts_with("```") {
                    let (lang, lines) = outer
                        .take_block_delimited("```", |inner| inner.parse_multiline_content())
                        .map_err(|err| match err {
                            ParsingError::Expected(offset, "the corresponding end delimiter") => {
                                ParsingError::InvalidMultiline(offset)
                            }
                            _ => err,
                        })?;
                    params.push(Expression::Multiline(lang, lines));
                } else if content.starts_with("\"") {
                    let parts =
                        outer.take_block_chars("a string literal", '"', '"', false, |inner| {
                            inner.parse_string_pieces(inner.source)
                        })?;
                    params.push(Expression::String(parts));
                } else if is_numeric(content) {
                    let numeric = outer.read_numeric()?;
                    params.push(Expression::Number(numeric));
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

    /// Trim any leading whitespace (space, tab, newline) from the front of
    /// the current parser text.
    fn trim_whitespace(&mut self) {
        if self
            .source
            .is_empty()
        {
            return;
        }

        let bytes = self
            .source
            .as_bytes();
        let mut i = 0;

        while i < bytes.len() {
            match bytes[i] {
                b' ' | b'\t' | b'\n' | b'\r' => {
                    i += 1;
                }
                _ => break,
            }
        }

        self.source = &self.source[i..];
        self.offset += i;
    }

    /// Parse attributes (roles and/or places) like @surgeon, ^kitchen, or @chef + ^bathroom
    fn read_attributes(&mut self) -> Result<Vec<Attribute<'i>>, ParsingError> {
        self.take_line(|inner| {
            let mut attributes = Vec::new();

            let line = inner.source;

            // Handle multiple attributes separated by +
            let parts: Vec<&str> = line
                .split('+')
                .collect();

            for part in parts {
                let trimmed = part.trim_ascii();

                // Check if it's a role '@'
                if let Some(captures) = regex!(r"^@([a-z][a-z0-9_]*)$").captures(trimmed) {
                    let role_name = captures
                        .get(1)
                        .ok_or(ParsingError::Expected(inner.offset, "role name after @"))?
                        .as_str();
                    let identifier = validate_identifier(role_name).ok_or(
                        ParsingError::InvalidIdentifier(inner.offset, role_name.to_string()),
                    )?;
                    attributes.push(Attribute::Role(identifier));
                }
                // Check if it's a place '^'
                else if let Some(captures) = regex!(r"^\^([a-z][a-z0-9_]*)$").captures(trimmed) {
                    let place_name = captures
                        .get(1)
                        .ok_or(ParsingError::Expected(inner.offset, "place name after ^"))?
                        .as_str();
                    let identifier = validate_identifier(place_name).ok_or(
                        ParsingError::InvalidIdentifier(inner.offset, place_name.to_string()),
                    )?;
                    attributes.push(Attribute::Place(identifier));
                } else {
                    return Err(ParsingError::InvalidStep(inner.offset));
                }
            }

            Ok(attributes)
        })
    }

    /// Parse role assignments, substeps, and code blocks, crucially with all
    /// of their subscopes also parsed.
    fn read_scopes(&mut self) -> Result<Vec<Scope<'i>>, ParsingError> {
        let mut scopes = Vec::new();

        while !self.is_finished() {
            self.trim_whitespace();
            if self.is_finished() {
                break;
            }

            let content = self.source;

            if is_attribute_assignment(content) {
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
            } else if malformed_step_pattern(content) {
                return Err(ParsingError::InvalidSubstep(self.offset));
            } else if malformed_response_pattern(content) {
                return Err(ParsingError::InvalidResponse(self.offset));
            } else if is_enum_response(content) {
                let responses = self.read_responses()?;
                scopes.push(Scope::ResponseBlock { responses });
            } else {
                break;
            }
        }
        Ok(scopes)
    }

    /// Parse an attribute block (role or place assignment) with its subscopes
    fn read_attribute_scope(&mut self) -> Result<Scope<'i>, ParsingError> {
        self.take_block_lines(is_attribute_assignment, is_attribute_assignment, |outer| {
            let attributes = outer.read_attributes()?;
            let subscopes = outer.read_scopes()?;

            Ok(Scope::AttributeBlock {
                attributes,
                subscopes,
            })
        })
    }

    /// Parse a code block scope with its subscopes (if any)
    fn read_code_scope(&mut self) -> Result<Scope<'i>, ParsingError> {
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
    content
        .trim_ascii_start()
        .starts_with('%')
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

#[allow(unused)]
fn is_signature(content: &str) -> bool {
    let re = regex!(r"\s*.+?\s*->\s*.+?\s*$");

    re.is_match(content)
}

fn analyze_malformed_signature(content: &str) -> usize {
    let mut tokens = content.split_ascii_whitespace();

    // Skip the first token
    let first_token = tokens.next();
    if first_token.is_none() {
        return 0;
    }

    // Find the second token
    if let Some(second_token) = tokens.next() {
        // Find where this token starts in the original content
        if let Some(pos) = content.find(second_token) {
            return pos;
        }
    }

    0 // fallback
}

fn analyze_magic_line(content: &str) -> usize {
    let trimmed = content.trim();

    // Point to start if doesn't begin with %
    if !trimmed.starts_with('%') {
        return 0;
    }

    // Point to where "technique" should be if missing or incorrect
    if !trimmed.contains("technique") {
        // Find position after % and skip whitespace to point to first char of wrong keyword
        if let Some(percent_pos) = content.find('%') {
            let after_percent = percent_pos + 1;
            let remaining = &content[after_percent..];
            for (i, ch) in remaining.char_indices() {
                if !ch.is_whitespace() {
                    return after_percent + i;
                }
            }
            return after_percent;
        }
        return 0;
    }

    // If both "technique" and "v1" are present but still invalid (like "v1.0"),
    // point to the character immediately after "v1"
    if trimmed.contains("technique") && trimmed.contains("v1") {
        if let Some(v1_pos) = content.find("v1") {
            return v1_pos + 2; // Position after "v1"
        }
    }

    // Point to where version should be if missing v1
    if !trimmed.contains("v1") {
        // Find position after "technique"
        if let Some(pos) = content.find("technique") {
            let after_technique = pos + "technique".len();
            // Skip whitespace to find the actual version string
            let remaining = &content[after_technique..];
            for (i, ch) in remaining.char_indices() {
                if !ch.is_whitespace() {
                    // If we found a 'v', point to the character after it (the version number)
                    if ch == 'v' && i + 1 < remaining.len() {
                        return after_technique + i + 1;
                    }
                    // Otherwise point to where we found the non-whitespace character
                    return after_technique + i;
                }
            }
            return after_technique;
        }
    }

    // If structure is roughly correct but still invalid, point to start
    0
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

/// Correct declarations are of the form
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
///
/// This function, however, is permissive. It identifies lines that could be
/// intended as procedure declarations (including malformed ones) so that
/// proper validation and error messages can be provided during the actual
/// parsing phase.
fn is_procedure_declaration(content: &str) -> bool {
    match content.split_once(':') {
        Some((before, _after)) => {
            let before = before.trim_ascii();

            // Check if the name part is valid
            let has_valid_name = if let Some((name, params)) = before.split_once('(') {
                // Has parameters: check name is identifier and params end with ')'
                is_identifier(name.trim_ascii()) && params.ends_with(')')
            } else {
                // No parameters: just check if it's an identifier
                is_identifier(before)
            };

            // For block isolation, we only need to check the identifier part.
            // Actual signature validation happens during parsing.
            has_valid_name
        }
        None => false,
    }
}

/// Detects any line that could potentially be a procedure declaration,
/// including malformed ones. Used for detecting the end-boundary of a
/// procedure.
///
/// The specific motivating case for using this instead of the strict
/// is_procedure_declaration() is that a malformed attempted declaration like
///
/// MyProcedure :
///
/// would be consumed as part of the previous procedure's body,
/// preventing us from attempting to parse it as a separate procedure and
/// reporting what turns out to be a better error.
fn potential_procedure_declaration(content: &str) -> bool {
    match content.split_once(':') {
        Some((before, after)) => {
            let before = before.trim_ascii();

            // Empty before colon -> only a declaration if there's something after
            if before.is_empty() {
                return !after
                    .trim_ascii()
                    .is_empty();
            }

            // Has parentheses -> likely trying to be a procedure with parameters
            if before.contains('(') {
                return true;
            }

            // Check if it looks like prose vs an identifier attempt
            // Prose typically: starts with capital, has multiple space-separated words
            // Identifiers: lowercase, possibly with underscores
            let first_char = before
                .chars()
                .next()
                .unwrap();
            let has_spaces = before.contains(' ');

            // If it starts with uppercase AND has spaces, it's probably prose
            if first_char.is_uppercase() && has_spaces {
                return false;
            }

            // Otherwise, could be a procedure declaration attempt
            true
        }
        None => false,
    }
}

fn is_procedure_body(content: &str) -> bool {
    let line = content.trim_ascii();

    // Empty lines are not body content (continue reading declaration)
    if line.is_empty() {
        return false;
    }

    // Check for procedure body indicators. At the end, if it doesn't look like signature, it's body.
    is_procedure_title(content)
        || is_step(content)
        || is_attribute_assignment(content)
        || is_code_block(content)
        || is_enum_response(content)
        || (!is_signature_part(content))
}

fn is_signature_part(content: &str) -> bool {
    let line = content.trim_ascii();

    // Empty lines are part of multiline signatures
    if line.is_empty() {
        return true;
    }

    // Lines containing arrows are signature parts
    if line.contains("->") {
        return true;
    }

    // Otherwise, check if the entire line is a valid genus
    is_genus(line)
}

fn is_procedure_title(content: &str) -> bool {
    content
        .trim_ascii_start()
        .starts_with('#')
}

// I'm not sure about anchoring this one on start and end, seeing as how it
// will be used when scanning.
fn is_invocation(content: &str) -> bool {
    let re = regex!(r"^\s*<");

    re.is_match(content)
}

fn is_code_block(content: &str) -> bool {
    let re = regex!(r"^\s*\{");

    re.is_match(content)
}

#[allow(unused)]
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

/// Detect patterns that look like steps but are invalid at the top-level
fn malformed_step_pattern(content: &str) -> bool {
    let re = regex!(r"^\s*([a-zA-Z]|[ivxIVX]+)\.\s+");
    re.is_match(content)
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

fn is_enum_response(content: &str) -> bool {
    let re = regex!(r"^\s*'.+?'");
    re.is_match(content)
}

/// Detect response patterns with double quotes
fn malformed_response_pattern(content: &str) -> bool {
    let re = regex!(r#"^\s*".+?"(\s*\|\s*".+?")+\s*$"#);
    re.is_match(content)
}

fn is_numeric(content: &str) -> bool {
    is_numeric_integral(content) || is_numeric_quantity(content)
}

fn is_numeric_integral(content: &str) -> bool {
    let integral = regex!(r"^\s*-?[0-9]+(\.[0-9]+)?\s*$");
    integral.is_match(content)
}

fn is_numeric_quantity(content: &str) -> bool {
    let scientific = regex!(
        r"^\s*-?[0-9]+(\.[0-9]+)?(\s*[a-zA-Z°/μ]|\s*±|\s*\+/-|\s*×|\s*x\s*10|\s*\*\s*10|\*\s*10)"
    );
    scientific.is_match(content)
}

fn is_string_literal(content: &str) -> bool {
    let re = regex!(r#"^\s*".*"\s*$"#);
    re.is_match(content)
}

fn is_attribute_assignment(input: &str) -> bool {
    // Matches any combination of @ and ^ attributes separated by +
    let re = regex!(r"^\s*[@^][a-z][a-z0-9_]*(\s*\+\s*[@^][a-z][a-z0-9_]*)*");
    re.is_match(input)
}

// This is a rather monsterous test battery, so we move it into a separate
// file. We use the path directive to avoid the need to put it into a
// subdirectory with the same name as this module.

#[cfg(test)]
#[path = "checks/parser.rs"]
mod check;

#[cfg(test)]
#[path = "checks/verify.rs"]
mod verify;

#[cfg(test)]
#[path = "checks/errors.rs"]
mod errors;
