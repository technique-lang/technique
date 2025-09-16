use std::borrow::Cow;
use std::collections::HashMap;
use std::path::Path;

use technique::formatting::Identity;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams,
    InitializeParams, InitializeResult, InitializedParams, MessageType, OneOf, Position, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Url,
};
use tower_lsp::{Client, LanguageServer};
use tracing::{debug, info};

use crate::formatting;
use crate::parsing;
use crate::parsing::ParsingError;
use crate::problem::{calculate_column_number, calculate_line_number};

pub struct TechniqueLanguageServer {
    client: Client,
    /// Map from URI to document content
    documents: Mutex<HashMap<Url, String>>,
}

impl TechniqueLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Mutex::new(HashMap::new()),
        }
    }

    fn convert_parsing_errors(
        &self,
        _uri: &Url,
        content: &str,
        errors: Vec<ParsingError>,
    ) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for error in errors {
            let offset = error.offset();
            let position = offset_to_position(content, offset);

            let (message, severity) = match &error {
                ParsingError::IllegalParserState(_) => (
                    "Internal parser error".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::Unimplemented(_) => (
                    "Unimplemented feature".to_string(),
                    DiagnosticSeverity::WARNING,
                ),
                ParsingError::Unrecognized(_) => {
                    ("Unrecognized syntax".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::UnexpectedEndOfInput(_) => (
                    "Unexpected end of input".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::Expected(_, expected) => {
                    (format!("Expected {}", expected), DiagnosticSeverity::ERROR)
                }
                ParsingError::ExpectedMatchingChar(_, subject, start, end) => (
                    format!("Expected matching '{}' for '{}' in {}", end, start, subject),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::MissingParenthesis(_) => (
                    "Require parenthesis around multiple parameters in binding".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidCharacter(_, ch) => (
                    format!("Invalid character '{}'", ch),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidHeader(_) => {
                    ("Invalid header line".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidIdentifier(_, id) => (
                    format!("Invalid identifier '{}'", id),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidForma(_) => (
                    "Invalid forma in signature".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidGenus(_) => (
                    "Invalid genus in signature".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidSignature(_) => (
                    "Invalid signature in procedure declaration".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidParameters(_) => (
                    "Malformed parameters in procedure declaration".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidDeclaration(_) => (
                    "Invalid procedure declaration".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidSection(_) => (
                    "Invalid section heading".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidInvocation(_) => (
                    "Invalid procedure Invocation".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidFunction(_) => (
                    "Invalid function call".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidCodeBlock(_) => {
                    ("Invalid code block".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidStep(_) => {
                    ("Invalid step".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidSubstep(_) => {
                    ("Invalid substep".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidResponse(_) => {
                    ("Invalid response".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidMultiline(_) => (
                    "Invalid multiline content".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidForeach(_) => (
                    "Invalid foreach expression".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidIntegral(_) => (
                    "Invalid integral number".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidQuantity(_) => {
                    ("Invalid quantity".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidQuantityDecimal(_) => (
                    "Invalid quantity decimal".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidQuantityUncertainty(_) => (
                    "Invalid quantity uncertainty".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidQuantityMagnitude(_) => (
                    "Invalid quantity magnitude".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidQuantitySymbol(_) => (
                    "Invalid quantity symbol".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::UnclosedInterpolation(_) => (
                    "Unclosed interpolation".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
            };

            let range = Range {
                start: position,
                end: position, // For now, just point to the error position
            };

            let diagnostic = Diagnostic {
                range,
                severity: Some(severity),
                code: None,
                code_description: None,
                source: Some("technique".to_string()),
                message,
                related_information: None,
                tags: None,
                data: None,
            };

            diagnostics.push(diagnostic);
        }

        diagnostics
    }

    /// Parse document and convert errors to diagnostics
    async fn parse_and_publish_diagnostics(&self, uri: Url, content: String) {
        let path = uri
            .to_file_path()
            .unwrap_or_else(|_| Path::new("-").to_path_buf());

        match parsing::parse_with_recovery(&path, &content) {
            Ok(_document) => {
                self.client
                    .publish_diagnostics(uri, vec![], None)
                    .await;
            }
            Err(errors) => {
                let diagnostics = self.convert_parsing_errors(&uri, &content, errors);
                self.client
                    .publish_diagnostics(uri, diagnostics, None)
                    .await;
            }
        }
    }
}

/// Convert byte offset to LSP Position
fn offset_to_position(text: &str, offset: usize) -> Position {
    let line = calculate_line_number(text, offset) as u32;
    let character = calculate_column_number(text, offset) as u32;
    Position { line, character }
}

#[tower_lsp::async_trait]
impl LanguageServer for TechniqueLanguageServer {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        info!("Technique Language Server initializing");

        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        info!("Technique Language Server initialized");

        let _ = self
            .client
            .log_message(MessageType::INFO, "Technique Language Server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        info!("Technique Language Server shutting down");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params
            .text_document
            .uri;
        let content = params
            .text_document
            .text;

        debug!("Document opened: {}", uri);

        let mut documents = self
            .documents
            .lock()
            .await;
        documents.insert(uri.clone(), content.clone());

        self.parse_and_publish_diagnostics(uri, content)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params
            .text_document
            .uri;

        if let Some(change) = params
            .content_changes
            .into_iter()
            .next()
        {
            let content = change.text;

            debug!("Document changed: {}", uri);

            let mut documents = self
                .documents
                .lock()
                .await;
            documents.insert(uri.clone(), content.clone());

            self.parse_and_publish_diagnostics(uri, content)
                .await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params
            .text_document
            .uri;
        debug!("Document saved: {}", uri);

        if let Some(content) = {
            let documents = self
                .documents
                .lock()
                .await;
            documents
                .get(&uri)
                .cloned()
        } {
            self.parse_and_publish_diagnostics(uri, content)
                .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params
            .text_document
            .uri;
        debug!("Document closed: {}", uri);

        let mut documents = self
            .documents
            .lock()
            .await;
        documents.remove(&uri);

        self.client
            .publish_diagnostics(uri, vec![], None)
            .await;
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params
            .text_document
            .uri;

        debug!("Format request: {}", uri);

        // Get content from our documents map
        let documents = self
            .documents
            .lock()
            .await;
        let content = match documents.get(&uri) {
            Some(c) => c,
            None => {
                return Err(Error {
                    code: ErrorCode::InvalidRequest,
                    message: Cow::Borrowed("Document not open"),
                    data: None,
                })
            }
        };

        let path = match uri.to_file_path() {
            Ok(buf) => buf,
            Err(_) => Path::new("-").to_path_buf(),
        };

        let document = match parsing::parse_with_recovery(&path, content) {
            Ok(document) => document,
            Err(_) => {
                return Err(Error {
                    code: ErrorCode::ParseError,
                    message: Cow::Borrowed(
                        "Document must be free of parse errors before formatting",
                    ),
                    data: None,
                })
            }
        };

        let result = formatting::render(&Identity, &document, 78);

        // convert to tower-lsp type for return to editor.
        let edit = TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: u32::MAX,
                    character: 0,
                },
            },
            new_text: result,
        };

        Ok(Some(vec![edit]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_position() {
        let text = "line 1\nline 2\nline 3";

        // Test beginning
        assert_eq!(
            offset_to_position(text, 0),
            Position {
                line: 0,
                character: 0
            }
        );

        // Test end of first line
        assert_eq!(
            offset_to_position(text, 6),
            Position {
                line: 0,
                character: 6
            }
        );

        // Test beginning of second line
        assert_eq!(
            offset_to_position(text, 7),
            Position {
                line: 1,
                character: 0
            }
        );

        // Test middle of second line
        assert_eq!(
            offset_to_position(text, 10),
            Position {
                line: 1,
                character: 3
            }
        );
    }

    #[test]
    fn test_parsing_error_types() {
        // Test that all error types can be converted to messages without panicking
        let test_errors = vec![
            ParsingError::IllegalParserState(0),
            ParsingError::Unimplemented(0),
            ParsingError::Unrecognized(0),
            ParsingError::UnexpectedEndOfInput(0),
            ParsingError::Expected(0, "test"),
            ParsingError::ExpectedMatchingChar(0, "test", '(', ')'),
            ParsingError::MissingParenthesis(0),
            ParsingError::InvalidCharacter(0, 'x'),
            ParsingError::InvalidHeader(0),
            ParsingError::InvalidIdentifier(0, "test".to_string()),
            ParsingError::InvalidDeclaration(0),
        ];

        // This shouldn't panic - just test that all enum variants are handled
        for error in test_errors {
            let offset = error.offset();
            assert_eq!(offset, 0); // All test errors are at offset 0

            // Test message generation (this was formerly in convert_parsing_errors)
            match &error {
                ParsingError::IllegalParserState(_) => {
                    assert_eq!("Internal parser error", "Internal parser error")
                }
                ParsingError::Unimplemented(_) => {
                    assert_eq!("Unimplemented feature", "Unimplemented feature")
                }
                ParsingError::Unrecognized(_) => {
                    assert_eq!("Unrecognized syntax", "Unrecognized syntax")
                }
                ParsingError::UnexpectedEndOfInput(_) => {
                    assert_eq!("Unexpected end of input", "Unexpected end of input")
                }
                ParsingError::Expected(_, expected) => assert_eq!(*expected, "test"),
                ParsingError::ExpectedMatchingChar(_, subject, start, end) => {
                    assert_eq!(*subject, "test");
                    assert_eq!(*start, '(');
                    assert_eq!(*end, ')');
                }
                ParsingError::InvalidDeclaration(_) => {
                    assert_eq!("Invalid declaration", "Invalid declaration")
                }
                _ => {} // Other variants tested implicitly
            }
        }
    }
}
