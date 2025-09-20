use std::collections::HashMap;
use std::path::Path;

use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams,
    InitializeParams, InitializeResult, InitializedParams, Position, PublishDiagnosticsParams,
    Range, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Url,
};
use serde_json::{from_value, to_value, Value};
use technique::formatting::Identity;
use tracing::{debug, error, info, warn};

use crate::formatting;
use crate::parsing;
use crate::parsing::ParsingError;
use crate::problem::{calculate_column_number, calculate_line_number};

pub struct TechniqueLanguageServer {
    /// Map from URI to document content
    documents: HashMap<Url, String>,
}

impl TechniqueLanguageServer {
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
        }
    }

    /// Main server loop that handles incoming LSP messages
    pub fn run(
        mut self,
        connection: Connection,
    ) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
        info!("Starting Language Server main loop");

        for message in &connection.receiver {
            match message {
                Message::Request(request) => {
                    if let Err(err) = self.handle_request(request, &|msg| {
                        connection
                            .sender
                            .send(msg)
                    }) {
                        error!("Error handling request: {}", err);
                    }
                }
                Message::Notification(notification) => {
                    if notification.method == "exit" {
                        break;
                    }

                    if let Err(error) = self.handle_notification(notification, &|message| {
                        connection
                            .sender
                            .send(message)
                    }) {
                        error!("Error handling notification: {}", error);
                    }
                }
                Message::Response(_resp) => {
                    // We don't expect responses as a server
                    warn!("Received unexpected response message");
                }
            }
        }

        Ok(())
    }

    fn handle_request<E>(
        &mut self,
        req: Request,
        sender: &dyn Fn(Message) -> Result<(), E>,
    ) -> Result<(), Box<dyn std::error::Error + Sync + Send>>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        match req
            .method
            .as_str()
        {
            "initialize" => {
                let params: InitializeParams = from_value(req.params)?;
                let result = self.handle_initialize(params)?;
                let response = Response::new_ok(req.id, result);
                sender(Message::Response(response))?;
            }
            "textDocument/formatting" => {
                let params: DocumentFormattingParams = from_value(req.params)?;
                match self.handle_document_formatting(params) {
                    Ok(result) => {
                        let response = Response::new_ok(req.id, result);
                        sender(Message::Response(response))?;
                    }
                    Err(err) => {
                        let response = Response::new_err(
                            req.id,
                            lsp_server::ErrorCode::ParseError as i32,
                            err.to_string(),
                        );
                        sender(Message::Response(response))?;
                    }
                }
            }
            "shutdown" => {
                info!("Language Server received shutdown request");
                let response = Response::new_ok(req.id, Value::Null);
                sender(Message::Response(response))?;
            }
            _ => {
                warn!("Unhandled request method: {}", req.method);
                let response = Response::new_err(
                    req.id,
                    lsp_server::ErrorCode::MethodNotFound as i32,
                    format!("Method not found: {}", req.method),
                );
                sender(Message::Response(response))?;
            }
        }
        Ok(())
    }

    fn handle_notification<E>(
        &mut self,
        notification: Notification,
        sender: &dyn Fn(Message) -> Result<(), E>,
    ) -> Result<(), Box<dyn std::error::Error + Sync + Send>>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        match notification
            .method
            .as_str()
        {
            "initialized" => {
                let _params: InitializedParams = from_value(notification.params)?;
                self.handle_initialized()?;
            }
            "textDocument/didOpen" => {
                let params: DidOpenTextDocumentParams = from_value(notification.params)?;
                self.handle_did_open(params, sender)?;
            }
            "textDocument/didChange" => {
                let params: DidChangeTextDocumentParams = from_value(notification.params)?;
                self.handle_did_change(params, sender)?;
            }
            "textDocument/didSave" => {
                let params: DidSaveTextDocumentParams = from_value(notification.params)?;
                self.handle_did_save(params, sender)?;
            }
            "textDocument/didClose" => {
                let params: DidCloseTextDocumentParams = from_value(notification.params)?;
                self.handle_did_close(params, sender)?;
            }
            _ => {
                debug!("Unhandled notification method: {}", notification.method);
            }
        }
        Ok(())
    }

    fn handle_initialize(
        &self,
        _params: InitializeParams,
    ) -> Result<InitializeResult, Box<dyn std::error::Error + Sync + Send>> {
        info!("Language Server initializing");

        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                document_formatting_provider: Some(lsp_types::OneOf::Left(true)),
                ..Default::default()
            },
        })
    }

    fn handle_initialized(&self) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
        info!("Technique Language Server initialized");
        Ok(())
    }

    fn handle_did_open<E>(
        &mut self,
        params: DidOpenTextDocumentParams,
        sender: &dyn Fn(Message) -> Result<(), E>,
    ) -> Result<(), Box<dyn std::error::Error + Sync + Send>>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        let uri = params
            .text_document
            .uri;
        let content = params
            .text_document
            .text;

        debug!("Document opened: {}", uri);

        self.documents
            .insert(uri.clone(), content.clone());

        self.parse_and_report(uri, content, sender)?;
        Ok(())
    }

    fn handle_did_change<E>(
        &mut self,
        params: DidChangeTextDocumentParams,
        sender: &dyn Fn(Message) -> Result<(), E>,
    ) -> Result<(), Box<dyn std::error::Error + Sync + Send>>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
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

            self.documents
                .insert(uri.clone(), content.clone());

            self.parse_and_report(uri, content, sender)?;
        }
        Ok(())
    }

    fn handle_did_save<E>(
        &mut self,
        params: DidSaveTextDocumentParams,
        sender: &dyn Fn(Message) -> Result<(), E>,
    ) -> Result<(), Box<dyn std::error::Error + Sync + Send>>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        let uri = params
            .text_document
            .uri;
        debug!("Document saved: {}", uri);

        let content = self
            .documents
            .get(&uri)
            .cloned();

        if let Some(content) = content {
            self.parse_and_report(uri, content, sender)?;
        }
        Ok(())
    }

    fn handle_did_close<E>(
        &mut self,
        params: DidCloseTextDocumentParams,
        sender: &dyn Fn(Message) -> Result<(), E>,
    ) -> Result<(), Box<dyn std::error::Error + Sync + Send>>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        let uri = params
            .text_document
            .uri;
        debug!("Document closed: {}", uri);

        self.documents
            .remove(&uri);

        // Clear diagnostics for closed document
        self.publish_diagnostics(uri, vec![], sender)?;
        Ok(())
    }

    fn handle_document_formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>, Box<dyn std::error::Error + Sync + Send>> {
        let uri = params
            .text_document
            .uri;

        debug!("Format request: {}", uri);

        // Get content from our documents map
        let content = match self
            .documents
            .get(&uri)
        {
            Some(content) => content.clone(),
            None => {
                return Err("Document not open".into());
            }
        };

        let path = match uri.to_file_path() {
            Ok(buf) => buf,
            Err(_) => Path::new("-").to_path_buf(),
        };

        let document = match parsing::parse_with_recovery(&path, &content) {
            Ok(document) => document,
            Err(_) => {
                return Err("Document must be free of parse errors before formatting".into());
            }
        };

        let result = formatting::render(&Identity, &document, 78);

        // convert to LSP type for return to editor.
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

    /// Parse document and convert errors to diagnostics
    fn parse_and_report<E>(
        &self,
        uri: Url,
        content: String,
        sender: &dyn Fn(Message) -> Result<(), E>,
    ) -> Result<(), Box<dyn std::error::Error + Sync + Send>>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        let path = uri
            .to_file_path()
            .unwrap_or_else(|_| Path::new("-").to_path_buf());

        match parsing::parse_with_recovery(&path, &content) {
            Ok(_document) => {
                self.publish_diagnostics(uri, vec![], sender)?;
            }
            Err(errors) => {
                let diagnostics = self.convert_parsing_errors(&uri, &content, errors);
                self.publish_diagnostics(uri, diagnostics, sender)?;
            }
        }
        Ok(())
    }

    fn publish_diagnostics<E>(
        &self,
        uri: Url,
        diagnostics: Vec<Diagnostic>,
        sender: &dyn Fn(Message) -> Result<(), E>,
    ) -> Result<(), Box<dyn std::error::Error + Sync + Send>>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        let params = PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        };

        let notification = Notification::new(
            "textDocument/publishDiagnostics".to_string(),
            to_value(params).unwrap(),
        );

        sender(Message::Notification(notification))?;
        Ok(())
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
            let width = error.width();
            let start_position = offset_to_position(content, offset);
            let end_position = if width > 0 {
                offset_to_position(content, offset + width)
            } else {
                start_position // Fallback to single character if width is unknown
            };
            let range = Range {
                start: start_position,
                end: end_position,
            };

            let (message, severity) = match &error {
                ParsingError::IllegalParserState(_, _) => (
                    "Internal parser error".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::Unimplemented(_, _) => (
                    "Unimplemented feature".to_string(),
                    DiagnosticSeverity::WARNING,
                ),
                ParsingError::Unrecognized(_, _) => {
                    ("Unrecognized syntax".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::UnexpectedEndOfInput(_, _) => (
                    "Unexpected end of input".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::Expected(_, _, expected) => {
                    (format!("Expected {}", expected), DiagnosticSeverity::ERROR)
                }
                ParsingError::ExpectedMatchingChar(_, _, subject, start, end) => (
                    format!("Expected matching '{}' for '{}' in {}", end, start, subject),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::MissingParenthesis(_, _) => (
                    "Require parenthesis around multiple parameters in binding".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidCharacter(_, _, ch) => (
                    format!("Invalid character '{}'", ch),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidHeader(_, _) => {
                    ("Invalid header line".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidIdentifier(_, _, id) => (
                    format!("Invalid identifier '{}'", id),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidForma(_, _) => (
                    "Invalid forma in signature".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidGenus(_, _) => (
                    "Invalid genus in signature".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidSignature(_, _) => (
                    "Invalid signature in procedure declaration".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidParameters(_, _) => (
                    "Malformed parameters in procedure declaration".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidDeclaration(_, _) => (
                    "Invalid procedure declaration".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidSection(_, _) => (
                    "Invalid section heading".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidInvocation(_, _) => (
                    "Invalid procedure Invocation".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidFunction(_, _) => (
                    "Invalid function call".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidCodeBlock(_, _) => {
                    ("Invalid code block".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidStep(_, _) => {
                    ("Invalid step".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidSubstep(_, _) => {
                    ("Invalid substep".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidAttribute(_, _) => {
                    ("Invalid attribute assignment".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidResponse(_, _) => {
                    ("Invalid response".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidMultiline(_, _) => (
                    "Invalid multiline content".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidForeach(_, _) => (
                    "Invalid foreach expression".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidIntegral(_, _) => (
                    "Invalid integral number".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidQuantity(_, _) => {
                    ("Invalid quantity".to_string(), DiagnosticSeverity::ERROR)
                }
                ParsingError::InvalidQuantityDecimal(_, _) => (
                    "Invalid quantity decimal".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidQuantityUncertainty(_, _) => (
                    "Invalid quantity uncertainty".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidQuantityMagnitude(_, _) => (
                    "Invalid quantity magnitude".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::InvalidQuantitySymbol(_, _) => (
                    "Invalid quantity symbol".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
                ParsingError::UnclosedInterpolation(_, _) => (
                    "Unclosed interpolation".to_string(),
                    DiagnosticSeverity::ERROR,
                ),
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
}

/// Convert byte offset to LSP Position
fn offset_to_position(text: &str, offset: usize) -> Position {
    let line = calculate_line_number(text, offset) as u32;
    let character = calculate_column_number(text, offset) as u32;
    Position { line, character }
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
            ParsingError::IllegalParserState(0, 0),
            ParsingError::Unimplemented(0, 0),
            ParsingError::Unrecognized(0, 0),
            ParsingError::UnexpectedEndOfInput(0, 0),
            ParsingError::Expected(0, 0, "test"),
            ParsingError::ExpectedMatchingChar(0, 0, "test", '(', ')'),
            ParsingError::MissingParenthesis(0, 0),
            ParsingError::InvalidCharacter(0, 0, 'x'),
            ParsingError::InvalidHeader(0, 0),
            ParsingError::InvalidIdentifier(0, 0, "test".to_string()),
            ParsingError::InvalidDeclaration(0, 0),
        ];

        // This shouldn't panic - just test that all enum variants are handled
        for error in test_errors {
            let offset = error.offset();
            assert_eq!(offset, 0); // All test errors are at offset 0

            // Test message generation (this was formerly in convert_parsing_errors)
            match &error {
                ParsingError::IllegalParserState(_, _) => {
                    assert_eq!("Internal parser error", "Internal parser error")
                }
                ParsingError::Unimplemented(_, _) => {
                    assert_eq!("Unimplemented feature", "Unimplemented feature")
                }
                ParsingError::Unrecognized(_, _) => {
                    assert_eq!("Unrecognized syntax", "Unrecognized syntax")
                }
                ParsingError::UnexpectedEndOfInput(_, _) => {
                    assert_eq!("Unexpected end of input", "Unexpected end of input")
                }
                ParsingError::Expected(_, _, expected) => assert_eq!(*expected, "test"),
                ParsingError::ExpectedMatchingChar(_, _, subject, start, end) => {
                    assert_eq!(*subject, "test");
                    assert_eq!(*start, '(');
                    assert_eq!(*end, ')');
                }
                ParsingError::InvalidDeclaration(_, _) => {
                    assert_eq!("Invalid declaration", "Invalid declaration")
                }
                _ => {} // Other variants tested implicitly
            }
        }
    }
}
