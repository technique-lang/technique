use lsp_server::Connection;
use lsp_types::{
    InitializeParams, OneOf, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};
use tracing::{debug, info};

mod server;

pub(crate) fn run_language_server() {
    debug!("Starting Technique Language Server");

    let (connection, threads) = Connection::stdio();

    let capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        document_formatting_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        ..Default::default()
    })
    .unwrap();

    // extract any initialization parameters passed from the editor.
    if let Ok(params) = connection.initialize(capabilities) {
        let params = serde_json::from_value::<InitializeParams>(params).unwrap();

        info!("Technique Language Server starting on stdin");

        let server = server::TechniqueLanguageServer::new(params);

        if let Err(e) = server.run(connection) {
            eprintln!("Server error: {}", e);
        }
    }

    threads
        .join()
        .unwrap();
}
