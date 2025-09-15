use tower_lsp::{LspService, Server};
use tracing::{debug, info};

mod server;

pub(crate) async fn run_language_server() {
    debug!("Starting Technique Language Server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) =
        LspService::build(|client| server::TechniqueLanguageServer::new(client)).finish();

    info!("Technique Language Server starting on stdio");

    Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}
