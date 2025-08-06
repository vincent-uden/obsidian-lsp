use std::collections::HashMap;
use std::fs::File;

use dashmap::DashMap;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::lsp_types::request::GotoDeclarationResponse;
use tower_lsp_server::lsp_types::*;
use tower_lsp_server::{Client, LanguageServer, LspService, Server};
use tracing::{Level, debug, info};

#[derive(Debug)]
struct Backend {
    client: Client,
    //               Uri     Contents
    doc_map: DashMap<String, String>,
}

impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL), // Move to partial updates?
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                definition_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("Server initalized");
        self.client
            .log_message(MessageType::ERROR, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        info!("Server shutting down");
        self.client
            .log_message(MessageType::ERROR, "shutting down!")
            .await;
        Ok(())
    }

    // TODO: Borrow
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(TextDocumentItem {
            text: params.text_document.text.clone(),
            uri: params.text_document.uri,
            version: params.text_document.version,
            language_id: String::new(),
        })
        .await
    }

    // TODO: Borrow
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            text: params.content_changes[0].text.clone(),
            uri: params.text_document.uri,
            version: params.text_document.version,
            language_id: String::new(),
        })
        .await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        debug!(
            "{:?} {:?}",
            params.text_document_position_params.text_document.uri,
            params.text_document_position_params.position
        );
        if let Some(contents) = self.doc_map.get(
            params
                .text_document_position_params
                .text_document
                .uri
                .as_str(),
        ) {
            debug!(
                "{:?} {:?}",
                params.text_document_position_params.position, *contents
            );
        }
        Ok(Some(GotoDefinitionResponse::Array(Vec::new())))
    }

    async fn goto_declaration(
        &self,
        params: request::GotoDeclarationParams,
    ) -> Result<Option<request::GotoDeclarationResponse>> {
        debug!("{params:?}");
        Ok(Some(GotoDeclarationResponse::Array(Vec::new())))
    }
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        debug!("On change: {params:?}");
        self.doc_map.insert(params.uri.to_string(), params.text);
        // TODO: Parsing and stuff
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let log_file = File::create(".obsidian-lsp.log").unwrap();
    let subscriber = tracing_subscriber::fmt::Subscriber::builder()
        .with_writer(log_file)
        .with_ansi(false)
        .with_max_level(Level::DEBUG)
        .finish();
    tracing::subscriber::set_global_default(subscriber).unwrap();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        doc_map: DashMap::new(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
