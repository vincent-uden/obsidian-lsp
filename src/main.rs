use std::fs::File;

use dashmap::DashMap;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::lsp_types::request::GotoDeclarationResponse;
use tower_lsp_server::lsp_types::*;
use tower_lsp_server::{Client, LanguageServer, LspService, Server};
use tracing::{Level, debug, info};

use crate::ast::Document;
use crate::index::VaultIndex;

mod ast;
mod index;

#[derive(Debug)]
struct Backend {
    client: Client,
    //               Uri     Contents
    doc_map: DashMap<String, Document>,
    vault_index: Option<VaultIndex>,
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
                document_symbol_provider: Some(OneOf::Left(true)),
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

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        debug!("Finding symbols");
        let mut symbols = vec![];

        if let Some(doc) = self.doc_map.get(params.text_document.uri.as_str()) {
            let mut all_nodes: Vec<_> = doc.nodes.iter().collect();
            all_nodes.sort_by_key(|(_, node)| node.range.start.line);

            let mut stack: Vec<DocumentSymbol> = vec![];

            for (_, node) in all_nodes {
                match &node.node_type {
                    ast::NodeType::Heading(level) => {
                        let heading_text = doc
                            .contents
                            .lines()
                            .nth(node.range.start.line as usize)
                            .unwrap_or("")
                            .trim_start_matches('#')
                            .trim();

                        let heading_symbol = DocumentSymbol {
                            name: heading_text.to_string(),
                            detail: None,
                            kind: SymbolKind::NAMESPACE,
                            tags: None,
                            deprecated: None,
                            range: node.range,
                            selection_range: node.range,
                            children: None,
                        };

                        while let Some(top) = stack.last() {
                            if top.kind == SymbolKind::NAMESPACE {
                                let top_level = doc
                                    .contents
                                    .lines()
                                    .find(|line| line.trim_start_matches('#').trim() == top.name)
                                    .map(|line| line.chars().take_while(|&c| c == '#').count())
                                    .unwrap_or(1);

                                if top_level >= *level {
                                    let popped = stack.pop().unwrap();
                                    if let Some(parent) = stack.last_mut() {
                                        if parent.children.is_none() {
                                            parent.children = Some(vec![]);
                                        }
                                        parent.children.as_mut().unwrap().push(popped);
                                    } else {
                                        symbols.push(popped);
                                    }
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }

                        stack.push(heading_symbol);
                    }
                    ast::NodeType::Link(link) => {
                        let link_symbol = DocumentSymbol {
                            name: link.address.clone(),
                            detail: None,
                            kind: SymbolKind::FIELD,
                            tags: None,
                            deprecated: None,
                            range: node.range,
                            selection_range: node.range,
                            children: None,
                        };
                        if let Some(current_heading) = stack.last_mut() {
                            if current_heading.children.is_none() {
                                current_heading.children = Some(vec![]);
                            }
                            current_heading.children.as_mut().unwrap().push(link_symbol);
                        } else {
                            symbols.push(link_symbol);
                        }
                    }
                    _ => {} // Skip paragraphs
                }
            }

            // Pop remaining headings from stack
            while let Some(heading) = stack.pop() {
                if let Some(parent) = stack.last_mut() {
                    if parent.children.is_none() {
                        parent.children = Some(vec![]);
                    }
                    parent.children.as_mut().unwrap().push(heading);
                } else {
                    symbols.push(heading);
                }
            }
        }

        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
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
        self.doc_map
            .insert(params.uri.to_string(), Document::from(params.text));
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
        vault_index: None,
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
