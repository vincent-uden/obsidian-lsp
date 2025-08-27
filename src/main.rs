use std::fs::File;
use std::path::Path;
use std::sync::Arc;

use dashmap::DashMap;
use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::lsp_types::request::GotoDeclarationResponse;
use tower_lsp_server::lsp_types::*;
use tower_lsp_server::{Client, LanguageServer, LspService, Server, UriExt};
use tracing::{Level, debug, info, warn};

use crate::ast::{Document, Node, NodeType};
use crate::index::{VaultIndex, index_vault};

mod ast;
mod index;

#[derive(Debug)]
struct Backend {
    client: Client,
    //               Uri     Contents
    doc_map: DashMap<String, Document>,
    vault_index: Arc<RwLock<Option<VaultIndex>>>,
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

    async fn initialized(&self, _params: InitializedParams) {
        info!("Server initialized");
        self.client
            .log_message(MessageType::INFO, "Obsidian LSP server initialized!")
            .await;
        
        // Index workspace in a separate task to avoid Send issues
        let client = self.client.clone();
        let vault_index = self.vault_index.clone();
        
        tokio::spawn(async move {
            if let Ok(workspace_folders) = client.workspace_folders().await {
                if let Some(folders) = workspace_folders {
                    for folder in folders {
                        if let Some(path) = folder.uri.to_file_path() {
                            Backend::index_workspace_static(&client, &vault_index, &path).await;
                            break; // Use the first workspace folder
                        }
                    }
                }
            }
        });
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
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        
        debug!("Goto definition requested for {:?} at {:?}", uri, position);
        
        // Get the document
        let doc = match self.doc_map.get(uri.as_str()) {
            Some(doc) => doc,
            None => {
                debug!("Document not found: {:?}", uri);
                return Ok(None);
            }
        };
        
        // Find the node at the cursor position
        let node = match self.find_node_at_position(&doc, position) {
            Some(node) => node,
            None => {
                debug!("No node found at position {:?}", position);
                return Ok(None);
            }
        };
        
        // Check if it's a link node
        if let NodeType::Link(link) = &node.node_type {
            debug!("Found link: {}", link.address);
            
            // Get the vault index
            let vault_index = self.vault_index.read().await;
            let index = match vault_index.as_ref() {
                Some(index) => index,
                None => {
                    debug!("Vault index not available");
                    return Ok(None);
                }
            };
            
            // Find the target file
            if let Some(target_path) = index.find_file(&link.address) {
                debug!("Found target file: {}", target_path.display());
                
                // Convert path to URI
                if let Some(target_uri) = Uri::from_file_path(&target_path) {
                    let location = Location {
                        uri: target_uri,
                        range: Range {
                            start: Position { line: 0, character: 0 },
                            end: Position { line: 0, character: 0 },
                        },
                    };
                    
                    return Ok(Some(GotoDefinitionResponse::Scalar(location)));
                }
            } else {
                debug!("Target file not found for link: {}", link.address);
            }
        } else {
            debug!("Node at position is not a link: {:?}", node.node_type);
        }
        
        Ok(None)
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
                            name: link.display_text.clone(),
                            detail: Some(format!("→ {}", link.address)),
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

    async fn index_workspace_static(
        client: &Client, 
        vault_index: &Arc<RwLock<Option<VaultIndex>>>, 
        root: &Path
    ) {
        info!("Indexing workspace at: {}", root.display());
        match index_vault(root).await {
            Ok(index) => {
                let file_count = index.files.len();
                info!("Successfully indexed {} files", file_count);
                let mut vault_index_guard = vault_index.write().await;
                *vault_index_guard = Some(index);
                client
                    .log_message(MessageType::INFO, &format!("Indexed {} files", file_count))
                    .await;
            }
            Err(e) => {
                let error_msg = format!("Failed to index workspace: {}", e);
                warn!("{}", error_msg);
                client
                    .log_message(MessageType::ERROR, &error_msg)
                    .await;
            }
        }
    }

    fn find_node_at_position<'a>(&self, doc: &'a Document, position: Position) -> Option<&'a Node> {
        for node in doc.nodes.values() {
            if position_in_range(position, node.range) {
                return Some(node);
            }
        }
        None
    }
}

fn position_in_range(position: Position, range: Range) -> bool {
    if position.line < range.start.line || position.line > range.end.line {
        return false;
    }
    
    if position.line == range.start.line && position.character < range.start.character {
        return false;
    }
    
    if position.line == range.end.line && position.character > range.end.character {
        return false;
    }
    
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_in_range() {
        let range = Range {
            start: Position { line: 1, character: 5 },
            end: Position { line: 1, character: 15 },
        };

        // Inside range
        assert!(position_in_range(Position { line: 1, character: 10 }, range));
        
        // At start boundary
        assert!(position_in_range(Position { line: 1, character: 5 }, range));
        
        // At end boundary
        assert!(position_in_range(Position { line: 1, character: 15 }, range));
        
        // Before range
        assert!(!position_in_range(Position { line: 1, character: 4 }, range));
        
        // After range
        assert!(!position_in_range(Position { line: 1, character: 16 }, range));
        
        // Different line
        assert!(!position_in_range(Position { line: 2, character: 10 }, range));
    }

    #[test]
    fn test_pipe_link_integration() {
        // Test that pipe links are parsed correctly and use address for resolution
        let doc_content = "This has a [[target-file|Custom Display]] link.";
        let doc = ast::Document::from(doc_content.to_string());

        // Find the link node
        let link_node = doc.nodes.values()
            .find(|node| matches!(node.node_type, ast::NodeType::Link(_)))
            .expect("Should find a link node");

        if let ast::NodeType::Link(link) = &link_node.node_type {
            assert_eq!(link.address, "target-file", "Address should be the part before the pipe");
            assert_eq!(link.display_text, "Custom Display", "Display text should be the part after the pipe");
            
            // The goto_definition logic should use link.address for file resolution
            println!("✓ Pipe link parsed correctly: address='{}', display='{}'", link.address, link.display_text);
        } else {
            panic!("Expected Link node type");
        }
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
        vault_index: Arc::new(RwLock::new(None)),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
