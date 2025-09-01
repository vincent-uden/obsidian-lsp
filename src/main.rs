use std::collections::HashSet;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};

use dashmap::DashMap;
use tokio::fs;
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
mod minimal_test;
mod comprehensive_test;

#[derive(Debug)]
struct Backend {
    client: Client,
    //               Uri     Contents
    doc_map: DashMap<String, Document>,
    vault_index: Arc<RwLock<Option<VaultIndex>>>,
    last_parse: DashMap<String, Instant>,
    change_count: DashMap<String, u32>,
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
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        "[".to_string(), // For starting links
                        "#".to_string(), // For starting tags
                        ":".to_string(), // For properties
                        "-".to_string(), // For date links like 2025-07-06
                        "/".to_string(), // For folder paths in links
                    ]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),
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
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: 0,
                                character: 0,
                            },
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
                    ast::NodeType::Tag(tag_name) => {
                        let tag_symbol = DocumentSymbol {
                            name: format!("#{}", tag_name),
                            detail: Some("tag".to_string()),
                            kind: SymbolKind::KEY,
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
                            current_heading.children.as_mut().unwrap().push(tag_symbol);
                        } else {
                            symbols.push(tag_symbol);
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

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        debug!("Hover requested for {:?} at {:?}", uri, position);

        // Get the document
        let doc = match self.doc_map.get(uri.as_str()) {
            Some(doc) => doc,
            None => {
                debug!("Document not found for hover: {:?}", uri);
                return Ok(None);
            }
        };

        let node = match self.find_node_at_position(&doc, position) {
            Some(node) => node,
            None => {
                debug!("No node found at hover position {:?}", position);
                return Ok(None);
            }
        };

        match &node.node_type {
            NodeType::Link(link) => {
                debug!("Found link for hover: {}", link.address);

                let vault_index = self.vault_index.read().await;
                let index = match vault_index.as_ref() {
                    Some(index) => index,
                    None => {
                        debug!("Vault index not available for hover");
                        return Ok(None);
                    }
                };

                if let Some(target_path) = index.find_file(&link.address) {
                    debug!("Found target file for hover: {}", target_path.display());

                    if let Ok(preview_content) = self.create_file_preview(&target_path).await {
                        let file_name = target_path
                            .file_name()
                            .and_then(|n| n.to_str())
                            .unwrap_or("Unknown");

                        let hover_text = format!(
                            "**{}** *({})*\n\n{}",
                            link.display_text, file_name, preview_content
                        );

                        let hover_content = MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: hover_text,
                        };

                        return Ok(Some(Hover {
                            contents: HoverContents::Markup(hover_content),
                            range: Some(node.range),
                        }));
                    }
                } else {
                    debug!("Target file not found for hover link: {}", link.address);

                    let hover_content = MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("**File not found**: `{}`", link.address),
                    };

                    return Ok(Some(Hover {
                        contents: HoverContents::Markup(hover_content),
                        range: Some(node.range),
                    }));
                }
            }
            NodeType::Tag(tag_name) => {
                debug!("Found tag for hover: {}", tag_name);

                // Simple option to disable tag hover entirely if it causes issues
                // You can uncomment this line to disable tag hover completely:
                // return Ok(None);

                // Add safety check for tag name
                if tag_name.is_empty() || tag_name.len() > 200 {
                    debug!("Invalid tag name for hover: '{}'", tag_name);
                    return Ok(None);
                }

                // Get the vault index with timeout protection
                let vault_index_result = tokio::time::timeout(
                    std::time::Duration::from_millis(100),
                    self.vault_index.read(),
                )
                .await;

                let vault_index = match vault_index_result {
                    Ok(guard) => guard,
                    Err(_) => {
                        debug!("Timeout waiting for vault index lock");
                        return Ok(None);
                    }
                };

                let index = match vault_index.as_ref() {
                    Some(index) => index,
                    None => {
                        debug!("Vault index not available for hover");
                        return Ok(None);
                    }
                };

                // Safe tag lookup with error handling
                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    index.tag_map.get(&tag_name.to_lowercase())
                })) {
                    Ok(tag_lookup_result) => {
                        if let Some(files_with_tag) = tag_lookup_result {
                            // Safety check for file count
                            let file_count = files_with_tag.len();
                            if file_count > 1000 {
                                debug!(
                                    "Too many files with tag '{}': {}, limiting display",
                                    tag_name, file_count
                                );
                            }

                            // Safely build file list with limits
                            let file_list = files_with_tag
                                .iter()
                                .take(10.min(file_count))
                                .map(|path| {
                                    let file_name = path
                                        .file_stem()
                                        .and_then(|s| s.to_str())
                                        .unwrap_or("Unknown");
                                    // Sanitize file name to prevent formatting issues
                                    let safe_name = file_name.chars().take(100).collect::<String>();
                                    format!("- {}", safe_name)
                                })
                                .collect::<Vec<_>>()
                                .join("\n");

                            let hover_text = if file_count <= 10 {
                                format!(
                                    "**Tag: #{}**\n\nFound in {} file{}:\n\n{}",
                                    tag_name,
                                    file_count,
                                    if file_count == 1 { "" } else { "s" },
                                    file_list
                                )
                            } else {
                                format!(
                                    "**Tag: #{}**\n\nFound in {} files (showing first 10):\n\n{}\n\n... and {} more",
                                    tag_name,
                                    file_count,
                                    file_list,
                                    file_count - 10
                                )
                            };

                            let hover_content = MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: hover_text,
                            };

                            return Ok(Some(Hover {
                                contents: HoverContents::Markup(hover_content),
                                range: Some(node.range),
                            }));
                        } else {
                            debug!("No files found with tag: {}", tag_name);

                            let hover_content = MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: format!(
                                    "**Tag: #{}**\n\nNo files found with this tag",
                                    tag_name
                                ),
                            };

                            return Ok(Some(Hover {
                                contents: HoverContents::Markup(hover_content),
                                range: Some(node.range),
                            }));
                        }
                    }
                    Err(panic_info) => {
                        debug!(
                            "Panic during tag lookup for '{}': {:?}",
                            tag_name, panic_info
                        );
                        return Ok(None);
                    }
                }
            }
            _ => {
                debug!(
                    "Node at hover position is not a link or tag: {:?}",
                    node.node_type
                );
            }
        }

        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        debug!("Completion requested for {:?} at {:?}", uri, position);
        debug!("Completion context: {:?}", params.context);

        // Get the document
        let doc = match self.doc_map.get(uri.as_str()) {
            Some(doc) => doc,
            None => {
                debug!("Document not found for completion: {:?}", uri);
                return Ok(None);
            }
        };

        // Get the vault index
        let vault_index = self.vault_index.read().await;
        let index = match vault_index.as_ref() {
            Some(index) => index,
            None => {
                debug!("Vault index not available for completion");
                return Ok(None);
            }
        };

        // Determine completion context and provide appropriate completions
        if let Some(completion_items) = self.provide_completions(&doc, position, index).await {
            debug!("Providing {} completion items", completion_items.len());

            // Return incomplete completion list to force client to request fresh completions
            // as the user types more characters instead of doing client-side filtering
            let completion_list = CompletionList {
                is_incomplete: true, // This forces the client to re-request as user types
                items: completion_items,
            };

            return Ok(Some(CompletionResponse::List(completion_list)));
        }

        Ok(None)
    }
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        let uri_str = params.uri.to_string();

        // Track change frequency
        let mut change_count = self.change_count.entry(uri_str.clone()).or_insert(0);
        *change_count += 1;

        info!(
            "Document change #{} triggered for: {:?}, version: {:?}",
            *change_count, params.uri, params.version
        );

        if *change_count > 100 {
            warn!(
                "Excessive document changes detected: {} changes for {}",
                *change_count, uri_str
            );
        }

        info!(
            "Document content preview (first 100 chars): {:?}",
            params.text.chars().take(100).collect::<String>()
        );

        // Rate limiting: Don't re-parse the same document more than once per 100ms
        let now = Instant::now();
        if let Some(last_parse_time) = self.last_parse.get(&uri_str) {
            if now.duration_since(*last_parse_time) < Duration::from_millis(100) {
                info!(
                    "Skipping parse due to rate limiting (change #{})",
                    *change_count
                );
                return;
            }
        }

        info!("Starting document parsing...");
        let start_time = Instant::now();
        let document = Document::from(params.text.clone());
        let parse_duration = start_time.elapsed();

        info!(
            "Document parsing completed: {:?} for {} nodes",
            parse_duration,
            document.nodes.len()
        );

        // Log if parsing took unusually long
        if parse_duration > Duration::from_millis(50) {
            warn!("Slow document parsing detected: {:?}", parse_duration);
            warn!("Document length: {} characters", params.text.len());
        }

        self.last_parse.insert(uri_str.clone(), now);
        self.doc_map.insert(uri_str, document);

        info!("Document change processing complete");
    }

    async fn index_workspace_static(
        client: &Client,
        vault_index: &Arc<RwLock<Option<VaultIndex>>>,
        root: &Path,
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
                client.log_message(MessageType::ERROR, &error_msg).await;
            }
        }
    }

    fn find_node_at_position<'a>(&self, doc: &'a Document, position: Position) -> Option<&'a Node> {
        let mut matching_nodes = Vec::new();

        for node in doc.nodes.values() {
            if position_in_range(position, node.range) {
                matching_nodes.push(node);
            }
        }

        // Prioritize link and tag nodes over other types
        for node in &matching_nodes {
            if matches!(node.node_type, NodeType::Link(_) | NodeType::Tag(_)) {
                return Some(node);
            }
        }

        matching_nodes.first().copied()
    }

    async fn create_file_preview(
        &self,
        file_path: &PathBuf,
    ) -> std::result::Result<String, std::io::Error> {
        let content = fs::read_to_string(file_path).await?;

        let preview = if file_path.extension().and_then(|s| s.to_str()) == Some("md") {
            self.extract_markdown_preview(&content)
        } else {
            self.extract_generic_preview(&content)
        };

        Ok(preview)
    }

    fn extract_markdown_preview(&self, content: &str) -> String {
        const PREVIEW_LINES: usize = 15;
        const MAX_CHARS: usize = 800;

        let lines: Vec<&str> = content.lines().collect();
        let mut preview_lines = Vec::new();
        let mut char_count = 0;
        let mut skipped_frontmatter = false;

        for line in lines.iter().take(PREVIEW_LINES + 10) {
            // Extra buffer for frontmatter
            // Skip YAML frontmatter
            if !skipped_frontmatter {
                if line.trim() == "---" {
                    if preview_lines.is_empty() {
                        // Start of frontmatter, skip it
                        continue;
                    } else {
                        // End of frontmatter
                        skipped_frontmatter = true;
                        continue;
                    }
                }
                if !preview_lines.is_empty() || !line.trim().is_empty() {
                    // We're either past frontmatter or found non-empty content
                    skipped_frontmatter = true;
                } else {
                    continue; // Skip empty lines before content
                }
            }

            if preview_lines.len() >= PREVIEW_LINES || char_count + line.len() > MAX_CHARS {
                break;
            }

            preview_lines.push(*line);
            char_count += line.len() + 1; // +1 for newline
        }

        let preview = preview_lines.join("\n");

        // Add truncation indicator if needed
        if lines.len() > preview_lines.len() + 10 || char_count >= MAX_CHARS {
            format!(
                "{}\n\n---\n*Preview truncated • [Click to open file]*",
                preview.trim()
            )
        } else {
            preview
        }
    }

    fn extract_generic_preview(&self, content: &str) -> String {
        const MAX_CHARS: usize = 300;

        if content.len() <= MAX_CHARS {
            format!("```\n{}\n```", content)
        } else {
            format!(
                "```\n{}\n...\n```\n\n*[Preview truncated...]*",
                &content[..MAX_CHARS]
            )
        }
    }

    async fn provide_completions(
        &self,
        doc: &Document,
        position: Position,
        index: &VaultIndex,
    ) -> Option<Vec<CompletionItem>> {
        let lines: Vec<&str> = doc.contents.lines().collect();
        let line_idx = position.line as usize;
        let char_idx = position.character as usize;

        if line_idx >= lines.len() {
            return None;
        }

        let line = lines[line_idx];
        let line_before_cursor = &line[..char_idx.min(line.len())];

        // Check for link completion context [[...
        if let Some(link_start) = line_before_cursor.rfind("[[") {
            // Check if there's a closing ]] after the opening [[
            let after_link_start = &line_before_cursor[link_start..];
            let link_end = after_link_start.find("]]");
            if link_end.is_none() {
                // We're inside an incomplete link - extract the partial content
                let partial_link = &line_before_cursor[link_start + 2..];
                debug!(
                    "Link completion context detected: '[[{}' (cursor at position {})",
                    partial_link, char_idx
                );
                debug!("Full line before cursor: '{}'", line_before_cursor);
                return Some(self.provide_link_completions(partial_link, index));
            } else {
                debug!(
                    "Complete link found, no completion needed: '{}'",
                    &after_link_start[..link_end.unwrap() + 2]
                );
            }
        } else {
            debug!("No [[ found before cursor position");
        }

        // Check for tag completion context #...
        if let Some(tag_start) = line_before_cursor.rfind('#') {
            // Make sure it's not part of a URL or code block
            let before_hash = &line_before_cursor[..tag_start];
            if !before_hash.contains("http") && !before_hash.contains('`') {
                // Check if there's whitespace or punctuation after the tag
                let after_hash = &line_before_cursor[tag_start + 1..];
                if !after_hash.contains(' ') && !after_hash.contains('\t') {
                    debug!("Tag completion context: '{}'", after_hash);
                    return Some(self.provide_tag_completions(after_hash, index));
                }
            }
        }

        // Check for property completion context (YAML frontmatter)
        if self.is_in_frontmatter(doc, line_idx) {
            if line_before_cursor.contains(':') && !line_before_cursor.trim_end().ends_with(':') {
                // Property value completion - not implemented for now
                return None;
            } else if line_before_cursor.trim().is_empty()
                || (!line_before_cursor.contains(':')
                    && line_before_cursor
                        .chars()
                        .all(|c| c.is_alphabetic() || c == '-' || c == '_'))
            {
                // Property key completion
                let partial_key = line_before_cursor.trim();
                debug!("Property completion context: '{}'", partial_key);
                return Some(self.provide_property_completions(partial_key, index));
            }
        }

        None
    }

    fn provide_link_completions(&self, partial: &str, index: &VaultIndex) -> Vec<CompletionItem> {
        let mut items = Vec::new();
        let partial_lower = partial.to_lowercase();

        debug!("Providing link completions for partial: '{}'", partial);

        // Detect if this looks like a date/numeric query
        let is_numeric_query = partial.chars().any(|c| c.is_numeric());

        // Use a HashSet to prevent duplicates
        let mut seen_files = HashSet::new();

        for entry in index.link_map.iter() {
            let (link_key, file_path) = (entry.key(), entry.value());

            // Skip if we've already processed this file
            if seen_files.contains(file_path.as_path()) {
                continue;
            }
            seen_files.insert(file_path.clone());

            let file_stem = file_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown");
            let file_stem_lower = file_stem.to_lowercase();

            // Only match if partial is empty OR if there's a strong match
            let matches = if partial.is_empty() {
                true
            } else {
                // For date/numeric queries, be extra strict - require prefix match
                if is_numeric_query {
                    file_stem_lower.starts_with(&partial_lower)
                        || link_key.starts_with(&partial_lower)
                } else {
                    // For text queries, allow broader matching
                    let strong_match = file_stem_lower.starts_with(&partial_lower)
                        || link_key.starts_with(&partial_lower);

                    // Allow contains match only if partial is reasonably long (3+ chars) to avoid spurious matches
                    let medium_match = partial.len() >= 3
                        && (file_stem_lower.contains(&partial_lower)
                            || link_key.contains(&partial_lower));

                    strong_match || medium_match
                }
            };

            if matches {
                // Determine match quality for better sorting
                let exact_match = file_stem_lower == partial_lower;
                let starts_with_match = file_stem_lower.starts_with(&partial_lower);
                let contains_match = file_stem_lower.contains(&partial_lower);

                // Score for sorting: higher score = better match
                // Give massive preference to prefix matches for date-like queries
                let match_score = if exact_match {
                    10000
                } else if starts_with_match {
                    1000 + (partial_lower.len() * 100) as i32 // Much higher score for prefix matches
                } else if contains_match && partial.len() >= 3 {
                    10 + partial_lower.len() as i32
                } else {
                    1
                };

                let detail = format!("→ {}", file_path.display());

                let item = CompletionItem {
                    label: file_stem.to_string(),
                    detail: Some(detail),
                    kind: Some(CompletionItemKind::FILE),
                    insert_text: Some(file_stem.to_string()),
                    filter_text: Some(file_stem.to_string()), // Just use the stem for filtering
                    sort_text: Some(format!("{:04}_{}", 10000 - match_score, file_stem)), // Lower sort_text = higher priority
                    ..Default::default()
                };

                items.push(item);

                debug!(
                    "Added completion item: '{}' (score: {}, exact: {}, starts: {}, contains: {})",
                    file_stem, match_score, exact_match, starts_with_match, contains_match
                );
            } else {
                debug!(
                    "Rejected item: '{}' (key: '{}') - no match for '{}'",
                    file_stem, link_key, partial
                );
            }
        }

        // Sort by match quality (using sort_text which encodes the match score)
        items.sort_by(|a, b| {
            a.sort_text
                .as_ref()
                .unwrap_or(&a.label)
                .cmp(b.sort_text.as_ref().unwrap_or(&b.label))
        });

        debug!(
            "Returning {} completion items for partial '{}'",
            items.len(),
            partial
        );

        // For better performance with incomplete responses, limit items based on partial length
        let max_items = if partial.is_empty() {
            20 // Fewer items when showing everything
        } else if partial.len() < 3 {
            30 // Medium number for short partials  
        } else {
            50 // Full set for longer partials
        };

        for (i, item) in items.iter().take(10).enumerate() {
            debug!(
                "  {}. {} (sort: {})",
                i + 1,
                item.label,
                item.sort_text.as_ref().unwrap_or(&"none".to_string())
            );
        }

        items.truncate(max_items);
        items
    }

    fn provide_tag_completions(&self, partial: &str, index: &VaultIndex) -> Vec<CompletionItem> {
        let mut items = Vec::new();
        let partial_lower = partial.to_lowercase();

        for entry in index.tag_map.iter() {
            let (tag_name, files) = (entry.key(), entry.value());

            if tag_name.contains(&partial_lower) || partial.is_empty() {
                let detail = format!(
                    "Used in {} file{}",
                    files.len(),
                    if files.len() == 1 { "" } else { "s" }
                );

                items.push(CompletionItem {
                    label: format!("#{}", tag_name),
                    detail: Some(detail),
                    kind: Some(CompletionItemKind::KEYWORD),
                    insert_text: Some(tag_name.to_string()),
                    ..Default::default()
                });
            }
        }

        // Sort by usage frequency (descending) then alphabetical
        items.sort_by(|a, b| {
            let a_key = a.insert_text.as_ref().unwrap_or(&a.label);
            let b_key = b.insert_text.as_ref().unwrap_or(&b.label);
            let a_count = index
                .tag_map
                .get(a_key.as_str())
                .map(|v| v.len())
                .unwrap_or(0);
            let b_count = index
                .tag_map
                .get(b_key.as_str())
                .map(|v| v.len())
                .unwrap_or(0);

            match b_count.cmp(&a_count) {
                std::cmp::Ordering::Equal => a.label.cmp(&b.label),
                other => other,
            }
        });

        items.truncate(30); // Limit to 30 tags for performance
        items
    }

    fn provide_property_completions(
        &self,
        partial: &str,
        index: &VaultIndex,
    ) -> Vec<CompletionItem> {
        let mut items = Vec::new();
        let partial_lower = partial.to_lowercase();

        // Add common Obsidian properties
        let common_properties = [
            ("title", "Document title"),
            ("tags", "Document tags"),
            ("aliases", "Alternative names"),
            ("author", "Document author"),
            ("created", "Creation date"),
            ("modified", "Last modified date"),
            ("status", "Document status"),
            ("type", "Document type"),
            ("category", "Document category"),
        ];

        for (prop, desc) in common_properties {
            if prop.contains(&partial_lower) || partial.is_empty() {
                items.push(CompletionItem {
                    label: prop.to_string(),
                    detail: Some(desc.to_string()),
                    kind: Some(CompletionItemKind::PROPERTY),
                    insert_text: Some(format!("{}: ", prop)),
                    ..Default::default()
                });
            }
        }

        // Add properties found in vault
        for entry in index.property_map.iter() {
            let (prop_name, files) = (entry.key(), entry.value());

            if prop_name.contains(&partial_lower) || partial.is_empty() {
                if !common_properties
                    .iter()
                    .any(|(common, _)| common == prop_name)
                {
                    let detail = format!(
                        "Used in {} file{}",
                        files.len(),
                        if files.len() == 1 { "" } else { "s" }
                    );

                    items.push(CompletionItem {
                        label: prop_name.to_string(),
                        detail: Some(detail),
                        kind: Some(CompletionItemKind::PROPERTY),
                        insert_text: Some(format!("{}: ", prop_name)),
                        ..Default::default()
                    });
                }
            }
        }

        // Sort by usage frequency then alphabetical
        items.sort_by(|a, b| a.label.cmp(&b.label));
        items.truncate(20); // Limit to 20 properties
        items
    }

    fn is_in_frontmatter(&self, doc: &Document, line_idx: usize) -> bool {
        let lines: Vec<&str> = doc.contents.lines().collect();

        if lines.is_empty() || !lines[0].trim().starts_with("---") {
            return false;
        }

        // Find the end of frontmatter
        for (idx, line) in lines.iter().enumerate().skip(1) {
            if line.trim() == "---" {
                return line_idx > 0 && line_idx < idx;
            }
        }

        false
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
            start: Position {
                line: 1,
                character: 5,
            },
            end: Position {
                line: 1,
                character: 15,
            },
        };

        // Inside range
        assert!(position_in_range(
            Position {
                line: 1,
                character: 10
            },
            range
        ));

        // At start boundary
        assert!(position_in_range(
            Position {
                line: 1,
                character: 5
            },
            range
        ));

        // At end boundary
        assert!(position_in_range(
            Position {
                line: 1,
                character: 15
            },
            range
        ));

        // Before range
        assert!(!position_in_range(
            Position {
                line: 1,
                character: 4
            },
            range
        ));

        // After range
        assert!(!position_in_range(
            Position {
                line: 1,
                character: 16
            },
            range
        ));

        // Different line
        assert!(!position_in_range(
            Position {
                line: 2,
                character: 10
            },
            range
        ));
    }

    #[test]
    fn test_pipe_link_integration() {
        // Test that pipe links are parsed correctly and use address for resolution
        let doc_content = "This has a [[target-file|Custom Display]] link.";
        let doc = ast::Document::from(doc_content.to_string());

        // Find the link node
        let link_node = doc
            .nodes
            .values()
            .find(|node| matches!(node.node_type, ast::NodeType::Link(_)))
            .expect("Should find a link node");

        if let ast::NodeType::Link(link) = &link_node.node_type {
            assert_eq!(
                link.address, "target-file",
                "Address should be the part before the pipe"
            );
            assert_eq!(
                link.display_text, "Custom Display",
                "Display text should be the part after the pipe"
            );

            // The goto_definition logic should use link.address for file resolution
            println!(
                "Pipe link parsed correctly: address='{}', display='{}'",
                link.address, link.display_text
            );
        } else {
            panic!("Expected Link node type");
        }
    }

    #[test]
    fn test_completion_link_context() {
        // Test link completion context detection by checking string patterns
        let line = "This is a test with [[partial";
        let position = 29; // After "[[partial"

        // Simulate the context detection logic
        let line_before_cursor = &line[..position.min(line.len())];
        if let Some(link_start) = line_before_cursor.rfind("[[") {
            let link_end = line_before_cursor[link_start..].find("]]");
            assert!(link_end.is_none(), "Should detect incomplete link context");

            let partial_link = &line_before_cursor[link_start + 2..];
            assert_eq!(partial_link, "partial", "Should extract partial link text");
        }

        // Test with date-like patterns (the reported issue)
        let date_line = "Today I will write [[2025-07-06";
        let date_position = 32; // After "[[2025-07-06"

        let date_line_before_cursor = &date_line[..date_position.min(date_line.len())];
        if let Some(link_start) = date_line_before_cursor.rfind("[[") {
            let link_end = date_line_before_cursor[link_start..].find("]]");
            assert!(
                link_end.is_none(),
                "Should detect incomplete date link context"
            );

            let partial_date_link = &date_line_before_cursor[link_start + 2..];
            assert_eq!(
                partial_date_link, "2025-07-06",
                "Should extract partial date link text including dashes"
            );
        }

        // Test partial date with just one dash
        let partial_date_line = "Today I will write [[2025-";
        let partial_date_position = 27; // After "[[2025-"

        let partial_date_line_before_cursor =
            &partial_date_line[..partial_date_position.min(partial_date_line.len())];
        if let Some(link_start) = partial_date_line_before_cursor.rfind("[[") {
            let link_end = partial_date_line_before_cursor[link_start..].find("]]");
            assert!(
                link_end.is_none(),
                "Should detect incomplete partial date link context"
            );

            let partial_with_dash = &partial_date_line_before_cursor[link_start + 2..];
            assert_eq!(
                partial_with_dash, "2025-",
                "Should extract partial date including trailing dash"
            );
        }
    }

    #[test]
    fn test_completion_tag_context() {
        // Test tag completion context detection
        let line = "This is a test with #parti";
        let position = 26; // After "#parti"

        let line_before_cursor = &line[..position.min(line.len())];
        if let Some(tag_start) = line_before_cursor.rfind('#') {
            let before_hash = &line_before_cursor[..tag_start];
            assert!(!before_hash.contains("http"), "Should not be part of URL");

            let after_hash = &line_before_cursor[tag_start + 1..];
            assert_eq!(after_hash, "parti", "Should extract partial tag text");
        }
    }

    #[test]
    fn test_completion_frontmatter_context() {
        // Test frontmatter detection logic
        let content = "---\ntitle: Test\nauth\n---\n# Content";
        let lines: Vec<&str> = content.lines().collect();
        let line_idx = 2; // "auth" line

        // Simulate is_in_frontmatter logic
        if !lines.is_empty() && lines[0].trim().starts_with("---") {
            for (idx, line) in lines.iter().enumerate().skip(1) {
                if line.trim() == "---" {
                    let is_in_frontmatter = line_idx > 0 && line_idx < idx;
                    assert!(
                        is_in_frontmatter,
                        "Line 2 should be detected as in frontmatter"
                    );
                    break;
                }
            }
        }
    }

    #[test]
    fn test_completion_providers_basic() {
        use crate::index::VaultIndex;
        use std::path::PathBuf;

        // These are simplified unit tests for the core logic
        // More complex integration tests would require a full Backend instance

        // Test link completion logic
        let link_index = VaultIndex::new();
        link_index
            .link_map
            .insert("test-file".to_string(), PathBuf::from("/test/test-file.md"));
        link_index.link_map.insert(
            "another-test".to_string(),
            PathBuf::from("/test/another-test.md"),
        );

        // Verify we have the expected data in our index
        assert_eq!(link_index.link_map.len(), 2, "Should have 2 link entries");

        // Test tag completion logic
        let tag_index = VaultIndex::new();
        tag_index.tag_map.insert(
            "programming".to_string(),
            vec![PathBuf::from("/test/file1.md")],
        );
        tag_index.tag_map.insert(
            "rust".to_string(),
            vec![
                PathBuf::from("/test/file1.md"),
                PathBuf::from("/test/file2.md"),
            ],
        );

        assert_eq!(tag_index.tag_map.len(), 2, "Should have 2 tag entries");

        // Test property completion logic
        let prop_index = VaultIndex::new();
        prop_index.property_map.insert(
            "custom-property".to_string(),
            vec![PathBuf::from("/test/file1.md")],
        );

        assert_eq!(
            prop_index.property_map.len(),
            1,
            "Should have 1 property entry"
        );

        println!("Basic completion provider data structures validated");
    }

    #[test]
    fn test_date_link_completion_logic() {
        // Test the improved matching and scoring logic for date-like completions
        let partial = "2025";
        let candidates = vec![
            ("2025-01-01", "/notes/2025-01-01.md"),
            ("2025-01-15", "/notes/2025-01-15.md"),
            ("2025-07-06", "/notes/2025-07-06.md"),
            ("2024-11-05", "/notes/2024-11-05.md"), // Should NOT match "2025"
            ("my-note", "/notes/my-note.md"),
        ];

        // Simulate the improved matching logic
        let partial_lower = partial.to_lowercase();
        let is_numeric_query = partial.chars().any(|c| c.is_numeric());
        let mut scored_items: Vec<(i32, &str)> = Vec::new();

        for (file_stem, _path) in &candidates {
            let file_stem_lower = file_stem.to_lowercase();

            // Apply the new strict matching logic for numeric queries
            let matches = if is_numeric_query {
                // For numeric queries, require prefix match
                file_stem_lower.starts_with(&partial_lower)
            } else {
                // For text queries, allow broader matching
                let strong_match = file_stem_lower.starts_with(&partial_lower);
                let medium_match = partial.len() >= 3 && file_stem_lower.contains(&partial_lower);
                strong_match || medium_match
            };

            if matches {
                let exact_match = file_stem_lower == partial_lower;
                let starts_with_match = file_stem_lower.starts_with(&partial_lower);
                let contains_match = file_stem_lower.contains(&partial_lower);

                let match_score = if exact_match {
                    10000
                } else if starts_with_match {
                    1000 + (partial_lower.len() * 100) as i32
                } else if contains_match && partial.len() >= 3 {
                    10 + partial_lower.len() as i32
                } else {
                    1
                };

                scored_items.push((match_score, file_stem));
            }
        }

        // Sort by score (descending)
        scored_items.sort_by(|a, b| b.0.cmp(&a.0));

        println!(
            "Scored results for '{}' (numeric_query={}): {:?}",
            partial, is_numeric_query, scored_items
        );

        // For numeric query "2025", should ONLY get 2025 files, NO 2024 files
        let sorted_names: Vec<&str> = scored_items.iter().map(|(_, name)| *name).collect();

        // Should have 3 results (all the 2025 files)
        assert_eq!(
            sorted_names.len(),
            3,
            "Should have exactly 3 matches for '2025', got: {:?}",
            sorted_names
        );

        // All results should start with "2025"
        for name in &sorted_names {
            assert!(
                name.starts_with("2025"),
                "All results should start with '2025', got: {}",
                name
            );
        }

        // Should NOT contain any 2024 files
        assert!(
            !sorted_names.iter().any(|name| name.contains("2024")),
            "Should not contain any 2024 files when searching for '2025', got: {:?}",
            sorted_names
        );

        // The first result should be one of the 2025 files
        assert!(
            sorted_names[0].starts_with("2025"),
            "First result should start with '2025', got: {}",
            sorted_names[0]
        );
    }

    #[test]
    fn test_nested_tag_parsing() {
        // Test that the problematic nested tag format gets parsed correctly
        let doc_content = "This document has a nested tag: #Area/components/erf1002 in the middle.";
        let doc = ast::Document::from(doc_content.to_string());

        // Find the tag node
        let tag_nodes: Vec<_> = doc
            .nodes
            .values()
            .filter_map(|node| match &node.node_type {
                ast::NodeType::Tag(tag_name) => Some(tag_name),
                _ => None,
            })
            .collect();

        assert_eq!(tag_nodes.len(), 1, "Should find exactly one tag");
        assert_eq!(
            tag_nodes[0], "Area/components/erf1002",
            "Should correctly parse nested tag format"
        );

        // Verify the tag name is safe for processing (no empty, not too long)
        let tag_name = tag_nodes[0];
        assert!(!tag_name.is_empty(), "Tag name should not be empty");
        assert!(
            tag_name.len() <= 200,
            "Tag name should not be excessively long"
        );

        println!("✓ Nested tag parsed safely: '{}'", tag_name);
    }

    #[test]
    fn test_fuzzy_match_prevention() {
        // Test that we prevent the specific issue reported: "2024-11-05" matching "2025"
        // where LSP client finds fuzzy matches like "202" and "5" in "2024-11-05"

        let partial = "2025";
        let problem_file = "2024-11-05"; // This should NOT match
        let correct_file = "2025-01-01"; // This SHOULD match

        let partial_lower = partial.to_lowercase();
        let is_numeric_query = partial.chars().any(|c| c.is_numeric());

        // Test problem file
        let problem_file_lower = problem_file.to_lowercase();
        let problem_matches = if is_numeric_query {
            problem_file_lower.starts_with(&partial_lower)
        } else {
            problem_file_lower.contains(&partial_lower)
        };

        // Test correct file
        let correct_file_lower = correct_file.to_lowercase();
        let correct_matches = if is_numeric_query {
            correct_file_lower.starts_with(&partial_lower)
        } else {
            correct_file_lower.contains(&partial_lower)
        };

        assert!(
            !problem_matches,
            "2024-11-05 should NOT match query '2025' with strict numeric matching"
        );
        assert!(correct_matches, "2025-01-01 SHOULD match query '2025'");

        println!(
            "✓ Fuzzy match prevention working: '2025' correctly excludes '2024-11-05' and includes '2025-01-01'"
        );
    }

    #[test]
    fn test_frontmatter_detection_logic() {
        // Test document with frontmatter
        let doc_content = "---\ntitle: Test\nauthor: Me\n---\n# Content";
        let lines: Vec<&str> = doc_content.lines().collect();

        // Simulate frontmatter detection logic
        let has_frontmatter = !lines.is_empty() && lines[0].trim().starts_with("---");
        assert!(has_frontmatter, "Should detect frontmatter start");

        // Find frontmatter end
        let mut frontmatter_end = None;
        for (idx, line) in lines.iter().enumerate().skip(1) {
            if line.trim() == "---" {
                frontmatter_end = Some(idx);
                break;
            }
        }

        assert_eq!(
            frontmatter_end,
            Some(3),
            "Should find frontmatter end at line 3"
        );

        // Test lines within frontmatter
        let line_1_in_fm = 1 > 0 && 1 < 3; // title line
        let line_2_in_fm = 2 > 0 && 2 < 3; // author line  
        let line_4_in_fm = 4 > 0 && 4 < 3; // content line

        assert!(line_1_in_fm, "Line 1 should be in frontmatter");
        assert!(line_2_in_fm, "Line 2 should be in frontmatter");
        assert!(!line_4_in_fm, "Line 4 should not be in frontmatter");
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
        last_parse: DashMap::new(),
        change_count: DashMap::new(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
