use std::sync::OnceLock;

use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::config::{self, Config};
use crate::document::Document;
use crate::features;

pub struct Backend {
    pub client: Client,
    pub documents: DashMap<Url, Document>,
    pub config: OnceLock<Config>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
            config: OnceLock::new(),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let workspace_root = params
            .root_uri
            .as_ref()
            .and_then(|uri| uri.to_file_path().ok());
        let cfg = config::load_config(workspace_root.as_deref());
        let _ = self.config.set(cfg);

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_highlight_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec!["(".to_string(), " ".to_string()]),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), " ".to_string()]),
                    retrigger_characters: Some(vec![" ".to_string()]),
                    ..Default::default()
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_range_formatting_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: features::semantic_tokens::TOKEN_TYPES.to_vec(),
                                token_modifiers: features::semantic_tokens::TOKEN_MODIFIERS
                                    .to_vec(),
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            ..Default::default()
                        },
                    ),
                ),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "autolisp-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "AutoLISP LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text;
        let doc = Document::new(text);
        self.publish_diagnostics(&uri, &doc).await;
        self.documents.insert(uri, doc);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        if let Some(change) = params.content_changes.into_iter().last() {
            if let Some(mut doc) = self.documents.get_mut(&uri) {
                doc.update(change.text);
                self.publish_diagnostics(&uri, &doc).await;
            }
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = &params.text_document.uri;
        // Clear diagnostics
        self.client
            .publish_diagnostics(uri.clone(), vec![], None)
            .await;
        self.documents.remove(uri);
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        if !self.config().lsp.enable {
            return Ok(None);
        }
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let result = self.documents.get(&uri).and_then(|doc| {
            features::definition::goto_definition(&doc, position).map(|mut loc| {
                loc.uri = uri.clone();
                GotoDefinitionResponse::Scalar(loc)
            })
        });

        // If not found in current document, search other documents
        if result.is_none() {
            if let Some(doc) = self.documents.get(&uri) {
                let offset = doc.position_to_offset(position);
                if let Some(name) = doc.symbols.symbol_at_offset(offset) {
                    let name = name.to_string();
                    for entry in self.documents.iter() {
                        if *entry.key() == uri {
                            continue;
                        }
                        let other_doc = entry.value();
                        if let Some(def) = other_doc.symbols.find_definition(&name) {
                            if def.kind == crate::analysis::SymbolKind::Function {
                                let start = other_doc.offset_to_position(def.name_span.start);
                                let end = other_doc.offset_to_position(def.name_span.end);
                                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                                    uri: entry.key().clone(),
                                    range: Range { start, end },
                                })));
                            }
                        }
                    }
                }
            }
        }

        Ok(result)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        if !self.config().lsp.enable {
            return Ok(None);
        }
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;

        let mut all_refs = Vec::new();

        if let Some(doc) = self.documents.get(&uri) {
            let mut refs = features::references::find_references(&doc, position, include_declaration);
            for r in &mut refs {
                r.uri = uri.clone();
            }
            let offset = doc.position_to_offset(position);
            let name = doc.symbols.symbol_at_offset(offset).map(|n| n.to_string());
            all_refs.extend(refs);

            // Search other documents
            if let Some(name) = name {
                for entry in self.documents.iter() {
                    if *entry.key() == uri {
                        continue;
                    }
                    let other_doc = entry.value();
                    let other_refs = other_doc.symbols.find_references(&name);
                    for r in other_refs {
                        if !include_declaration && r.is_definition {
                            continue;
                        }
                        let start = other_doc.offset_to_position(r.span.start);
                        let end = other_doc.offset_to_position(r.span.end);
                        all_refs.push(Location {
                            uri: entry.key().clone(),
                            range: Range { start, end },
                        });
                    }
                }
            }
        }

        if all_refs.is_empty() {
            Ok(None)
        } else {
            Ok(Some(all_refs))
        }
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        if !self.config().lsp.enable {
            return Ok(None);
        }
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let highlights = self
            .documents
            .get(&uri)
            .map(|doc| features::highlight::document_highlight(&doc, position))
            .unwrap_or_default();

        if highlights.is_empty() {
            Ok(None)
        } else {
            Ok(Some(highlights))
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        if !self.config().lsp.enable {
            return Ok(None);
        }
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let result = self
            .documents
            .get(&uri)
            .and_then(|doc| features::hover::hover(&doc, position));

        Ok(result)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        if !self.config().lsp.enable {
            return Ok(None);
        }
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let mut items = self
            .documents
            .get(&uri)
            .map(|doc| features::completion::completion(&doc, position))
            .unwrap_or_default();

        // Add functions from other open documents
        if let Some(doc) = self.documents.get(&uri) {
            let offset = doc.position_to_offset(position);
            let prefix = features::completion::get_prefix_from_text(&doc.text, offset);
            let prefix_upper = prefix.to_uppercase();

            let existing_labels: std::collections::HashSet<String> =
                items.iter().map(|i| i.label.clone()).collect();

            for entry in self.documents.iter() {
                if *entry.key() == uri {
                    continue;
                }
                let other_doc = entry.value();
                for def in &other_doc.symbols.definitions {
                    if def.kind != crate::analysis::SymbolKind::Function {
                        continue;
                    }
                    if !prefix_upper.is_empty() && !def.name.starts_with(&prefix_upper) {
                        continue;
                    }
                    let label = def.name.to_lowercase();
                    if existing_labels.contains(&label) {
                        continue;
                    }
                    let params_str = def.params.join(" ").to_lowercase();
                    items.push(CompletionItem {
                        label,
                        kind: Some(CompletionItemKind::FUNCTION),
                        detail: Some(format!("({} {})", def.name.to_lowercase(), params_str)),
                        ..Default::default()
                    });
                }
            }
        }

        if items.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(items)))
        }
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        if !self.config().lsp.enable {
            return Ok(None);
        }
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let result = self
            .documents
            .get(&uri)
            .and_then(|doc| features::signature::signature_help(&doc, position));

        Ok(result)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        if !self.config().lsp.enable {
            return Ok(None);
        }
        let uri = params.text_document.uri;

        let symbols = self
            .documents
            .get(&uri)
            .map(|doc| features::document_symbol::document_symbol(&doc))
            .unwrap_or_default();

        if symbols.is_empty() {
            Ok(None)
        } else {
            Ok(Some(DocumentSymbolResponse::Nested(symbols)))
        }
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        if !self.config().format.enable {
            return Ok(None);
        }
        let uri = params.text_document.uri;
        let cfg = self.config.get().cloned().unwrap_or_default();

        let edits = self
            .documents
            .get(&uri)
            .map(|doc| features::formatting::format_document(&doc, &params.options, &cfg.format))
            .unwrap_or_default();

        if edits.is_empty() {
            Ok(None)
        } else {
            Ok(Some(edits))
        }
    }

    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        if !self.config().format.enable {
            return Ok(None);
        }
        let uri = params.text_document.uri;
        let cfg = self.config.get().cloned().unwrap_or_default();

        let edits = self
            .documents
            .get(&uri)
            .map(|doc| {
                features::formatting::format_range(&doc, &params.range, &params.options, &cfg.format)
            })
            .unwrap_or_default();

        if edits.is_empty() {
            Ok(None)
        } else {
            Ok(Some(edits))
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        if !self.config().lsp.enable {
            return Ok(None);
        }
        let uri = params.text_document.uri;

        let tokens = self
            .documents
            .get(&uri)
            .map(|doc| features::semantic_tokens::semantic_tokens_full(&doc))
            .unwrap_or_default();

        if tokens.is_empty() {
            Ok(None)
        } else {
            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })))
        }
    }
}

impl Backend {
    fn config(&self) -> &Config {
        self.config.get().expect("config not initialized")
    }

    async fn publish_diagnostics(&self, uri: &Url, doc: &Document) {
        if !self.config().lsp.enable {
            self.client
                .publish_diagnostics(uri.clone(), vec![], None)
                .await;
            return;
        }
        let errors = crate::analysis::collect_errors(&doc.ast);
        let diagnostics: Vec<Diagnostic> = errors
            .into_iter()
            .map(|(msg, span)| {
                let start = doc.offset_to_position(span.start);
                let end = doc.offset_to_position(span.end);
                Diagnostic {
                    range: Range { start, end },
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("autolisp-lsp".to_string()),
                    message: msg,
                    ..Default::default()
                }
            })
            .collect();
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
}
