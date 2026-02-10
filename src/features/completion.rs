use tower_lsp::lsp_types::*;

use crate::analysis::SymbolKind;
use crate::builtins;
use crate::document::Document;

pub fn completion(doc: &Document, position: Position) -> Vec<CompletionItem> {
    let offset = doc.position_to_offset(position);
    let prefix = get_prefix(&doc.text, offset);
    let prefix_upper = prefix.to_uppercase();

    let mut items = Vec::new();

    // Built-in functions
    if prefix_upper.is_empty() {
        // After '(' show all builtins
        for builtin in builtins::all() {
            items.push(builtin_to_completion(builtin));
        }
    } else {
        for builtin in builtins::by_prefix(&prefix_upper) {
            items.push(builtin_to_completion(builtin));
        }
    }

    // User-defined symbols
    for def in &doc.symbols.definitions {
        if !prefix_upper.is_empty() && !def.name.starts_with(&prefix_upper) {
            continue;
        }

        // Skip if already covered by builtins
        if builtins::lookup(&def.name).is_some() {
            continue;
        }

        let kind = match def.kind {
            SymbolKind::Function => CompletionItemKind::FUNCTION,
            SymbolKind::Variable => CompletionItemKind::VARIABLE,
            SymbolKind::Parameter => CompletionItemKind::VARIABLE,
            SymbolKind::LocalVar => CompletionItemKind::VARIABLE,
        };

        let detail = match def.kind {
            SymbolKind::Function => {
                let params_str = def.params.join(" ");
                Some(format!("({} {})", def.name.to_lowercase(), params_str.to_lowercase()))
            }
            _ => Some(format!("{} ({})", def.name.to_lowercase(), match def.kind {
                SymbolKind::Variable => "variable",
                SymbolKind::Parameter => "parameter",
                SymbolKind::LocalVar => "local variable",
                _ => "",
            })),
        };

        items.push(CompletionItem {
            label: def.name.to_lowercase(),
            kind: Some(kind),
            detail,
            documentation: def.doc_comment.as_ref().map(|d| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::PlainText,
                    value: d.clone(),
                })
            }),
            ..Default::default()
        });
    }

    // Deduplicate by label
    items.sort_by(|a, b| a.label.cmp(&b.label));
    items.dedup_by(|a, b| a.label == b.label);

    items
}

pub fn get_prefix_from_text<'a>(text: &'a str, offset: usize) -> &'a str {
    get_prefix(text, offset)
}

fn get_prefix(text: &str, offset: usize) -> &str {
    let start = text[..offset]
        .rfind(|c: char| c == '(' || c == ')' || c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\'' || c == '"')
        .map(|i| i + 1)
        .unwrap_or(0);
    &text[start..offset]
}

fn builtin_to_completion(builtin: &'static builtins::BuiltinFunction) -> CompletionItem {
    CompletionItem {
        label: builtin.name.to_lowercase(),
        kind: Some(CompletionItemKind::FUNCTION),
        detail: Some(builtin.signature.to_string()),
        documentation: Some(Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: builtin.description.to_string(),
        })),
        ..Default::default()
    }
}
