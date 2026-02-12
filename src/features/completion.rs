use tower_lsp::lsp_types::*;

use crate::analysis::SymbolKind;
use crate::builtins;
use crate::document::Document;

pub fn completion(doc: &Document, position: Position) -> Vec<CompletionItem> {
    let offset = doc.position_to_offset(position);
    let prefix = get_prefix(&doc.text, offset);
    let prefix_upper = prefix.to_uppercase();

    let mut items = Vec::new();

    // Snippet completions
    items.extend(snippet_completions(&prefix_upper));

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

fn snippet_completions(prefix_upper: &str) -> Vec<CompletionItem> {
    static SNIPPETS: &[(&str, &str, &str)] = &[
        (
            "defun",
            "(defun ${1:name} (${2:params})\n  ${0}\n)",
            "Define a function",
        ),
        (
            "defun (command)",
            "(defun c:${1:name} (${2:/ locals})\n  ${0}\n)",
            "Define a command function",
        ),
        (
            "if",
            "(if ${1:test}\n  ${2:then}\n  ${3:else}\n)",
            "If-then-else conditional",
        ),
        (
            "if (progn)",
            "(if ${1:test}\n  (progn\n    ${0}\n  )\n)",
            "If with progn block",
        ),
        (
            "cond",
            "(cond\n  (${1:test1} ${2:result1})\n  (T ${0})\n)",
            "Multi-branch conditional",
        ),
        (
            "while",
            "(while ${1:test}\n  ${0}\n)",
            "While loop",
        ),
        (
            "foreach",
            "(foreach ${1:item} ${2:list}\n  ${0}\n)",
            "Iterate over list elements",
        ),
        (
            "repeat",
            "(repeat ${1:count}\n  ${0}\n)",
            "Repeat N times",
        ),
        (
            "setq",
            "(setq ${1:var} ${0})",
            "Set variable value",
        ),
        (
            "lambda",
            "(lambda (${1:params}) ${0})",
            "Anonymous function",
        ),
        (
            "progn",
            "(progn\n  ${0}\n)",
            "Evaluate expressions sequentially",
        ),
        (
            "mapcar",
            "(mapcar '${1:function} ${0})",
            "Apply function to list elements",
        ),
        (
            "vl-catch-all-apply",
            "(vl-catch-all-apply '${1:function} (list ${0}))",
            "Apply function with error trapping",
        ),
        (
            "*error*",
            "(defun *error* (msg)\n  ${0}\n  (princ)\n)",
            "Error handler function",
        ),
        (
            "ssget",
            "(ssget \"${1:X}\" ${0})",
            "Create selection set",
        ),
        (
            "entget",
            "(entget (car (entsel ${0})))",
            "Get entity data from user selection",
        ),
    ];

    SNIPPETS
        .iter()
        .filter(|(label, _, _)| {
            prefix_upper.is_empty() || label.to_uppercase().starts_with(prefix_upper)
        })
        .map(|(label, body, detail)| CompletionItem {
            label: label.to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some(detail.to_string()),
            insert_text: Some(body.to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some(format!("0_{}", label)),
            ..Default::default()
        })
        .collect()
}
