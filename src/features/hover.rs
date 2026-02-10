use tower_lsp::lsp_types::*;

use crate::analysis::SymbolKind;
use crate::builtins;
use crate::document::Document;

pub fn hover(doc: &Document, position: Position) -> Option<Hover> {
    let offset = doc.position_to_offset(position);
    let name = doc.symbols.symbol_at_offset(offset)?;

    // Check built-in functions first
    if let Some(builtin) = builtins::lookup(name) {
        let contents = format!(
            "```autolisp\n{}\n```\n---\n{}",
            builtin.signature, builtin.description
        );
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: contents,
            }),
            range: None,
        });
    }

    // Check user-defined functions
    let defs = doc.symbols.find_definitions(name);
    if let Some(def) = defs.iter().find(|d| d.kind == SymbolKind::Function) {
        let params_str = def.params.join(" ");
        let locals_str = if def.locals.is_empty() {
            String::new()
        } else {
            format!(" / {}", def.locals.join(" "))
        };
        let signature = format!("(defun {} ({}{})\n  ...)", def.name, params_str, locals_str);

        let mut contents = format!("```autolisp\n{}\n```", signature);
        if let Some(ref doc_comment) = def.doc_comment {
            contents.push_str(&format!("\n---\n{}", doc_comment));
        }

        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: contents,
            }),
            range: None,
        });
    }

    // Check variables/parameters
    if let Some(def) = defs.first() {
        let kind_str = match def.kind {
            SymbolKind::Variable => "variable",
            SymbolKind::Parameter => "parameter",
            SymbolKind::LocalVar => "local variable",
            SymbolKind::Function => unreachable!(),
        };
        let contents = format!("```autolisp\n{}\n```\n---\n*{}*", def.name, kind_str);
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: contents,
            }),
            range: None,
        });
    }

    None
}
