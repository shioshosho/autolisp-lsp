use tower_lsp::lsp_types::*;

use crate::analysis::{self, SymbolKind};
use crate::builtins;
use crate::document::Document;

pub fn signature_help(doc: &Document, position: Position) -> Option<SignatureHelp> {
    let offset = doc.position_to_offset(position);

    // Find the function call context at cursor position
    let (func_name, arg_index) = analysis::find_function_call_at(&doc.ast, offset)?;

    // Check built-in functions
    if let Some(builtin) = builtins::lookup(&func_name) {
        let parameters: Vec<ParameterInformation> = builtin
            .params
            .iter()
            .map(|p| ParameterInformation {
                label: ParameterLabel::Simple(p.name.to_string()),
                documentation: Some(Documentation::String(p.description.to_string())),
            })
            .collect();

        let sig = SignatureInformation {
            label: builtin.signature.to_string(),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: builtin.description.to_string(),
            })),
            parameters: Some(parameters),
            active_parameter: Some(arg_index as u32),
        };

        return Some(SignatureHelp {
            signatures: vec![sig],
            active_signature: Some(0),
            active_parameter: Some(arg_index as u32),
        });
    }

    // Check user-defined functions
    let defs = doc.symbols.find_definitions(&func_name);
    if let Some(def) = defs.iter().find(|d| d.kind == SymbolKind::Function) {
        let parameters: Vec<ParameterInformation> = def
            .params
            .iter()
            .map(|p| ParameterInformation {
                label: ParameterLabel::Simple(p.to_lowercase()),
                documentation: None,
            })
            .collect();

        let params_str = def.params.join(" ").to_lowercase();
        let locals_str = if def.locals.is_empty() {
            String::new()
        } else {
            format!(" / {}", def.locals.join(" ").to_lowercase())
        };
        let label = format!(
            "({} {}{})",
            def.name.to_lowercase(),
            params_str,
            locals_str
        );

        let sig = SignatureInformation {
            label,
            documentation: def.doc_comment.as_ref().map(|d| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: d.clone(),
                })
            }),
            parameters: Some(parameters),
            active_parameter: Some(arg_index as u32),
        };

        return Some(SignatureHelp {
            signatures: vec![sig],
            active_signature: Some(0),
            active_parameter: Some(arg_index as u32),
        });
    }

    None
}
