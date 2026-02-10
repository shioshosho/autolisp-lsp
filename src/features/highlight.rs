use tower_lsp::lsp_types::*;

use crate::document::Document;

pub fn document_highlight(doc: &Document, position: Position) -> Vec<DocumentHighlight> {
    let offset = doc.position_to_offset(position);
    let name = match doc.symbols.symbol_at_offset(offset) {
        Some(n) => n.to_string(),
        None => return vec![],
    };

    let refs = doc.symbols.find_references(&name);
    refs.into_iter()
        .map(|r| {
            let start = doc.offset_to_position(r.span.start);
            let end = doc.offset_to_position(r.span.end);
            let kind = if r.is_definition {
                Some(DocumentHighlightKind::WRITE)
            } else {
                Some(DocumentHighlightKind::READ)
            };
            DocumentHighlight {
                range: Range { start, end },
                kind,
            }
        })
        .collect()
}
