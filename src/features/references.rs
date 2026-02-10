use tower_lsp::lsp_types::*;

use crate::document::Document;

pub fn find_references(doc: &Document, position: Position, include_declaration: bool) -> Vec<Location> {
    let offset = doc.position_to_offset(position);
    let name = match doc.symbols.symbol_at_offset(offset) {
        Some(n) => n.to_string(),
        None => return vec![],
    };

    let refs = doc.symbols.find_references(&name);
    refs.into_iter()
        .filter(|r| include_declaration || !r.is_definition)
        .map(|r| {
            let start = doc.offset_to_position(r.span.start);
            let end = doc.offset_to_position(r.span.end);
            Location {
                uri: Url::parse("file:///dummy").unwrap(),
                range: Range { start, end },
            }
        })
        .collect()
}
