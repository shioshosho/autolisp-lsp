use tower_lsp::lsp_types::*;

use crate::document::Document;

pub fn goto_definition(doc: &Document, position: Position) -> Option<Location> {
    let offset = doc.position_to_offset(position);
    let name = doc.symbols.symbol_at_offset(offset)?;

    // Find the definition (prefer Function kind)
    let defs = doc.symbols.find_definitions(name);
    let def = defs
        .iter()
        .find(|d| d.kind == crate::analysis::SymbolKind::Function)
        .or_else(|| defs.first())?;

    let start = doc.offset_to_position(def.name_span.start);
    let end = doc.offset_to_position(def.name_span.end);

    Some(Location {
        uri: Url::parse("file:///dummy").unwrap(), // Will be replaced by caller
        range: Range { start, end },
    })
}
