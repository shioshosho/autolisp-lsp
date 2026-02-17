use tower_lsp::lsp_types::*;

use crate::analysis::SymbolKind as AnalysisSymbolKind;
use crate::document::Document;

#[allow(deprecated)]
pub fn document_symbol(doc: &Document) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();

    for def in &doc.symbols.definitions {
        match def.kind {
            AnalysisSymbolKind::Function => {
                let range = Range {
                    start: doc.offset_to_position(def.span.start),
                    end: doc.offset_to_position(def.span.end),
                };
                let selection_range = Range {
                    start: doc.offset_to_position(def.name_span.start),
                    end: doc.offset_to_position(def.name_span.end),
                };

                let params_str = def.params.join(" ").to_lowercase();
                let detail = if params_str.is_empty() {
                    None
                } else {
                    Some(format!("({})", params_str))
                };

                // Collect children: parameters and local variables scoped to this function
                let mut children = Vec::new();
                for child_def in &doc.symbols.definitions {
                    if child_def.name == def.name && child_def.kind == def.kind {
                        continue;
                    }
                    match child_def.kind {
                        AnalysisSymbolKind::Parameter | AnalysisSymbolKind::LocalVar => {
                            // Check if this child is scoped within this function
                            if child_def.span.start >= def.span.start
                                && child_def.span.end <= def.span.end
                            {
                                let child_kind = match child_def.kind {
                                    AnalysisSymbolKind::Parameter => SymbolKind::VARIABLE,
                                    AnalysisSymbolKind::LocalVar => SymbolKind::VARIABLE,
                                    _ => unreachable!(),
                                };
                                let child_range = Range {
                                    start: doc.offset_to_position(child_def.name_span.start),
                                    end: doc.offset_to_position(child_def.name_span.end),
                                };
                                children.push(DocumentSymbol {
                                    name: child_def.name.to_lowercase(),
                                    detail: None,
                                    kind: child_kind,
                                    tags: None,
                                    deprecated: None,
                                    range: child_range,
                                    selection_range: child_range,
                                    children: None,
                                });
                            }
                        }
                        _ => {}
                    }
                }

                symbols.push(DocumentSymbol {
                    name: def.name.to_lowercase(),
                    detail,
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range,
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                });
            }
            AnalysisSymbolKind::Variable => {
                // Top-level setq variables only (not scoped inside a function)
                let is_top_level = !doc.symbols.definitions.iter().any(|f| {
                    f.kind == AnalysisSymbolKind::Function
                        && def.span.start >= f.span.start
                        && def.span.end <= f.span.end
                });
                if !is_top_level {
                    continue;
                }

                let range = Range {
                    start: doc.offset_to_position(def.name_span.start),
                    end: doc.offset_to_position(def.name_span.end),
                };

                symbols.push(DocumentSymbol {
                    name: def.name.to_lowercase(),
                    detail: None,
                    kind: SymbolKind::VARIABLE,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range: range,
                    children: None,
                });
            }
            _ => {}
        }
    }

    symbols
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(deprecated)]
    fn symbol_names(symbols: &[DocumentSymbol]) -> Vec<&str> {
        symbols.iter().map(|s| s.name.as_str()).collect()
    }

    #[test]
    fn test_defun_symbols() {
        let doc = Document::new(
            "(defun my-func (x y / temp)\n  (+ x y)\n)\n(defun another () nil)".to_string(),
        );
        let symbols = document_symbol(&doc);
        let names = symbol_names(&symbols);
        assert!(names.contains(&"my-func"));
        assert!(names.contains(&"another"));
        assert_eq!(symbols.len(), 2);
    }

    #[allow(deprecated)]
    #[test]
    fn test_defun_children() {
        let doc =
            Document::new("(defun my-func (x y / temp)\n  (+ x y)\n)".to_string());
        let symbols = document_symbol(&doc);
        assert_eq!(symbols.len(), 1);
        let func = &symbols[0];
        assert_eq!(func.kind, SymbolKind::FUNCTION);
        let children = func.children.as_ref().unwrap();
        let child_names: Vec<&str> = children.iter().map(|c| c.name.as_str()).collect();
        assert!(child_names.contains(&"x"));
        assert!(child_names.contains(&"y"));
        assert!(child_names.contains(&"temp"));
    }

    #[allow(deprecated)]
    #[test]
    fn test_top_level_variable() {
        let doc = Document::new("(setq *global* 42)\n(defun foo () nil)".to_string());
        let symbols = document_symbol(&doc);
        let names = symbol_names(&symbols);
        assert!(names.contains(&"*global*"));
        assert!(names.contains(&"foo"));
    }

    #[allow(deprecated)]
    #[test]
    fn test_scoped_variable_excluded() {
        let doc = Document::new("(defun foo () (setq local-var 1))".to_string());
        let symbols = document_symbol(&doc);
        // Only the function should appear at top level
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "foo");
    }

    #[test]
    fn test_empty_document() {
        let doc = Document::new("".to_string());
        let symbols = document_symbol(&doc);
        assert!(symbols.is_empty());
    }
}
