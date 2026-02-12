use crate::parser::token::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Variable,
    Parameter,
    LocalVar,
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub name: String,
    pub kind: SymbolKind,
    pub span: Span,
    pub name_span: Span,
    pub params: Vec<String>,
    pub locals: Vec<String>,
    pub doc_comment: Option<String>,
}

#[derive(Debug, Clone)]
pub struct SymbolReference {
    pub name: String,
    pub span: Span,
    pub is_definition: bool,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    pub definitions: Vec<SymbolInfo>,
    pub references: Vec<SymbolReference>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_definition(&mut self, info: SymbolInfo) {
        self.definitions.push(info);
    }

    pub fn add_reference(&mut self, reference: SymbolReference) {
        self.references.push(reference);
    }

    pub fn find_definition(&self, name: &str) -> Option<&SymbolInfo> {
        let upper = name.to_uppercase();
        self.definitions.iter().find(|d| d.name == upper)
    }

    pub fn find_definitions(&self, name: &str) -> Vec<&SymbolInfo> {
        let upper = name.to_uppercase();
        self.definitions.iter().filter(|d| d.name == upper).collect()
    }

    pub fn find_references(&self, name: &str) -> Vec<&SymbolReference> {
        let upper = name.to_uppercase();
        self.references.iter().filter(|r| r.name == upper).collect()
    }

    /// Find the most specific definition for a symbol at a given span.
    /// Prefers scoped definitions (parameters/locals) whose enclosing function
    /// contains the given span, falling back to global definitions.
    pub fn find_definition_at(&self, name: &str, span: Span) -> Option<&SymbolInfo> {
        let upper = name.to_uppercase();
        let mut best: Option<&SymbolInfo> = None;
        for d in &self.definitions {
            if d.name != upper {
                continue;
            }
            match d.kind {
                SymbolKind::Parameter | SymbolKind::LocalVar => {
                    // Check if the reference is within the scope of this definition
                    if span.start >= d.span.start && span.end <= d.span.end {
                        // Prefer the narrowest scope
                        if let Some(prev) = best {
                            let prev_size = prev.span.end - prev.span.start;
                            let this_size = d.span.end - d.span.start;
                            if this_size < prev_size {
                                best = Some(d);
                            }
                        } else {
                            best = Some(d);
                        }
                    }
                }
                _ => {
                    if best.is_none() {
                        best = Some(d);
                    }
                }
            }
        }
        best
    }

    pub fn symbol_at_offset(&self, offset: usize) -> Option<&str> {
        // Check references first (more numerous)
        for r in &self.references {
            if offset >= r.span.start && offset < r.span.end {
                return Some(&r.name);
            }
        }
        // Check definitions
        for d in &self.definitions {
            if offset >= d.name_span.start && offset < d.name_span.end {
                return Some(&d.name);
            }
        }
        None
    }
}
