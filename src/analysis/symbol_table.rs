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
