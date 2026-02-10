pub mod analyzer;
pub mod symbol_table;

pub use analyzer::{analyze, collect_errors, find_function_call_at};
pub use symbol_table::{SymbolInfo, SymbolKind, SymbolReference, SymbolTable};
