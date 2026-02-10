use tower_lsp::lsp_types::Position;

use crate::analysis::{self, SymbolTable};
use crate::parser::ast::AstNode;

pub struct Document {
    pub text: String,
    pub line_offsets: Vec<usize>,
    pub ast: Vec<AstNode>,
    pub comments: Vec<AstNode>,
    pub symbols: SymbolTable,
}

impl Document {
    pub fn new(text: String) -> Self {
        let line_offsets = compute_line_offsets(&text);
        let (ast, comments) = crate::parser::parse(&text);
        let symbols = analysis::analyze(&ast);
        Self {
            text,
            line_offsets,
            ast,
            comments,
            symbols,
        }
    }

    pub fn update(&mut self, text: String) {
        self.line_offsets = compute_line_offsets(&text);
        let (ast, comments) = crate::parser::parse(&text);
        self.symbols = analysis::analyze(&ast);
        self.ast = ast;
        self.comments = comments;
        self.text = text;
    }

    pub fn offset_to_position(&self, offset: usize) -> Position {
        let line = self
            .line_offsets
            .partition_point(|&o| o <= offset)
            .saturating_sub(1);
        let line_start = self.line_offsets[line];
        let utf16_col = self.text[line_start..offset].encode_utf16().count();
        Position::new(line as u32, utf16_col as u32)
    }

    pub fn position_to_offset(&self, position: Position) -> usize {
        let line = position.line as usize;
        if line >= self.line_offsets.len() {
            return self.text.len();
        }
        let line_start = self.line_offsets[line];
        let line_text = if line + 1 < self.line_offsets.len() {
            &self.text[line_start..self.line_offsets[line + 1]]
        } else {
            &self.text[line_start..]
        };

        let mut utf16_count = 0u32;
        for (byte_idx, ch) in line_text.char_indices() {
            if utf16_count >= position.character {
                return line_start + byte_idx;
            }
            utf16_count += ch.len_utf16() as u32;
        }
        line_start + line_text.len()
    }

    pub fn line_count(&self) -> usize {
        self.line_offsets.len()
    }

    /// Extract the word (symbol-like token) at the given byte offset from raw text.
    /// This is used as a fallback when symbol_at_offset doesn't find anything
    /// (e.g., for keywords like `defun` that the parser consumes into special AST nodes).
    pub fn word_at_offset(&self, offset: usize) -> Option<&str> {
        if offset >= self.text.len() {
            return None;
        }
        let bytes = self.text.as_bytes();
        // Check if offset is within a word character
        if !is_symbol_char(bytes[offset]) {
            return None;
        }
        // Find word boundaries
        let start = (0..offset)
            .rev()
            .take_while(|&i| is_symbol_char(bytes[i]))
            .last()
            .unwrap_or(offset);
        let end = (offset..bytes.len())
            .take_while(|&i| is_symbol_char(bytes[i]))
            .last()
            .map(|i| i + 1)
            .unwrap_or(offset);
        if start < end {
            Some(&self.text[start..end])
        } else {
            None
        }
    }

    pub fn line_text(&self, line: usize) -> &str {
        if line >= self.line_offsets.len() {
            return "";
        }
        let start = self.line_offsets[line];
        let end = if line + 1 < self.line_offsets.len() {
            self.line_offsets[line + 1]
        } else {
            self.text.len()
        };
        &self.text[start..end]
    }
}

fn compute_line_offsets(text: &str) -> Vec<usize> {
    let mut offsets = vec![0];
    for (i, b) in text.bytes().enumerate() {
        if b == b'\n' {
            offsets.push(i + 1);
        }
    }
    offsets
}

fn is_symbol_char(b: u8) -> bool {
    matches!(b,
        b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9'
        | b'-' | b'_' | b'*' | b'+' | b'/' | b'='
        | b'<' | b'>' | b'!' | b'?' | b'.' | b':'
        | b'\\' | b'#' | b'~' | b'$' | b'%' | b'&'
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_position_roundtrip() {
        let doc = Document::new("first line\nsecond line\nthird".to_string());
        let pos = doc.offset_to_position(11);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);

        let offset = doc.position_to_offset(Position::new(1, 7));
        assert_eq!(&doc.text[offset..offset + 4], "line");
    }

    #[test]
    fn test_line_count() {
        let doc = Document::new("a\nb\nc".to_string());
        assert_eq!(doc.line_count(), 3);
    }

    #[test]
    fn test_parsing_on_create() {
        let doc = Document::new("(defun test () (+ 1 2))".to_string());
        assert!(!doc.ast.is_empty());
        assert!(!doc.symbols.definitions.is_empty());
    }

    #[test]
    fn test_symbol_table() {
        let doc = Document::new(
            "(defun my-func (x y / temp)\n  (setq result (+ x y))\n  result\n)".to_string(),
        );
        // Should have: my-func (function), x, y (params), temp (local), result (setq var)
        assert!(doc.symbols.find_definition("MY-FUNC").is_some());
        assert!(doc.symbols.find_definition("X").is_some());
        assert!(doc.symbols.find_definition("RESULT").is_some());
    }
}
