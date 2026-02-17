use tower_lsp::lsp_types::{SemanticToken, SemanticTokenModifier, SemanticTokenType};

use crate::builtins;
use crate::document::Document;
use crate::parser::ast::AstNode;
use crate::parser::token::Span;

pub static TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,  // 0
    SemanticTokenType::VARIABLE,  // 1
    SemanticTokenType::PARAMETER, // 2
    SemanticTokenType::KEYWORD,   // 3
    SemanticTokenType::OPERATOR,  // 4
    SemanticTokenType::STRING,    // 5
    SemanticTokenType::NUMBER,    // 6
    SemanticTokenType::COMMENT,   // 7
    SemanticTokenType::TYPE,      // 8
];

pub static TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DEFINITION,      // bit 0
    SemanticTokenModifier::DEFAULT_LIBRARY, // bit 1
];

const TT_FUNCTION: u32 = 0;
const TT_VARIABLE: u32 = 1;
const TT_PARAMETER: u32 = 2;
const TT_KEYWORD: u32 = 3;
const TT_OPERATOR: u32 = 4;
const TT_STRING: u32 = 5;
const TT_NUMBER: u32 = 6;
const TT_COMMENT: u32 = 7;
const TT_TYPE: u32 = 8;

const MOD_NONE: u32 = 0;
const MOD_DEFINITION: u32 = 1 << 0;
const MOD_DEFAULT_LIBRARY: u32 = 1 << 1;

struct RawToken {
    span: Span,
    token_type: u32,
    modifiers: u32,
}

pub fn semantic_tokens_full(doc: &Document) -> Vec<SemanticToken> {
    let mut raw_tokens = Vec::new();

    for node in &doc.ast {
        collect_tokens_from_node(node, doc, &mut raw_tokens);
    }

    // Collect comments from doc.comments (comments stripped from AST by parser)
    for comment in &doc.comments {
        if let AstNode::Comment(_, span) = comment {
            raw_tokens.push(RawToken {
                span: *span,
                token_type: TT_COMMENT,
                modifiers: MOD_NONE,
            });
        }
    }

    // Sort by position
    raw_tokens.sort_by_key(|t| t.span.start);

    // Delta-encode
    encode_tokens(&raw_tokens, doc)
}

fn collect_tokens_from_node(node: &AstNode, doc: &Document, tokens: &mut Vec<RawToken>) {
    match node {
        AstNode::IntegerLit(_, span) | AstNode::RealLit(_, span) => {
            tokens.push(RawToken {
                span: *span,
                token_type: TT_NUMBER,
                modifiers: MOD_NONE,
            });
        }
        AstNode::StringLit(_, span) => {
            tokens.push(RawToken {
                span: *span,
                token_type: TT_STRING,
                modifiers: MOD_NONE,
            });
        }
        AstNode::Nil(span) | AstNode::T(span) => {
            tokens.push(RawToken {
                span: *span,
                token_type: TT_TYPE,
                modifiers: MOD_NONE,
            });
        }
        AstNode::Symbol(name, span) => {
            let (tt, mods) = classify_symbol(name, doc, *span);
            tokens.push(RawToken {
                span: *span,
                token_type: tt,
                modifiers: mods,
            });
        }
        AstNode::Comment(_, span) => {
            tokens.push(RawToken {
                span: *span,
                token_type: TT_COMMENT,
                modifiers: MOD_NONE,
            });
        }
        AstNode::List(children, _span) => {
            // The first child of a list in function-call position may be a function/operator
            if let Some((first, rest)) = children.split_first() {
                if let AstNode::Symbol(name, sym_span) = first {
                    let (tt, mods) = classify_head_symbol(name);
                    tokens.push(RawToken {
                        span: *sym_span,
                        token_type: tt,
                        modifiers: mods,
                    });
                } else {
                    collect_tokens_from_node(first, doc, tokens);
                }
                for child in rest {
                    collect_tokens_from_node(child, doc, tokens);
                }
            }
        }
        AstNode::Defun {
            name,
            name_span,
            params,
            locals,
            body,
            span,
            ..
        } => {
            // "defun" or "defun-q" keyword
            let kw_span = find_keyword_span(&doc.text, span.start, "defun-q")
                .or_else(|| find_keyword_span(&doc.text, span.start, "defun"));
            if let Some(kw_span) = kw_span {
                tokens.push(RawToken {
                    span: kw_span,
                    token_type: TT_KEYWORD,
                    modifiers: MOD_NONE,
                });
            }

            // Function name (definition)
            tokens.push(RawToken {
                span: *name_span,
                token_type: TT_FUNCTION,
                modifiers: MOD_DEFINITION,
            });

            // Check for "c:" prefix - if name starts with C: it's still just a function definition
            let _ = name; // name used above via name_span

            // Parameters
            for (_, param_span) in params {
                tokens.push(RawToken {
                    span: *param_span,
                    token_type: TT_PARAMETER,
                    modifiers: MOD_DEFINITION,
                });
            }

            // Local variables
            for (_, local_span) in locals {
                tokens.push(RawToken {
                    span: *local_span,
                    token_type: TT_VARIABLE,
                    modifiers: MOD_DEFINITION,
                });
            }

            // Body
            for child in body {
                collect_tokens_from_node(child, doc, tokens);
            }
        }
        AstNode::Lambda {
            params, body, span, ..
        } => {
            // "lambda" keyword
            if let Some(kw_span) = find_keyword_span(&doc.text, span.start, "lambda") {
                tokens.push(RawToken {
                    span: kw_span,
                    token_type: TT_KEYWORD,
                    modifiers: MOD_NONE,
                });
            }

            // Parameters
            for (_, param_span) in params {
                tokens.push(RawToken {
                    span: *param_span,
                    token_type: TT_PARAMETER,
                    modifiers: MOD_DEFINITION,
                });
            }

            // Body
            for child in body {
                collect_tokens_from_node(child, doc, tokens);
            }
        }
        AstNode::Quote(inner, _) => {
            collect_tokens_from_node(inner, doc, tokens);
        }
        AstNode::DottedPair(left, right, _) => {
            collect_tokens_from_node(left, doc, tokens);
            collect_tokens_from_node(right, doc, tokens);
        }
        AstNode::Error(_, _) => {}
    }
}

/// Classify a symbol in head (function-call) position of a list.
fn classify_head_symbol(name: &str) -> (u32, u32) {
    let upper = name.to_uppercase();

    if is_keyword(&upper) {
        return (TT_KEYWORD, MOD_NONE);
    }
    if is_operator(&upper) {
        return (TT_OPERATOR, MOD_NONE);
    }
    if builtins::lookup(&upper).is_some() {
        return (TT_FUNCTION, MOD_DEFAULT_LIBRARY);
    }
    (TT_FUNCTION, MOD_NONE)
}

/// Classify a symbol in non-head position.
fn classify_symbol(name: &str, doc: &Document, span: Span) -> (u32, u32) {
    let upper = name.to_uppercase();

    if is_keyword(&upper) {
        return (TT_KEYWORD, MOD_NONE);
    }
    if is_operator(&upper) {
        return (TT_OPERATOR, MOD_NONE);
    }

    // Check if it's a parameter or local in the symbol table
    if let Some(def) = doc.symbols.find_definition_at(&upper, span) {
        return match def.kind {
            crate::analysis::SymbolKind::Function => (TT_FUNCTION, MOD_NONE),
            crate::analysis::SymbolKind::Parameter => (TT_PARAMETER, MOD_NONE),
            crate::analysis::SymbolKind::LocalVar => (TT_VARIABLE, MOD_NONE),
            crate::analysis::SymbolKind::Variable => (TT_VARIABLE, MOD_NONE),
        };
    }

    // Check builtin
    if builtins::lookup(&upper).is_some() {
        return (TT_FUNCTION, MOD_DEFAULT_LIBRARY);
    }

    // Default: variable
    (TT_VARIABLE, MOD_NONE)
}

fn is_keyword(upper: &str) -> bool {
    matches!(
        upper,
        "DEFUN"
            | "DEFUN-Q"
            | "IF"
            | "COND"
            | "WHILE"
            | "REPEAT"
            | "FOREACH"
            | "PROGN"
            | "SETQ"
            | "AND"
            | "OR"
            | "NOT"
            | "COMMAND"
            | "COMMAND-S"
            | "LAMBDA"
            | "QUOTE"
            | "FUNCTION"
    )
}

fn is_operator(upper: &str) -> bool {
    matches!(
        upper,
        "+" | "-"
            | "*"
            | "/"
            | "="
            | "/="
            | "<"
            | ">"
            | "<="
            | ">="
            | "1+"
            | "1-"
            | "~"
            | "EQ"
            | "EQUAL"
            | "LOGAND"
            | "LOGIOR"
            | "LSH"
            | "BOOLE"
            | "REM"
    )
}

/// Find the span of a keyword (like "defun") inside the source text starting from `start`.
/// The defun node span starts at '(', so we skip '(' and whitespace to find the keyword.
fn find_keyword_span(text: &str, start: usize, keyword: &str) -> Option<Span> {
    let slice = &text[start..];
    // Skip '('
    let after_paren = slice.strip_prefix('(')?;
    let offset_after_paren = start + 1;
    // Skip whitespace
    let trimmed = after_paren.trim_start();
    let ws_len = after_paren.len() - trimmed.len();
    let kw_start = offset_after_paren + ws_len;

    // Case-insensitive match
    let kw_len = keyword.len();
    if trimmed.len() >= kw_len
        && trimmed[..kw_len].eq_ignore_ascii_case(keyword)
    {
        Some(Span::new(kw_start, kw_start + kw_len))
    } else {
        None
    }
}

fn encode_tokens(raw: &[RawToken], doc: &Document) -> Vec<SemanticToken> {
    let mut result = Vec::with_capacity(raw.len());
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for tok in raw {
        let start_pos = doc.offset_to_position(tok.span.start);
        let end_pos = doc.offset_to_position(tok.span.end);

        let line = start_pos.line;
        let start_char = start_pos.character;

        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            start_char - prev_start
        } else {
            start_char
        };

        // For multi-line tokens (like block comments), just use the length on the first line
        let length = if start_pos.line == end_pos.line {
            end_pos.character - start_pos.character
        } else {
            // Approximate: use the text length in UTF-16
            let text_slice = &doc.text[tok.span.start..tok.span.end];
            text_slice.encode_utf16().count() as u32
        };

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: tok.token_type,
            token_modifiers_bitset: tok.modifiers,
        });

        prev_line = line;
        prev_start = start_char;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_semantic_tokens() {
        let doc = Document::new("(setq x 10)".to_string());
        let tokens = semantic_tokens_full(&doc);
        assert!(!tokens.is_empty());
        // Should have tokens for: setq (keyword), x (variable), 10 (number)
        assert!(tokens.len() >= 3);
    }

    #[test]
    fn test_defun_tokens() {
        let doc = Document::new("(defun test (a b / c)\n  (+ a b)\n)".to_string());
        let tokens = semantic_tokens_full(&doc);
        // Should have: defun(keyword), test(function/def), a(param/def), b(param/def),
        // c(variable/def), +(operator), a(param), b(param)
        assert!(tokens.len() >= 7);
    }

    #[test]
    fn test_comment_tokens() {
        let doc = Document::new("; this is a comment\n(+ 1 2)".to_string());
        let tokens = semantic_tokens_full(&doc);
        // Should include comment token, +, 1, 2
        assert!(tokens.len() >= 4);
    }

    #[test]
    fn test_string_token() {
        let doc = Document::new("(setq s \"hello\")".to_string());
        let tokens = semantic_tokens_full(&doc);
        // setq(keyword), s(variable), "hello"(string)
        assert!(tokens.len() >= 3);
    }

    #[test]
    fn test_builtin_function_token() {
        let doc = Document::new("(strcat \"a\" \"b\")".to_string());
        let tokens = semantic_tokens_full(&doc);
        // strcat should have DEFAULT_LIBRARY modifier
        assert!(!tokens.is_empty());
        // First token should be strcat with builtin modifier
        assert_eq!(tokens[0].token_modifiers_bitset, MOD_DEFAULT_LIBRARY);
    }

    #[test]
    fn test_nil_t_tokens() {
        let doc = Document::new("(if T nil)".to_string());
        let tokens = semantic_tokens_full(&doc);
        // if(keyword), T(type), nil(type)
        assert!(tokens.len() >= 3);
    }

    #[test]
    fn test_operator_tokens() {
        let doc = Document::new("(+ 1 2)".to_string());
        let tokens = semantic_tokens_full(&doc);
        assert!(!tokens.is_empty());
        // First token is '+' (operator)
        assert_eq!(tokens[0].token_type, TT_OPERATOR);
    }

    #[test]
    fn test_lambda_tokens() {
        let doc = Document::new("(lambda (x) (* x x))".to_string());
        let tokens = semantic_tokens_full(&doc);
        // lambda(keyword), x(param/def), *(operator), x(param), x(param)
        assert!(tokens.len() >= 4);
    }

    #[test]
    fn test_delta_encoding() {
        let doc = Document::new("(+ 1 2)\n(- 3 4)".to_string());
        let tokens = semantic_tokens_full(&doc);
        assert!(tokens.len() >= 6);
        // Second line tokens should have delta_line > 0
        let second_line_token = tokens.iter().find(|t| t.delta_line > 0);
        assert!(second_line_token.is_some());
    }

    #[test]
    fn test_find_keyword_span() {
        let text = "(defun foo () nil)";
        let span = find_keyword_span(text, 0, "defun");
        assert!(span.is_some());
        let s = span.unwrap();
        assert_eq!(&text[s.start..s.end], "defun");
    }

    #[test]
    fn test_find_keyword_span_case_insensitive() {
        let text = "(DEFUN foo () nil)";
        let span = find_keyword_span(text, 0, "defun");
        assert!(span.is_some());
        let s = span.unwrap();
        assert_eq!(&text[s.start..s.end], "DEFUN");
    }

    #[test]
    fn test_find_keyword_span_defun_q() {
        let text = "(defun-q foo () nil)";
        let span = find_keyword_span(text, 0, "defun-q");
        assert!(span.is_some());
        let s = span.unwrap();
        assert_eq!(&text[s.start..s.end], "defun-q");
    }

    #[test]
    fn test_defun_q_keyword_token() {
        let doc = Document::new("(defun-q test (a) a)".to_string());
        let tokens = semantic_tokens_full(&doc);
        // First token should be defun-q as keyword
        assert!(!tokens.is_empty());
        assert_eq!(tokens[0].token_type, TT_KEYWORD);
        // defun-q is 7 chars
        assert_eq!(tokens[0].length, 7);
    }

    #[test]
    fn test_bitwise_operator_tokens() {
        let doc = Document::new("(logand 7 3)".to_string());
        let tokens = semantic_tokens_full(&doc);
        assert!(!tokens.is_empty());
        assert_eq!(tokens[0].token_type, TT_OPERATOR);
    }
}
