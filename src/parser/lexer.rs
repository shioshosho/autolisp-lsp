use super::token::{Span, Token};

pub struct Lexer<'a> {
    source: &'a str,
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            pos: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while self.pos < self.bytes.len() {
            self.skip_whitespace();
            if self.pos >= self.bytes.len() {
                break;
            }
            let token = self.next_token();
            tokens.push(token);
        }
        tokens
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<u8> {
        let b = self.bytes.get(self.pos).copied();
        if b.is_some() {
            self.pos += 1;
        }
        b
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.bytes.len() {
            let b = self.bytes[self.pos];
            if b == b' ' || b == b'\t' || b == b'\r' || b == b'\n' {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn next_token(&mut self) -> Token {
        let start = self.pos;
        let b = self.bytes[self.pos];

        match b {
            b'(' => {
                self.pos += 1;
                Token::LParen(Span::new(start, self.pos))
            }
            b')' => {
                self.pos += 1;
                Token::RParen(Span::new(start, self.pos))
            }
            b'\'' => {
                self.pos += 1;
                Token::Quote(Span::new(start, self.pos))
            }
            b';' => self.lex_comment(start),
            b'"' => self.lex_string(start),
            _ => self.lex_atom(start),
        }
    }

    fn lex_comment(&mut self, start: usize) -> Token {
        self.pos += 1; // skip ';'

        // Block comment: ;| ... |;
        if self.pos < self.bytes.len() && self.bytes[self.pos] == b'|' {
            self.pos += 1; // skip '|'
            let content_start = self.pos;
            loop {
                if self.pos + 1 >= self.bytes.len() {
                    // Unterminated block comment - consume rest
                    let text = self.source[content_start..self.bytes.len()].to_string();
                    self.pos = self.bytes.len();
                    return Token::BlockComment(text, Span::new(start, self.pos));
                }
                if self.bytes[self.pos] == b'|' && self.bytes[self.pos + 1] == b';' {
                    let text = self.source[content_start..self.pos].to_string();
                    self.pos += 2; // skip '|;'
                    return Token::BlockComment(text, Span::new(start, self.pos));
                }
                self.pos += 1;
            }
        }

        // Line comment: ; ...
        let content_start = self.pos;
        while self.pos < self.bytes.len() && self.bytes[self.pos] != b'\n' {
            self.pos += 1;
        }
        let text = self.source[content_start..self.pos].to_string();
        Token::LineComment(text, Span::new(start, self.pos))
    }

    fn lex_string(&mut self, start: usize) -> Token {
        self.pos += 1; // skip opening '"'
        let mut value = String::new();
        loop {
            match self.advance() {
                None => {
                    return Token::Error(
                        "unterminated string".to_string(),
                        Span::new(start, self.pos),
                    );
                }
                Some(b'\\') => match self.advance() {
                    Some(b'\\') => value.push('\\'),
                    Some(b'"') => value.push('"'),
                    Some(b'n') => value.push('\n'),
                    Some(b'r') => value.push('\r'),
                    Some(b't') => value.push('\t'),
                    Some(b'e') => value.push('\x1b'),
                    Some(ch) => {
                        value.push('\\');
                        value.push(ch as char);
                    }
                    None => {
                        return Token::Error(
                            "unterminated string escape".to_string(),
                            Span::new(start, self.pos),
                        );
                    }
                },
                Some(b'"') => {
                    return Token::StringLit(value, Span::new(start, self.pos));
                }
                Some(ch) => {
                    value.push(ch as char);
                }
            }
        }
    }

    fn lex_atom(&mut self, start: usize) -> Token {
        // Collect characters until delimiter
        let atom_start = self.pos;
        while self.pos < self.bytes.len() {
            let b = self.bytes[self.pos];
            if b == b'('
                || b == b')'
                || b == b'"'
                || b == b';'
                || b == b'\''
                || b == b' '
                || b == b'\t'
                || b == b'\r'
                || b == b'\n'
            {
                break;
            }
            self.pos += 1;
        }

        let text = &self.source[atom_start..self.pos];
        let span = Span::new(start, self.pos);

        if text.is_empty() {
            self.pos += 1;
            return Token::Error(
                format!("unexpected character: {}", self.source[start..start + 1].chars().next().unwrap_or('?')),
                Span::new(start, self.pos),
            );
        }

        let upper = text.to_uppercase();

        // Check for dot (used in dotted pairs)
        if text == "." {
            return Token::Dot(span);
        }

        // Check for NIL and T
        if upper == "NIL" {
            return Token::Nil(span);
        }
        if upper == "T" && text.len() == 1 {
            return Token::T(span);
        }

        // Try parsing as number
        if let Some(token) = self.try_parse_number(text, span) {
            return token;
        }

        // Symbol (store uppercase for case-insensitive comparison)
        Token::Symbol(upper, span)
    }

    fn try_parse_number(&self, text: &str, span: Span) -> Option<Token> {
        // Try integer first
        if let Ok(v) = text.parse::<i64>() {
            return Some(Token::Integer(v, span));
        }

        // Try real (including scientific notation like 1.5e10)
        if let Ok(v) = text.parse::<f64>() {
            // Only treat as real if it looks like a number
            let first = text.as_bytes()[0];
            if first.is_ascii_digit() || first == b'+' || first == b'-' || first == b'.' {
                return Some(Token::Real(v, span));
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> Vec<Token> {
        Lexer::new(input).tokenize()
    }

    #[test]
    fn test_parens() {
        let tokens = lex("()");
        assert!(matches!(tokens[0], Token::LParen(_)));
        assert!(matches!(tokens[1], Token::RParen(_)));
    }

    #[test]
    fn test_string() {
        let tokens = lex(r#""hello world""#);
        match &tokens[0] {
            Token::StringLit(s, _) => assert_eq!(s, "hello world"),
            _ => panic!("expected string"),
        }
    }

    #[test]
    fn test_string_escape() {
        let tokens = lex(r#""line1\nline2""#);
        match &tokens[0] {
            Token::StringLit(s, _) => assert_eq!(s, "line1\nline2"),
            _ => panic!("expected string"),
        }
    }

    #[test]
    fn test_numbers() {
        let tokens = lex("42 3.14 -7 1.5e10");
        assert!(matches!(tokens[0], Token::Integer(42, _)));
        assert!(matches!(tokens[1], Token::Real(v, _) if (v - 3.14).abs() < 1e-10));
        assert!(matches!(tokens[2], Token::Integer(-7, _)));
        assert!(matches!(tokens[3], Token::Real(_, _)));
    }

    #[test]
    fn test_symbols() {
        let tokens = lex("defun setq MyVar");
        match &tokens[0] {
            Token::Symbol(s, _) => assert_eq!(s, "DEFUN"),
            _ => panic!("expected symbol"),
        }
        match &tokens[1] {
            Token::Symbol(s, _) => assert_eq!(s, "SETQ"),
            _ => panic!("expected symbol"),
        }
        match &tokens[2] {
            Token::Symbol(s, _) => assert_eq!(s, "MYVAR"),
            _ => panic!("expected symbol"),
        }
    }

    #[test]
    fn test_line_comment() {
        let tokens = lex("; this is a comment\n(+ 1 2)");
        assert!(matches!(tokens[0], Token::LineComment(_, _)));
        assert!(matches!(tokens[1], Token::LParen(_)));
    }

    #[test]
    fn test_block_comment() {
        let tokens = lex(";| block comment |; (+ 1 2)");
        assert!(matches!(tokens[0], Token::BlockComment(_, _)));
        assert!(matches!(tokens[1], Token::LParen(_)));
    }

    #[test]
    fn test_nil_and_t() {
        let tokens = lex("nil T");
        assert!(matches!(tokens[0], Token::Nil(_)));
        assert!(matches!(tokens[1], Token::T(_)));
    }

    #[test]
    fn test_quote() {
        let tokens = lex("'(1 2 3)");
        assert!(matches!(tokens[0], Token::Quote(_)));
        assert!(matches!(tokens[1], Token::LParen(_)));
    }

    #[test]
    fn test_dotted_pair() {
        let tokens = lex("(1 . 2)");
        assert!(matches!(tokens[0], Token::LParen(_)));
        assert!(matches!(tokens[1], Token::Integer(1, _)));
        assert!(matches!(tokens[2], Token::Dot(_)));
        assert!(matches!(tokens[3], Token::Integer(2, _)));
        assert!(matches!(tokens[4], Token::RParen(_)));
    }
}
