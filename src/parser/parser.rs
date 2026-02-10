use super::ast::AstNode;
use super::token::{Span, Token};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    comments: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        // Separate comments from code tokens
        let mut code_tokens = Vec::new();
        let mut comments = Vec::new();
        for token in tokens {
            match &token {
                Token::LineComment(_, _) | Token::BlockComment(_, _) => {
                    comments.push(token);
                }
                _ => code_tokens.push(token),
            }
        }
        Self {
            tokens: code_tokens,
            pos: 0,
            comments,
        }
    }

    pub fn parse(mut self) -> (Vec<AstNode>, Vec<AstNode>) {
        let mut nodes = Vec::new();

        // Add comment nodes
        let comment_nodes: Vec<AstNode> = self
            .comments
            .iter()
            .map(|c| match c {
                Token::LineComment(text, span) | Token::BlockComment(text, span) => {
                    AstNode::Comment(text.clone(), *span)
                }
                _ => unreachable!(),
            })
            .collect();

        while self.pos < self.tokens.len() {
            match self.parse_expr() {
                Some(node) => nodes.push(node),
                None => {
                    // Skip unrecoverable token
                    if self.pos < self.tokens.len() {
                        let span = self.tokens[self.pos].span();
                        nodes.push(AstNode::Error("unexpected token".to_string(), span));
                        self.pos += 1;
                    }
                }
            }
        }

        (nodes, comment_nodes)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.pos);
        if token.is_some() {
            self.pos += 1;
        }
        token
    }

    fn parse_expr(&mut self) -> Option<AstNode> {
        match self.peek()? {
            Token::LParen(_) => self.parse_list(),
            Token::Quote(_) => self.parse_quote(),
            Token::RParen(span) => {
                let span = *span;
                self.pos += 1;
                Some(AstNode::Error("unexpected ')'".to_string(), span))
            }
            _ => self.parse_atom(),
        }
    }

    fn parse_atom(&mut self) -> Option<AstNode> {
        let token = self.advance()?;
        match token {
            Token::Integer(v, span) => Some(AstNode::IntegerLit(*v, *span)),
            Token::Real(v, span) => Some(AstNode::RealLit(*v, *span)),
            Token::StringLit(s, span) => Some(AstNode::StringLit(s.clone(), *span)),
            Token::Symbol(s, span) => Some(AstNode::Symbol(s.clone(), *span)),
            Token::Nil(span) => Some(AstNode::Nil(*span)),
            Token::T(span) => Some(AstNode::T(*span)),
            Token::Dot(span) => Some(AstNode::Error("unexpected '.'".to_string(), *span)),
            Token::Error(msg, span) => Some(AstNode::Error(msg.clone(), *span)),
            _ => None,
        }
    }

    fn parse_quote(&mut self) -> Option<AstNode> {
        let quote_span = self.peek()?.span();
        self.pos += 1; // skip quote

        match self.parse_expr() {
            Some(inner) => {
                let full_span = quote_span.merge(inner.span());
                Some(AstNode::Quote(Box::new(inner), full_span))
            }
            None => Some(AstNode::Error(
                "expected expression after quote".to_string(),
                quote_span,
            )),
        }
    }

    fn parse_list(&mut self) -> Option<AstNode> {
        let lparen_span = self.peek()?.span();
        self.pos += 1; // skip '('

        // Check if this is a defun or lambda
        if let Some(token) = self.peek() {
            if let Token::Symbol(name, _) = token {
                let upper = name.to_uppercase();
                if upper == "DEFUN" || upper == "DEFUN-Q" {
                    return self.parse_defun(lparen_span);
                }
                if upper == "LAMBDA" {
                    return self.parse_lambda(lparen_span);
                }
            }
        }

        self.parse_list_body(lparen_span)
    }

    fn parse_list_body(&mut self, lparen_span: Span) -> Option<AstNode> {
        let mut elements: Vec<AstNode> = Vec::new();

        loop {
            match self.peek() {
                None => {
                    // Unterminated list - error recovery
                    let end = if let Some(last) = elements.last() {
                        last.span().end
                    } else {
                        lparen_span.end
                    };
                    return Some(AstNode::Error(
                        "unterminated list".to_string(),
                        Span::new(lparen_span.start, end),
                    ));
                }
                Some(Token::RParen(span)) => {
                    let end_span = *span;
                    self.pos += 1;
                    let full_span = lparen_span.merge(end_span);
                    return Some(AstNode::List(elements, full_span));
                }
                Some(Token::Dot(_)) => {
                    // Dotted pair: (a . b)
                    self.pos += 1;
                    if elements.len() != 1 {
                        return Some(AstNode::Error(
                            "invalid dotted pair".to_string(),
                            lparen_span,
                        ));
                    }
                    let car = elements.remove(0);
                    let cdr = match self.parse_expr() {
                        Some(e) => e,
                        None => {
                            return Some(AstNode::Error(
                                "expected expression after dot".to_string(),
                                lparen_span,
                            ))
                        }
                    };
                    // Expect closing paren
                    match self.peek() {
                        Some(Token::RParen(span)) => {
                            let end_span = *span;
                            self.pos += 1;
                            let full_span = lparen_span.merge(end_span);
                            return Some(AstNode::DottedPair(
                                Box::new(car),
                                Box::new(cdr),
                                full_span,
                            ));
                        }
                        _ => {
                            return Some(AstNode::Error(
                                "expected ')' after dotted pair".to_string(),
                                lparen_span,
                            ))
                        }
                    }
                }
                _ => {
                    if let Some(node) = self.parse_expr() {
                        elements.push(node);
                    } else {
                        break;
                    }
                }
            }
        }

        Some(AstNode::List(
            elements,
            Span::new(lparen_span.start, self.pos),
        ))
    }

    fn parse_defun(&mut self, lparen_span: Span) -> Option<AstNode> {
        self.pos += 1; // skip 'defun'/'defun-q'

        // Parse function name
        let (name, name_span) = match self.peek() {
            Some(Token::Symbol(n, s)) => {
                let name = n.clone();
                let span = *s;
                self.pos += 1;
                (name, span)
            }
            _ => {
                // Fall back to generic list parsing
                return self.parse_list_body(lparen_span);
            }
        };

        // Parse parameter list
        let (params, locals) = match self.peek() {
            Some(Token::LParen(_)) => {
                self.pos += 1;
                self.parse_param_list()
            }
            _ => (vec![], vec![]),
        };

        // Check for doc comment (string literal as first body expression)
        let mut doc_comment = None;
        let mut body: Vec<AstNode> = Vec::new();

        // Parse body
        loop {
            match self.peek() {
                None => {
                    let end = if let Some(last) = body.last() {
                        last.span().end
                    } else {
                        name_span.end
                    };
                    return Some(AstNode::Error(
                        "unterminated defun".to_string(),
                        Span::new(lparen_span.start, end),
                    ));
                }
                Some(Token::RParen(span)) => {
                    let end_span = *span;
                    self.pos += 1;
                    let full_span = lparen_span.merge(end_span);
                    return Some(AstNode::Defun {
                        name,
                        name_span,
                        params,
                        locals,
                        body,
                        doc_comment,
                        span: full_span,
                    });
                }
                _ => {
                    if let Some(node) = self.parse_expr() {
                        // First string literal can be a doc comment
                        if body.is_empty() && doc_comment.is_none() {
                            if let AstNode::StringLit(ref s, _) = node {
                                doc_comment = Some(s.clone());
                                continue;
                            }
                        }
                        body.push(node);
                    } else {
                        break;
                    }
                }
            }
        }

        Some(AstNode::Error(
            "unterminated defun".to_string(),
            lparen_span,
        ))
    }

    fn parse_lambda(&mut self, lparen_span: Span) -> Option<AstNode> {
        self.pos += 1; // skip 'lambda'

        // Parse parameter list
        let (params, _locals) = match self.peek() {
            Some(Token::LParen(_)) => {
                self.pos += 1;
                self.parse_param_list()
            }
            _ => (vec![], vec![]),
        };

        let mut body: Vec<AstNode> = Vec::new();

        loop {
            match self.peek() {
                None => {
                    return Some(AstNode::Error(
                        "unterminated lambda".to_string(),
                        lparen_span,
                    ));
                }
                Some(Token::RParen(span)) => {
                    let end_span = *span;
                    self.pos += 1;
                    let full_span = lparen_span.merge(end_span);
                    return Some(AstNode::Lambda {
                        params,
                        body,
                        span: full_span,
                    });
                }
                _ => {
                    if let Some(node) = self.parse_expr() {
                        body.push(node);
                    } else {
                        break;
                    }
                }
            }
        }

        Some(AstNode::Error(
            "unterminated lambda".to_string(),
            lparen_span,
        ))
    }

    fn parse_param_list(&mut self) -> (Vec<(String, Span)>, Vec<(String, Span)>) {
        let mut params = Vec::new();
        let mut locals = Vec::new();
        let mut in_locals = false;

        loop {
            match self.peek() {
                None | Some(Token::RParen(_)) => {
                    if self.peek().is_some() {
                        self.pos += 1; // skip ')'
                    }
                    return (params, locals);
                }
                Some(Token::Symbol(name, span)) => {
                    let name = name.clone();
                    let span = *span;
                    self.pos += 1;

                    // '/' separates params from local variables
                    if name == "/" {
                        in_locals = true;
                        continue;
                    }

                    if in_locals {
                        locals.push((name, span));
                    } else {
                        params.push((name, span));
                    }
                }
                _ => {
                    self.pos += 1; // skip unexpected token
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::Lexer;

    fn parse(input: &str) -> Vec<AstNode> {
        let tokens = Lexer::new(input).tokenize();
        let (nodes, _) = Parser::new(tokens).parse();
        nodes
    }

    #[test]
    fn test_simple_list() {
        let nodes = parse("(+ 1 2)");
        assert_eq!(nodes.len(), 1);
        assert!(matches!(nodes[0], AstNode::List(_, _)));
    }

    #[test]
    fn test_defun() {
        let nodes = parse("(defun my-func (x y / temp) \"Doc string\" (+ x y))");
        assert_eq!(nodes.len(), 1);
        match &nodes[0] {
            AstNode::Defun {
                name,
                params,
                locals,
                doc_comment,
                body,
                ..
            } => {
                assert_eq!(name, "MY-FUNC");
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].0, "X");
                assert_eq!(params[1].0, "Y");
                assert_eq!(locals.len(), 1);
                assert_eq!(locals[0].0, "TEMP");
                assert_eq!(doc_comment.as_deref(), Some("Doc string"));
                assert_eq!(body.len(), 1);
            }
            _ => panic!("expected Defun"),
        }
    }

    #[test]
    fn test_nested_list() {
        let nodes = parse("(setq x (+ 1 (* 2 3)))");
        assert_eq!(nodes.len(), 1);
    }

    #[test]
    fn test_quote() {
        let nodes = parse("'(1 2 3)");
        assert_eq!(nodes.len(), 1);
        assert!(matches!(nodes[0], AstNode::Quote(_, _)));
    }

    #[test]
    fn test_dotted_pair() {
        let nodes = parse("(0 . \"LINE\")");
        assert_eq!(nodes.len(), 1);
        assert!(matches!(nodes[0], AstNode::DottedPair(_, _, _)));
    }

    #[test]
    fn test_error_recovery() {
        let nodes = parse("(+ 1 2) ) (setq x 3)");
        // Should parse first list, error for stray ')', and then next list
        assert!(nodes.len() >= 2);
    }

    #[test]
    fn test_lambda() {
        let nodes = parse("(lambda (x) (* x x))");
        assert_eq!(nodes.len(), 1);
        assert!(matches!(nodes[0], AstNode::Lambda { .. }));
    }
}
