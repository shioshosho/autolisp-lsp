#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen(Span),
    RParen(Span),
    Quote(Span),
    Dot(Span),
    Integer(i64, Span),
    Real(f64, Span),
    StringLit(String, Span),
    Symbol(String, Span),
    LineComment(String, Span),
    BlockComment(String, Span),
    Nil(Span),
    T(Span),
    Error(String, Span),
}

impl Token {
    pub fn span(&self) -> Span {
        match self {
            Token::LParen(s)
            | Token::RParen(s)
            | Token::Quote(s)
            | Token::Dot(s)
            | Token::Nil(s)
            | Token::T(s) => *s,
            Token::Integer(_, s)
            | Token::Real(_, s)
            | Token::StringLit(_, s)
            | Token::Symbol(_, s)
            | Token::LineComment(_, s)
            | Token::BlockComment(_, s)
            | Token::Error(_, s) => *s,
        }
    }
}
