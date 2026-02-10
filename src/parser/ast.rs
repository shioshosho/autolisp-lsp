use super::token::Span;

#[derive(Debug, Clone)]
pub enum AstNode {
    IntegerLit(i64, Span),
    RealLit(f64, Span),
    StringLit(String, Span),
    Symbol(String, Span),
    Nil(Span),
    T(Span),
    List(Vec<AstNode>, Span),
    Defun {
        name: String,
        name_span: Span,
        params: Vec<(String, Span)>,
        locals: Vec<(String, Span)>,
        body: Vec<AstNode>,
        doc_comment: Option<String>,
        span: Span,
    },
    Lambda {
        params: Vec<(String, Span)>,
        body: Vec<AstNode>,
        span: Span,
    },
    Quote(Box<AstNode>, Span),
    DottedPair(Box<AstNode>, Box<AstNode>, Span),
    Comment(String, Span),
    Error(String, Span),
}

impl AstNode {
    pub fn span(&self) -> Span {
        match self {
            AstNode::IntegerLit(_, s)
            | AstNode::RealLit(_, s)
            | AstNode::StringLit(_, s)
            | AstNode::Symbol(_, s)
            | AstNode::Nil(s)
            | AstNode::T(s)
            | AstNode::List(_, s)
            | AstNode::Quote(_, s)
            | AstNode::DottedPair(_, _, s)
            | AstNode::Comment(_, s)
            | AstNode::Error(_, s) => *s,
            AstNode::Defun { span, .. } => *span,
            AstNode::Lambda { span, .. } => *span,
        }
    }
}
