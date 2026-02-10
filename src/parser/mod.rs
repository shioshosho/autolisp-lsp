pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

use ast::AstNode;
use lexer::Lexer;
use parser::Parser;

pub fn parse(source: &str) -> (Vec<AstNode>, Vec<AstNode>) {
    let tokens = Lexer::new(source).tokenize();
    Parser::new(tokens).parse()
}
