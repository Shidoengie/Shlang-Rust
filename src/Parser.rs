use std::iter::Peekable;

use crate::AstNodes;
use crate::Token::*;
use crate::AstNodes::*;
use crate::Scanner;
use AstNodes::*;
pub struct TokenIter<'input> {
    lexer: Scanner<'input>,
}

impl<'input> TokenIter<'input> {
    pub fn new(input: &'input str) -> Self {
        Self { lexer: Scanner::new(input) }
    }
}

impl<'input> Iterator for TokenIter<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next()
    }
}
pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    input:  &'input str,
    tokens: Peekable<I>,
}

impl<'input> Parser<'input, TokenIter<'input>> {

pub fn new(input: &'input str) -> Parser<'input, TokenIter<'input>> {
    Parser {
        input,
        tokens: TokenIter::new(input).peekable(),
    }
}
pub fn peek(&mut self) -> Option<TokenType>{
    match self.tokens.peek() {
        Some(tok) => {
            Some(tok.kind.clone())
        },
        _ =>{None}
    }
}
pub fn peek_next(&mut self) -> Option<TokenType>{
    match self.tokens.peek(). {
        Some(tok) => {
            Some(tok.kind.clone())
        },
        _ =>{None}
    }
}
pub fn is(&mut self,kind:TokenType) -> bool{
    match self.peek() {
        Some(peeked) => {
            peeked == kind
        },
        _ =>{false}
    }
}
pub fn next(&mut self) -> Option<Token> {
    self.tokens.next()
}
pub fn consume(&mut self,expected:TokenType){
    let token = self.next().expect(&format!(
        "Expected to consume `{expected:?}`, but there was no next token",
    ));
    assert_eq!(
        token.kind, expected,
        "Expected to consume `{expected:?}`, but found `{:?}`",token.kind
    );
}
pub fn parse_expr(&mut self) -> Node {
    match self.peek().unwrap() {
        
        _ =>{
            todo!()
        }
    }
}
fn parse_var() {
    
}
}