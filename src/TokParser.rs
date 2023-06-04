use std::iter::Peekable;

use crate::AstNodes;
use crate::Token::*;
use crate::AstNodes::*;
use crate::Scanner;
use AstNodes::*;
#[derive(Clone)]
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
#[derive(Clone)]
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
        tokens: TokenIter::new(input).peekable()
    }
}
pub fn text(&mut self,token:Token) -> String {
    return self.input[token.span.0..token.span.1].to_string();
}
fn peek(&mut self) -> Option<Token>{
    match self.tokens.peek() {
        Some(tok) => {
            Some(tok.clone())
        },
        _ =>{None}
    }
}
fn peek_next(&mut self) -> Option<Token>{
    let mut cloned = self.clone();
    cloned.next();
    cloned.next()
}
fn is(&mut self,kind:TokenType) -> bool{
    match self.peek() {
        Some(peeked) => {
            peeked.kind == kind
        },
        _ =>{false}
    }
}
fn next(&mut self) -> Option<Token> {
    self.tokens.next()
}
pub fn consume(&mut self,expected:TokenType) -> Token{
    let token = self.next().expect(&format!(
        "Expected to consume `{expected:?}`, but there was no next token",
    ));
    assert_eq!(
        token.kind, expected,
        "Expected to consume `{expected:?}`, but found `{:?}`",token.kind
    );
    return token;
    
}
fn parse_vardef(&mut self)->Node{
    self.next();
    let mut identTok:Token;
    match self.peek() {
        Some(tok) =>{
            identTok = tok;
        },
        _=>{
            panic!("invalid syntax")
        }
    }
    
    let var_name = self.text(identTok);
    self.next();
    match self.peek().expect(format!("Invalid {:?}",self.peek()).as_str()).kind {
        TokenType::EOL =>{
            self.next();
            return Node::declaration(
                var_name, 
                Value::NoneType.as_node()

            )
        },
        TokenType::EQUAL =>{
            self.next();
            self.parse_expr()
        },
        _ =>{
            panic!("Invalid Expression");
        }
    }
}
fn match_expr(&mut self,value:TokenType)->Node { 
    let mut left:Node;
    match value {
        TokenType::STR => {
            let peeked = self.peek();
            Value::Str(self.text(peeked.unwrap())).as_node()
        },
        _=>{
            todo!()
        }
    
    }
}   

fn parse_expr(&mut self)->Node{
    match self.peek() {
        Some(val) => {
            todo!()
        }
        _=>{
            todo!()
        }
    }
}

pub fn parse_top(&mut self) -> Option<Node> {
    let peeked = self.peek();
    let mut token:Token;
    match peeked {
        Some(tok)=>{
            token = tok;
        }
        _=>{
            return None;
        }
    }
    match token.kind {
        TokenType::VAR => {
            
            return Some(self.parse_vardef());
        },
        TokenType::FUNC => {
            todo!()
        },   
        _ =>{
            panic!("Invalid Token at toplevel: {token:?}")
        }
    }
    
}
pub fn batch_parse(&mut self) -> Node {
    let mut body: NodeStream = vec![];
    loop {
        let parsed = self.parse_top();
        if parsed.is_none(){break;}
        body.push(parsed.unwrap());
    }
    return Node::block(body);
}

}