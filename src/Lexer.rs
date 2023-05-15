use std::str::Chars;

use crate::Token::*;
#[derive(Debug,Clone)]
pub struct Scanner<'a> {
    chars: Chars<'a>,
    tokenStream: Vec<Token>,
    source: String,
    isAtend:bool,
    size:usize
}
impl<'a> Scanner<'a> {

fn peek(&self) -> Option<char> { self.chars.clone().next() }

fn advance(&mut self) -> Option<char> { self.chars.next() }
fn currentHas(&mut self, expected: char) -> bool {
    match self.peek().as_mut(){
        Some(expected) => true,
        _ => false
    }
}
fn addToken(&mut self,kind:TokenType,span:(usize,usize)){
    let tok = Token::new(kind,span);
    self.tokenStream.push(tok);
}
fn currentIndx(&self)->usize{
    self.size-self.chars.clone().count()
}

fn num(&mut self,last:char) {
    println!("TODO");
}
fn str(&mut self) {
    let start = self.currentIndx();
    let mut last = self.advance();
    loop {
        
        match last {
            Some('"') => break,
            None =>{
                self.isAtend = true;
                break;
            }
            _ => {
                last = self.advance();
                continue;
            }
        }
        
    }
    
    let stop = self.currentIndx()-1.max(0);
    self.addToken(TokenType::STR,(start,stop));
}
fn pushAdvance(&mut self,kind:TokenType,range:(usize,usize)){
    self.advance();
    self.addToken(kind,range);
}
fn getToken(&mut self){
    let start = self.currentIndx();
    let last = self.advance();
    let range = (start,self.currentIndx());
    if last.is_none(){
        self.isAtend = true;
        return;
    }
    let current = last.unwrap();
    match last.unwrap() {
        '.'=> self.addToken(TokenType::DOT,range),
        ','=> self.addToken(TokenType::COMMA,range),
        '{'=> self.addToken(TokenType::LBRACE,range),
        '}'=> self.addToken(TokenType::RBRACE,range),
        '('=> self.addToken(TokenType::LPAREN,range),
        ')'=> self.addToken(TokenType::RPAREN,range),
        '['=> self.addToken(TokenType::LBRACK,range),
        ']'=> self.addToken(TokenType::RBRACK,range),
        '-'=> self.addToken(TokenType::MINUS,range),
        '+'=> self.addToken(TokenType::PLUS,range),
        '*'=> self.addToken(TokenType::STAR,range),
        '/'=> self.addToken(TokenType::SLASH,range),
        '%'=> self.addToken(TokenType::MODULO,range),
        ':'=> self.addToken(TokenType::COLON,range),
        ';'=> {},
        '"'=> self.str(),
        '<'=>{
            if self.currentHas('=') {
                self.pushAdvance(TokenType::LESSER_EQUAL,(start,self.currentIndx()));
            }
            else {
                self.addToken(TokenType::LESSER,range)
            }
        },
        '>'=>{
            if self.currentHas('=') {
                
                self.pushAdvance(TokenType::GREATER_EQUAL,(start,self.currentIndx()));
            }
            else {
                self.addToken(TokenType::GREATER,range)
            }
        },
        '!'=>{
            if self.currentHas('=') {
                self.pushAdvance(TokenType::ISDIFERENT,(start,self.currentIndx()));
            }
            else {
                self.addToken(TokenType::BANG,range)
            }
        },
        '='=>{
            if self.currentHas('=') {
                self.advance();
                self.addToken(TokenType::ISEQUAL,(start,self.currentIndx()));
            }
            else {
                self.addToken(TokenType::EQUAL,range)
            }
        },
        ' ' => {},
        '\t' => {},
        '\r' => {},
        _ => {
            
            if current.is_digit(10) {
                self.num(current);
            }
            else if current.is_alphanumeric() {
                println!("not implemented");
            }
            else{
                panic!("Unexpected char: {current}");
            }
        },
    }
}
pub fn scanTokens(&mut self) -> Vec<Token>{
    while !self.isAtend {
        self.getToken();
    }
    self.addToken(TokenType::EOFL,(self.size,self.size));
    return self.tokenStream.clone();
}
pub fn new(src:&'a str)->Self{
    
    return Scanner { 
        chars: src.chars(), 
        tokenStream:[].to_vec(), 
        source:String::from(src),
        isAtend:false,
        size:src.chars().count()
    };
}

}