use std::str::Chars;

use crate::Token::*;
#[derive(Debug,Clone)]
pub struct Scanner<'a> {
    chars: Chars<'a>,
    tokenStream: Vec<Token>
    
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
fn num(&mut self,last:char) {
    let mut indx = self.current;
    loop {
        
        if !(lastDigit.is_digit(10) || lastDigit == '.'){break;}
    }
    self.addToken(TokenType::NUM);
}
fn str(&mut self) {
    let mut last = self.advance();
    let mut endStr = String::new();
    loop {
        endStr.push(last);
        if(self.isAtEnd()){
            panic!("Unterminated String")
        }
        last = self.advance();
        if last == '"'{break;}
    }
    self.addToken(TokenType::STR);
}
fn getToken(&mut self){
    let last = self.advance();
    match last {
        '.'=> self.addToken(TokenType::DOT),
        ','=> self.addToken(TokenType::COMMA),
        '{'=> self.addToken(TokenType::LBRACE),
        '}'=> self.addToken(TokenType::RBRACE),
        '('=> self.addToken(TokenType::LPAREN),
        ')'=> self.addToken(TokenType::RPAREN),
        '['=> self.addToken(TokenType::LBRACK),
        ']'=> self.addToken(TokenType::RBRACK),
        '-'=> self.addToken(TokenType::MINUS),
        '+'=> self.addToken(TokenType::PLUS),
        '*'=> self.addToken(TokenType::STAR),
        '/'=> self.addToken(TokenType::SLASH),
        '%'=> self.addToken(TokenType::MODULO),
        ':'=> self.addToken(TokenType::COLON),
        ';'=> {},
        '"'=> self.str(),
        '<'=>{
            if self.currentHas('=') {
                self.addToken(TokenType::LESSER_EQUAL);
            }
            else {
                self.addToken(TokenType::LESSER)
            }
        },
        '>'=>{
            if self.currentHas('=') {
                self.addToken(TokenType::GREATER_EQUAL);
            }
            else {
                self.addToken(TokenType::GREATER)
            }
        },
        '!'=>{
            if self.currentHas('=') {
                self.addToken(TokenType::ISDIFERENT);
            }
            else {
                self.addToken(TokenType::BANG)
            }
        },
        '='=>{
            if self.currentHas('=') {
                self.addToken(TokenType::ISEQUAL);
            }
            else {
                self.addToken(TokenType::EQUAL)
            }
        },
        ' ' => {},
        '\t' => {},
        '\r' => {},
        _ => {
            if last.is_digit(10) {
                self.num(last);
            }
            else if last.is_alphanumeric() {
                println!("not implemented");
            }
            else{
                panic!("Unexpected char: {last}");
            }
        },
    }
}
pub fn scanTokens(&mut self) -> Vec<Token>{
    while !self.isAtEnd() {
        self.getToken();
    }
    self.addToken(TokenType::EOFL);
    return self.tokenStream.clone();
}
pub fn new(src:&'a str)->Self{
    
    Scanner { chars: src.chars(), tokenStream:[].to_vec()}
}

}