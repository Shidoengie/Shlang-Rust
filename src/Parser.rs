use crate::Token::*;
use crate::AstNodes::*;
struct Parser {
    stream: Vec<Token>,
    current: usize,
}

impl Parser {
    fn peek_next(&mut self) -> Token {
        self.stream[self.current + 1]
    }
    fn peek(&mut self) -> Token {
        self.stream[self.current]
    }
    fn expect(&mut self, what: Token) -> bool {
        self.peek_next() == what
    }
    fn next(&mut self) -> Token {
        self.current += 1;
        self.stream[self.current]
    }
    
    pub fn parse(&self) -> Node {
        
    }
    fn new(stream: Vec<Token>)->Self{
        Parser{stream:stream,current:0}
    }
}
