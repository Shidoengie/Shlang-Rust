use std::iter::Peekable;

use crate::AstNodes;
use crate::Token::*;
use crate::AstNodes::*;
struct Parser<I> 
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
    current: usize,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    
    
    pub fn parse(&self) -> Node {
        
        return Node::fromVal(Value::NoneType);
    }
    fn new(stream: Vec<Token>)->Self{
        
        Parser{tokens:stream.into_iter().peekable(),current:0}
    }
}
