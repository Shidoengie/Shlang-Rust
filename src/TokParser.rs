use std::iter::Peekable;

use crate::AstNodes;
use crate::AstNodes::*;
use crate::Scanner;
use crate::Token::*;
use std::string;
use AstNodes::*;
#[derive(Clone)]
pub struct TokenIter<'input> {
    lexer: Scanner<'input>,
}

impl<'input> TokenIter<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            lexer: Scanner::new(input),
        }
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
    input: &'input str,
    tokens: Peekable<I>,
}

impl<'input> Parser<'input, TokenIter<'input>> {
    pub fn new(input: &'input str) -> Parser<'input, TokenIter<'input>> {
        Parser {
            input,
            tokens: TokenIter::new(input).peekable(),
        }
    }
    pub fn text(&mut self, token: Token) -> String {
        return self.input[token.span.0..token.span.1].to_string();
    }
    fn peek(&mut self) -> Option<Token> {
        match self.tokens.peek() {
            Some(tok) => Some(tok.clone()),
            _ => None,
        }
    }
    fn peek_kind(&mut self) -> Option<TokenType> {
        match self.tokens.peek() {
            Some(tok) => Some(tok.kind.clone()),
            _ => None,
        }
    }
    fn peek_next(&mut self) -> Option<Token> {
        let mut cloned = self.clone();
        cloned.next();
        return cloned.next();
    }
    fn peek_advance(&mut self) -> Option<Token> {
        self.next();
        return self.peek();
    }
    fn is(&mut self, kind: TokenType) -> bool {
        match self.peek() {
            Some(peeked) => peeked.kind == kind,
            _ => false,
        }
    }
    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }
    pub fn consume(&mut self, expected: TokenType) -> Token {
        let token = self.next().expect(&format!(
            "Expected to consume `{expected:?}`, but there was no next token",
        ));
        assert_eq!(
            token.kind, expected,
            "Expected to consume `{expected:?}`, but found `{:?}`",
            token.kind
        );
        return token;
    }
    fn expect(&mut self, expected: TokenType) -> Token {
        self.next();
        let token: Token = self.peek().expect("invalid syntax");
        if token.kind != expected {
            panic!("expected token: {expected:?} but got: {:?}", token.kind)
        } else {
            return token;
        }
    }
    fn parse_vardef(&mut self) -> Node {
        let identTok: Token = self.expect(TokenType::IDENTIFIER);
        let var_name = self.text(identTok);
        self.next();
        match self
            .peek()
            .expect(format!("Invalid {:?}", self.peek()).as_str())
            .kind
        {
            TokenType::EOL => {
                self.next();
                return Declaration {
                    var_name: var_name,
                    value: Box::new(Value::NoneType.into()),
                }
                .into();
            }
            TokenType::EQUAL => {
                self.next();
                let val = self.parse_expr();
                self.next();
                return Declaration {
                    var_name: var_name,
                    value: Box::new(val),
                }
                .into();
            }
            val => {
                panic!("Invalid expression token: {val:?}");
            }
        }
    }
    fn unary_minus(&mut self) -> Node {
        todo!()
    }
    fn simple_parse(&mut self, peeked: Option<Token>) -> Node {
        let Some(value) = peeked else {todo!()};

        match value.kind {
            TokenType::STR => {
                return Value::Str(self.text(value)).into();
            }
            TokenType::NUM => {
                return Value::Num((self.text(value).parse().unwrap())).into();
            }
            TokenType::FALSE => {
                return Value::Bool(false).into();
            }
            TokenType::TRUE => {
                return Value::Bool(true).into();
            }
            TokenType::IDENTIFIER => {
                return Variable {
                    name: self.text(value),
                }
                .into();
            }
            TokenType::LPAREN => {
                println!("{value:?}");
                let expr = self.parse_expr();
                assert_eq!(
                    self.peek().expect("Unterminated parentheses").kind,
                    TokenType::RPAREN,
                    "Unterminated parentheses"
                );
                self.next();
                return expr;
            }
            _ => {
                todo!();
            }
        };
    }
    fn parse_operator(&mut self, left: Node, kind: BinaryOp) -> Node {
        self.next();
        return BinaryNode {
            kind,
            left: Box::new(left),
            right: Box::new(self.parse_expr()),
        }
        .into();
    }
    fn parse_expr(&mut self) -> Node {
        let val = self.peek();
        self.next();

        let left = self.simple_parse(val);
        let Some(token) = self.peek() else {return left;};
        
        match token.kind {
            TokenType::EOL => {
                return left;
            }
            TokenType::PLUS => return self.parse_operator(left, BinaryOp::ADD),
            TokenType::MINUS => return self.parse_operator(left, BinaryOp::SUBTRACT),
            TokenType::STAR => return self.parse_operator(left, BinaryOp::MULTIPLY),
            TokenType::SLASH => return self.parse_operator(left, BinaryOp::DIVIDE),
            TokenType::PERCENT => return self.parse_operator(left, BinaryOp::MODULO),
            TokenType::GREATER_EQUAL => return self.parse_operator(left, BinaryOp::GREATER_EQUAL),
            TokenType::GREATER => return self.parse_operator(left, BinaryOp::GREATER),
            TokenType::LESSER_EQUAL => return self.parse_operator(left, BinaryOp::LESSER_EQUAL),
            TokenType::LESSER => return self.parse_operator(left, BinaryOp::LESSER),
            TokenType::DOUBLE_EQUAL => return self.parse_operator(left, BinaryOp::ISEQUAL),
            TokenType::BANG_EQUAL => return self.parse_operator(left, BinaryOp::ISDIFERENT),
            TokenType::AND => return self.parse_operator(left, BinaryOp::AND),
            TokenType::OR => return self.parse_operator(left, BinaryOp::OR),
            tok => {
                return left;
            }
        }
        todo!()
    }
    pub fn parse_top(&mut self) -> Option<Node> {
        match self.peek()?.kind {
            TokenType::VAR => Some(self.parse_vardef()),
            TokenType::FUNC => todo!(),
            token => panic!("Invalid Token at toplevel: {token:?}"),
        }
    }
    pub fn batch_parse(&mut self) -> Node {
        let mut body: NodeStream = vec![];
        loop {
            let parsed = self.parse_top();
            if parsed.is_none() {
                break;
            }
            body.push(parsed.unwrap());
        }
        return Node::block(body);
    }
}
