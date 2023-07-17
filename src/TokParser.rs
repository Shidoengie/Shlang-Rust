use core::panic;
use std::iter::Peekable;

use crate::AstNodes::*;
use crate::Scanner;
use crate::Token::*;
use colored::*;
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
    pub fn line_pos(&mut self, token: Token) -> usize {
        return self.input[..token.span.0]
            .chars()
            .filter(|ch| *ch == '\n')
            .count()
            + 1;
    }
    fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }
    fn peek_some(&mut self) -> Token {
        self.tokens.peek().cloned().expect(
            format!(
                "{} Expected to find another token but none was found",
                "ERROR!".red()
            )
            .as_str(),
        )
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
    fn error_builder(&mut self, msg: &str, token: Token) -> String {
        let position = self.line_pos(token.clone());
        let (start, stop) = (
            self.input[..token.span.0].to_string(),
            self.input[token.span.1..].to_string(),
        );
        let marked = format!("{start}{}{stop}", self.text(token).red());
        let lines: Vec<&str> = marked.lines().collect();
        let line = lines[position - 1];
        
        let I = "|".blue();
        return format!("{} {msg}\n{position} {I} {line}", "ERROR!".red(),);
    }
    fn check_valid(&mut self, expected: TokenType, token: Token){
        if token.kind != expected {
            println!();
            let err = self.error_builder(
                format!("expected token {expected:?} but got token {:?}", token.kind).as_str(),
                token,
            );
            println!("{err}");
            println!();
            panic!();
        }
    }

    fn expect(&mut self, expected: TokenType) -> Token {
        let token: Token = self.peek_some();
        self.check_valid(expected, token.clone());
        return token;
    }
    fn parse_vardef(&mut self) -> Node {
        self.next();
        let ident: Token = self.expect(TokenType::IDENTIFIER);
        let var_name = self.text(ident);
        self.next();
        match self.peek_some().kind {
            TokenType::EOL => {
                self.next();
                return Declaration {
                    var_name: var_name,
                    value: Box::new(Value::Null.into()),
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
    fn unary_operator(&mut self, kind: UnaryOp) -> Node {
        let token = self.peek();
        self.next();
        let right = self.simple_parse(token);
        return UnaryNode {
            kind,
            object: Box::new(right),
        }
        .into();
    }
    fn binary_operator(&mut self, left: Node, kind: BinaryOp) -> Node {
        self.next();
        return BinaryNode {
            kind,
            left: Box::new(left),
            right: Box::new(self.parse_expr()),
        }
        .into();
    }

    fn parse_paren(&mut self, paren: Token) -> Node {
        let expr = self.parse_expr();

        let err = self.error_builder("Unterminated parentheses", paren);
        let Some(end) = self.peek() else {
            println!();
            println!("{err}");
            println!();
            panic!();
        };
        self.check_valid(TokenType::RPAREN, end);
        self.next();
        return expr;
    }
    fn simple_parse(&mut self, peeked: Option<Token>) -> Node {
        let Some(value) = peeked else {todo!()};

        match value.kind {
            TokenType::STR => {
                return Value::Str(self.text(value)).into();
            }
            TokenType::NUM => {
                return Value::Num(self.text(value).parse().unwrap()).into();
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

            TokenType::NOT | TokenType::BANG => return self.unary_operator(UnaryOp::NOT),
            TokenType::MINUS => return self.unary_operator(UnaryOp::NEGATIVE),
            TokenType::PLUS => return self.unary_operator(UnaryOp::POSITIVE),
            TokenType::LPAREN => return self.parse_paren(value),
            unexpected => {
                panic!("{unexpected:?}");
            }
        };
    }
    fn parse_call(&mut self, callee: Node) -> Node {
        let mut params: NodeStream = vec![];
        self.next();
        let mut token = dbg!(self.peek_some());
        if token.kind == TokenType::RPAREN {
            self.next();
            return Call {
                args: Box::new(params),
                callee: Box::new(callee),
            }
            .into();
        }
        while token.kind != TokenType::RPAREN {
            let expr = dbg!(self.parse_expr());
            token = dbg!(self.peek_some());
            match token.kind {
                TokenType::RPAREN => {
                    params.push(expr);
                    break;
                }
                TokenType::COMMA => {
                    params.push(expr);
                    self.next();
                }
                _ => panic!("invalid token:{:?}", self.text(token)),
            }
        }
        self.next();

        let val: Node = Call {
            args: Box::new(params),
            callee: Box::new(callee),
        }
        .into();
        self.parse_operator(token, val)
    }
    fn parse_operator(&mut self, previous: Token, left: Node) -> Node {
        let Some(token) = self.peek() else {return left;};
        match token.kind {
            TokenType::EQUAL => {
                self.check_valid(TokenType::IDENTIFIER, previous.clone());
                let var_name = self.text(previous);
                self.next();
                return Assignment{
                    var_name,
                    value:Box::new(self.parse_expr())
                }.into();
            }
            TokenType::LPAREN => return self.parse_call(left),
            TokenType::PLUS => return self.binary_operator(left, BinaryOp::ADD),
            TokenType::MINUS => return self.binary_operator(left, BinaryOp::SUBTRACT),
            TokenType::STAR => return self.binary_operator(left, BinaryOp::MULTIPLY),
            TokenType::SLASH => return self.binary_operator(left, BinaryOp::DIVIDE),
            TokenType::PERCENT => return self.binary_operator(left, BinaryOp::MODULO),
            TokenType::GREATER_EQUAL => return self.binary_operator(left, BinaryOp::GREATER_EQUAL),
            TokenType::GREATER => return self.binary_operator(left, BinaryOp::GREATER),
            TokenType::LESSER_EQUAL => return self.binary_operator(left, BinaryOp::LESSER_EQUAL),
            TokenType::LESSER => return self.binary_operator(left, BinaryOp::LESSER),
            TokenType::DOUBLE_EQUAL => return self.binary_operator(left, BinaryOp::ISEQUAL),
            TokenType::BANG_EQUAL => return self.binary_operator(left, BinaryOp::ISDIFERENT),
            TokenType::AND | TokenType::AMPERSAND => {
                return self.binary_operator(left, BinaryOp::AND)
            }
            TokenType::OR | TokenType::PIPE => return self.binary_operator(left, BinaryOp::OR),
            _ => {
                return left;
            }
        }
    }
    pub fn parse_expr(&mut self) -> Node {
        let value = self.peek();
        self.next();
        let left = self.simple_parse(value.clone());
        let Some(peeked) = value else {todo!()};
        self.parse_operator(peeked, left)
    }
    pub fn parse_top(&mut self) -> Option<Node> {
        match self.peek()?.kind {
            TokenType::VAR => Some(self.parse_vardef()),
            TokenType::FUNC => todo!(),
            token => panic!("Invalid Token at toplevel: {token:?}"),
        }
    }
    pub fn batch_parse(&mut self) -> Block {
        let mut body: NodeStream = vec![];
        loop {
            let Some(parsed) = self.parse_top() else {break;};
            body.push(parsed);
        }
        return Block{body:Box::new(body)};
    }

}
