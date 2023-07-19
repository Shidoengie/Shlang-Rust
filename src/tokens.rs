use std::collections::HashMap;
use crate::spans::*;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    STR,
    NUM,
    IDENTIFIER,
    EOL,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACK,
    RBRACK,
    EQUAL,
    DOT,
    LESSER,
    GREATER,
    GREATER_EQUAL,
    LESSER_EQUAL,
    COMMA,
    COLON,
    BANG,
    PERCENT,
    DOUBLE_EQUAL,
    BANG_EQUAL,
    AND,
    NOT,
    OR,
    IF,
    ELSE,
    ELIF,
    FUNC,
    RETURN,
    LOOP,
    WHILE,
    BREAK,
    FALSE,
    TRUE,
    VAR,
    DO,
    AMPERSAND,
    PIPE,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenType,
    pub span: (usize, usize),
}
impl Token {
    pub fn new(kind: TokenType, span: Span) -> Self {
        Token {
            kind,
            span,
        }
    }
}

pub fn map_keyword(text: String) -> Option<TokenType> {
    match text.as_str() {
        "true" => Some(TokenType::TRUE),
        "false" => Some(TokenType::FALSE),
        "if" => Some(TokenType::IF),
        "elif" => Some(TokenType::ELIF),
        "else" => Some(TokenType::ELSE),
        "func" => Some(TokenType::FUNC),
        "return" => Some(TokenType::RETURN),
        "loop" => Some(TokenType::LOOP),
        "while" => Some(TokenType::WHILE),
        "break" => Some(TokenType::BREAK),
        "var" => Some(TokenType::VAR),
        "and" => Some(TokenType::AND),
        "not" => Some(TokenType::NOT),
        "or" => Some(TokenType::OR),
        "do" => Some(TokenType::DO),
        _ => None,
    }
}
