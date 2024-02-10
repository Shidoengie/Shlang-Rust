use crate::spans::*;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    STR,
    NUM,
    IDENTIFIER,
    SEMICOLON,
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
    NULL,
    STRUCT,
    CONTINUE,
    PLUS_EQUAL,
    MINUS_EQUAL,
    STAR_EQUAL,
    SLASH_EQUAL,
    NEW,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenType,
    pub span: (usize, usize),
}
pub trait TokenEq {
    fn is(&self, kind: &TokenType) -> bool;
    fn isnt(&self, kind: &TokenType) -> bool;
}
impl Token {
    pub fn new(kind: TokenType, span: Span) -> Self {
        Token { kind, span }
    }
}
impl TokenEq for Token {
    fn is(&self, kind: &TokenType) -> bool {
        &self.kind == kind
    }
    fn isnt(&self, kind: &TokenType) -> bool {
        &self.kind != kind
    }
}

impl TokenEq for Option<Token> {
    fn is(&self, kind: &TokenType) -> bool {
        match self.clone() {
            Some(tok) => &tok.kind == kind,
            None => false,
        }
    }
    fn isnt(&self, kind: &TokenType) -> bool {
        match self.clone() {
            Some(tok) => &tok.kind != kind,
            None => false,
        }
    }
}
pub fn map_keyword(text: String) -> Option<TokenType> {
    let res = match text.as_str() {
        "true" => TokenType::TRUE,
        "false" => TokenType::FALSE,
        "if" => TokenType::IF,
        "else" => TokenType::ELSE,
        "func" => TokenType::FUNC,
        "return" => TokenType::RETURN,
        "loop" => TokenType::LOOP,
        "while" => TokenType::WHILE,
        "break" => TokenType::BREAK,
        "var" => TokenType::VAR,
        "and" => TokenType::AND,
        "not" => TokenType::NOT,
        "or" => TokenType::OR,
        "do" => TokenType::DO,
        "null" => TokenType::NULL,
        "struct" => TokenType::STRUCT,
        "continue" => TokenType::CONTINUE,
        "new" => TokenType::NEW,
        _ => return None,
    };
    Some(res)
}
