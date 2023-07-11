use std::collections::HashMap;
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
    PRINT,
    INPUT,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub struct Token {
    pub kind: TokenType,
    pub span: (usize, usize),
}

impl Token {
    pub fn new(kind: TokenType, span: (usize, usize)) -> Token {
        Token {
            kind: kind,
            span: span,
        }
    }
    pub fn mapKeyword(text: String) -> Option<TokenType> {
        let keywordMap = HashMap::from([
            ("true", TokenType::TRUE),
            ("false", TokenType::FALSE),
            ("input", TokenType::INPUT),
            ("print", TokenType::PRINT),
            ("if", TokenType::IF),
            ("elif", TokenType::ELIF),
            ("else", TokenType::ELSE),
            ("func", TokenType::FUNC),
            ("return", TokenType::RETURN),
            ("loop", TokenType::LOOP),
            ("while", TokenType::WHILE),
            ("break", TokenType::BREAK),
            ("var", TokenType::VAR),
            ("and", TokenType::AND),
            ("not", TokenType::NOT),
            ("or", TokenType::OR),
            ("do", TokenType::DO),
        ]);
        keywordMap.get(text.as_str()).cloned()
    }
}
