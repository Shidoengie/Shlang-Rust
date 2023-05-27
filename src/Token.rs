use std::collections::HashMap;
#[derive(Debug,Clone,PartialEq, Eq)]
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
    MODULO,
    ISEQUAL,
    ISDIFERENT,
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
    EOFL,
    FALSE,
    TRUE,
    VAR
}

#[derive(Debug,Clone,PartialEq, Eq)]

pub struct Token {
    pub kind:TokenType,
    pub span:(usize,usize)
}

impl Token {

pub fn new(kind:TokenType,span:(usize,usize)) -> Token{
    Token { kind: kind, span:span}
}
pub fn mapKeyword(text:String)->Option<TokenType>{
    let keywordMap = HashMap::from([
        ("true",TokenType::TRUE),
        ("false",TokenType::FALSE),
        ("input",TokenType::INPUT),
        ("print",TokenType::PRINT),
        ("if",TokenType::IF),
        ("elif",TokenType::ELIF),
        ("else",TokenType::ELSE),
        ("func",TokenType::FUNC),
        ("return",TokenType::RETURN),
        ("loop",TokenType::LOOP),
        ("loop",TokenType::WHILE),
        ("break",TokenType::BREAK),
        ("var",TokenType::VAR)
    ]);
    keywordMap.get(text.as_str()).cloned()
}
}