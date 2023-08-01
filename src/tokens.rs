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
    VOID,
    NULL,
    STRUCT,
    CONTINUE,
    PLUS_EQUAL,
    MINUS_EQUAL,
    STAR_EQUAL,
    SLASH_EQUAL
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenType,
    pub span: (usize, usize),
}
pub trait TokenEq {
    fn is(&self,kind: &TokenType) -> bool;
    fn isnt(&self,kind: &TokenType) -> bool;
}
impl Token {
    pub fn new(kind: TokenType, span: Span) -> Self {
        Token {
            kind,
            span,
        }
    }
}
impl TokenEq for Token {
    fn is(&self,kind: &TokenType) -> bool{
        &self.kind == kind
    }
    fn isnt(&self,kind: &TokenType) -> bool{
        &self.kind != kind
    }
}
type MaybeToken = Option<Token>;
impl TokenEq for MaybeToken{
    fn is(&self,kind: &TokenType) -> bool {
        match self.clone() {
            Some(tok)=>return &tok.kind==kind,
            None => return false
        }
    }
    fn isnt(&self,kind: &TokenType) -> bool {
        match self.clone() {
            Some(tok)=>return &tok.kind!=kind,
            None => return false
        }
    }
}
pub fn map_keyword(text: String) -> Option<TokenType> {
    match text.as_str() {
        "true" => Some(TokenType::TRUE),
        "false" => Some(TokenType::FALSE),
        "if" => Some(TokenType::IF),
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
        "void"=>Some(TokenType::VOID),
        "null"=>Some(TokenType::NULL),
        "struct"=>Some(TokenType::STRUCT),
        "continue"=>Some(TokenType::CONTINUE),
        _ => None,
    }
}
