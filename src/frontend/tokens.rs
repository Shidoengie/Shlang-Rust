use crate::{charvec::CharVec, spans::*};
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    Str(CharVec),
    Float,
    Int,
    At,
    Dollar,
    DualPipe,
    DualAmpersand,

    Identifier,
    Semicolon,
    Plus,
    Minus,
    Start,
    Slash,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Equal,
    Dot,
    Lesser,
    Greater,
    GreaterEqual,
    LesserEqual,
    Comma,
    Colon,
    Bang,
    Percent,
    DoubleEqual,
    BangEqual,
    And,
    Not,
    Or,
    If,
    Else,
    Func,
    Return,
    Loop,
    While,
    Break,
    False,
    True,
    Var,
    Do,
    Ampersand,
    Pipe,
    Null,
    Struct,
    Continue,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    New,
    For,
    In,
    Question,
    DualQuestion,
    QuestionEqual,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenType,
    pub span: Span,
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
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "func" => TokenType::Func,
        "return" => TokenType::Return,
        "loop" => TokenType::Loop,
        "while" => TokenType::While,
        "break" => TokenType::Break,
        "var" => TokenType::Var,
        "and" => TokenType::And,
        "not" => TokenType::Not,
        "or" => TokenType::Or,
        "do" => TokenType::Do,
        "null" => TokenType::Null,
        "struct" => TokenType::Struct,
        "continue" => TokenType::Continue,
        "new" => TokenType::New,
        "for" => TokenType::For,
        "in" => TokenType::In,
        _ => return None,
    };
    Some(res)
}
