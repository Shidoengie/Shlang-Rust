
#[derive(Debug,Clone)]
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
    BREAK,
    EOFL,
    FALSE,
    TRUE,
}
#[derive(Debug,Clone)]
pub struct Token {
    pub kind:TokenType,
    pub span:(usize,usize)
}
impl Token {
    pub fn new(kind:TokenType,span:(usize,usize)) -> Token{
        Token { kind: kind, span:span}
    }
}