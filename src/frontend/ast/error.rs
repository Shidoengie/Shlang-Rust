use crate::frontend::ast::Node;
use crate::frontend::{ast::error, tokens::TokenType};
use crate::lang_errors::LangError;
use crate::spans::Spanned;
#[derive(Clone, Debug)]
pub enum ParseError {
    Unspecified(String),
    InvalidToken(TokenType, TokenType),
    UnexpectedToken(TokenType),
    UnexpectedToplevel(TokenType),
    UnterminatedParetheses(TokenType),
    UnexpectedStreamEnd,
    UnexpectedFieldNode(Node),
    UnexpectedVoidExpression,
}
impl LangError for Spanned<ParseError> {
    fn msg(&self) -> String {
        use ParseError::*;
        match &self.item {
            InvalidToken(expected, got) => {
                format!("expected token {expected:?} but got token {:?}", got)
            }
            UnexpectedToken(tok) => format!("Unexpected token {:?}", tok),
            UnexpectedToplevel(_) => "Unexpected token at toplevel".to_string(),
            UnexpectedStreamEnd => "Expected To find another token but none was found".to_string(),
            UnterminatedParetheses(_) => "Unterminated parentheses".to_string(),
            UnexpectedFieldNode(_) => "Invalid Node in struct feilds".to_string(),
            UnexpectedVoidExpression => "Unexpected void expression".to_string(),
            Unspecified(err) => err.to_owned(),
        }
    }
}
