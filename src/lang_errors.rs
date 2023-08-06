use crate::ast_nodes::*;
use crate::{spans::*, tokens::*};
use colored::*;
#[derive(Clone)]

pub struct ErrorBuilder {
    pub input: String,
}
impl ErrorBuilder {
    pub fn new(input: String) -> Self {
        Self { input }
    }
    pub fn emit(&self, msg: &str, span: Span) {
        eprintln!("{}", self.build(msg, span));
    }
    pub fn panic_emit(&self, msg: &str, span: Span) {
        self.emit(msg, span);
        panic!()
    }
    pub fn build(&self, msg: &str, span: Span) -> String {
        let position = self.line_pos(span.clone());
        let (start, stop) = (
            self.input[..span.0].to_string(),
            self.input[span.1..].to_string(),
        );
        let marked = format!(
            "{start}{}{stop}",
            self.input[span.0..span.1].to_string().red()
        );
        let lines: Vec<&str> = marked.lines().collect();
        let line = lines[position - 1];
        return format!("{} {msg}\n{position} {} {line}", "ERROR!".red(), "|".blue(),);
    }
    pub fn line_pos(&self, span: Span) -> usize {
        return self.input[..span.0]
            .chars()
            .filter(|ch| *ch == '\n')
            .count()
            + 1;
    }
}
pub trait LangError {
    fn msg(&self, err_out: ErrorBuilder);
}
#[derive(Clone, Debug)]
pub enum ParseError {
    InvalidToken(TokenType, Token),
    UnexpectedToken(Token),
    UnexpectedToplevel(Token),
    UnterminatedParetheses(Token),
    UnexpectedStreamEnd,
    UnexpectedFieldNode(NodeSpan),
}
impl LangError for ParseError {
    fn msg(&self, err_out: ErrorBuilder) {
        use ParseError::*;
        match &self {
            InvalidToken(expected, got) => err_out.emit(
                format!("expected token {expected:?} but got token {:?}", got.kind).as_str(),
                got.span,
            ),
            UnexpectedToken(token) => err_out.emit("Unexpected token", token.span),
            UnexpectedToplevel(token) => err_out.emit("Unexpected token at toplevel", token.span),
            UnexpectedStreamEnd => eprintln!(
                "{} Expected To find another token but none was found",
                "ERROR".red()
            ),
            UnterminatedParetheses(paren) => err_out.emit("Unterminated parentheses", paren.span),
            UnexpectedFieldNode(node) => err_out.emit("Invalid Node in struct feilds", node.span),
        }
    }
}
