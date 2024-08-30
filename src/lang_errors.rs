use std::fmt::format;

use crate::frontend::{nodes::*, tokens::*};
use crate::spans::*;
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
        let position = self.line_pos(span);
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
        format!("{} {msg}\n{position} {} {line}", "ERROR!".red(), "|".blue(),)
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
    fn msg(&self) -> String;

    fn print_msg(&self, err_out: ErrorBuilder)
    where
        Self: SpanUtil,
    {
        err_out.emit(self.msg().as_str(), self.get_span());
    }
}

#[derive(Clone, Debug)]
pub enum ParseError {
    InvalidToken(TokenType, TokenType),
    UnexpectedToken(TokenType),
    UnexpectedToplevel(TokenType),
    UnterminatedParetheses(TokenType),
    UnexpectedStreamEnd,
    UnexpectedFieldNode(Node),
    UnexpectedStatement,
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
            UnexpectedStatement => "Unexpected statement expression".to_string(),
        }
    }
}
impl IntoSpanned for ParseError {}
#[derive(Debug, Clone)]
pub enum InterpreterError {
    MixedTypes(Type, Type),
    InvalidType(Vec<Type>, Type),
    InvalidControl,
    VoidAssignment,
    NonExistentVar(String),
    InvalidAssignment(String),
    InvalidConstructor,
    InvalidOp(BinaryOp, Type),
    InvalidArgSize(u32, u32),
    InvalidBinary(Type),
    Unspecified(String),
}
impl IntoSpanned for InterpreterError {}
impl LangError for Spanned<InterpreterError> {
    fn msg(&self) -> String {
        use InterpreterError::*;
        match &self.item {
            MixedTypes(first, last) => format!("Mixed types: {first:?} and {last:?}"),

            InvalidType(accepted, got) => {
                let opts = String::from_iter(
                    format!("{accepted:?}")
                        .chars()
                        .filter(|c| c != &'[' && c != &']'),
                )
                .replace(',', " or ");
                if accepted.len() > 1 {
                    return format!("Invalid types expected: {opts:?} but got {got:?}");
                }
                format!("Invalid type expected: {opts:?} but got {got:?}")
            }
            InvalidControl => "Unexpected control flow node".to_string(),
            VoidAssignment => "Attempted to assign void to a variable".to_string(),
            NonExistentVar(name) => "Couldnt find variable with name: ".to_string() + name.as_str(),
            InvalidAssignment(name) => {
                "Attempted to assign to non existent variable with name: ".to_string()
                    + name.as_str()
            }
            InvalidConstructor => "Attempted to construct a non existent struct".to_string(),
            InvalidOp(op, inv) => format!("Cant do {op:?} operation with type {inv:?}"),
            InvalidArgSize(expected, got) => format!(
                "Invalid args size expected {expected:?} arguments but got {got:?} arguments"
            ),
            InvalidBinary(got) => format!("Invalid type in binary operation: {:?}", got),
            Unspecified(msg) => msg.to_string(),
        }
    }
}
