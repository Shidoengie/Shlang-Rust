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
    fn print_msg(&self, err_out: ErrorBuilder);
    fn msg(&self) -> String;
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
    fn msg(&self) -> String {
        use ParseError::*;
        match &self {
            InvalidToken(expected, got) => {
                format!("expected token {expected:?} but got token {:?}", got.kind)
            }
            UnexpectedToken(_) => "Unexpected token".to_string(),
            UnexpectedToplevel(_) => "Unexpected token at toplevel".to_string(),
            UnexpectedStreamEnd => "Expected To find another token but none was found".to_string(),
            UnterminatedParetheses(_) => "Unterminated parentheses".to_string(),
            UnexpectedFieldNode(_) => "Invalid Node in struct feilds".to_string(),
        }
    }
    fn print_msg(&self, err_out: ErrorBuilder) {
        use ParseError::*;

        let span = match &self {
            InvalidToken(_, got) => got.span,
            UnexpectedToken(token) => token.span,
            UnexpectedToplevel(token) => token.span,
            UnexpectedStreamEnd => {
                eprintln!("{} {}", "ERROR".red(), self.msg());
                return;
            }
            UnterminatedParetheses(paren) => paren.span,
            UnexpectedFieldNode(node) => node.span,
        };
        err_out.emit(self.msg().as_str(), span);
    }
}
#[derive(Debug)]
pub enum InterpreterError {
    MixedTypes(Type, Type, Span),
    InvalidType(Vec<Type>, Type, Span),
    InvalidControl(Span),
    VoidAssignment(Span),
    NonExistentVar(String, Span),
    InvalidAssignment(String, Span),
    InvalidConstructor(Span),
    InvalidOp(BinaryOp, Type, Span),
    InvalidArgSize(u32, u32, Span),
    InvalidBinary(Type, Span),
}
impl LangError for InterpreterError {
    fn msg(&self) -> String {
        use InterpreterError::*;
        match &self {
            MixedTypes(first, last, _) => format!("Mixed types: {first:?} and {last:?}"),

            InvalidType(accepted, got, _) => {
                let opts = String::from_iter(
                    format!("{accepted:?}")
                        .chars()
                        .filter(|c| c != &'[' && c != &']'),
                )
                .replace(',', " or ");
                format!("Invalid Types expected: {opts:?} but got {got:?}")
            }
            InvalidControl(_) => "Invalid control flow node".to_string(),
            VoidAssignment(_) => "Attempted to assign void to a variable".to_string(),
            NonExistentVar(name, _) => {
                "Couldnt find variable with name: ".to_string() + name.as_str()
            }
            InvalidAssignment(name, _) => {
                "Attempted to assign to non existent variable with name: ".to_string()
                    + name.as_str()
            }
            InvalidConstructor(_) => "Attempted to construct a non existent struct".to_string(),
            InvalidOp(op, inv, _) => format!("Cant do {op:?} operation with type {inv:?}"),
            InvalidArgSize(expected, got, _) => format!(
                "Invalid args size expected {expected:?} arguments but got {got:?} arguments"
            ),
            InvalidBinary(got, _) => format!("Invalid type in binary operation: {:?}", got),
        }
    }
    fn print_msg(&self, err_out: ErrorBuilder) {
        use InterpreterError::*;
        let span = match &self {
            MixedTypes(_, _, span) => span,
            InvalidType(_, _, span) => span,
            InvalidControl(span) => span,
            VoidAssignment(span) => span,
            NonExistentVar(_, span) => span,
            InvalidAssignment(_, span) => span,
            InvalidConstructor(span) => span,
            InvalidOp(_, _, span) => span,
            InvalidArgSize(_, _, span) => span,
            InvalidBinary(_, span) => span,
        };
        err_out.emit(self.msg().as_str(), *span);
    }
}
