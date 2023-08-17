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
    fn msg(&self, err_out: ErrorBuilder) {
        use InterpreterError::*;
        match &self {
            MixedTypes(first, last, span) => err_out.emit(
                format!("Mixed types: {first:?} and {last:?}").as_str(),
                *span,
            ),
            InvalidType(accepted, got, span) => {
                let opts = String::from_iter(
                    format!("{accepted:?}")
                        .chars()
                        .filter(|c| c != &'[' && c != &']'),
                )
                .replace(",", " or ");
                err_out.emit(
                    format!("Invalid Types expected: {opts:?} but got {got:?}").as_str(),
                    *span,
                )
            }
            InvalidControl(span) => err_out.emit("Invalid control flow node", *span),
            VoidAssignment(span) => err_out.emit("Attempted to assign void to a variable", *span),
            NonExistentVar(name, span) => err_out.emit(
                ("Couldnt find variable with name: ".to_string() + name.as_str()).as_str(),
                *span,
            ),
            InvalidAssignment(name, span) => err_out.emit(
                ("Attempted to assign to non existent variable with name: ".to_string()
                    + name.as_str())
                .as_str(),
                *span,
            ),
            InvalidConstructor(span) => {
                err_out.emit("Attempted to construct a non existent struct", *span)
            }
            InvalidOp(op, inv, span) => err_out.emit(
                format!("Cant do {op:?} operation with type {inv:?}").as_str(),
                *span,
            ),
            InvalidArgSize(expected, got, span) => err_out.emit(
                format!(
                    "Invalid args size expected {expected:?} arguments but got {got:?} arguments"
                )
                .as_str(),
                *span,
            ),
            InvalidBinary(got, span) => err_out.emit(
                format!("Invalid type in binary operation: {:?}", got).as_str(),
                *span,
            ),
        }
    }
}
