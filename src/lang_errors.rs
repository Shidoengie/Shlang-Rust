use crate::backend::values::Type;
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
        eprintln!("{}", self.build(msg, span, false));
    }
    pub fn emit_panic(&self, msg: &str, span: Span) {
        eprintln!("{}", self.build(msg, span, true));
    }
    pub fn build(&self, msg: &str, span: Span, panicked: bool) -> String {
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
        format!(
            "{} {msg}\n{position} {} {line}",
            if panicked {
                "PANICKED!".blue()
            } else {
                "ERROR!".red()
            },
            "|".blue(),
        )
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
    fn err<T>(self) -> Result<T, Self>
    where
        Self: Sized,
    {
        return Err(self);
    }
}

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

#[derive(Debug, Clone)]
pub enum InterpreterError {
    MixedTypes(Type, Type),
    InvalidType(Vec<Type>, Type),
    InvalidControl,
    VoidAssignment,
    NonExistentVar(String),
    MethodNotFound(String, Option<String>),
    InvalidAssignment(String),
    InvalidConstructor,
    InvalidOp(BinaryOp, Type),
    InvalidArgSize(u32, u32),
    InvalidBinary(Type),
    Panic(String),
    Unspecified(String),
}

impl LangError for Spanned<InterpreterError> {
    fn msg(&self) -> String {
        use InterpreterError::*;
        match &self.item {
            MethodNotFound(method, ty) => {
                let Some(obj) = ty else {
                    return format!("Method {method} not found.");
                };
                format!("Method {method} not found on {obj} ")
            }
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
            InvalidArgSize(expected, got) => {
                let expected_txt = if expected == &1 {
                    "argument"
                } else {
                    "arguments"
                };
                let got_txt = if got == &1 { "argument" } else { "arguments" };
                format!(
                "Invalid argument size expected {expected:?} {expected_txt} but got {got:?} {got_txt}"
                )
            }
            InvalidBinary(got) => format!("Invalid type in binary operation: {:?}", got),
            Unspecified(msg) => msg.to_string(),
            Panic(msg) => msg.to_string(),
        }
    }
    fn print_msg(&self, err_out: ErrorBuilder) {
        match self.item {
            InterpreterError::Panic(_) => err_out.emit_panic(self.msg().as_str(), self.get_span()),
            _ => err_out.emit(self.msg().as_str(), self.get_span()),
        }
    }
}
