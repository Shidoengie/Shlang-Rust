use std::fmt::Display;

use crate::{
    frontend::ir::instructions::Value,
    lang_errors::{LangError, MsgBuilder},
    spanmap::SpanMap,
    spans::{IntoSpanned, Span, Spanned},
};

#[derive(Debug, Clone)]
pub enum ErrCode {
    Unspecified(String),
    InvalidOffset,
    EmptyStack,
    InvalidType { expected: Type, got: Type },
    MixedTypes { first: Type, last: Type },
    UnsupportedOperation { op: String, target: Type },
    InvalidStackIndex(usize),
}
impl LangError for Spanned<ErrCode> {
    fn msg(&self) -> ariadne::Report<Span> {
        match &self.item {
            ErrCode::EmptyStack => {
                MsgBuilder::build_err("Expected another item in the stack", self.span)
                    .with_err_label("This shouldve pushed another item.")
                    .finish()
            }
            ErrCode::MixedTypes { first, last } => MsgBuilder::build_err("Mixed types", self.span)
                .with_err_label(format!("The type {first:?} is not the same as {last:?}"))
                .finish(),
            ErrCode::InvalidOffset => MsgBuilder::build_err("Invalid offset", self.span)
                .with_err_label("This points to a non existent op.")
                .finish(),
            ErrCode::InvalidType { expected, got } => {
                MsgBuilder::build_err("Invalid type", self.span)
                    .with_err_label(format!("Expected type {expected:?} but got {got:?}."))
                    .finish()
            }
            ErrCode::UnsupportedOperation { op, target } => {
                MsgBuilder::build_err("Invalid operand", self.span)
                    .with_err_label(format!("Cant use operator {op} on type {target:?}"))
                    .finish()
            }
            ErrCode::Unspecified(unspec) => {
                MsgBuilder::build_unspecified_err(unspec.to_string(), self.span)
            }
            ErrCode::InvalidStackIndex(idx) => {
                MsgBuilder::build_err(format!("Invalid stack index {idx}"), self.span)
                    .with_err_label(format!("This points to an invalid address."))
                    .finish()
            }
        }
    }
}
impl ErrCode {
    pub fn into_vmerr(self, index: usize) -> VmErr {
        VmErr {
            index: index,
            code: self,
        }
    }
}
/// This enum is used for type errors, as to ease constructing such errors.
/// Dont use this enum for no other purpose.
#[derive(Debug, Clone)]
pub enum Type {
    String,
    Int,
    Float,
    Bool,
    List,
    Null,
    Function,
    Custom(String),
}
impl From<Value> for Type {
    fn from(value: Value) -> Self {
        match value {
            Value::Bool(_) => Self::Bool,
            Value::Float(_) => Self::Float,
            Value::Int(_) => Self::Int,
            Value::Null => Self::Null,
            Value::String(_) => Self::String,
        }
    }
}
#[derive(Debug)]
pub struct VmErr {
    index: usize,
    code: ErrCode,
}
impl VmErr {
    pub fn new(index: usize, code: ErrCode) -> Self {
        return Self { index, code };
    }

    pub fn other(index: usize, msg: impl Display) -> Self {
        return Self {
            index,
            code: ErrCode::Unspecified(msg.to_string()),
        };
    }
    pub fn get_spanned_code(&self, map: &SpanMap) -> Spanned<ErrCode> {
        let span = map[self.index];

        self.code.clone().to_spanned(span)
    }
    pub fn to_spanned_code(self, map: &SpanMap) -> Spanned<ErrCode> {
        let span = map[self.index];
        self.code.to_spanned(span)
    }
}
