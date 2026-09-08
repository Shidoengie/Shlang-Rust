use super::values::Value;
use crate::{
	collections::{SpanMap, spans::*},
	lang_errors::{ErrorBox, LangError, LangSpan, MsgBuilder, ToErrorBox},
};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum ErrCode {
	Unspecified(String),
	InvalidOffset,
	EmptyStack,
	ExpectedStackFrame,
	UsedBeforeInit,
	InvalidType { expected: Type, got: Type },
	MixedTypes { first: Type, last: Type },
	UnsupportedOperation { op: String, target: Type },
	InvalidStackIndex(usize),
	InvalidArgs { expected: u8, got: u8 },
	StackOverflow,
	IndexOutOfBounds,
}
impl LangError for ErrorBox<ErrCode> {
	fn msg(&self) -> ariadne::ReportBuilder<LangSpan, ariadne::ReportKind> {
		match &self.kind {
			ErrCode::EmptyStack => {
				MsgBuilder::build_err("Expected another item in the stack", self.span)
					.with_err_label("This shouldve pushed another item.")
					.get_inner()
			}
			ErrCode::MixedTypes { first, last } => MsgBuilder::build_err("Mixed types", self.span)
				.with_err_label(format!("The type {first:?} is not the same as {last:?}"))
				.get_inner(),
			ErrCode::InvalidOffset => MsgBuilder::build_err("Invalid offset", self.span)
				.with_err_label("This points to a non existent op.")
				.get_inner(),
			ErrCode::ExpectedStackFrame => {
				MsgBuilder::build_err("Expected a stack frame", self.span)
					.with_err_label("A stack frame should have been present.")
					.get_inner()
			}
			ErrCode::InvalidType { expected, got } => {
				MsgBuilder::build_err("Invalid type", self.span)
					.with_err_label(format!("Expected type {expected:?} but got {got:?}."))
					.get_inner()
			}
			ErrCode::UnsupportedOperation { op, target } => {
				MsgBuilder::build_err("Invalid operand", self.span)
					.with_err_label(format!(
						"The operation \"{op}\" on type {target}, isnt valid."
					))
					.get_inner()
			}
			ErrCode::Unspecified(unspec) => {
				MsgBuilder::build_unspecified_err(unspec.to_string(), self.span)
			}
			ErrCode::InvalidArgs { expected, got } => {
				MsgBuilder::build_err("Invalid argument size", self.span)
					.with_err_label(format!(
						"This function expected {expected} {arg_msg1}, but {got} {arg_msg2}.",
						arg_msg1 = if *expected == 1 {
							"argument"
						} else {
							"arguments"
						},
						arg_msg2 = if *got == 1 { "argument" } else { "arguments" },
					))
					.get_inner()
			}
			ErrCode::InvalidStackIndex(idx) => {
				MsgBuilder::build_err(format!("Invalid stack index {idx}"), self.span)
					.with_err_label("This points to an invalid address.".to_string())
					.get_inner()
			}
			ErrCode::UsedBeforeInit => {
				MsgBuilder::build_err(format!("This value hasnt been defined yet"), self.span)
					.with_err_label("This points to an invalid address.".to_string())
					.get_inner()
			}
			ErrCode::StackOverflow => MsgBuilder::build_err("Stack overflow", self.span)
				.with_err_label(
					"A call to this function was made and it overflowed the stack.".to_string(),
				)
				.get_inner(),
			ErrCode::IndexOutOfBounds => MsgBuilder::build_err("Index out of bounds", self.span)
				.with_err_label("On this expression.")
				.get_inner(),
		}
	}
}
impl ErrCode {
	pub fn into_vmerr(self, index: usize) -> VmErr {
		VmErr { index, code: self }
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
	ObjectRef,
}
impl From<Value> for Type {
	fn from(value: Value) -> Self {
		match value {
			Value::Bool(_) => Self::Bool,
			Value::Float(_) => Self::Float,
			Value::Int(_) => Self::Int,
			Value::Null => Self::Null,
			Value::Undefined => Self::Null,
			Value::String(_) => Self::String,
			Value::Function(_) => Self::Function,
			Value::NativeFunction(_) => Self::Function,
			Value::ObjectRef(_) => Self::ObjectRef,
			_ => todo!(),
		}
	}
}
impl Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Custom(ty) => f.write_str(ty),
			ty => f.write_str(&format!("{ty:?}").to_lowercase()),
		}
	}
}
#[derive(Debug)]
pub struct VmErr {
	index: usize,
	code: ErrCode,
}
impl VmErr {
	pub fn new(index: usize, code: impl Into<ErrCode>) -> Self {
		Self {
			index,
			code: code.into(),
		}
	}

	pub fn other(index: usize, msg: impl Display) -> Self {
		Self {
			index,
			code: ErrCode::Unspecified(msg.to_string()),
		}
	}
	pub fn get_spanned_code(&self, map: &SpanMap) -> Spanned<ErrCode> {
		let span = map[self.index];

		self.code.clone().to_spanned(span)
	}
	pub fn into_errorbox(self, map: &SpanMap, id: crate::collections::FileId) -> ErrorBox<ErrCode> {
		let span = map[self.index];
		self.code.to_errorbox(span, id)
	}
}
