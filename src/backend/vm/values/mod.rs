use std::{
	any::Any,
	collections::HashMap,
	fmt::{self, Write},
};
mod native;
use crate::backend::vm::{
	StackVM,
	error::{ErrCode, Type},
};
pub use native::*;

#[derive(Debug, Clone, Default)]
#[repr(u8)]
pub enum Value {
	#[default]
	Undefined = 0,
	Null,
	Int(i64),
	Float(f64),
	Bool(bool),
	String(String),
	Function(Function),
	NativeFunction(NativeFunction),
	ObjectRef(usize),
}
impl Value {
	pub fn lang_debug_fmt(&self, f: &mut impl Write) -> std::fmt::Result {
		match self {
			Self::Bool(v) => write!(f, "{v}"),
			Self::ObjectRef(v) => write!(f, "<object@{v}>"),
			Self::Int(v) => write!(f, "{v}i"),
			Self::Float(v) => write!(f, "{v}f"),
			Self::String(v) => write!(f, "\"{v}\""),
			Self::Null => write!(f, "null"),
			Self::Undefined => write!(f, "undefined"),
			Self::Function(v) => write!(f, "<function@{}>", v.address),
			Self::NativeFunction(_) => write!(f, "<nativefunction>"),
		}
	}
}
impl fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Bool(v) => write!(f, "{v}"),
			Self::Int(v) => write!(f, "{v}"),
			Self::Float(v) => write!(f, "{v}"),
			Self::String(v) => write!(f, "{v}"),
			Self::ObjectRef(v) => write!(f, "<object@{v}>"),
			Self::Null => write!(f, "null"),
			Self::Undefined => write!(f, "undefined"),
			Self::Function(v) => write!(f, "<function@{}>", v.address),
			Self::NativeFunction(_) => write!(f, "<nativefunction>"),
		}
	}
}
#[derive(Debug, Clone)]
pub struct Class {
	pub name: Option<usize>,
	pub static_fields: HashMap<usize, Value>,
	pub instance_fields: HashMap<usize, Value>,
}
#[derive(Debug, Clone)]
pub struct Instance {
	pub name: Option<usize>,
	pub fields: HashMap<usize, Value>,
}
#[derive(Debug, Clone, Copy)]
pub struct Function {
	pub address: usize,
	pub local_count: usize,
	pub param_count: u8,
}
impl From<Function> for Value {
	fn from(value: Function) -> Self {
		Self::Function(value)
	}
}

#[derive(Debug)]
pub enum Object {
	Native(Box<dyn NativeTrait>),
	Instance(Instance),
	Class(Class),
}
