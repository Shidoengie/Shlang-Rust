use std::{
	any::Any,
	collections::HashMap,
	fmt::{self, Write},
	num::NonZeroU8,
};
mod native;
use crate::{
	backend::vm::{
		StackVM,
		error::{ErrCode, Type},
	},
	symbols::{FieldKey, SymbolId},
};
pub use native::*;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
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
	#[serde(skip)]
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

#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
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
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Class {
	pub name: Option<SymbolId>,
	#[serde(with = "crate::symbols::field_key_map")]
	pub fields: HashMap<FieldKey, Field>,
}
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Field {
	pub value: Value,
	pub metadata: FieldData,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Deserialize, Serialize)]
pub struct FieldData(u8);
impl FieldData {
	pub const PRIVATE: Self = FieldData(1 << 0);
	pub const READONLY: Self = FieldData(1 << 1);

	pub fn new(is_private: bool, is_readonly: bool) -> Self {
		let mut data = 0u8;
		if is_private {
			data |= Self::PRIVATE.0;
		}
		if is_readonly {
			data |= Self::READONLY.0;
		}
		Self(data)
	}
	pub const fn is_private(&self) -> bool {
		(self.0 & Self::PRIVATE.0) != 0
	}
	pub const fn is_readonly(&self) -> bool {
		(self.0 & Self::READONLY.0) != 0
	}
}
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Instance {
	pub name: Option<SymbolId>,
	pub fields: HashMap<SymbolId, Value>,
}

#[derive(Debug, Deserialize, Serialize)]
pub enum Object {
	#[serde(skip)]
	Native(Box<dyn NativeTrait>),
	Instance(Instance),
	Class(Class),
}
