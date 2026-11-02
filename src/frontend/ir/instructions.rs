use core::fmt;
use std::{
	collections::HashMap,
	fmt::{Debug, Display},
};

use crate::symbols::{FieldKey, SymbolId};

#[derive(Clone, Debug, Default)]
#[repr(u8)]
pub enum IrNode {
	#[default]
	NoOp = 0,
	Label(String),
	Push(IrLiteral),
	LoadLocal(usize),
	StoreLocal(usize),
	LoadGlobal(usize),
	StoreGlobal(usize),
	Pop,
	Goto(String),
	Branch(String),
	NotBranch(String),
	Add,
	Mult,
	Div,
	Sub,
	Mod,
	And,
	Or,
	Greater,
	Lesser,
	GreaterEq,
	LesserEq,
	NotEq,
	Eq,
	/// Null coalescing binary instruction
	NullCo,
	Not,
	Neg,
	/// Halts program execution
	Stop,
	Ret,
	SetNull,
	SwapWith(IrLiteral),
	/// Pops a function out of stack, and their arguments, then calling it
	Call(u8),
	Flush,
	FlushNull,
	Index,
	IndexMut,
	MakeList(usize),
	GetProperty(SymbolId),
	SetProperty(SymbolId),
	NewInstance,
}

impl Display for IrNode {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			IrNode::Label(label) => write!(f, "{}:", label),
			IrNode::Push(lit) => write!(f, "push {}", lit),
			IrNode::SetNull => write!(f, "setnull"),
			IrNode::SwapWith(value) => write!(f, "swapwith {value}"),
			others => {
				let mut string_repr = format!("{others:?}");
				string_repr.make_ascii_lowercase();
				string_repr = string_repr.replace('(', " ");
				string_repr = string_repr.replace(")", "");
				write!(f, "{}", string_repr)
			}
		}
	}
}
#[derive(Debug, Clone, Default)]
#[repr(u8)]
pub enum IrLiteral {
	#[default]
	Null = 0,
	Int(i64),
	Float(f64),
	Bool(bool),
	String(String),
	Function(Function),
	Class(ClassLit),
}
impl fmt::Display for IrLiteral {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Bool(v) => write!(f, "{v}"),
			Self::Int(v) => write!(f, "{v}i"),
			Self::Float(v) => write!(f, "{v}f"),
			Self::String(v) => write!(f, "\"{v}\""),
			Self::Null => write!(f, "null"),
			Self::Function(v) => write!(f, "<function@{}>", v.address),
			Self::Class(v) => write!(f, "<class#{:?}>", v.name),
		}
	}
}

#[derive(Debug, Clone)]
pub struct Function {
	pub address: String,
	pub local_count: usize,
	pub param_count: u8,
}
impl From<Function> for IrLiteral {
	fn from(value: Function) -> Self {
		Self::Function(value)
	}
}
#[derive(Clone, Debug)]
pub struct ClassLit {
	pub name: Option<SymbolId>,
	pub fields: HashMap<FieldKey, Field>,
	pub methods: HashMap<FieldKey, Method>,
}

#[derive(Clone, Debug)]
pub struct Field {
	pub default: Option<IrLiteral>,
	pub readonly: bool,
	pub private: bool,
}

#[derive(Debug, Clone)]
pub struct Method {
	pub address: String,
	pub local_count: usize,
	pub param_count: u8,
	pub private: bool,
}
