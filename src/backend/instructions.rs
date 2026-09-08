use crate::{
	backend::vm::values::*, collections::{FileId, SpanMap}, idents::IdentArray,
	utils::compact_iter_debug,
};

use std::{
	collections::HashMap,
	fmt::{Debug, Display},
};

pub struct ByteCode<'a> {
	pub file_id: FileId,
	pub ops: Box<[OpCode]>,
	pub op_args: Box<[usize]>,
	pub span_map: SpanMap,
	pub global_count: usize,
	pub local_count: usize,
	pub globals: Box<[Value]>,
	pub const_pool: Box<[Value]>,
	pub ident_pool: IdentArray<'a>,
}

impl Debug for ByteCode<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("ByteCode")
			.field("span_map", &self.span_map)
			.field("global_count", &self.global_count)
			.finish()?;
		write!(f, " = ")?;
		compact_iter_debug(f, self.ops.iter())
	}
}
impl Display for ByteCode<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.ops.is_empty() {
			write!(f, "0 | ")?;
			return Ok(());
		}
		for (index, op) in self.ops.iter().enumerate() {
			writeln!(f, "{index:<2} | {op}")?;
		}
		Ok(())
	}
}
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
	NoOp = 0,
	Push,
	SwapWith,
	LoadLocal,
	StoreLocal,
	LoadGlobal,
	StoreGlobal,
	SetNull,
	Pop,
	Goto,
	Branch,
	NotBranch,
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
	Exit,
	Ret,

	/// Pops a function out of the current stack, their arguments, then calling it
	CallN,
	Call,
	Call1,
	Call2,
	Call3,
	Call4,
	Flush,
	FlushNull,
	Index,
	IndexMut,
	MakeList,
	MakeClass,
	NewAnonClass,
	SetProperty,
	GetProperty,
}

impl Display for OpCode {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut string_repr = format!("{self:?}");
		string_repr.make_ascii_lowercase();
		string_repr = string_repr.replace('(', " ");
		string_repr = string_repr.replace(")", "");
		write!(f, "{}", string_repr)
	}
}
