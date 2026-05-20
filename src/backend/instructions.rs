use crate::{backend::vm::values::*, spanmap::SpanMap, utils::compact_iter_debug};

use std::fmt::{Debug, Display};

pub struct ByteCode {
    pub ops: Vec<OpCode>,
    pub span_map: SpanMap,
    pub global_count: usize,
    pub local_count: usize,
    pub globals: Vec<Value>,
}
impl Debug for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ByteCode")
            .field("span_map", &self.span_map)
            .field("global_count", &self.global_count)
            .finish()?;
        write!(f, " = ")?;
        compact_iter_debug(f, self.ops.iter())
    }
}
impl Display for ByteCode {
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
#[derive(Clone, Debug)]
#[repr(u8)]
pub enum OpCode {
    NoOp = 0,
    Push(Value),
    SwapWith(Value),
    LoadLocal(usize),
    StoreLocal(usize),
    LoadGlobal(usize),
    StoreGlobal(usize),
    SetNull,
    Pop,
    Goto(usize),
    Branch(usize),
    NotBranch(usize),
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
    Call(u8),
    Flush,
    FlushNull,
    Index,
    IndexMut,
    MakeList(usize),
}
impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Push(lit) => {
                write!(f, "push ")?;
                lit.lang_debug_fmt(f)
            }
            Self::SwapWith(value) => {
                write!(f, "swapwith ",)?;
                value.lang_debug_fmt(f)
            }
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
