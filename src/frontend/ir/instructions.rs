use core::fmt;
use std::sync::Arc;

use crate::backend::vm::StackVM;

#[derive(Debug, Clone)]

pub enum OpCode {
    Push(Value),
    LoadLocal(usize),
    StoreLocal(usize),
    LoadGlobal(usize),
    StoreGlobal(usize),
    Pop,
    Goto(i32),
    Branch(i32),
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
    /// Pops a function out of stack, and their arguments, then calling iy
    Call(u8),
    /// Frees stack slots at a given index with a given length
    PopSlots {
        start: usize,
        len: usize,
    },
}
#[derive(Debug, Clone, Default)]
#[repr(u8)]
pub enum Value {
    #[default]
    Null = 0,
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Function(Arc<Function>),
    NativeFunction(NativeFunction),
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(v) => write!(f, "{v}"),
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::String(v) => write!(f, "{v}"),
            Self::Null => write!(f, "null"),
            Self::Function(v) => write!(f, "<function@{}>", v.address),
            Self::NativeFunction(_) => write!(f, "<nativefunction>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub address: usize,
    pub local_count: usize,
    pub param_count: usize,
}

pub type FuncPtr = fn(ctx: &mut StackVM, args: Vec<(Value, usize)>) -> Value;
#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub func: FuncPtr,
    pub param_count: i16,
}
impl NativeFunction {
    pub fn new(func: FuncPtr, param_count: i16) -> Self {
        Self { func, param_count }
    }
}
impl From<NativeFunction> for Value {
    fn from(value: NativeFunction) -> Self {
        Self::NativeFunction(value)
    }
}
