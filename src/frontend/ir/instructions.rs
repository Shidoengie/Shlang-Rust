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
    Goto(i16),
    Branch(i16),
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
#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Null,
    Function(Arc<Function>),
    NativeFunction(NativeFunction),
}
#[derive(Debug, Clone)]
pub struct Function {
    pub proc: Vec<OpCode>,
    pub num_locals: usize,
    pub param_count: usize,
}

pub type FuncPtr = fn(ctx: &mut StackVM, args: Vec<Value>) -> Value;
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
