use core::fmt;
use std::{fmt::Debug, sync::Arc, u8};

use crate::backend::vm::StackVM;

#[derive(Clone, Debug)]

pub enum OpCode {
    NoOp,
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
// impl Debug for OpCode {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Push(val) => f.debug_tuple("Push"),
//             Self::LoadLocal(address) => todo!(),
//             Self::StoreLocal(address) => todo!(),
//             Self::LoadGlobal(address) => todo!(),
//             Self::StoreGlobal(address) => todo!(),
//             Self::Pop => todo!(),
//             Self::Goto(offset) => todo!(),
//             Self::Branch(offset) => todo!(),
//             Self::Add => todo!(),
//             Self::Mult => todo!(),
//             Self::Div => todo!(),
//             Self::Sub => todo!(),
//             Self::Mod => todo!(),
//             Self::And => todo!(),
//             Self::Or => todo!(),
//             Self::Greater => todo!(),
//             Self::Lesser => todo!(),
//             Self::GreaterEq => todo!(),
//             Self::LesserEq => todo!(),
//             Self::NotEq => todo!(),
//             Self::Eq => todo!(),

//             Self::NullCo => todo!(),
//             Self::Not => todo!(),
//             Self::Neg => todo!(),

//             Self::Stop => todo!(),
//             Self::Ret => todo!(),

//             Self::Call(params) => todo!(),

//             Self::PopSlots { start, len } => todo!(),
//         }
//     }
// }
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
    pub param_count: u8,
}
impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Self::Function(Arc::new(value))
    }
}
pub type FuncPtr = fn(ctx: &mut StackVM, args: &[(Value, usize)]) -> Value;
#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub func: FuncPtr,
    pub param_count: u8,
}
impl NativeFunction {
    pub const VARIADIC_VALUE: u8 = u8::MAX;
    pub fn new(func: FuncPtr, param_count: u8) -> Self {
        Self { func, param_count }
    }
    pub fn new_variadic(func: FuncPtr) -> Self {
        Self {
            func,
            param_count: Self::VARIADIC_VALUE,
        }
    }
    ///Determines if a given parameter length is the accepted parameter count
    pub fn is_arglen_valid(&self, arg_len: u8) -> bool {
        return self.is_variadic() || arg_len == self.param_count;
    }
    pub fn is_variadic(&self) -> bool {
        return self.param_count == Self::VARIADIC_VALUE;
    }
}
impl From<NativeFunction> for Value {
    fn from(value: NativeFunction) -> Self {
        Self::NativeFunction(value)
    }
}
