use crate::{backend::vm::StackVM, spanmap::SpanMap, utils::compact_iter_debug};
use core::fmt;
use std::{
    fmt::{Debug, Display},
    sync::Arc,
};
pub struct ByteCode {
    pub ops: Vec<OpCode>,
    pub span_map: SpanMap,
    pub global_count: usize,
    pub local_count: usize,
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
            writeln!(f, "{pos} | {op}", pos = format!("{index:<2}"))?;
        }
        Ok(())
    }
}
#[derive(Clone, Debug)]
#[repr(u8)]
pub enum OpCode {
    NoOp = 0,
    Push(Value),
    LoadLocal(usize),
    StoreLocal(usize),
    LoadGlobal(usize),
    StoreGlobal(usize),
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
    Stop,
    Ret,
    /// Pops a function out of stack, and their arguments, then calling it
    Call(u8),
}
impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoOp => f.write_str("noop"),
            Self::Push(lit) => write!(f, "push {}", lit),
            Self::LoadLocal(index) => write!(f, "loadlocal {}", index),
            Self::StoreLocal(index) => write!(f, "storelocal {}", index),
            Self::LoadGlobal(index) => write!(f, "loadglobal {}", index),
            Self::StoreGlobal(index) => write!(f, "storeglobal {}", index),
            Self::Pop => f.write_str("pop"),
            Self::Goto(label) => write!(f, "goto {}", label),
            Self::Branch(label) => write!(f, "branch {}", label),
            Self::NotBranch(label) => write!(f, "notbranch {}", label),
            Self::Add => f.write_str("add"),
            Self::Mult => f.write_str("mult"),
            Self::Div => f.write_str("div"),
            Self::Sub => f.write_str("sub"),
            Self::Mod => f.write_str("mod"),
            Self::And => f.write_str("and"),
            Self::Or => f.write_str("or"),
            Self::Greater => f.write_str("greater"),
            Self::Lesser => f.write_str("lesser"),
            Self::GreaterEq => f.write_str("greatereq"),
            Self::LesserEq => f.write_str("lessereq"),
            Self::NotEq => f.write_str("noteq"),
            Self::Eq => f.write_str("eq"),
            Self::NullCo => f.write_str("nullco"),
            Self::Not => f.write_str("not"),
            Self::Neg => f.write_str("neg"),
            Self::Stop => f.write_str("stop"),
            Self::Ret => f.write_str("ret"),
            Self::Call(arity) => write!(f, "call {}", arity),
        }
    }
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
    pub param_count: u8,
}
impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Self::Function(Arc::new(value))
    }
}
pub type FuncPtr = fn(ctx: &mut StackVM, args: &[Value]) -> Value;
#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub func: FuncPtr,
    pub param_count: u8,
}
impl NativeFunction {
    pub const VARIADIC_VALUE: u8 = u8::MAX;
    pub const fn new(func: FuncPtr, param_count: u8) -> Self {
        Self { func, param_count }
    }
    pub const fn new_variadic(func: FuncPtr) -> Self {
        Self {
            func,
            param_count: Self::VARIADIC_VALUE,
        }
    }
    ///Determines if a given parameter length is the accepted parameter count
    pub const fn is_arglen_valid(&self, arg_len: u8) -> bool {
        self.is_variadic() || arg_len == self.param_count
    }
    pub const fn is_variadic(&self) -> bool {
        self.param_count == Self::VARIADIC_VALUE
    }
}
impl From<NativeFunction> for Value {
    fn from(value: NativeFunction) -> Self {
        Self::NativeFunction(value)
    }
}
