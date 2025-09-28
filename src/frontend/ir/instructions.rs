use core::fmt;
use std::{
    fmt::{Debug, Display},
    u8,
};

#[derive(Clone, Debug)]
#[repr(u8)]
pub enum IrNode {
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
    /// Pops a function out of stack, and their arguments, then calling it
    Call(u8),
}
impl Display for IrNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrNode::NoOp => f.write_str("noop"),
            IrNode::Label(label) => write!(f, "{}:", label),
            IrNode::Push(lit) => write!(f, "push {}", lit),
            IrNode::LoadLocal(index) => write!(f, "loadlocal {}", index),
            IrNode::StoreLocal(index) => write!(f, "storelocal {}", index),
            IrNode::LoadGlobal(index) => write!(f, "loadglobal {}", index),
            IrNode::StoreGlobal(index) => write!(f, "storeglobal {}", index),
            IrNode::Pop => f.write_str("pop"),
            IrNode::Goto(label) => write!(f, "goto {}", label),
            IrNode::Branch(label) => write!(f, "branch {}", label),
            IrNode::NotBranch(label) => write!(f, "notbranch {}", label),
            IrNode::Add => f.write_str("add"),
            IrNode::Mult => f.write_str("mult"),
            IrNode::Div => f.write_str("div"),
            IrNode::Sub => f.write_str("sub"),
            IrNode::Mod => f.write_str("mod"),
            IrNode::And => f.write_str("and"),
            IrNode::Or => f.write_str("or"),
            IrNode::Greater => f.write_str("greater"),
            IrNode::Lesser => f.write_str("lesser"),
            IrNode::GreaterEq => f.write_str("greatereq"),
            IrNode::LesserEq => f.write_str("lessereq"),
            IrNode::NotEq => f.write_str("noteq"),
            IrNode::Eq => f.write_str("eq"),
            IrNode::NullCo => f.write_str("nullco"),
            IrNode::Not => f.write_str("not"),
            IrNode::Neg => f.write_str("neg"),
            IrNode::Stop => f.write_str("stop"),
            IrNode::Ret => f.write_str("ret"),
            IrNode::Call(arity) => write!(f, "call {}", arity),
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
