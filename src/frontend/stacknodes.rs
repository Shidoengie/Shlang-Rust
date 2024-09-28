use super::nodes::{BinaryOp, UnaryOp};

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Float(f64),
    Int(i64),
    Bool(bool),
    Str(String),
}
#[derive(Debug, Clone)]
pub enum StackOp {
    Push(Value),
    Load(usize),
    Store(usize),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    Pop,
    Goto(usize, bool),
}
