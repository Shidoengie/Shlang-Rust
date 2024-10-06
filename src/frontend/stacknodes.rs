use std::fmt::Display;

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Null,
    Num,
    Closure,
    Bool,
    Str,
    Function,
    List,
    AnonStruct,
    Struct(String),
    Ref,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let txt = match self {
            Self::Null => "null",
            Self::Function => "func",
            Self::Bool => "bool",
            Self::Num => "num",
            Self::Str => "str",
            Self::List => "list",
            Self::Closure => "closure",
            Self::Struct(id) => id,
            Self::AnonStruct => "struct",
            Self::Ref => "ref",
        };
        write!(f, "{txt}")
    }
}
