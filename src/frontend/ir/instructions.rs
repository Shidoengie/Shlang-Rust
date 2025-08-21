#[derive(Debug, Clone)]

pub enum OpCode {
    Push(Value),
    Load(usize),
    Store(usize),
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
    Stop,
    Ret,
    /// Deletes a range of values from the stack
    FreeRange(usize, usize),
}
#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Null,
}
