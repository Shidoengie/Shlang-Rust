pub enum OpCode {
    Push(Value),
    Load(usize),
    Store(usize),
    Pop,
    Goto(usize),
    Branch(usize, bool),

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
    ///Null coalescing binary instruction
    NullCo,
    Not,
    Neg,
}
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Null,
}
