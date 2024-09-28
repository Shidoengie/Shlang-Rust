#[derive(Debug, Clone)]

pub enum MathOp {
    Add,
    Subtract,
    Divide,
    Multiply,
    Modulo,
    And,
    Or,
    IsEqual,
    IsDifferent,
    Greater,
    Lesser,
    GreaterOrEqual,
    LesserOrEqual,
    NullCoalescing,
    Not,
}

pub enum Value {}

pub enum StackOp {
    Push(f32),
    Load(usize),
    Store(usize),
    MathOp(MathOp),
    Goto(usize, bool),
}
pub type Stack = Vec<StackOp>;
