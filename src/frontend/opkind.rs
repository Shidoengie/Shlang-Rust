#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
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
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
	Negative,
	Not,
}
