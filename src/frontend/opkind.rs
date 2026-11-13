use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
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

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum UnaryOp {
	Negative,
	Not,
}
