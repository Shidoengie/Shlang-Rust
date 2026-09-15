use variant_name::VariantName;

use crate::collections::{FileId, spans::*};
use crate::frontend::opkind::{BinaryOp, UnaryOp};
use crate::idents::{Ident, IdentArray, IdentId};

use std::collections::*;
use std::fmt::Debug;
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
	Lowest,
	Assign,      // =
	Or,          // or, ||
	And,         // and, &&
	Equality,    // ==, !=
	Comparison,  // <, >, <=, >=
	Nullish,     // ??
	Sum,         // +, -
	Product,     // *, /, %
	Unary,       // -, !
	Call,        // my_func()
	Index,       // my_list[0]
	Constructor, // my_obj{x:10}
	Member,      // my_obj.field
}
#[derive(Debug, Clone)]
pub struct Ast<'a> {
	pub node: Spanned<Node>,
	pub ident_pool: IdentArray<'a>,
	pub file_id: FileId,
}
#[derive(Clone)]
pub struct Program<'a> {
	pub proc: Vec<NodeSpan>,
	pub ident_pool: IdentArray<'a>,
	pub file_id: FileId,
}
impl Debug for Program<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if !f.alternate() {
			f.debug_struct("Program")
				.field("proc", &self.proc)
				.field(&"ident_pool", &self.ident_pool)
				.field(&"file_id", &self.file_id)
				.finish()?;
		}
		f.debug_struct("Program")
			.field(&"ident_pool", &self.ident_pool)
			.field(&"file_id", &self.file_id)
			.finish()?;
		f.write_str(" = ")?;
		if self.proc.len() == 1 {
			write!(f, "{:#?}", self.proc[0])?;
		} else {
			write!(f, "{:#?}", self.proc)?;
		}
		Ok(())
	}
}
#[derive(Clone, PartialEq, VariantName)]
pub enum Node {
	Null,
	Bool(bool),
	Str(String),
	Float(f64),
	Int(i64),
	BinaryNode(BinaryNode),
	UnaryNode(UnaryNode),
	Result(NodeRef),
	Return(NodeRef),
	BreakNode,
	ContinueNode,
	Decl(Decl),
	Assignment { target: NodeRef, value: NodeRef },
	Variable(IdentId),
	Index { target: NodeRef, index: NodeRef },
	FunctionLit(FunctionLit),
	ListLit(Vec<NodeSpan>),
	Call(Call),
	Branch(Branch),
	Loop(Block),
	While(While),
	ForLoop(ForLoop),
	DoBlock(Block),
	Constructor(Constructor),
	StructLit(HashMap<IdentId, NodeSpan>),
	RecordLit(HashMap<IdentId, NodeSpan>),
	ClassLit(ClassLit),
	FieldAccess(FieldAccess),
	SelfTy,
	DontResult,
	SelfValue,
}

impl Node {
	pub fn can_result(&self) -> bool {
		!matches!(
			self.clone(),
			Self::Decl(_)
				| Self::Assignment {
					target: _,
					value: _
				} | Self::Return(_)
				| Self::BreakNode
				| Self::ContinueNode
		)
	}
}
impl Debug for Node {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Node::Bool(val) => {
				if *val {
					write!(f, "true")
				} else {
					write!(f, "false")
				}
			}
			Node::ClassLit(class) => write!(f, "{class:#?}"),
			Node::RecordLit(map) => f.debug_map().entries(map).finish(),
			Node::Int(num) => write!(f, "Int({num})"),
			Node::Float(num) => write!(f, "Float({num})"),
			Node::Str(txt) => write!(f, r#""{txt}""#),
			Node::Variable(val) => write!(f, "Var({val:?})"),
			Node::Null => f.write_str("Null"),
			Node::BreakNode => f.write_str("Break"),
			Node::ContinueNode => f.write_str("Continue"),
			Node::Decl(decl) => Debug::fmt(decl, f),
			Node::Index { target, index } => f
				.debug_struct("Index")
				.field("target", target)
				.field("index", index)
				.finish(),
			Node::ListLit(list) => {
				f.write_str("List")?;
				f.debug_set().entries(list).finish()
			}
			Node::DoBlock(block) => {
				write!(f, "Do")?;
				f.debug_set().entries(&block.item).finish()
			}
			Node::Loop(block) => {
				write!(f, "Loop")?;
				f.debug_set().entries(&block.item).finish()
			}

			Node::Return(ret) => f.debug_tuple("Return").field(&ret.item).finish(),
			Node::Result(ret) => f.debug_tuple("Result").field(&ret.item).finish(),
			Node::BinaryNode(node) => Debug::fmt(node, f),
			Node::UnaryNode(node) => Debug::fmt(node, f),
			Node::Constructor(node) => Debug::fmt(node, f),
			Node::StructLit(node) => write!(f, "{node:#?}"),
			Node::Assignment { target, value } => write!(f, "Assign({target:#?} = {value:#?})"),
			Node::FieldAccess(node) => Debug::fmt(node, f),
			Node::While(node) => Debug::fmt(node, f),
			Node::Call(node) => Debug::fmt(node, f),
			Node::Branch(node) => Debug::fmt(node, f),
			Node::ForLoop(node) => Debug::fmt(node, f),

			Node::DontResult => f.write_str("DontResult"),

			Node::FunctionLit(func) => Debug::fmt(func, f),
			Node::SelfTy => f.write_str("SelfTy"),
			Node::SelfValue => f.write_str("SelfValue"),
		}
	}
}
pub type NodeSpan = Spanned<Node>;
pub type NodeRef = Spanned<Box<Node>>;
impl NodeSpan {
	pub fn wrap_in_result(self) -> Self {
		let span = self.span;
		Node::Result(self.box_item()).to_spanned(span)
	}
}
pub trait IntoNodespan {
	fn to_nodespan(self, span: Span) -> NodeSpan;
}

pub type Block = Spanned<Vec<Spanned<Node>>>;

macro_rules! nodes_from {
    ($($name:ident)*) => {
        $(
            impl ::core::convert::From<$name> for Node {
                fn from(node: $name) -> Self {
                    Self::$name(node)
                }
            }
            impl IntoNodespan for $name {
                fn to_nodespan(self,span:Span) -> NodeSpan {
                    Spanned::new(Node::$name(self),span)
                }

            }
        )*
    }
}
nodes_from! { Decl FunctionLit UnaryNode Constructor  FieldAccess BinaryNode Call Branch While ForLoop}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryNode {
	pub kind: BinaryOp,
	pub left: NodeRef,
	pub right: NodeRef,
}

impl BinaryNode {
	pub fn is(&self, kind: &BinaryOp) -> bool {
		self.kind.eq(kind)
	}
	pub fn isnt(&self, kind: &BinaryOp) -> bool {
		self.kind.ne(kind)
	}
}

#[derive(Clone, PartialEq)]
pub struct UnaryNode {
	pub kind: UnaryOp,
	pub target: NodeRef,
}
impl Debug for UnaryNode {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple("Unary")
			.field(&self.kind)
			.field(&self.target)
			.finish()
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Call {
	pub callee: NodeRef,
	pub args: Vec<NodeSpan>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct FieldAccess {
	pub target: NodeRef,
	pub requested: Spanned<AccessType>,
}
#[derive(Clone, Debug, PartialEq)]
pub enum AccessType {
	Property(Ident),
	Method {
		callee: IdentId,
		callee_span: Span,
		args: Vec<NodeSpan>,
		arg_span: Span,
	},
}

#[derive(Clone, Debug, PartialEq)]

pub struct Constructor {
	pub target: NodeRef,
	pub params: HashMap<IdentId, NodeSpan>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
	pub condition: NodeRef,
	pub if_block: Block,
	pub else_block: Option<Block>,
}
impl Branch {
	pub fn new_single(condition: NodeSpan, block: Block) -> Self {
		Self {
			condition: condition.box_item(),
			if_block: block,
			else_block: None,
		}
	}
	pub fn new(condition: NodeSpan, if_block: Block, else_block: Block) -> Self {
		Self {
			condition: condition.box_item(),
			if_block,
			else_block: Some(else_block),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
	pub condition: NodeRef,
	pub proc: Block,
}
#[derive(Clone, Debug, PartialEq)]
pub struct ForLoop {
	pub ident: IdentId,
	pub ident_span: Span,
	pub list: NodeRef,
	pub proc: Spanned<Vec<Spanned<Node>>>,
}

#[derive(Clone, PartialEq)]
pub struct Decl {
	pub name: IdentId,
	pub expr: NodeRef,
	pub readonly: bool,
	pub hoisted: bool,
	pub modifier_span: Option<Span>,
	pub is_item: bool,
}
impl Debug for Decl {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str("Decl")?;
		if self.readonly && self.hoisted {
			f.write_str("(global readonly)")?;
		} else if self.readonly {
			f.write_str("(readonly)")?;
		} else if self.hoisted {
			f.write_str("(global)")?;
		}
		write!(f, "::{:?} = {:?}", self.name, self.expr)
	}
}

impl Decl {
	pub fn new(name: IdentId, expr: NodeRef) -> Self {
		Self {
			name,
			expr,
			readonly: false,
			hoisted: false,
			modifier_span: None,
			is_item: false,
		}
	}
	pub fn with_modifier_span(self, span: Span) -> Self {
		Self {
			modifier_span: Some(span),
			..self
		}
	}
	pub fn as_readonly(self) -> Self {
		Self {
			readonly: true,
			..self
		}
	}
	pub fn as_hoisted(self) -> Self {
		Self {
			hoisted: true,
			..self
		}
	}
	pub fn as_item(self) -> Self {
		Self {
			is_item: true,
			..self
		}
	}
}

#[derive(Clone, PartialEq)]
pub struct FunctionLit {
	pub block: Block,
	pub args: Vec<Spanned<IdentId>>,
	pub captures: bool,
}
impl Debug for FunctionLit {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.captures {
			write!(f, "Closure(")?;
		} else {
			write!(f, "Function(")?;
		}
		for (index, name) in self.args.iter().enumerate() {
			write!(f, "{:?}", name.item)?;
			if index + 1 != self.args.len() {
				write!(f, ", ")?;
			}
		}
		write!(f, ")")?;
		f.debug_set().entries(&self.block.item).finish()
	}
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ClassLit {
	pub name: Option<Ident>,
	pub fields: HashMap<(IdentId, bool), Spanned<Field>>,
	pub methods: HashMap<(IdentId, bool), Spanned<Method>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
	pub default: Option<NodeSpan>,
	pub readonly: bool,
	pub name_span: Span,
	pub modifier_span: Span,
	pub private: bool,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Method {
	pub block: Block,
	pub args: Vec<Ident>,
	pub name_span: Span,
	pub modifier_span: Span,
	pub private: bool,
}
