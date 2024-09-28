use crate::spans::*;
use rayon::prelude::*;

use std::collections::*;
use std::fmt::{Debug, Display};

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDef {
    pub block: NodeStream,
    pub args: Vec<String>,
    pub captures: bool,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Null,
    Void,
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
            Self::Void => "void",
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
#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    BinaryNode(BinaryNode),
    UnaryNode(UnaryNode),
    ResultNode(Spanned<Box<Node>>),
    ReturnNode(Spanned<Box<Node>>),
    BreakNode,
    ContinueNode,
    Declaration(String, Spanned<Box<Self>>),
    Assignment(Assignment),
    Variable(String),

    Call(Call),

    Branch(Branch),
    Loop(NodeStream),
    While(While),
    ForLoop(ForLoop),
    DoBlock(NodeStream),
    Constructor(Constructor),
    FuncDef(FuncDef),
    StructDef(StructDef),
    FieldAccess(FieldAccess),
    Index { target: NodeRef, index: NodeRef },
    ListLit(Vec<NodeSpan>),
    NullNode,
    Float(f64),
    Int(i64),
    Bool(bool),
    Str(String),
    DontResult,
}
impl Node {
    pub fn can_result(&self) -> bool {
        if let Self::StructDef(def) = self {
            return def.name.is_none();
        }
        !matches!(
            self.clone(),
            Self::Declaration(..)
                | Self::Assignment(..)
                | Self::ReturnNode(..)
                | Self::BreakNode
                | Self::ContinueNode
        )
    }
}

pub type NodeSpan = Spanned<Node>;
type NodeRef = Spanned<Box<Node>>;
impl NodeSpan {
    pub fn wrap_in_result(&self) -> Self {
        let value = self.clone();
        Node::ResultNode(value.clone().box_item()).as_spanned(value.span)
    }
}
pub trait IntoNodespan {
    fn to_nodespan(self, span: Span) -> NodeSpan;
}

pub type NodeStream = Vec<Spanned<Node>>;

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
                    Spanned::new(Node::$name(self.clone()),span)
                }

            }
        )*
    }
}
nodes_from! { FuncDef UnaryNode Constructor StructDef  FieldAccess BinaryNode Call Assignment Branch While ForLoop}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Divide,
    Multiply,
    Modulo,
    And,
    OR,
    IsEqual,
    IsDifferent,
    Greater,
    Lesser,
    GreaterOrEqual,
    LesserOrEqual,
    NullCoalescing,
}

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
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not,
}
#[derive(Clone, Debug, PartialEq)]
pub struct UnaryNode {
    pub kind: UnaryOp,
    pub target: NodeRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub target: NodeRef,
    pub value: NodeRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Call {
    pub callee: NodeRef,
    pub args: NodeStream,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Field {
    Declaration(String, Spanned<Box<Node>>),
    StructDef(StructDef),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldAccess {
    pub target: NodeRef,
    pub requested: Spanned<AccessType>,
}
#[derive(Clone, Debug, PartialEq)]
pub enum AccessType {
    Method(String, Vec<Spanned<Node>>),
    Property(String),
}
#[derive(Clone, Debug, PartialEq)]
pub struct StructDef {
    pub name: Option<String>,
    pub fields: Vec<Spanned<Field>>,
}
#[derive(Clone, Debug, PartialEq)]

pub struct Constructor {
    pub name: String,
    pub params: HashMap<String, NodeSpan>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub condition: NodeRef,
    pub if_block: NodeStream,
    pub else_block: Option<NodeStream>,
}
impl Branch {
    pub fn new_single(condition: NodeSpan, block: NodeStream) -> Self {
        Self {
            condition: condition.box_item(),
            if_block: block,
            else_block: None,
        }
    }
    pub fn new(condition: NodeSpan, if_block: NodeStream, else_block: NodeStream) -> Self {
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
    pub proc: NodeStream,
}
#[derive(Clone, Debug, PartialEq)]
pub struct ForLoop {
    pub ident: String,
    pub list: NodeRef,
    pub proc: NodeStream,
}
