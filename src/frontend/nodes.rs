use crate::spans::*;
use rayon::prelude::*;

use std::collections::*;
use std::fmt::{Debug, Display, Write};
use std::ops::Deref;

#[derive(Clone, PartialEq)]
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
            Node::Int(num) => write!(f, "Int({num})"),
            Node::Float(num) => write!(f, "Float({num})"),
            Node::Str(txt) => write!(f, r#""{txt}""#),
            Node::Variable(val) => write!(f, "Var({val})"),
            Node::NullNode => f.write_str("Null"),
            Node::BreakNode => f.write_str("Break"),
            Node::ContinueNode => f.write_str("continue"),
            Node::Declaration(name, val) => {
                write!(f, "Declare({name}) as {val:?}")
            }
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
                f.debug_set().entries(block).finish()
            }
            Node::Loop(block) => {
                write!(f, "Loop")?;
                f.debug_set().entries(block).finish()
            }

            Node::ReturnNode(ret) => f.debug_tuple("Return").field(ret.item.deref()).finish(),
            Node::ResultNode(ret) => f.debug_tuple("Result").field(ret.item.deref()).finish(),
            Node::BinaryNode(bin) => write!(f, "{bin:#?}"),
            Node::UnaryNode(node) => write!(f, "{node:#?}"),
            Node::Constructor(node) => write!(f, "{node:#?}"),
            Node::StructDef(node) => write!(f, "{node:#?}"),
            Node::Branch(node) => write!(f, "{node:#?}"),
            Node::ForLoop(node) => write!(f, "{node:#?}"),
            Node::Assignment(node) => write!(f, "{node:#?}"),
            Node::FieldAccess(node) => write!(f, "{node:#?}"),
            Node::While(node) => write!(f, "{node:#?}"),
            Node::Call(node) => write!(f, "{node:#?}"),
            Node::FuncDef(node) => write!(f, "{node:#?}"),
            Node::DontResult => f.write_str("DontResult"),
        }
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
    Or,
    IsEqual,
    IsDifferent,
    Greater,
    Lesser,
    GreaterOrEqual,
    LesserOrEqual,
    NullCoalescing,
}

#[derive(Clone, PartialEq)]
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
impl Debug for BinaryNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("{:?}", self.kind))
            .field(&self.left)
            .field(&self.right)
            .finish()
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not,
}
#[derive(Clone, PartialEq)]
pub struct UnaryNode {
    pub kind: UnaryOp,
    pub target: NodeRef,
}
impl Debug for UnaryNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({:?})", self.kind, self.target)
    }
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

#[derive(Clone, PartialEq)]
pub enum Field {
    Declaration(String, Spanned<Box<Node>>),
    StructDef(StructDef),
}
impl Debug for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declaration(name, val) => {
                write!(f, "Declare({name}) as {val:?}")
            }

            Self::StructDef(node) => write!(f, "{node:#?}"),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct FieldAccess {
    pub target: NodeRef,
    pub requested: Spanned<AccessType>,
}
#[derive(Clone, PartialEq)]
pub enum AccessType {
    Method(String, Vec<Spanned<Node>>),
    Property(String),
}
impl Debug for AccessType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Property(prop) => write!(f, ".{prop}"),
            Self::Method(name, args) => {
                write!(f, ".")?;
                if args.is_empty() {
                    return write!(f, "{name}()");
                }
                let mut buffer = f.debug_tuple(&name);
                for node in args {
                    buffer.field(node);
                }
                buffer.finish()
            }
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct StructDef {
    pub name: Option<String>,
    pub fields: Vec<Spanned<Field>>,
}
impl Debug for StructDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Struct ")?;
        if let Some(ref name) = self.name {
            write!(f, "{name}")?;
        }
        f.debug_set().entries(&self.fields).finish()
    }
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

#[derive(Clone, PartialEq)]
pub struct FuncDef {
    pub block: NodeStream,
    pub args: Vec<String>,
    pub captures: bool,
}
impl Debug for FuncDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.captures {
            write!(f, "Closure(")?;
        } else {
            write!(f, "Function(")?;
        }
        for (index, name) in self.args.iter().enumerate() {
            f.write_str(&name)?;
            if index + 1 != self.args.len() {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")?;

        f.debug_set().entries(&self.block).finish()
    }
}
