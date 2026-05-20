use std::{collections::HashMap, fmt::Debug};

use crate::{
    frontend::opkind::*,
    spans::{Span, Spanned},
};

type NodeRefSpan = Spanned<Box<ResolvedNode>>;

#[derive(Clone)]
pub enum ResolvedNode {
    Null,
    Bool(bool),
    String(String),
    Float(f64),
    Int(i64),
    BinaryNode {
        kind: BinaryOp,
        left: NodeRefSpan,
        right: NodeRefSpan,
    },
    UnaryNode(UnaryOp, NodeRefSpan),
    Result(NodeRefSpan),
    Return(NodeRefSpan),
    Break,
    Continue,

    Assignment {
        target: NodeRefSpan,
        value: NodeRefSpan,
    },
    Variable {
        id: usize,
        is_global: bool,
    },

    Decl(Decl),

    Index {
        target: NodeRefSpan,
        index: NodeRefSpan,
    },
    FunctionLit(FunctionLit),
    ListLit(Vec<Spanned<ResolvedNode>>),
    Call {
        callee: NodeRefSpan,
        args: Vec<Spanned<ResolvedNode>>,
    },

    Branch(Branch),

    Loop(Block),
    While {
        condition: NodeRefSpan,
        block: Block,
    },
    Constructor {
        target: NodeRefSpan,
        params: HashMap<String, Spanned<ResolvedNode>>,
    },
    ForLoop {
        loop_var: usize,
        list: NodeRefSpan,
        block: Block,
    },

    DoBlock(Block),
    StructDef(HashMap<String, NodeRefSpan>),
    RecordLit(HashMap<String, NodeRefSpan>),
    FieldAccess(NodeRefSpan, Spanned<AccessType>),
}
impl Debug for ResolvedNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(val) => {
                if *val {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            Self::RecordLit(map) => f.debug_map().entries(map).finish(),
            Self::Int(num) => write!(f, "Int({num})"),
            Self::Float(num) => write!(f, "Float({num})"),
            Self::String(val) => write!(f, r#""{val}""#),
            Self::Variable { id, is_global } => {
                write!(f, "Variable(")?;
                if *is_global {
                    write!(f, "global {id})")?;
                } else {
                    write!(f, "{id})")?;
                };
                Ok(())
            }
            Self::Null => f.write_str("Null"),
            Self::Break => f.write_str("Break"),
            Self::Continue => f.write_str("Continue"),
            Self::Decl(decl) => f
                .debug_struct("Decl")
                .field("id", &decl.id)
                .field("expr", &decl.expr)
                .field("is_global", &decl.is_global)
                .finish(),
            Self::Index { target, index } => f
                .debug_struct("Index")
                .field("target", target)
                .field("index", index)
                .finish(),
            Self::ListLit(list) => {
                f.write_str("List")?;
                f.debug_list().entries(list).finish()
            }
            Self::DoBlock(block) => {
                write!(f, "Do ")?;
                f.debug_set().entries(&block.item).finish()
            }
            Self::Loop(block) => {
                write!(f, "Loop ")?;
                f.debug_set().entries(&block.item).finish()
            }

            Self::Return(ret) => f.debug_tuple("Return").field(&ret.item).finish(),
            Self::Result(ret) => f.debug_tuple("Result").field(&ret.item).finish(),
            Self::BinaryNode { kind, left, right } => f
                .debug_struct("BinaryNode")
                .field("left", left)
                .field("right", right)
                .field("op", kind)
                .finish(),
            Self::UnaryNode(op, node) => f.debug_tuple("Unary").field(op).field(node).finish(),
            Self::Constructor { target, params } => f
                .debug_struct("Constructor")
                .field("target", target)
                .field("params", params)
                .finish(),
            Self::Branch(branch) => f
                .debug_struct("Branch")
                .field("condition", &branch.condition)
                .field("if_block", &branch.if_block)
                .field("else_block", &branch.else_block)
                .finish(),
            Self::ForLoop {
                loop_var,
                list,
                block,
            } => f
                .debug_struct("ForLoop")
                .field("loop_var", loop_var)
                .field("list", list)
                .field("block", block)
                .finish(),
            Self::Assignment { target, value } => write!(f, "Assign({target:#?} = {value:#?})"),
            Self::FieldAccess(target, access) => f
                .debug_tuple("FieldAccess")
                .field(target)
                .field(access)
                .finish(),
            Self::While { condition, block } => f
                .debug_struct("While")
                .field("condition", condition)
                .field("block", block)
                .finish(),
            Self::Call { callee, args } => f
                .debug_struct("Call")
                .field("callee", callee)
                .field("args", args)
                .finish(),
            Self::StructDef(map) => f.debug_map().entries(map).finish(),
            Self::FunctionLit(func) => {
                if func.captures {
                    write!(f, "Closure(")?;
                } else {
                    write!(f, "Function(")?;
                }

                for (index, id) in func.idents.iter().enumerate() {
                    write!(f, "{}", id)?;
                    if index + 1 != func.idents.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")?;

                f.debug_set().entries(&func.block.item).finish()
            }
        }
    }
}
impl ResolvedNode {
    pub fn is_literal(&self) -> bool {
        return matches!(
            self,
            Self::RecordLit(_)
                | Self::FunctionLit(_)
                | Self::Null
                | Self::Bool(_)
                | Self::String(_)
                | Self::Float(_)
                | Self::Int(_)
        );
    }
}
pub type Block = Spanned<Vec<RNodeSpan>>;

#[derive(Clone, Debug)]
pub struct Branch {
    pub condition: NodeRefSpan,
    pub if_block: Block,
    pub else_block: Option<Block>,
}
#[derive(Clone, Debug)]
pub struct Decl {
    pub id: usize,
    pub expr: NodeRefSpan,
    pub is_global: bool,
}

#[derive(Clone, Debug)]
pub struct FunctionLit {
    pub captures: bool,
    pub idents: Vec<usize>,
    pub block: Block,
    pub local_count: usize,
}
#[derive(Clone, Debug)]
pub enum AccessType {
    Property(String),
    Method {
        callee: String,
        callee_span: Span,
        args: Vec<RNodeSpan>,
        arg_span: Span,
    },
}
pub trait IntoRNodespan {
    fn to_rnodespan(self, span: Span) -> RNodeSpan;
}
macro_rules! rnodes_from {
    ($($name:ident)*) => {
        $(
            impl ::core::convert::From<$name> for ResolvedNode {
                fn from(node: $name) -> Self {
                    Self::$name(node)
                }
            }
            impl IntoRNodespan for $name {
                fn to_rnodespan(self,span:Span) -> RNodeSpan {
                    Spanned::new(ResolvedNode::$name(self),span)
                }

            }
        )*
    }
}
rnodes_from! { Decl Branch FunctionLit }

#[derive(Debug)]
pub struct ResolvedAstNode {
    pub node: RNodeSpan,
    pub global_count: usize,
    pub local_count: usize,
}
impl ResolvedAstNode {
    pub fn new(node: RNodeSpan, global_count: usize, local_count: usize) -> Self {
        Self {
            node,

            global_count,
            local_count,
        }
    }
}
#[derive(Debug)]
pub struct ResolvedAst {
    pub proc: Vec<RNodeSpan>,
    pub global_count: usize,
    pub local_count: usize,
}

pub type RNodeSpan = Spanned<ResolvedNode>;
