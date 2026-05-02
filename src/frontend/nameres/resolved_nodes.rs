use std::{collections::HashMap, fmt::Debug};

use crate::{
    frontend::opkind::*,
    spans::{Span, Spanned},
};

#[derive(Clone, Debug)]
pub enum ResolvedNode {
    Null,
    Bool(bool),
    String(String),
    Float(f64),
    Int(i64),
    BinaryNode {
        kind: BinaryOp,
        left: Spanned<Box<ResolvedNode>>,
        right: Spanned<Box<ResolvedNode>>,
    },
    UnaryNode(UnaryOp, Spanned<Box<ResolvedNode>>),
    Result(Spanned<Box<ResolvedNode>>),
    Return(Spanned<Box<ResolvedNode>>),
    Break,
    Continue,

    Assignment {
        target: Spanned<Box<ResolvedNode>>,
        value: Spanned<Box<ResolvedNode>>,
    },
    Variable {
        id: usize,
        is_global: bool,
    },

    Decl(Decl),

    Index {
        target: Spanned<Box<ResolvedNode>>,
        index: Spanned<Box<ResolvedNode>>,
    },
    FunctionLit(FunctionLit),
    ListLit(Vec<Spanned<ResolvedNode>>),
    Call {
        callee: Spanned<Box<ResolvedNode>>,
        args: Vec<Spanned<ResolvedNode>>,
    },

    Branch(Branch),

    Loop(Block),
    While {
        condition: Spanned<Box<ResolvedNode>>,
        block: Block,
    },
    Constructor {
        target: Spanned<Box<ResolvedNode>>,
        params: HashMap<String, Spanned<ResolvedNode>>,
    },
    ForLoop {
        loop_var: usize,
        list: Spanned<Box<ResolvedNode>>,
        block: Block,
    },

    DoBlock(Block),
    StructDef(HashMap<String, Spanned<Box<ResolvedNode>>>),
    RecordLit(HashMap<String, Spanned<Box<ResolvedNode>>>),
    FieldAccess(Spanned<Box<ResolvedNode>>, Spanned<AccessType>),
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
    pub condition: Spanned<Box<ResolvedNode>>,
    pub if_block: Block,
    pub else_block: Option<Block>,
}
#[derive(Clone, Debug)]
pub struct Decl {
    pub id: usize,
    pub expr: Spanned<Box<ResolvedNode>>,
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
