use crate::spans::*;

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

#[derive(Clone, PartialEq)]
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
    Declaration(Declaration),
    Assignment { target: NodeRef, value: NodeRef },
    Variable(String),
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
    StructLit(HashMap<String, NodeSpan>),
    RecordLit(HashMap<String, NodeSpan>),
    FieldAccess(FieldAccess),
    DontResult,
}
impl Node {
    pub fn variant_name(&self) -> &'static str {
        match self {
            Node::Null => "Null",
            Node::Bool(_) => "Bool",
            Node::Str(_) => "Str",
            Node::Float(_) => "Float",
            Node::Int(_) => "Int",
            Node::BinaryNode(_) => "BinaryNode",
            Node::UnaryNode(_) => "UnaryNode",
            Node::Result(_) => "ResultNode",
            Node::Return(_) => "ReturnNode",
            Node::BreakNode => "BreakNode",
            Node::ContinueNode => "ContinueNode",
            Node::Declaration(_) => "VarDecl",
            Node::Assignment { .. } => "Assignment",
            Node::Variable(_) => "Variable",
            Node::Index { .. } => "Index",
            Node::FunctionLit(_) => "FuncDef",
            Node::ListLit(_) => "ListLit",
            Node::Call(_) => "Call",
            Node::Branch(_) => "Branch",
            Node::Loop(_) => "Loop",
            Node::While(_) => "While",
            Node::ForLoop(_) => "ForLoop",
            Node::DoBlock(_) => "DoBlock",
            Node::Constructor(_) => "Constructor",
            Node::StructLit(_) => "StructDef",
            Node::RecordLit(_) => "RecordLit",
            Node::FieldAccess(_) => "FieldAccess",
            Node::DontResult => "DontResult",
        }
    }
    pub fn can_result(&self) -> bool {
        !matches!(
            self.clone(),
            Self::Declaration(_)
                | Self::Assignment {
                    target: _,
                    value: _
                }
                | Self::Return(_)
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
            Node::RecordLit(map) => f.debug_map().entries(map).finish(),
            Node::Int(num) => write!(f, "Int({num})"),
            Node::Float(num) => write!(f, "Float({num})"),
            Node::Str(txt) => write!(f, r#""{txt}""#),
            Node::Variable(val) => write!(f, "Var({val})"),
            Node::Null => f.write_str("Null"),
            Node::BreakNode => f.write_str("Break"),
            Node::ContinueNode => f.write_str("Continue"),
            Node::Declaration(decl) => {
                if decl.readonly {
                    write!(f, "LetDecl")?;
                } else {
                    write!(f, "VarDecl")?;
                }
                write!(f, "({name} = {expr:?})", name = decl.name, expr = decl.expr)
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
                f.debug_set().entries(&block.item).finish()
            }
            Node::Loop(block) => {
                write!(f, "Loop")?;
                f.debug_set().entries(&block.item).finish()
            }

            Node::Return(ret) => f.debug_tuple("Return").field(&ret.item).finish(),
            Node::Result(ret) => f.debug_tuple("Result").field(&ret.item).finish(),
            Node::BinaryNode(bin) => write!(f, "{bin:#?}"),
            Node::UnaryNode(node) => write!(f, "{node:#?}"),
            Node::Constructor(node) => write!(f, "{node:#?}"),
            Node::StructLit(node) => write!(f, "{node:#?}"),
            Node::Branch(node) => write!(f, "{node:#?}"),
            Node::ForLoop(node) => write!(f, "{node:#?}"),
            Node::Assignment { target, value } => write!(f, "Assign({target:#?} = {value:#?})"),
            Node::FieldAccess(node) => write!(f, "{node:#?}"),
            Node::While(node) => write!(f, "{node:#?}"),
            Node::Call(node) => write!(f, "{node:#?}"),

            Node::DontResult => f.write_str("DontResult"),

            Node::FunctionLit(func) => {
                if func.captures {
                    write!(f, "Closure(")?;
                } else {
                    write!(f, "Function(")?;
                }

                for (index, name) in func.args.iter().enumerate() {
                    f.write_str(name)?;
                    if index + 1 != func.args.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")?;

                f.debug_set().entries(&func.block.item).finish()
            }
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
nodes_from! { Declaration FunctionLit UnaryNode Constructor  FieldAccess BinaryNode Call Branch While ForLoop}

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
    Negative,
    Not,
}
#[derive(Clone, Debug, PartialEq)]
pub struct UnaryNode {
    pub kind: UnaryOp,
    pub target: NodeRef,
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
    Property(String),
    Method {
        callee: String,
        callee_span: Span,
        args: Vec<NodeSpan>,
        arg_span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]

pub struct Constructor {
    pub target: NodeRef,
    pub params: HashMap<String, NodeSpan>,
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
    pub ident: String,
    pub list: NodeRef,
    pub proc: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub name: String,
    pub expr: NodeRef,
    pub readonly: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionLit {
    pub block: Block,
    pub args: Vec<String>,
    pub captures: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Decl(Declaration),
}
