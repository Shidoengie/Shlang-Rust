use crate::frontend::tokens::TokenType;
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

/// Helper to get the precedence of a given token type.
impl From<&TokenType> for Precedence {
    fn from(kind: &TokenType) -> Self {
        match kind {
            TokenType::Equal
            | TokenType::QuestionEqual
            | TokenType::PlusEqual
            | TokenType::MinusEqual
            | TokenType::StarEqual
            | TokenType::SlashEqual => Precedence::Assign,
            TokenType::Or | TokenType::DualPipe => Precedence::Or,
            TokenType::And | TokenType::DualAmpersand => Precedence::And,
            TokenType::DoubleEqual | TokenType::BangEqual => Precedence::Equality,
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Lesser
            | TokenType::LesserEqual => Precedence::Comparison,
            TokenType::DualQuestion => Precedence::Nullish,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Slash | TokenType::Star | TokenType::Percent => Precedence::Product,
            TokenType::LParen => Precedence::Call,
            TokenType::LBrace => Precedence::Constructor,
            TokenType::LBracket => Precedence::Index,
            TokenType::Dot => Precedence::Member,
            _ => Precedence::Lowest,
        }
    }
}

/// Helper to convert a token type into its corresponding BinaryOp.
impl From<TokenType> for BinaryOp {
    fn from(kind: TokenType) -> Self {
        match kind {
            TokenType::Plus => BinaryOp::Add,
            TokenType::Minus => BinaryOp::Subtract,
            TokenType::Slash => BinaryOp::Divide,
            TokenType::Star => BinaryOp::Multiply,
            TokenType::Percent => BinaryOp::Modulo,
            TokenType::And | TokenType::DualAmpersand => BinaryOp::And,
            TokenType::Or | TokenType::DualPipe => BinaryOp::Or,
            TokenType::DoubleEqual => BinaryOp::IsEqual,
            TokenType::BangEqual => BinaryOp::IsDifferent,
            TokenType::Greater => BinaryOp::Greater,
            TokenType::Lesser => BinaryOp::Lesser,
            TokenType::GreaterEqual => BinaryOp::GreaterOrEqual,
            TokenType::LesserEqual => BinaryOp::LesserOrEqual,
            TokenType::DualQuestion => BinaryOp::NullCoalescing,
            // This panic should ideally never be reached if the parser logic is correct.
            _ => panic!("Cannot convert token type {:?} to a BinaryOp", kind),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct FuncDef {
    pub block: NodeStream,
    pub args: Vec<String>,
    pub captures: bool,
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
    ResultNode(NodeRef),
    ReturnNode(NodeRef),
    BreakNode,
    ContinueNode,
    Declaration(String, NodeRef),
    Assignment { target: NodeRef, value: NodeRef },
    Variable(String),
    Index { target: NodeRef, index: NodeRef },
    FuncDef(FuncDef),
    ListLit(Vec<NodeSpan>),
    Call(Call),

    Branch(Branch),
    Loop(NodeStream),
    While(While),
    ForLoop(ForLoop),
    DoBlock(NodeStream),
    Constructor(Constructor),
    StructDef(StructDef),
    FieldAccess(FieldAccess),
    DontResult,
}
impl Node {
    pub fn can_result(&self) -> bool {
        if let Self::StructDef(def) = self {
            return def.name.is_none();
        }
        !matches!(
            self.clone(),
            Self::Declaration(_, _)
                | Self::Assignment {
                    target: _,
                    value: _
                }
                | Self::ReturnNode(_)
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
            Node::Null => f.write_str("Null"),
            Node::BreakNode => f.write_str("Break"),
            Node::ContinueNode => f.write_str("Continue"),
            Node::Declaration(name, expr) => {
                write!(f, "Declare({name} = {expr:?})",)
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

            Node::ReturnNode(ret) => f.debug_tuple("Return").field(&ret.item).finish(),
            Node::ResultNode(ret) => f.debug_tuple("Result").field(&ret.item).finish(),
            Node::BinaryNode(bin) => write!(f, "{bin:#?}"),
            Node::UnaryNode(node) => write!(f, "{node:#?}"),
            Node::Constructor(node) => write!(f, "{node:#?}"),
            Node::StructDef(node) => write!(f, "{node:#?}"),
            Node::Branch(node) => write!(f, "{node:#?}"),
            Node::ForLoop(node) => write!(f, "{node:#?}"),
            Node::Assignment { target, value } => write!(f, "Assign({target:#?} = {value:#?})"),
            Node::FieldAccess(node) => write!(f, "{node:#?}"),
            Node::While(node) => write!(f, "{node:#?}"),
            Node::Call(node) => write!(f, "{node:#?}"),

            Node::DontResult => f.write_str("DontResult"),

            Node::FuncDef(func) => {
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

                f.debug_set().entries(&func.block).finish()
            }
        }
    }
}
pub type NodeSpan = Spanned<Node>;
pub type NodeRef = Spanned<Box<Node>>;
impl NodeSpan {
    pub fn wrap_in_result(self) -> Self {
        let span = self.span;
        Node::ResultNode(self.box_item()).to_spanned(span)
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
nodes_from! { FuncDef UnaryNode Constructor StructDef  FieldAccess BinaryNode Call Branch While ForLoop}

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
    pub object: NodeRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Call {
    pub callee: NodeRef,
    pub args: NodeStream,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Field {
    Declaration(String, NodeRef),
    StructDef(StructDef),
}
impl Spanned<Field> {
    pub fn to_node(self) -> NodeSpan {
        match self.item {
            Field::Declaration(name, expr) => Node::Declaration(name, expr).to_spanned(self.span),
            Field::StructDef(decl) => Node::StructDef(decl.clone()).to_spanned(self.span),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct FieldAccess {
    pub target: NodeRef,
    pub requested: NodeRef,
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
