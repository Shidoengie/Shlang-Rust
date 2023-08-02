use std::collections::*;

use crate::spans::*;
#[derive(Clone, Debug, PartialEq)]

pub enum Control {
    Return(Box<Value>, Type),
    Result(Box<Value>, Type),
    Break,
    Continue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Void,
    Control(Control),
    Num(f64),
    Bool(bool),
    Str(String),
    Function(Function),
    BuiltinFunc(BuiltinFunc),
    Struct(Struct),
}
pub trait ControlUnwrap {
    fn unwrap_result(self) -> Self;
}
pub type TypedValue = (Value, Type);
impl ControlUnwrap for TypedValue {
    fn unwrap_result(self) -> Self {
        let Value::Control(Control::Result(res_val,res_type)) = self.0 else {
            return self;
        };
        return (*res_val, res_type);
    }
}
pub type ValueStream = Vec<Value>;
impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Bool(_) => return Type::Bool,
            Value::BuiltinFunc(_) | Value::Function(_) => return Type::Function,
            Value::Null => return Type::Null,
            Value::Void => return Type::Void,
            Value::Str(_) => return Type::Str,
            Value::Num(_) => return Type::Num,
            Value::Control(_) => return Type::Never,
            Value::Struct(s) => return Type::UserDefined(s.id.clone()),
        }
    }
    pub fn to_nodespan(&self, span: Span) -> NodeSpan {
        Spanned::new(Node::Value(Box::new(self.clone())), span)
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub id: String,
    pub vars: VarMap,
    pub structs: HashMap<String, Struct>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub block: BlockRef,
    pub args: Vec<String>,
}
impl Function {
    pub fn new(block: BlockSpan, args: Vec<String>) -> Self {
        Self {
            block: Box::new(block),
            args,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFunc {
    pub function: fn(VarMap, ValueStream) -> Value,
    pub arg_size: i16,
}
impl From<Function> for Value {
    fn from(x: Function) -> Self {
        Value::Function(x)
    }
}
impl From<Control> for Value {
    fn from(x: Control) -> Self {
        Value::Control(x)
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Null,
    Void,
    Num,
    Bool,
    Str,
    Function,
    Never,
    UserDefined(String),
}
impl Type {
    pub fn is_void(&self) -> bool {
        self == &Self::Void
    }
    pub fn is_numeric(&self) -> bool {
        match self {
            Self::Bool | Self::Num => return true,
            _ => return false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Value(Box<Value>),
    Block(BlockSpan),
    BinaryNode(BinaryNode),
    UnaryNode(UnaryNode),
    ResultNode(Box<NodeSpan>),
    ReturnNode(Box<NodeSpan>),
    BreakNode,
    ContinueNode,
    Declaration(Declaration),
    Assignment(Assignment),
    Variable(Variable),
    Call(Call),
    Branch(Branch),
    Loop(Loop),
    While(While),
    DoBlock(DoBlock),
    Construct(Construct),
    StructDef(StructDef),
    DontResult,
}
impl Node {
    pub fn to_spanned(&self, span: Span) -> NodeSpan {
        return NodeSpan::new(self.clone(), span);
    }
    pub fn can_result(&self) -> bool {
        match self.clone() {
            Self::Value(_)
            | Self::BinaryNode(_)
            | Self::UnaryNode(_)
            | Self::Variable(_)
            | Self::Call(_) => true,
            _ => false,
        }
    }
}

pub type NodeSpan = Spanned<Node>;
pub type NodeRef = Box<NodeSpan>;
impl NodeSpan {
    pub fn wrap_in_result(&self) -> Self {
        let value = self.clone();
        Spanned::new(Node::ResultNode(Box::new(value.clone())), value.span)
    }
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
    pub fn to_block(self) -> Block {
        Block {
            body: Box::new(vec![self]),
        }
    }
    pub fn to_feildspan(&self) -> Result<Spanned<Field>, ()> {
        match &self.unspanned {
            Node::Declaration(decl) => {
                return Ok(Spanned::new(Field::Declaration(decl.clone()), self.span))
            }
            Node::StructDef(def) => {
                return Ok(Spanned::new(Field::StructDef(def.clone()), self.span))
            }
            _ => {},
        }
        eprintln!("invalid node in struct feilds IMPROVE ME");
        return Err(());
    }
}
pub trait IntoBlock {
    fn to_block(self) -> Block;
    fn to_blockspan(self, span: Span) -> BlockSpan;
}
pub type NodeStream = Vec<NodeSpan>;
impl IntoBlock for NodeStream {
    fn to_block(self) -> Block {
        Block {
            body: Box::new(self),
        }
    }
    fn to_blockspan(self, span: Span) -> BlockSpan {
        Spanned::new(
            Block {
                body: Box::new(self),
            },
            span,
        )
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub body: Box<NodeStream>,
}
pub type BlockSpan = Spanned<Block>;
pub type BlockRef = Box<Spanned<Block>>;
impl BlockSpan {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

impl From<Control> for Node {
    fn from(x: Control) -> Self {
        Node::Value(Box::new(Value::Control(x)))
    }
}
impl From<Value> for Node {
    fn from(x: Value) -> Self {
        Node::Value(Box::new(x))
    }
}
macro_rules! nodes_from {
    ($($name:ident)*) => {
        $(
            impl ::core::convert::From<$name> for Node {
                fn from(node: $name) -> Self {
                    Self::$name(node)
                }
            }
            impl $name {
                pub fn to_nodespan(&self,span:Span) -> NodeSpan {
                    Spanned::new(Node::$name(self.clone()),span)
                }

            }
        )*
    }
}
nodes_from! { UnaryNode Construct StructDef DoBlock BinaryNode Call Variable Assignment Declaration Branch While Loop}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    ADD,
    SUBTRACT,
    DIVIDE,
    MULTIPLY,
    MODULO,
    AND,
    OR,
    ISEQUAL,
    ISDIFERENT,
    GREATER,
    LESSER,
    GREATER_EQUAL,
    LESSER_EQUAL,
    ACCESS,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryNode {
    pub kind: BinaryOp,
    pub left: NodeRef,
    pub right: NodeRef,
}
impl BinaryNode {
    pub fn is(&self, kind: &BinaryOp) -> bool {
        return self.kind.eq(kind);
    }
    pub fn isnt(&self, kind: &BinaryOp) -> bool {
        return self.kind.ne(kind);
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    NEGATIVE,
    NOT,
}
#[derive(Clone, Debug, PartialEq)]
pub struct UnaryNode {
    pub kind: UnaryOp,
    pub object: NodeRef,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub var_name: String,
    pub value: NodeRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub var_name: String,
    pub value: NodeRef,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Variable {
    pub name: String,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Call {
    pub callee: NodeRef,
    pub args: Box<NodeStream>,
}

#[derive(Clone, Debug, PartialEq)]

pub struct Construct {
    pub obj: String,
    pub params: Box<VarMap>,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Field {
    Declaration(Declaration),
    StructDef(StructDef),
}
impl Spanned<Field> {
    pub fn to_nodespan(&self) -> NodeSpan {
        match &self.unspanned {
            Field::Declaration(decl) => NodeSpan::new(Node::Declaration(decl.clone()), self.span),
            Field::StructDef(decl) => NodeSpan::new(Node::StructDef(decl.clone()), self.span),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<Spanned<Field>>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct StructAssign {
    pub obj: String,
    pub params: Box<VarMap>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct DoBlock {
    pub body: BlockRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub condition: NodeRef,
    pub if_block: BlockRef,
    pub else_block: Option<BlockRef>,
}
impl Branch {
    pub fn new_single(condition: NodeSpan, block: BlockSpan) -> Self {
        Self {
            condition: Box::new(condition),
            if_block: Box::new(block),
            else_block: None,
        }
    }
    pub fn new(condition: NodeSpan, if_block: BlockSpan, else_block: BlockSpan) -> Self {
        Self {
            condition: Box::new(condition),
            if_block: Box::new(if_block),
            else_block: Some(Box::new(else_block)),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Loop {
    pub proc: BlockRef,
}
#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub condition: NodeRef,
    pub proc: BlockRef,
}
pub type VarMap = HashMap<String, Value>;
#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub vars: VarMap,
    pub structs: HashMap<String, Struct>,
}

impl Scope {
    pub fn get_var(&self, var_name: String) -> Option<Value> {
        if let Some(var) = self.vars.get(&var_name) {
            return Some(var.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.get_var(var_name);
        }

        None
    }
    pub fn define(&mut self, var_name: String, val: Value) {
        if let Value::Struct(obj) = &val {
            self.structs.insert(var_name.clone(), obj.clone());
        }
        self.vars.insert(var_name, val);
    }
    pub fn new(parent: Option<Box<Scope>>, vars: VarMap, structs: HashMap<String, Struct>) -> Self {
        Scope {
            parent,
            vars,
            structs,
        }
    }
    pub fn travel(&mut self) {
        let Some(parent) = self.parent.clone() else {panic!()};
        let grandpa = parent.clone().parent;
        self.parent = grandpa;
        self.vars = parent.vars;
    }
    pub fn assign(&mut self, var_name: String, value: Value) -> Option<Value> {
        if let Some(var) = self.vars.get_mut(&var_name) {
            *var = value;
            return Some(var.clone());
        }
        if let Some(parent) = &mut self.parent {
            return parent.assign(var_name, value);
        }
        None
    }
}
