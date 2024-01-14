use std::collections::*;

use crate::spans::*;
pub trait TypeIdent {
    fn is_void(&self) -> bool;
    fn is_numeric(&self) -> bool;
}
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Void,
    Num(f64),
    Bool(bool),
    Str(String),
    Function(Function),
    BuiltinFunc(BuiltinFunc),
    Struct(Struct),
    StructRef(u32),
}

pub type TypedValue = (Value, Type);

pub type ValueStream = Vec<Value>;
impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Bool(_) => Type::Bool,
            Value::BuiltinFunc(_) | Value::Function(_) => Type::Function,
            Value::Null => Type::Null,
            Value::Void => Type::Void,
            Value::Str(_) => Type::Str,
            Value::Num(_) => Type::Num,
            Value::Struct(s) => Type::UserDefined(s.id.clone()),
            Value::StructRef(id) => Type::Ref(*id),
        }
    }

    pub fn matches_typeof(&self, val: &Self) -> bool {
        self.get_type() == val.get_type()
    }
}
impl TypeIdent for Value {
    fn is_void(&self) -> bool {
        self.get_type().is_void()
    }
    fn is_numeric(&self) -> bool {
        self.get_type().is_numeric()
    }
}
impl IntoNodespan for Value {
    fn to_nodespan(self, span: Span) -> NodeSpan {
        Spanned::new(Node::Value(self), span)
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub id: String,
    pub env: Scope,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub block: NodeStream,
    pub args: Vec<String>,
}
impl Function {
    pub fn new(block: NodeStream, args: Vec<String>) -> Self {
        Self { block, args }
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

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Null,
    Void,
    Num,
    Bool,
    Str,
    Function,
    Ref(u32),
    UserDefined(String),
}
impl TypeIdent for Type {
    fn is_void(&self) -> bool {
        self == &Self::Void
    }
    fn is_numeric(&self) -> bool {
        matches!(self, Self::Bool | Self::Num)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Value(Value),
    BinaryNode(BinaryNode),
    UnaryNode(UnaryNode),
    ResultNode(Box<NodeSpan>),
    ReturnNode(Box<NodeSpan>),
    BreakNode,
    ContinueNode,
    Declaration(Declaration),
    Assignment(Assignment),
    Variable(String),
    Call(Call),
    Branch(Branch),
    Loop(NodeStream),
    While(While),
    DoBlock(NodeStream),
    Constructor(Constructor),
    StructDef(StructDef),
    FieldAccess(FieldAccess),
    DontResult,
}
impl Node {
    pub fn to_spanned(&self, span: Span) -> NodeSpan {
        NodeSpan::new(self.clone(), span)
    }
    pub fn can_result(&self) -> bool {
        matches!(
            self.clone(),
            Self::Value(_)
                | Self::BinaryNode(_)
                | Self::UnaryNode(_)
                | Self::Variable(_)
                | Self::Call(_)
        )
    }
}

pub type NodeSpan = Spanned<Node>;
pub type NodeRef = Box<Spanned<Node>>;
impl NodeSpan {
    pub fn wrap_in_result(&self) -> Self {
        let value = self.clone();
        Spanned::new(Node::ResultNode(Box::new(value.clone())), value.span)
    }
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}
pub trait IntoNodespan {
    fn to_nodespan(self, span: Span) -> NodeSpan;
}

pub type NodeStream = Vec<Spanned<Node>>;

impl From<Value> for Node {
    fn from(x: Value) -> Self {
        Node::Value(x)
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
            impl IntoNodespan for $name {
                fn to_nodespan(self,span:Span) -> NodeSpan {
                    Spanned::new(Node::$name(self.clone()),span)
                }

            }
        )*
    }
}
nodes_from! { UnaryNode Constructor StructDef  FieldAccess BinaryNode Call Assignment Declaration Branch While}

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
    pub target: NodeRef,
    pub value: NodeRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Call {
    pub callee: NodeRef,
    pub args: Box<NodeStream>,
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
pub struct FieldAccess {
    pub target: NodeRef,
    pub requested: NodeRef,
}
#[derive(Clone, Debug, PartialEq)]
pub struct StructDef {
    pub name: String,
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
            condition: Box::new(condition),
            if_block: block,
            else_block: None,
        }
    }
    pub fn new(condition: NodeSpan, if_block: NodeStream, else_block: NodeStream) -> Self {
        Self {
            condition: Box::new(condition),
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
pub type VarMap = HashMap<String, Value>;
#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub vars: HashMap<String, Value>,
    pub structs: HashMap<String, Struct>,
}

impl Scope {
    pub fn get_var(&self, var_name: impl AsRef<str>) -> Option<Value> {
        if let Some(var) = self.vars.get(var_name.as_ref()) {
            return Some(var.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.get_var(var_name);
        }
        None
    }
    pub fn get_struct(&self, struct_name: &String) -> Option<Struct> {
        if let Some(obj) = self.structs.get(struct_name) {
            return Some(obj.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.get_struct(struct_name);
        }
        None
    }
    pub fn define(&mut self, var_name: String, val: Value) {
        if let Value::Struct(obj) = &val {
            self.structs.insert(var_name.clone(), obj.clone());
        }
        self.vars.insert(var_name, val);
    }
    pub fn new(
        parent: Option<Box<Scope>>,
        vars: HashMap<String, Value>,
        structs: HashMap<String, Struct>,
    ) -> Self {
        Scope {
            parent,
            vars,
            structs,
        }
    }
    pub fn new_child_in(parent: Scope) -> Self {
        Scope {
            parent: Some(Box::new(parent)),
            vars: HashMap::from([]),
            structs: HashMap::from([]),
        }
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
