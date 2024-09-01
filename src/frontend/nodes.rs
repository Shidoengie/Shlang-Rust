use slotmap::{new_key_type, SlotMap};

use crate::backend::scope::Scope;
use crate::spans::*;

use std::collections::*;
use std::fmt::{Debug, Display};
use std::mem;
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
    Ref(RefKey),
    List(Vec<Value>),
}
pub trait ValueRepr {
    fn repr(&self) -> String;
}
pub type TypedValue = (Value, Type);
fn list_repr(list: &[Value]) -> String {
    if list.is_empty() {
        return "[]".to_owned();
    }
    let mut out = String::new();
    for val in list {
        out += format!(",{}", val.repr()).as_str();
    }
    out = out.strip_prefix(',').unwrap().to_string();
    out = "[".to_string() + out.as_str() + "]";
    out
}
impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Bool(_) => Type::Bool,
            Value::BuiltinFunc(_) | Value::Function(_) => Type::Function,
            Value::Null => Type::Null,
            Value::Void => Type::Void,
            Value::Str(_) => Type::Str,
            Value::Num(_) => Type::Num,
            Value::List(_) => Type::List,
            Value::Struct(obj) => Type::Struct(obj.id.clone()),
            Value::Ref(_) => Type::Ref,
        }
    }

    pub fn is_void(&self) -> bool {
        self == &Self::Void
    }
    pub fn matches_typeof(&self, val: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(val)
    }
}
impl ValueRepr for Value {
    fn repr(&self) -> String {
        match self {
            Self::Num(num) => num.to_string(),
            Self::Bool(cond) => cond.to_string(),
            Self::Str(txt) => format!("\"{txt}\""),
            Self::Null => "null".to_string(),
            Self::Void => "void".to_string(),
            Self::List(list) => list_repr(list),
            Self::Struct(obj) => obj.repr(),
            Self::Function(func) => func.repr(),
            Self::Ref(id) => format!("ref {:?}", id),
            _ => "unnamed".to_string(),
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            Self::Num(num) => num.to_string(),
            Self::Bool(cond) => cond.to_string(),
            Self::Str(txt) => txt.to_string(),
            Self::Null => "null".to_string(),
            Self::Void => "void".to_string(),
            Self::List(list) => list_repr(list),
            Self::Struct(obj) => obj.repr(),
            Self::Function(func) => func.repr(),
            Self::Ref(id) => format!("ref{:?}", id),
            _ => "unnamed".to_string(),
        };
        write!(f, "{out}")
    }
}
impl IntoNodespan for Value {
    fn to_nodespan(self, span: Span) -> NodeSpan {
        Spanned::new(Node::Value(self), span)
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub id: Option<String>,
    pub env: Scope,
}
impl ValueRepr for Struct {
    fn repr(&self) -> String {
        let mut buffer = if let Some(name) = &self.id {
            name.to_owned()
        } else {
            String::new()
        };
        buffer += "{ ";
        let vars = &self.env.vars;
        for (k, v) in vars.iter() {
            buffer += k;
            buffer += ":";
            buffer += &v.repr();
            if vars.values().last().unwrap() != v {
                buffer += ", "
            }
        }
        buffer += " }";
        buffer
    }
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
impl ValueRepr for Function {
    fn repr(&self) -> String {
        let mut buffer = String::from("func(");
        for i in &self.args {
            buffer += i;
            if self.args.last().unwrap() != i {
                buffer += ", "
            }
        }
        buffer += ") ";
        buffer
    }
}
impl From<Function> for Value {
    fn from(x: Function) -> Self {
        Value::Function(x)
    }
}
new_key_type! {
    pub struct RefKey;
}

type FuncPtr = fn(&mut Scope, Vec<Value>, &mut SlotMap<RefKey, Value>) -> Value;
#[derive(Clone)]
pub struct BuiltinFunc {
    pub function: FuncPtr,
    pub arg_size: i16,
    pub id: String,
}
impl Debug for BuiltinFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFunc")
            .field("id", &self.id)
            .field("arg_size", &self.arg_size)
            .finish()
    }
}
impl PartialEq for BuiltinFunc {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl BuiltinFunc {
    pub fn new(id: String, function: FuncPtr, arg_size: i16) -> Self {
        Self {
            function,
            arg_size,
            id,
        }
    }
}
impl From<BuiltinFunc> for Value {
    fn from(x: BuiltinFunc) -> Self {
        Value::BuiltinFunc(x)
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
    List,
    Struct(Option<String>),
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
            Self::Struct(Some(id)) => id,
            Self::Struct(None) => "struct",
            Self::Ref => "ref",
        };
        write!(f, "{txt}")
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
    Index {
        target: Box<NodeSpan>,
        index: Box<NodeSpan>,
    },
    ListLit(Vec<NodeSpan>),
    Call(Call),

    Branch(Branch),
    Loop(NodeStream),
    While(While),
    ForLoop(ForLoop),
    DoBlock(NodeStream),
    Constructor(Constructor),
    ConstuctorFunc {
        name: String,
        args: Vec<NodeSpan>,
    },
    StructDef(StructDef),
    FieldAccess(FieldAccess),
    DontResult,
}
impl Node {
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
nodes_from! { UnaryNode Constructor StructDef  FieldAccess BinaryNode Call Assignment Declaration Branch While ForLoop}

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
    pub args: NodeStream,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Field {
    Declaration(Declaration),
    StructDef(StructDef),
}
impl Spanned<Field> {
    pub fn to_node(&self) -> NodeSpan {
        match &self.item {
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
#[derive(Clone, Debug, PartialEq)]
pub struct ForLoop {
    pub ident: String,
    pub list: Box<NodeSpan>,
    pub proc: NodeStream,
}
pub type VarMap = HashMap<String, Value>;
