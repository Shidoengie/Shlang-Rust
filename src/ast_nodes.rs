use std::collections::*;

use crate::spans::*;
#[derive(Clone, Debug, PartialEq)]

pub enum Control {
    Return(Box<Value>, Type),
    Result(Box<Value>, Type),
    Break,
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
}
pub type TypedValue = (Value, Type);
pub type ValueStream = Vec<Value>;
impl Value {
    pub fn get_type(&self) -> Type {
        match *self {
            Value::Bool(_) => return Type::Bool,
            Value::BuiltinFunc(_) | Value::Function(_) => return Type::Function,
            Value::Null => return Type::Null,
            Value::Void => return Type::Void,
            Value::Str(_) => return Type::Str,
            Value::Num(_) => return Type::Num,
            Value::Control(_) => return Type::Never,
        }
    }
    pub fn to_nodespan(&self,span:Span)->NodeSpan{
        Spanned::new(Node::Value(Box::new(self.clone())),span)
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    block: NodeRef,
    args: Vec<String>,
}
impl Function{
    pub fn new(block:NodeSpan,args:Vec<String>)->Self{
        Self{
            block:Box::new(block),
            args
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFunc {
    function: fn(ValueStream) -> Value,
    arg_size: i16,
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
    Block(Block),
    DoBlock(DoBlock),
    BinaryNode(BinaryNode),
    UnaryNode(UnaryNode),
    ResultNode(Box<NodeSpan>),
    ReturnNode(Box<NodeSpan>),
    BreakNode,
    Declaration(Declaration),
    Assignment(Assignment),
    Variable(Variable),
    Call(Call),
    Branch(Branch),
    Loop(Loop),
    While(While),
}
impl Node {
    pub fn declaration(name: &str, value: NodeSpan) -> Self {
        return Node::Declaration(Declaration {
            var_name: String::from(name),
            value: Box::new(value),
        });
    }
    pub fn block(body: NodeStream) -> Self {
        return Node::Block(Block {
            body: Box::new(body),
        });
    }
    pub fn to_spanned(&self,span:Span)->NodeSpan{
        return NodeSpan::new(self.clone(), span);
    }
}
pub type NodeSpan = Spanned<Node>;
impl NodeSpan {
    pub fn wrap_in_result(&self)->Self{
        let value = self.clone();
        Spanned::new(Node::ResultNode(Box::new(value.clone())), value.span)
    }
}
pub type NodeStream = Vec<NodeSpan>;
pub type NodeRef = Box<NodeSpan>;


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
nodes_from! { UnaryNode DoBlock BinaryNode Call Variable Assignment Declaration Branch While Loop Block}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub body: Box<NodeStream>,
}
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
pub struct DoBlock {
    pub body: NodeRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub condition: NodeRef,
    pub if_block: NodeRef,
    pub else_block: Option<NodeRef>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Loop {
    pub proc: NodeRef,
}
#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub condition: NodeRef,
    pub proc: NodeRef,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub var_map: HashMap<String, Value>,
}

impl Scope {
    pub fn get_var(&self, var_name: String) -> Option<Value> {
        if let Some(var) = self.var_map.get(&var_name) {
            return Some(var.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.get_var(var_name);
        }

        None
    }
    pub fn define(&mut self, var_name: String, val: Value) {
        dbg!(self.var_map.insert(var_name, val));
        dbg!(&self.var_map);
    }
    pub fn new(parent: Option<Box<Scope>>, var_map: HashMap<String, Value>) -> Self {
        Scope { parent, var_map }
    }
    pub fn travel(&mut self) {
        let Some(parent) = self.parent.clone() else {panic!()};
        let grandpa = parent.clone().parent;
        self.parent = grandpa;
        self.var_map = parent.var_map;
    }
    pub fn assign(&mut self, var_name: String, value: Value) -> Option<Value> {
        if let Some(var) = self.var_map.get_mut(&var_name) {
            *var = value;
            return Some(var.clone());
        }
        if let Some(parent) = &mut self.parent {
            return parent.assign(var_name, value);
        }
        None
    }
}
