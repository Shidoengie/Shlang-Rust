use std::collections::*;
use std::rc::Rc;
#[derive(Clone, Debug, PartialEq)]
pub enum Control {
    Return(Node),
    Break,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    NoneType,
    Control(Control),
    Num(f64),
    Bool(bool),
    Str(String),
    Function(Function),
    BuiltinFunc(BuiltinFunc),
}
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    block: Box<Block>,
    args: Vec<String>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFunc {
    funcPointer: fn(Vec<Value>) -> Value,
    argSize: i16,
}
type ValueStream = Vec<Value>;

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Value(Box<Value>),
    Block(Block),
    DoBlock(DoBlock),
    BinaryNode(BinaryNode),
    UnaryNode(UnaryNode),
    Declaration(Declaration),
    Assignment(Assignment),
    Variable(Variable),
    Call(Call),
    Branch(Branch),
    Loop(Loop),
    While(While),
}

impl Node {
    pub fn declaration(name: &str, value: Node) -> Self {
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
}
impl From<Function> for Value {
    fn from(x: Function) -> Self {
        Value::Function(x)
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
        )*
    }
}
nodes_from! { UnaryNode DoBlock BinaryNode Call Variable Assignment Declaration Branch While Loop Block}
pub type NodeStream = Vec<Node>;
pub type NodeRef = Box<Node>;

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub body: Box<NodeStream>,
}
pub type BlockRef = Box<Block>;
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
    POSITIVE,
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

impl Declaration {
    fn new(var_name: String, v: impl Into<NodeRef>) -> Self {
        Self {
            var_name,
            value: v.into(),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub var_name: String,
    pub value: NodeRef,
}
impl Assignment {
    fn new(var_name: String, v: impl Into<NodeRef>) -> Self {
        Self {
            var_name,
            value: v.into(),
        }
    }
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
    pub body: Box<NodeStream>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub condition: NodeRef,
    pub if_block: BlockRef,
    pub else_block: Option<BlockRef>,
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
        self.var_map.insert(var_name, val);
    }
    fn assign(&mut self, var_name: String, value: Value) -> Option<Value> {
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
