
use std::collections::*;
use std::rc::Rc;
#[derive(Clone)]
pub enum Control {
    Return(Node),
    Break
}
#[derive(Clone)]
pub enum Value{
    NoneType,
    Control(Control),
    Num(f32),
    Bool(bool),
    Str(String),
    Function(Function),
    BuiltinFunc(BuiltinFunc)
}
#[derive(Clone)]
struct Function {
    block:Box<Block>,
    args:Vec<String>
}
#[derive(Clone)]
struct BuiltinFunc{
    funcPointer:fn(Vec<Value>) -> Value,
    argSize:i16
}
type ValueStream = Vec<Value>;
#[derive(Clone)]
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
#[derive(Clone)]
pub enum UnaryOp{
    NEGATIVE,
	POSITIVE,
	NOT,
}
#[derive(Clone)]
pub enum Node {
    Value(Box<Value>),
    Block(Block),
    BinaryNode(BinaryNode),
    UnaryNode(UnaryNode),
    Declaration(Declaration),
    Assignment(Assignment),
    Variable(Variable),
    Call(Call),
    
}
type NodeStream = Vec<Node>;
type NodeRef = Box<Node>;
#[derive(Clone)]
struct Block {
    body:NodeStream
}
type BlockRef = Box<Block>;
#[derive(Clone)]
struct BinaryNode {
    Type:BinaryOp,
    left:NodeRef,
    right:NodeRef
}
#[derive(Clone)]
struct UnaryNode {
    Type:UnaryOp,
    object:NodeRef,
}
#[derive(Clone)]
struct Declaration{
    var_name:String,
    value:NodeRef
}
#[derive(Clone)]
struct Assignment{
    var_name:String,
    value:NodeRef
}
#[derive(Clone)]
struct Variable{
    name:String
}
#[derive(Clone)]
struct Call{
    callee:Variable,
    args:Rc<NodeStream>,
}
#[derive(Clone)]
struct BranchNode{
    condition:NodeRef,
    if_block:BlockRef,
    else_block:NodeRef
}
#[derive(Clone)]
struct LoopNode{
    proc:BlockRef,
}
#[derive(Clone)]
struct WhileNode{
    condition:NodeRef,
    proc:BlockRef,
}
#[derive(Clone)]
struct Scope {
    parent:Option<Box<Scope>>,
    var_map:HashMap<String,Value>
}
impl Scope {
pub fn get_var(&self,var_name:String) -> Option<Value>{
    if let Some(var) = self.var_map.get(&var_name) {
      return Some(var.clone());
    }    
    if let Some(parent) = &self.parent {
      return parent.get_var(var_name);
    }
    
    None
}
pub fn define(&mut self,var_name:String,val:Value){
    self.var_map.insert(var_name, val);
}
fn assign(&mut self, var_name: String, value: Value) -> Option<Value>{
    if let Some(var) = self.var_map.get_mut(&var_name) {
        *var = value;
        return Some(var.clone());
    }    
    if let Some(parent) = &mut self.parent {
        return parent.assign(var_name,value);
    }
    None
}
}