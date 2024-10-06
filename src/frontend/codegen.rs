use crate::frontend::nodes::*;
use crate::frontend::parser::*;
use crate::frontend::stacknodes::StackOp as Op;
use crate::frontend::stacknodes::*;
use crate::lang_errors::LangError;
use crate::spans::Spanned;
use crate::spans::*;
type GenRes<T> = Result<T, Spanned<GenErr>>;
pub enum GenErr {
    NonExistentVar(String),
    InvalidAssignment(String),
    Unspecified(String),
}
impl LangError for Spanned<GenErr> {
    fn msg(&self) -> String {
        match &self.item {
            GenErr::NonExistentVar(name) => {
                "Couldnt find variable with name: ".to_string() + name.as_str()
            }
            GenErr::InvalidAssignment(name) => {
                "Attempted to assign to non existent variable with name: ".to_string()
                    + name.as_str()
            }
            GenErr::Unspecified(msg) => msg.to_owned(),
        }
    }
}

pub struct IRgen {
    pub stack: Vec<Spanned<Op>>,
    idents: Vec<String>,
}
impl IRgen {
    pub fn new() -> Self {
        Self {
            stack: vec![],
            idents: vec![],
        }
    }
    pub fn generate(&mut self, input: &str) -> GenRes<&[Spanned<Op>]> {
        let mut parser = Parser::new(input);

        let (prog, _) = parser.parse().unwrap();

        for node in prog {
            self.node_gen(node)?;
        }
        return Ok(&self.stack);
    }
    fn push_val(&mut self, val: Value, span: impl SpanUtil) -> GenRes<()> {
        self.stack
            .push(StackOp::Push(val).to_spanned(span.take_span()));
        Ok(())
    }
    fn get_var(&mut self, name: &String, span: impl SpanUtil) -> GenRes<usize> {
        let Ok(index) = self.idents.binary_search(name) else {
            return Err(GenErr::NonExistentVar(name.to_owned()).to_spanned(span));
        };
        return Ok(index);
    }
    fn node_gen(&mut self, node: NodeSpan) -> GenRes<()> {
        let span = node.span;
        match node.item {
            Node::Float(num) => self.push_val(Value::Float(num), span),
            Node::Int(num) => self.push_val(Value::Int(num), span),
            Node::Bool(cond) => self.push_val(Value::Bool(cond), span),
            Node::Str(txt) => self.push_val(Value::Str(txt), span),
            Node::BinaryNode(expr) => {
                self.node_gen(expr.left.deref_item())?;
                self.node_gen(expr.right.deref_item())?;
                let op = StackOp::BinaryOp(expr.kind).to_spanned(span);
                self.stack.push(op);
                Ok(())
            }
            Node::UnaryNode(expr) => {
                self.node_gen(expr.target.deref_item())?;

                self.stack
                    .push(StackOp::UnaryOp(expr.kind).to_spanned(span));
                Ok(())
            }
            Node::Declaration(name, expr) => {
                self.node_gen(expr.deref_item())?;
                self.idents.push(name);
                Ok(())
            }
            Node::Assignment(request) => {
                let expr = request.value.deref_item();
                let target_span = request.target.span;
                match request.target.deref_item().item {
                    Node::Variable(name) => {
                        let index = self.get_var(&name, target_span)?;
                        self.node_gen(expr)?;
                        self.stack.push(StackOp::Store(index).to_spanned(span));
                        Ok(())
                    }
                    _ => todo!(),
                }
            }
            Node::Variable(name) => {
                let index = self.get_var(&name, span)?;
                self.stack.push(Op::Load(index).to_spanned(span));
                Ok(())
            }
            _ => {
                todo!()
            }
        }
    }
}
