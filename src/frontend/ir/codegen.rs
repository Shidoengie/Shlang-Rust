use std::collections::HashMap;

use super::instructions::{OpCode as Op, *};

use crate::*;
use frontend::nodes::*;
use lang_errors::LangError;
use spans::Spanned;
use spans::*;
type GenRes<T = ()> = Result<T, Spanned<GenErr>>;
pub enum GenErr {
    Unspecified(String),
}
impl LangError for Spanned<GenErr> {
    fn msg(&self) -> String {
        match &self.item {
            GenErr::Unspecified(msg) => msg.to_owned(),
        }
    }
}
impl From<BinaryOp> for Op {
    fn from(value: BinaryOp) -> Self {
        use BinaryOp as Bin;
        match value {
            Bin::Add => Op::Add,
            Bin::Subtract => Op::Sub,
            Bin::Divide => Op::Div,
            Bin::Greater => Op::Greater,
            Bin::IsEqual => Op::Eq,
            Bin::IsDifferent => Op::NotEq,
            Bin::NullCoalescing => Op::NullCo,
            Bin::Or => Op::Or,
            Bin::Multiply => Op::Mult,
            Bin::Modulo => Op::Mod,
            Bin::GreaterOrEqual => Op::GreaterEq,
            Bin::And => Op::And,
            Bin::Lesser => Op::Lesser,
            Bin::LesserOrEqual => Op::LesserEq,
        }
    }
}
#[derive(Default)]
pub struct IRgen {
    pub stack: Vec<Op>,
    idents: HashMap<String, usize>,
}
impl IRgen {
    pub fn generate(&mut self, input: &str) -> GenRes<&[Op]> {
        let mut parser = frontend::Parser::new(input);

        let prog = parser.parse().unwrap();

        for node in prog {
            self.node_gen(node)?;
        }
        return Ok(&self.stack);
    }
    fn push_val(&mut self, val: Value) -> GenRes {
        self.add_op(Op::Push(val));
        Ok(())
    }
    fn get_var(&mut self, name: impl AsRef<str>, span: Span) -> GenRes<usize> {
        Ok(self.idents[name.as_ref()])
    }
    fn add_op(&mut self, op: Op) {
        self.stack.push(op);
    }
    fn node_gen(&mut self, node: Spanned<Node>) -> GenRes {
        let span = node.span;
        match node.item {
            Node::Float(num) => self.push_val(Value::Float(num)),
            Node::Int(num) => self.push_val(Value::Int(num)),
            Node::Bool(cond) => self.push_val(Value::Bool(cond)),
            Node::Str(txt) => self.push_val(Value::String(txt)),
            Node::BinaryNode(expr) => {
                self.node_gen(expr.left.deref_item())?;
                self.node_gen(expr.right.deref_item())?;
                self.add_op(expr.kind.into());
                Ok(())
            }
            Node::UnaryNode(expr) => {
                self.node_gen(expr.object.deref_item())?;
                match expr.kind {
                    UnaryOp::Negative => self.add_op(Op::Neg),
                    UnaryOp::Not => self.add_op(Op::Not),
                }
                Ok(())
            }
            Node::Declaration(name, expr) => {
                let index = self.idents.len() + 1;
                self.node_gen(expr.deref_item())?;
                self.idents.insert(name, index);
                self.add_op(Op::Store(index));
                Ok(())
            }
            Node::Assignment { target, value } => {
                let target_span = target.span;
                match target.deref_item().item {
                    Node::Variable(name) => {
                        let index = self.get_var(&name, target_span)?;
                        self.node_gen(value.deref_item())?;
                        self.add_op(Op::Store(index));
                        Ok(())
                    }
                    Node::Index { target, index } => todo!(),

                    _ => todo!(),
                }
            }
            Node::Variable(name) => {
                let index = self.get_var(&name, span)?;
                self.add_op(Op::Load(index));
                Ok(())
            }
            _ => {
                todo!()
            }
        }
    }
}
