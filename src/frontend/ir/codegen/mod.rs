mod error;
pub use error::GenErr;

use std::collections::HashMap;

use super::instructions::{OpCode as Op, *};

use crate::*;
use frontend::ast::nodes::*;
use spans::Spanned;

pub type Result<T = ()> = std::result::Result<T, Spanned<GenErr>>;
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
    stack: Vec<Op>,
    idents: HashMap<String, usize>,
}
impl IRgen {
    pub fn generate_expr(expr: NodeSpan) -> Result<Vec<Op>> {
        let mut codegen = Self::default();
        codegen.node_gen(expr)?;
        Ok(codegen.stack)
    }

    fn gen_globals(&mut self, globals: &[DeclType]) {
        for decl in globals {
            match decl {
                DeclType::VarDecl(decl) => self.insert_ident(decl.name.to_owned()),
            };
        }
    }
    pub fn generate(prog: Vec<DeclType>) -> Result<Vec<Op>> {
        let mut codegen = Self::default();
        codegen.gen_top_level(prog)?;
        Ok(codegen.stack)
    }
    pub fn gen_top_level(&mut self, prog: Vec<DeclType>) -> Result {
        self.gen_globals(&prog);
        for decl in prog {
            match decl {
                DeclType::VarDecl(decl) => {
                    self.node_gen(decl.expr.deref_item())?;
                    self.add_op(Op::Store(self.idents[&decl.name]));
                }
            }
        }
        Ok(())
    }
    fn push_val(&mut self, val: Value) -> Result {
        self.add_op(Op::Push(val));
        Ok(())
    }
    fn get_var(&mut self, name: impl AsRef<str>) -> Result<usize> {
        Ok(self.idents[name.as_ref()])
    }
    fn add_op(&mut self, op: Op) {
        self.stack.push(op);
    }
    fn insert_ident(&mut self, ident: String) -> usize {
        let index = self.idents.len() + 1;
        self.idents.insert(ident, index);
        index
    }
    fn gen_vardecl(&mut self, decl: VarDecl) -> Result {
        self.node_gen(decl.expr.deref_item())?;
        let index = self.insert_ident(decl.name);
        self.add_op(Op::Store(index));
        Ok(())
    }
    fn gen_block(&mut self, block: Vec<NodeSpan>) -> Result {
        for node in block {
            self.node_gen(node)?;
        }
        Ok(())
    }
    fn node_gen(&mut self, node: NodeSpan) -> Result {
        let span = node.span;
        match node.item {
            Node::Float(num) => self.push_val(Value::Float(num))?,
            Node::Int(num) => self.push_val(Value::Int(num))?,
            Node::Bool(cond) => self.push_val(Value::Bool(cond))?,
            Node::Str(txt) => self.push_val(Value::String(txt))?,
            Node::BinaryNode(expr) => {
                self.node_gen(expr.left.deref_item())?;
                self.node_gen(expr.right.deref_item())?;
                self.add_op(expr.kind.into());
            }
            Node::UnaryNode(expr) => {
                self.node_gen(expr.target.deref_item())?;
                match expr.kind {
                    UnaryOp::Negative => self.add_op(Op::Neg),
                    UnaryOp::Not => self.add_op(Op::Not),
                }
            }
            Node::VarDecl(decl) => self.gen_vardecl(decl)?,
            Node::Assignment { target, value } => self.gen_assignment(target, value)?,
            Node::DoBlock(block) => self.gen_block(block)?,
            Node::Variable(name) => {
                let index = self.get_var(&name)?;
                self.add_op(Op::Load(index));
            }
            _ => {
                todo!()
            }
        };
        Ok(())
    }
    fn gen_assignment(&mut self, target: Spanned<Box<Node>>, value: Spanned<Box<Node>>) -> Result {
        let target_span = target.span;
        match target.deref_item().item {
            Node::Variable(name) => {
                let index = self.get_var(&name)?;
                self.node_gen(value.deref_item())?;
                self.add_op(Op::Store(index));
                Ok(())
            }
            Node::Index { target, index } => todo!(),

            _ => todo!(),
        }
    }
}
