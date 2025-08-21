mod error;
pub use error::GenErr;

use std::collections::HashMap;

use super::instructions::{OpCode as Op, *};

use crate::{
    frontend::nameres::resolved_nodes::{ResolvedNode as RNode, *},
    *,
};
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
    node_pool: NodePool,
}
impl IRgen {
    pub fn generate_expr(expr: ResolvedAstNode) -> Result<Vec<Op>> {
        let mut codegen = Self {
            node_pool: expr.1,
            ..Default::default()
        };
        codegen.node_gen(expr.0)?;
        Ok(codegen.stack)
    }

    pub fn generate(prog: ResolvedAst) -> Result<Vec<Op>> {
        let mut codegen = Self {
            node_pool: prog.1,
            ..Default::default()
        };
        codegen.gen_top_level(prog.0)?;
        Ok(codegen.stack)
    }
    pub fn gen_top_level(&mut self, prog: Vec<Spanned<ResItem>>) -> Result {
        for decl in prog {
            match decl.item {
                ResItem::Decl(decl) => {
                    self.node_gen(decl.expr)?;
                    self.add_op(Op::Store(decl.id));
                }
            }
        }
        Ok(())
    }
    fn push_val(&mut self, val: Value) -> Result {
        self.add_op(Op::Push(val));
        Ok(())
    }
    fn add_op(&mut self, op: Op) {
        self.stack.push(op);
    }
    fn gen_vardecl(&mut self, decl: ResolvedDecl) -> Result {
        self.node_gen(decl.expr)?;
        self.add_op(Op::Store(decl.id));
        Ok(())
    }
    fn gen_block(&mut self, block: Vec<RNodeRef>) -> Result {
        for node in block {
            self.node_gen(node)?;
        }
        Ok(())
    }
    fn gen_branch(
        &mut self,
        condition: RNodeRef,
        if_block: Vec<RNodeRef>,
        else_block: Option<Vec<RNodeRef>>,
    ) -> Result {
        self.node_gen(condition)?;
        let branch_op_index = self.stack.len();
        self.add_op(Op::Branch(0));
        self.gen_block(if_block)?;
        if let Some(else_block) = else_block {
            let goto_op_index = self.stack.len();
            self.add_op(Op::Goto(0));
            let else_start_pos = self.stack.len();
            let branch_offset = (else_start_pos as i16) - ((branch_op_index + 1) as i16);
            self.stack[branch_op_index] = Op::Branch(branch_offset);
            self.gen_block(else_block)?;
            let end_pos = self.stack.len();
            let goto_offset = (end_pos as i16) - ((goto_op_index + 1) as i16);
            self.stack[goto_op_index] = Op::Goto(goto_offset);
        } else {
            let end_pos = self.stack.len();
            let branch_offset = (end_pos as i16) - ((branch_op_index + 1) as i16);
            self.stack[branch_op_index] = Op::Branch(branch_offset);
        }

        Ok(())
    }
    fn node_gen(&mut self, node: RNodeRef) -> Result {
        let node = &self.node_pool[node.0];

        match node.item.clone() {
            RNode::Float(num) => self.push_val(Value::Float(num))?,
            RNode::Int(num) => self.push_val(Value::Int(num))?,
            RNode::Bool(cond) => self.push_val(Value::Bool(cond))?,
            RNode::Str(txt) => self.push_val(Value::String(txt.to_owned()))?,
            RNode::BinaryNode { left, right, kind } => {
                self.node_gen(left)?;
                self.node_gen(right)?;
                self.add_op(kind.clone().into());
            }
            RNode::UnaryNode(kind, expr) => {
                self.node_gen(expr)?;
                match kind {
                    UnaryOp::Negative => self.add_op(Op::Neg),
                    UnaryOp::Not => self.add_op(Op::Not),
                }
            }
            RNode::Decl(decl) => self.gen_vardecl(decl)?,
            RNode::Assignment { target, value } => self.gen_assignment(target, value)?,
            RNode::DoBlock(block) => self.gen_block(block)?,
            RNode::Variable(id) => {
                self.add_op(Op::Load(id));
            }
            RNode::Branch {
                condition,
                if_block,
                else_block,
            } => self.gen_branch(condition, if_block, else_block)?,
            _ => {
                todo!()
            }
        };
        Ok(())
    }
    fn gen_assignment(&mut self, target_ref: RNodeRef, value_ref: RNodeRef) -> Result {
        match self.node_pool[target_ref.0].item.clone() {
            RNode::Variable(id) => {
                self.node_gen(value_ref)?;
                self.add_op(Op::Store(id.clone()));
                Ok(())
            }
            RNode::Index { target, index } => todo!(),

            _ => todo!(),
        }
    }
}
