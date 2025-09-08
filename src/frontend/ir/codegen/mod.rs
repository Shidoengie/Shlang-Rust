mod error;
pub use error::GenErr;

use super::instructions::{OpCode as Op, *};

use crate::{
    frontend::nameres::resolved_nodes::{ResolvedNode as RNode, *},
    spanmap::SpanMap,
    spans::{IntoSpanned, Span},
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
    span_map: SpanMap,
}

impl IRgen {
    /// Generates bytecode and a source map for a single expression.
    pub fn generate_expr(expr: ResolvedAstNode) -> Result<(Vec<Op>, SpanMap)> {
        let mut codegen = Self {
            node_pool: expr.1,
            ..Default::default()
        };
        codegen.node_gen(expr.0)?;
        Ok((codegen.stack, codegen.span_map))
    }

    /// Generates bytecode and a source map for a full program.
    pub fn generate(prog: ResolvedAst) -> Result<(Vec<Op>, SpanMap)> {
        let mut codegen = Self {
            node_pool: prog.1,
            ..Default::default()
        };
        codegen.gen_top_level(prog.0)?;
        Ok((codegen.stack, codegen.span_map))
    }

    fn gen_top_level(&mut self, prog: Vec<Spanned<ResItem>>) -> Result {
        for decl in prog {
            let decl_span = decl.span;
            if let ResItem::Decl(resolved_decl) = decl.item {
                self.gen_vardecl(resolved_decl, decl_span)?;
            }
        }
        Ok(())
    }

    /// Atomically adds an opcode and its corresponding source span.
    fn add_op(&mut self, op: Op, span: Span) {
        let op_index = self.stack.len();
        self.stack.push(op);
        // This is safe because we always append, guaranteeing sorted and non-overlapping ranges.
        self.span_map.push_unchecked(op_index, op_index + 1, span);
    }

    fn push_val(&mut self, val: Value, span: Span) -> Result {
        self.add_op(Op::Push(val), span);
        Ok(())
    }

    fn gen_vardecl(&mut self, decl: ResolvedDecl, span: Span) -> Result {
        self.node_gen(decl.expr)?;
        self.add_op(Op::Store(decl.id), span);
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
        span: Span,
    ) -> Result {
        self.node_gen(condition)?;
        let branch_op_index = self.stack.len();
        self.add_op(Op::Branch(0), span);

        self.gen_block(if_block)?;

        if let Some(else_block) = else_block {
            let goto_op_index = self.stack.len();
            self.add_op(Op::Goto(0), span);

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

    fn node_gen(&mut self, node_ref: RNodeRef) -> Result {
        let node = self.node_pool[node_ref].clone();
        let span = node.span;

        match node.item {
            RNode::Null => self.push_val(Value::Null, span)?,
            RNode::Float(num) => self.push_val(Value::Float(num), span)?,
            RNode::Int(num) => self.push_val(Value::Int(num), span)?,
            RNode::Bool(cond) => self.push_val(Value::Bool(cond), span)?,
            RNode::Str(txt) => self.push_val(Value::String(txt), span)?,

            RNode::BinaryNode { left, right, kind } => {
                self.node_gen(left)?;
                self.node_gen(right)?;
                self.add_op(kind.into(), span);
            }
            RNode::UnaryNode(kind, expr) => {
                self.node_gen(expr)?;
                let op = match kind {
                    UnaryOp::Negative => Op::Neg,
                    UnaryOp::Not => Op::Not,
                };
                self.add_op(op, span);
            }
            RNode::Decl(decl) => self.gen_vardecl(decl, span)?,
            RNode::Assignment { target, value } => self.gen_assignment(target, value, span)?,
            RNode::DoBlock(block) => self.gen_block(block)?,
            RNode::Variable(id) => self.add_op(Op::Load(id), span),
            RNode::Branch {
                condition,
                if_block,
                else_block,
            } => self.gen_branch(condition, if_block, else_block, span)?,

            RNode::While { condition, block } => {
                let loop_start = self.stack.len();
                self.node_gen(condition)?;

                let branch_op_index = self.stack.len();
                self.add_op(Op::Branch(0), span);

                self.gen_block(block)?;

                let jump_back_offset = (loop_start as i16) - ((self.stack.len() + 1) as i16);
                self.add_op(Op::Goto(jump_back_offset), span);

                let loop_end = self.stack.len();
                let exit_loop_offset = (loop_end as i16) - ((branch_op_index + 1) as i16);
                self.stack[branch_op_index] = Op::Branch(exit_loop_offset);
            }
            RNode::ReturnNode(expr) => {
                self.node_gen(expr)?;
                self.add_op(Op::Ret, span);
            }
            _ => {
                let repr = self.node_pool.stringify_node(node_ref);
                todo!("{repr}");
            }
        };
        Ok(())
    }

    fn gen_assignment(&mut self, target_ref: RNodeRef, value_ref: RNodeRef, span: Span) -> Result {
        match self.node_pool[target_ref].item.clone() {
            RNode::Variable(id) => {
                self.node_gen(value_ref)?;
                self.add_op(Op::Store(id), span);
                Ok(())
            }
            _ => todo!(),
        }
    }
}
