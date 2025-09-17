mod error;
use std::sync::Arc;

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
        let mut bytecode = Vec::new();
        codegen.node_gen(expr.0, &mut bytecode)?;
        Ok((bytecode, codegen.span_map))
    }

    /// Generates bytecode and a source map for a full program.
    pub fn generate(prog: ResolvedAst) -> Result<(Vec<Op>, SpanMap)> {
        let mut codegen = Self {
            node_pool: prog.1,
            ..Default::default()
        };
        let mut bytecode = Vec::new();
        codegen.gen_top_level(prog.0, &mut bytecode)?;
        Ok((bytecode, codegen.span_map))
    }

    fn gen_top_level(&mut self, prog: Vec<Spanned<ResItem>>, bytecode: &mut Vec<OpCode>) -> Result {
        for decl in prog {
            let decl_span = decl.span;
            if let ResItem::Decl(resolved_decl) = decl.item {
                self.gen_vardecl(resolved_decl, decl_span, bytecode)?;
            }
        }
        Ok(())
    }

    /// Atomically adds an opcode and its corresponding source span.
    fn add_op(&mut self, op: Op, span: Span, bytecode: &mut Vec<OpCode>) {
        let op_index = bytecode.len();
        bytecode.push(op);
        // This is safe because we always append, guaranteeing sorted and non-overlapping ranges.
        self.span_map.push_unchecked(op_index, op_index + 1, span);
    }

    fn push_val(&mut self, val: Value, span: Span, bytecode: &mut Vec<OpCode>) -> Result {
        self.add_op(Op::Push(val), span, bytecode);
        Ok(())
    }

    fn gen_vardecl(
        &mut self,
        decl: ResolvedDecl,
        span: Span,
        bytecode: &mut Vec<OpCode>,
    ) -> Result {
        self.node_gen(decl.expr, bytecode)?;
        self.add_op(Op::StoreLocal(decl.id), span, bytecode);
        Ok(())
    }

    fn gen_block(&mut self, block: Vec<RNodeRef>, bytecode: &mut Vec<OpCode>) -> Result<usize> {
        let mut num_locals = 0usize;
        for node in block {
            if matches!(&self.node_pool[node].item, &RNode::Decl(_)) {
                num_locals += 1
            }
            self.node_gen(node, bytecode)?;
        }
        Ok(num_locals)
    }

    fn gen_branch(
        &mut self,
        condition: RNodeRef,
        if_block: Vec<RNodeRef>,
        else_block: Option<Vec<RNodeRef>>,
        span: Span,
        bytecode: &mut Vec<OpCode>,
    ) -> Result {
        self.node_gen(condition, bytecode)?;
        let branch_op_index = bytecode.len();
        self.add_op(Op::Branch(0), span, bytecode);

        self.gen_block(if_block, bytecode)?;

        if let Some(else_block) = else_block {
            let goto_op_index = bytecode.len();
            self.add_op(Op::Goto(0), span, bytecode);

            let else_start_pos = bytecode.len();
            let branch_offset = (else_start_pos as i16) - ((branch_op_index + 1) as i16);
            bytecode[branch_op_index] = Op::Branch(branch_offset);

            self.gen_block(else_block, bytecode)?;

            let end_pos = bytecode.len();
            let goto_offset = (end_pos as i16) - ((goto_op_index + 1) as i16);
            bytecode[goto_op_index] = Op::Goto(goto_offset);
        } else {
            let end_pos = bytecode.len();
            let branch_offset = (end_pos as i16) - ((branch_op_index + 1) as i16);
            bytecode[branch_op_index] = Op::Branch(branch_offset);
        }

        Ok(())
    }

    fn node_gen(&mut self, node_ref: RNodeRef, bytecode: &mut Vec<OpCode>) -> Result {
        let node = self.node_pool[node_ref].clone();
        let span = node.span;

        match node.item {
            RNode::Null => self.push_val(Value::Null, span, bytecode)?,
            RNode::Float(num) => self.push_val(Value::Float(num), span, bytecode)?,
            RNode::Int(num) => self.push_val(Value::Int(num), span, bytecode)?,
            RNode::Bool(cond) => self.push_val(Value::Bool(cond), span, bytecode)?,
            RNode::Str(txt) => self.push_val(Value::String(txt), span, bytecode)?,

            RNode::BinaryNode { left, right, kind } => {
                self.node_gen(left, bytecode)?;
                self.node_gen(right, bytecode)?;
                self.add_op(kind.into(), span, bytecode);
            }
            RNode::UnaryNode(kind, expr) => {
                self.node_gen(expr, bytecode)?;
                let op = match kind {
                    UnaryOp::Negative => Op::Neg,
                    UnaryOp::Not => Op::Not,
                };
                self.add_op(op, span, bytecode);
            }
            RNode::Decl(decl) => self.gen_vardecl(decl, span, bytecode)?,
            RNode::Assignment { target, value } => {
                self.gen_assignment(target, value, span, bytecode)?
            }
            RNode::DoBlock(block) => {
                self.gen_block(block, bytecode)?;
            }
            RNode::Variable { id, is_global } => self.add_op(
                if is_global {
                    Op::LoadGlobal(id)
                } else {
                    Op::LoadLocal(id)
                },
                span,
                bytecode,
            ),
            RNode::Branch {
                condition,
                if_block,
                else_block,
            } => self.gen_branch(condition, if_block, else_block, span, bytecode)?,

            RNode::While { condition, block } => {
                let loop_start = bytecode.len();
                self.node_gen(condition, bytecode)?;

                let branch_op_index = bytecode.len();
                self.add_op(Op::Branch(0), span, bytecode);

                self.gen_block(block, bytecode)?;

                let jump_back_offset = (loop_start as i16) - ((bytecode.len() + 1) as i16);
                self.add_op(Op::Goto(jump_back_offset), span, bytecode);

                let loop_end = bytecode.len();
                let exit_loop_offset = (loop_end as i16) - ((branch_op_index + 1) as i16);
                bytecode[branch_op_index] = Op::Branch(exit_loop_offset);
            }
            RNode::FuncDef {
                captures,
                idents,
                block,
            } => {
                if captures {
                    todo!()
                }
                let mut func_code = Vec::new();
                let num_locals = self.gen_block(block, &mut func_code)? + idents.len();
                let param_count = idents.len();
                self.push_val(
                    Value::Function(Arc::new(Function {
                        proc: func_code,
                        num_locals,
                        param_count,
                    })),
                    span,
                    bytecode,
                )?;
            }
            RNode::Call { callee, args } => {
                let arg_len = args.len();
                if arg_len > u8::MAX.into() {
                    return Err(GenErr::TooManyArguments(arg_len).to_spanned(span));
                }
                for node in args {
                    self.node_gen(node, bytecode)?;
                }

                self.node_gen(callee, bytecode)?;
                bytecode.push(OpCode::Call(arg_len as u8))
            }
            RNode::ReturnNode(expr) => {
                self.node_gen(expr, bytecode)?;
                self.add_op(Op::Ret, span, bytecode);
            }

            _ => {
                let repr = self.node_pool.stringify_node(node_ref);
                todo!("{repr}");
            }
        };
        Ok(())
    }

    fn gen_assignment(
        &mut self,
        target_ref: RNodeRef,
        value_ref: RNodeRef,
        span: Span,
        bytecode: &mut Vec<OpCode>,
    ) -> Result {
        match self.node_pool[target_ref].item.clone() {
            RNode::Variable { id, is_global } => {
                self.node_gen(value_ref, bytecode)?;

                self.add_op(
                    if is_global {
                        Op::StoreGlobal(id)
                    } else {
                        Op::StoreLocal(id)
                    },
                    span,
                    bytecode,
                );

                Ok(())
            }
            _ => todo!(),
        }
    }
}
