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
#[derive(Debug)]
pub struct Bytecode {
    pub ops: Vec<Op>,
    pub span_map: SpanMap,
    pub global_count: usize,
    pub local_count: usize,
}

/// `IRgen` is responsible for traversing the Resolved AST (`RNode`) and
/// generating a linear stream of virtual machine instructions (`OpCode`).
#[derive(Default)]
pub struct IRgen {
    node_pool: NodePool,
    span_map: SpanMap,
}

impl IRgen {
    /// Generates bytecode and a source map for a single expression.
    pub fn generate_expr(expr: ResolvedAstNode) -> Result<Bytecode> {
        let mut codegen = Self {
            node_pool: expr.pool,
            ..Default::default()
        };
        let mut bytecode = Vec::new();
        codegen.node_gen(expr.node, &mut bytecode)?;

        Ok(Bytecode {
            ops: bytecode,
            span_map: codegen.span_map,
            global_count: expr.global_count,
            local_count: expr.local_count,
        })
    }

    /// Generates bytecode and a source map for a full program.
    pub fn generate(prog: ResolvedAst) -> Result<Bytecode> {
        let mut codegen = Self {
            node_pool: prog.pool,
            ..Default::default()
        };
        let mut bytecode = Vec::new();
        codegen.gen_top_level(prog.proc, &mut bytecode)?;
        Ok(Bytecode {
            ops: bytecode,
            span_map: codegen.span_map,
            global_count: prog.global_count,
            local_count: prog.local_count,
        })
    }

    fn gen_top_level(&mut self, prog: Vec<Spanned<ResItem>>, bytecode: &mut Vec<OpCode>) -> Result {
        for decl in prog {
            let decl_span = decl.span;
            //This isnt a let else or if let because ResItem will have more variants in the future
            match decl.item {
                ResItem::Decl(resolved_decl) => {
                    self.gen_vardecl(resolved_decl, decl_span, bytecode)?;
                }
            }
        }
        Ok(())
    }

    /// Associates an opcode with its source span for debugging and error reporting.
    fn add_op(&mut self, op: Op, span: Span, bytecode: &mut Vec<OpCode>) {
        let op_index = bytecode.len();
        bytecode.push(op);
        // This is safe because we only ever append, which maintains the sorted,
        // non-overlapping invariant required by SpanMap.
        self.span_map.push_unchecked(op_index, op_index + 1, span);
    }

    /// Helper to generate a `Push` instruction for a literal value.
    fn push_val(&mut self, val: Value, span: Span, bytecode: &mut Vec<OpCode>) -> Result {
        self.add_op(Op::Push(val), span, bytecode);
        Ok(())
    }

    /// Dispatches bytecode generation to a specific function based on the node's type.
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
                self.gen_binary(left, right, kind, span, bytecode)?
            }
            RNode::UnaryNode(kind, expr) => self.gen_unary(kind, expr, span, bytecode)?,
            RNode::Decl(decl) => self.gen_vardecl(decl, span, bytecode)?,
            RNode::Assignment { target, value } => {
                self.gen_assignment(target, value, span, bytecode)?
            }
            RNode::DoBlock(block) => {
                self.gen_block(block, bytecode)?;
            }
            RNode::Variable { id, is_global } => {
                self.gen_variable_load(id, is_global, span, bytecode)?
            }
            RNode::Branch {
                condition,
                if_block,
                else_block,
            } => self.gen_branch(condition, if_block, else_block, span, bytecode)?,
            RNode::While { condition, block } => {
                self.gen_while(condition, block, span, bytecode)?
            }
            RNode::FuncLit(func) => self.gen_func_def(func, span, bytecode)?,
            RNode::Call { callee, args } => self.gen_call(callee, args, span, bytecode)?,
            RNode::ReturnNode(expr) => self.gen_return(expr, span, bytecode)?,
            _ => {
                let repr = self.node_pool.stringify_node(node_ref);
                todo!("{repr}");
            }
        };
        Ok(())
    }

    /// Generates `left`, `right`, then the `op` to act on them.
    fn gen_binary(
        &mut self,
        left: RNodeRef,
        right: RNodeRef,
        kind: BinaryOp,
        span: Span,
        bytecode: &mut Vec<OpCode>,
    ) -> Result {
        self.node_gen(left, bytecode)?;
        self.node_gen(right, bytecode)?;
        self.add_op(kind.into(), span, bytecode);
        Ok(())
    }

    /// Generates `expr`, then the `op` to act on it.
    fn gen_unary(
        &mut self,
        kind: UnaryOp,
        expr: RNodeRef,
        span: Span,
        bytecode: &mut Vec<OpCode>,
    ) -> Result {
        self.node_gen(expr, bytecode)?;
        let op = match kind {
            UnaryOp::Negative => Op::Neg,
            UnaryOp::Not => Op::Not,
        };
        self.add_op(op, span, bytecode);
        Ok(())
    }

    /// Generates the expression's value, then stores it in a local.
    fn gen_vardecl(
        &mut self,
        decl: ResolvedDecl,
        span: Span,
        bytecode: &mut Vec<OpCode>,
    ) -> Result {
        self.node_gen(decl.expr, bytecode)?;
        if decl.is_global {
            self.add_op(Op::StoreGlobal(decl.id), span, bytecode);
        } else {
            self.add_op(Op::StoreLocal(decl.id), span, bytecode);
        }
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

    /// Generates an instruction to load a variable's value onto the stack.
    fn gen_variable_load(
        &mut self,
        id: usize,
        is_global: bool,
        span: Span,
        bytecode: &mut Vec<OpCode>,
    ) -> Result {
        self.add_op(
            if is_global {
                Op::LoadGlobal(id)
            } else {
                Op::LoadLocal(id)
            },
            span,
            bytecode,
        );
        Ok(())
    }

    /// Generates code for a sequence of statements and returns the number of new locals.
    fn gen_block(&mut self, block: Vec<RNodeRef>, bytecode: &mut Vec<OpCode>) -> Result<usize> {
        let mut local_count = 0usize;
        for node in block {
            if matches!(&self.node_pool[node].item, &RNode::Decl(_)) {
                local_count += 1
            }
            self.node_gen(node, bytecode)?;
        }
        Ok(local_count)
    }

    /// Generates conditional logic, using placeholder jumps that are patched later.
    fn gen_branch(
        &mut self,
        condition: RNodeRef,
        if_block: Vec<RNodeRef>,
        else_block: Option<Vec<RNodeRef>>,
        span: Span,
        bytecode: &mut Vec<OpCode>,
    ) -> Result {
        self.node_gen(condition, bytecode)?;
        // Emit a conditional jump to be patched later. The offset is unknown until
        // we know the size of the 'if' block.
        let branch_op_index = bytecode.len();
        self.add_op(Op::Branch(0), span, bytecode);

        self.gen_block(if_block, bytecode)?;

        if let Some(else_block) = else_block {
            // Unconditionally jump over the 'else' block after the 'if' block executes.
            let goto_op_index = bytecode.len();
            self.add_op(Op::Goto(0), span, bytecode);

            // Now we know where the 'else' block starts, so we can patch the first jump.
            let else_start_pos = bytecode.len();
            let branch_offset = (else_start_pos as i32) - ((branch_op_index + 1) as i32);
            bytecode[branch_op_index] = Op::Branch(branch_offset);

            self.gen_block(else_block, bytecode)?;

            // Now we know where the 'if-else' construct ends, so patch the 'goto'.
            let end_pos = bytecode.len();
            let goto_offset = (end_pos as i32) - ((goto_op_index + 1) as i32);
            bytecode[goto_op_index] = Op::Goto(goto_offset);
        } else {
            // No 'else' block, so just patch the initial jump to skip the 'if' body.
            let end_pos = bytecode.len();
            let branch_offset = (end_pos as i32) - ((branch_op_index + 1) as i32);
            bytecode[branch_op_index] = Op::Branch(branch_offset);
        }

        Ok(())
    }

    /// Generates a loop with a conditional exit and a jump back to the start.
    fn gen_while(
        &mut self,
        condition: RNodeRef,
        block: Vec<RNodeRef>,
        span: Span,
        bytecode: &mut Vec<OpCode>,
    ) -> Result {
        let loop_start = bytecode.len();
        self.node_gen(condition, bytecode)?;

        // Jump out of the loop if the condition is false. The offset is patched later.
        let branch_op_index = bytecode.len();
        self.add_op(Op::Branch(0), span, bytecode);

        self.gen_block(block, bytecode)?;

        // Unconditionally jump back to the condition check.
        let jump_back_offset = (loop_start as i32) - ((bytecode.len() + 1) as i32);
        self.add_op(Op::Goto(jump_back_offset), span, bytecode);

        // Now we know the loop's end, so we can patch the exit jump.
        let loop_end = bytecode.len();
        let exit_loop_offset = (loop_end as i32) - ((branch_op_index + 1) as i32);
        bytecode[branch_op_index] = Op::Branch(exit_loop_offset);
        Ok(())
    }

    /// Compiles a function's body and wraps it in a callable `Function` value.
    fn gen_func_def(&mut self, func: FuncLit, span: Span, bytecode: &mut Vec<OpCode>) -> Result {
        if func.captures {
            todo!()
        }

        // The function definition is an expression. We must jump over its body
        // during sequential execution, as it's only run when called.
        let jump_over_index = bytecode.len();
        self.add_op(Op::Goto(0), span, bytecode); // Placeholder offset

        // Compile the function body to find its size and content.
        let mut func_code = Vec::new();
        self.gen_block(func.block, &mut func_code)?;
        let param_count = func.idents.len();
        let local_count = param_count + func.local_count;

        let address = bytecode.len();
        bytecode.append(&mut func_code);

        // Now that we know the size of the function body, patch the initial jump.
        let jump_offset = (bytecode.len() as i32) - ((jump_over_index + 1) as i32);
        bytecode[jump_over_index] = Op::Goto(jump_offset);

        // Finally, push the function object itself onto the stack. This is the
        // result of the function definition expression.
        self.push_val(
            Value::Function(Arc::new(Function {
                address,
                local_count,
                param_count,
            })),
            span,
            bytecode,
        )?;
        Ok(())
    }

    /// Generates code to evaluate arguments, then the callee, then call.
    fn gen_call(
        &mut self,
        callee: RNodeRef,
        args: Vec<RNodeRef>,
        span: Span,
        bytecode: &mut Vec<OpCode>,
    ) -> Result {
        let arg_len = args.len();
        if arg_len > u8::MAX.into() {
            return Err(GenErr::TooManyArguments(arg_len).to_spanned(span));
        }
        // The calling convention requires arguments to be pushed onto the stack first.
        for node in args {
            self.node_gen(node, bytecode)?;
        }
        // Then, the function to be called is pushed.
        self.node_gen(callee, bytecode)?;
        self.add_op(OpCode::Call(arg_len as u8), span, bytecode);
        Ok(())
    }

    /// Generates the return value, then the `Ret` instruction.
    fn gen_return(&mut self, expr: RNodeRef, span: Span, bytecode: &mut Vec<OpCode>) -> Result {
        self.node_gen(expr, bytecode)?;
        self.add_op(Op::Ret, span, bytecode);
        Ok(())
    }
}
