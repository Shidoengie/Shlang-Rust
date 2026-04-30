mod error;
use std::fmt::{Debug, Display, format};

pub use error::GenErr;

use super::instructions::{IrNode as Op, *};

use crate::{
    frontend::{
        nameres::resolved_nodes::{ResolvedNode as RNode, *},
        opkind::*,
    },
    spanmap::SpanMap,
    spans::{IntoSpanned, Span},
    utils::compact_iter_debug,
    *,
};
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
pub struct Ir {
    pub ops: Vec<Op>,
    pub span_map: SpanMap,
    pub global_count: usize,
    pub local_count: usize,
}
impl Debug for Ir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ir")
            .field("span_map", &self.span_map)
            .field("global_count", &self.global_count)
            .finish()?;
        write!(f, " = ")?;
        compact_iter_debug(f, self.ops.iter())
    }
}
impl Display for Ir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ops.is_empty() {
            write!(f, "0 | ")?;
            return Ok(());
        }
        for (index, op) in self.ops.iter().enumerate() {
            writeln!(f, "{pos} | {op}", pos = format!("{index:<2}"),)?;
        }
        Ok(())
    }
}
/// `IRgen` is responsible for traversing the Resolved AST (`RNode`) and
/// generating a linear stream of virtual machine instructions (`OpCode`).
#[derive(Default)]
pub struct IRgen {
    span_map: SpanMap,
    label_counter: usize,
    loop_stack: Vec<usize>,
}

impl IRgen {
    pub fn new() -> Self {
        return Self {
            loop_stack: Vec::with_capacity(4),
            ..Default::default()
        };
    }
    /// Generates bytecode and a source map for a single expression.
    pub fn generate_expr(expr: ResolvedAstNode) -> Result<Ir> {
        let mut codegen = Self {
            ..Default::default()
        };
        let mut bytecode = Vec::new();
        codegen.node_gen(expr.node, &mut bytecode)?;

        Ok(Ir {
            ops: bytecode,
            span_map: codegen.span_map,
            global_count: expr.global_count,
            local_count: expr.local_count,
        })
    }

    /// Generates bytecode and a source map for a full program.
    pub fn generate(prog: ResolvedAst) -> Result<Ir> {
        let mut codegen = Self {
            ..Default::default()
        };
        let mut bytecode = Vec::new();
        codegen.gen_top_level(prog.proc, &mut bytecode)?;
        Ok(Ir {
            ops: bytecode,
            span_map: codegen.span_map,
            global_count: prog.global_count,
            local_count: prog.local_count,
        })
    }

    fn gen_top_level(
        &mut self,
        prog: Vec<Spanned<ResolvedNode>>,
        bytecode: &mut Vec<IrNode>,
    ) -> Result {
        for node in prog {
            self.node_gen(node, bytecode)?;
        }
        Ok(())
    }
    fn gen_label_name(&mut self, name: &str) -> String {
        let name = format!("{name}@{}", self.label_counter);
        self.label_counter += 1;
        name
    }
    /// Helper to generate a `Push` instruction for a literal value.
    fn push_val(&mut self, val: IrLiteral, span: Span, bytecode: &mut Vec<IrNode>) {
        self.span_map.push(bytecode.len(), bytecode.len() + 1, span);
        bytecode.push(Op::Push(val));
    }
    /// Dispatches bytecode generation to a specific function based on the node's type.
    fn node_gen(&mut self, node: RNodeSpan, bytecode: &mut Vec<IrNode>) -> Result {
        let span = node.span;
        match node.item {
            RNode::Null => self.push_val(IrLiteral::Null, span, bytecode),
            RNode::Float(num) => self.push_val(IrLiteral::Float(num), span, bytecode),
            RNode::Int(num) => self.push_val(IrLiteral::Int(num), span, bytecode),
            RNode::Bool(cond) => self.push_val(IrLiteral::Bool(cond), span, bytecode),
            RNode::String(txt) => self.push_val(IrLiteral::String(txt), span, bytecode),

            RNode::BinaryNode { left, right, kind } => {
                self.gen_binary(left.deref_item(), right.deref_item(), kind, span, bytecode)?
            }
            RNode::UnaryNode(kind, expr) => {
                self.gen_unary(kind, expr.deref_item(), span, bytecode)?
            }
            RNode::Decl(decl) => self.gen_vardecl(decl, span, bytecode)?,
            RNode::Assignment { target, value } => {
                self.gen_assignment(target.deref_item(), value.deref_item(), span, bytecode)?
            }
            RNode::DoBlock(block) => {
                self.gen_block(block, bytecode)?;
            }
            RNode::Variable { id, is_global } => {
                self.gen_variable_load(id, is_global, span, bytecode)?
            }
            RNode::Branch(branch) => self.gen_branch(branch, span, bytecode)?,
            RNode::While { condition, block } => {
                self.gen_while(condition.deref_item(), block, span, bytecode)?
            }
            RNode::FunctionLit(func) => self.gen_func_lit(func, span, bytecode)?,
            RNode::Call { callee, args } => {
                self.gen_call(callee.deref_item(), args, span, bytecode)?
            }
            RNode::Continue => {
                let Some(loopid) = self.loop_stack.last() else {
                    return Err(GenErr::Unspecified("Invalid loop controlflow".to_owned())
                        .to_spanned(node.span));
                };
                bytecode.push(IrNode::Goto(format!("loop_start@{loopid}")))
            }
            RNode::Break => {
                let Some(loopid) = self.loop_stack.last() else {
                    return Err(GenErr::Unspecified("Invalid loop controlflow".to_owned())
                        .to_spanned(node.span));
                };
                bytecode.push(IrNode::Goto(format!("loop_end@{loopid}")))
            }
            RNode::Return(expr) => self.gen_return(expr.deref_item(), span, bytecode)?,
            node => {
                todo!("{node:?}");
            }
        };
        Ok(())
    }

    /// Generates `left`, `right`, then the `op` to act on them.
    fn gen_binary(
        &mut self,
        left: RNodeSpan,
        right: RNodeSpan,
        kind: BinaryOp,
        span: Span,
        bytecode: &mut Vec<IrNode>,
    ) -> Result {
        let start = bytecode.len();
        self.node_gen(left, bytecode)?;
        self.node_gen(right, bytecode)?;
        bytecode.push(kind.into());
        let stop = bytecode.len();
        self.span_map.push(start, stop, span);
        Ok(())
    }

    /// Generates `expr`, then the `op` to act on it.
    fn gen_unary(
        &mut self,
        kind: UnaryOp,
        expr: RNodeSpan,
        span: Span,
        bytecode: &mut Vec<IrNode>,
    ) -> Result {
        let start = bytecode.len();
        self.node_gen(expr, bytecode)?;
        let op = match kind {
            UnaryOp::Negative => Op::Neg,
            UnaryOp::Not => Op::Not,
        };
        bytecode.push(op);
        self.span_map.push(start, bytecode.len(), span);
        Ok(())
    }

    /// Generates the expression's value, then stores it in a local.
    fn gen_vardecl(&mut self, decl: Decl, span: Span, bytecode: &mut Vec<IrNode>) -> Result {
        let start = bytecode.len();
        self.node_gen(decl.expr.deref_item(), bytecode)?;
        if decl.is_global {
            bytecode.push(Op::StoreGlobal(decl.id));
        } else {
            bytecode.push(Op::StoreLocal(decl.id));
        }
        let stop = bytecode.len();
        self.span_map.push(start, stop, span);
        Ok(())
    }

    fn gen_assignment(
        &mut self,
        target: RNodeSpan,
        value: RNodeSpan,
        span: Span,
        bytecode: &mut Vec<IrNode>,
    ) -> Result {
        let start = bytecode.len();
        match target.item.clone() {
            RNode::Variable { id, is_global } => {
                self.node_gen(value, bytecode)?;
                bytecode.push(if is_global {
                    Op::StoreGlobal(id)
                } else {
                    Op::StoreLocal(id)
                });
            }
            _ => todo!(),
        };
        let stop = bytecode.len();
        self.span_map.push(start, stop, span);
        Ok(())
    }

    /// Generates an instruction to load a variable's value onto the stack.
    fn gen_variable_load(
        &mut self,
        id: usize,
        is_global: bool,
        span: Span,
        bytecode: &mut Vec<IrNode>,
    ) -> Result {
        let start = bytecode.len();
        bytecode.push(if is_global {
            Op::LoadGlobal(id)
        } else {
            Op::LoadLocal(id)
        });
        let stop = bytecode.len();
        self.span_map.push(start, stop, span);
        Ok(())
    }

    /// Generates code for a sequence of statements and returns the number of new locals.
    fn gen_block(&mut self, block: Block, bytecode: &mut Vec<IrNode>) -> Result {
        let start_index = bytecode.len();
        let span = block.span;
        for node in block {
            self.node_gen(node, bytecode)?;
        }
        let end_index = bytecode.len();
        self.span_map.push(start_index, end_index, span);
        Ok(())
    }

    fn gen_branch(&mut self, branch: Branch, span: Span, bytecode: &mut Vec<IrNode>) -> Result {
        if branch.if_block.is_empty()
            && branch
                .else_block
                .as_ref()
                .is_some_and(|else_block| else_block.is_empty())
        {
            return Ok(());
        }
        let start = bytecode.len();
        self.node_gen(branch.condition.deref_item(), bytecode)?;

        let branch_op_index = bytecode.len();
        bytecode.push(Op::NoOp);
        self.gen_block(branch.if_block, bytecode)?;
        let if_block_end_index = bytecode.len();
        bytecode.push(Op::NoOp);
        if let Some(else_block) = branch.else_block {
            let else_label = self.gen_label_name("else");
            bytecode[branch_op_index] = IrNode::Branch(else_label.clone());
            bytecode.push(IrNode::Label(else_label));
            self.gen_block(else_block, bytecode)?;

            let end_if_label = self.gen_label_name("end_if");
            bytecode[if_block_end_index] = IrNode::Goto(end_if_label.clone());
            bytecode.push(IrNode::Label(end_if_label));
        } else {
            let end_if_label = self.gen_label_name("end_if");
            bytecode[branch_op_index] = IrNode::Branch(end_if_label.clone());
            bytecode.push(IrNode::Label(end_if_label));
        }

        self.span_map.push(start, bytecode.len(), span);
        Ok(())
    }

    /// Generates a loop with a conditional exit and a jump back to the start.
    fn gen_while(
        &mut self,
        condition: RNodeSpan,
        block: Block,
        span: Span,
        bytecode: &mut Vec<IrNode>,
    ) -> Result {
        let start = bytecode.len();
        self.loop_stack.push(self.label_counter);
        let end_label = format!("loop_end@{}", self.label_counter);
        let start_label = self.gen_label_name("loop_start");
        bytecode.push(IrNode::Label(start_label.clone()));
        self.node_gen(condition, bytecode)?;
        let block_start_index = bytecode.len();
        bytecode.push(Op::NoOp);
        self.gen_block(block, bytecode)?;
        bytecode[block_start_index] = IrNode::Branch(end_label.clone());
        bytecode.push(IrNode::Goto(start_label));
        bytecode.push(IrNode::Label(end_label.clone()));
        self.loop_stack.pop();
        self.span_map.push(start, bytecode.len(), span);
        Ok(())
    }

    /// Compiles a function's body and wraps it in a callable `Function` value.
    fn gen_func_lit(
        &mut self,
        func: FunctionLit,
        span: Span,
        bytecode: &mut Vec<IrNode>,
    ) -> Result {
        if func.captures {
            todo!()
        }
        let func_start_label = self.gen_label_name("func_start");
        bytecode.push(IrNode::Label(func_start_label.clone()));
        let start = bytecode.len();
        bytecode.push(IrNode::NoOp); // Placeholder offset

        let mut func_code = Vec::new();
        if func.block.is_empty() {
            func_code.push(IrNode::Push(IrLiteral::Null));
            func_code.push(IrNode::Ret);
        } else {
            self.gen_block(func.block, &mut func_code)?;
        }
        if func.idents.len() > u8::MAX.into() {
            return Err(GenErr::TooManyArguments(func.idents.len()).to_spanned(span));
        }
        let param_count = func.idents.len() as u8;
        let local_count = param_count as usize + func.local_count;
        func_code.push(Op::Push(IrLiteral::Null));
        func_code.push(Op::Ret);
        bytecode.append(&mut func_code);
        let func_end_label = self.gen_label_name("func_end");
        bytecode[start] = IrNode::Goto(func_end_label.clone());
        bytecode.push(IrNode::Label(func_end_label));

        self.push_val(
            Function {
                address: func_start_label,
                local_count,
                param_count,
            }
            .into(),
            span,
            bytecode,
        );
        self.span_map.push(start, bytecode.len(), span);
        Ok(())
    }

    /// Generates code to evaluate arguments, then the callee, then call.
    fn gen_call(
        &mut self,
        callee: RNodeSpan,
        args: Vec<RNodeSpan>,
        span: Span,
        bytecode: &mut Vec<IrNode>,
    ) -> Result {
        let start = bytecode.len();
        let arg_len = args.len();
        if arg_len > u8::MAX.into() {
            return Err(GenErr::TooManyArguments(arg_len).to_spanned(span));
        }

        let args_start = bytecode.len();
        for node in args {
            self.node_gen(node, bytecode)?;
        }
        self.span_map.push(args_start, bytecode.len(), span);

        self.node_gen(callee, bytecode)?;
        bytecode.push(IrNode::Call(arg_len as u8));
        self.span_map.push(start, bytecode.len(), span);
        Ok(())
    }

    /// Generates the return value, then the `Ret` instruction.
    fn gen_return(&mut self, expr: RNodeSpan, span: Span, bytecode: &mut Vec<IrNode>) -> Result {
        let start = bytecode.len();
        self.node_gen(expr, bytecode)?;
        bytecode.push(Op::Ret);
        self.span_map.push(start, bytecode.len(), span);
        Ok(())
    }
}
