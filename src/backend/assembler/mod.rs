use crate::backend::instructions::*;
use crate::backend::vm::values::*;
use crate::frontend::ir::instructions::IrNode;
use crate::frontend::ir::{codegen::Ir, instructions::IrLiteral};
use std::{collections::HashMap, sync::Arc};

pub struct Assembler {
    label_map: HashMap<String, usize>,
}

impl Assembler {
    pub fn assemble(ir: Ir) -> ByteCode {
        let mut label_map = HashMap::new();
        let mut output_offset = 0;

        for node in &ir.ops {
            match node {
                IrNode::Label(name) => {
                    label_map.insert(name.clone(), output_offset);
                }
                _ => {
                    output_offset += 1;
                }
            }
        }

        let assembler = Self { label_map };
        let mut output: Vec<OpCode> = Vec::new();
        let mut globals = vec![];
        for lit in ir.globals {
            globals.push(assembler.literal_to_value(lit));
        }
        for node in ir.ops {
            if let IrNode::Label(_) = node {
                continue;
            }

            let op = assembler.node_to_opcode(node);
            output.push(op);
        }

        ByteCode {
            ops: output,
            span_map: ir.span_map,
            global_count: ir.global_count,
            local_count: ir.local_count,
            globals,
        }
    }

    fn node_to_opcode(&self, node: IrNode) -> OpCode {
        match node {
            IrNode::NoOp => OpCode::NoOp,
            IrNode::Label(_) => unreachable!("Labels should be filtered out before calling this"),
            IrNode::Push(lit) => OpCode::Push(self.literal_to_value(lit)),
            IrNode::LoadLocal(idx) => OpCode::LoadLocal(idx),
            IrNode::StoreLocal(idx) => OpCode::StoreLocal(idx),
            IrNode::LoadGlobal(idx) => OpCode::LoadGlobal(idx),
            IrNode::StoreGlobal(idx) => OpCode::StoreGlobal(idx),
            IrNode::Pop => OpCode::Pop,
            IrNode::Goto(label) => OpCode::Goto(self.get_label_address(&label)),
            IrNode::Branch(label) => OpCode::Branch(self.get_label_address(&label)),
            IrNode::NotBranch(label) => OpCode::NotBranch(self.get_label_address(&label)),
            IrNode::Add => OpCode::Add,
            IrNode::Mult => OpCode::Mult,
            IrNode::Div => OpCode::Div,
            IrNode::Sub => OpCode::Sub,
            IrNode::Mod => OpCode::Mod,
            IrNode::And => OpCode::And,
            IrNode::Or => OpCode::Or,
            IrNode::Greater => OpCode::Greater,
            IrNode::Lesser => OpCode::Lesser,
            IrNode::GreaterEq => OpCode::GreaterEq,
            IrNode::LesserEq => OpCode::LesserEq,
            IrNode::NotEq => OpCode::NotEq,
            IrNode::Eq => OpCode::Eq,
            IrNode::NullCo => OpCode::NullCo,
            IrNode::Not => OpCode::Not,
            IrNode::Neg => OpCode::Neg,
            IrNode::Stop => OpCode::Exit,
            IrNode::Ret => OpCode::Ret,
            IrNode::SwapWith(lit) => OpCode::SwapWith(self.literal_to_value(lit)),
            IrNode::SetNull => OpCode::SetNull,
            IrNode::Flush => OpCode::Flush,
            IrNode::FlushNull => OpCode::FlushNull,
            IrNode::Index => OpCode::Index,
            IrNode::IndexMut => OpCode::IndexMut,
            IrNode::Call(arity) => OpCode::Call(arity),
            IrNode::MakeList(list) => OpCode::MakeList(list),
        }
    }

    fn literal_to_value(&self, literal: IrLiteral) -> Value {
        match literal {
            IrLiteral::Null => Value::Null,
            IrLiteral::Int(v) => Value::Int(v),
            IrLiteral::Float(v) => Value::Float(v),
            IrLiteral::Bool(v) => Value::Bool(v),
            IrLiteral::String(v) => Value::String(v),
            IrLiteral::Function(func) => {
                let address = self.get_label_address(&func.address);
                Value::Function(Function {
                    address,
                    local_count: func.local_count,
                    param_count: func.param_count,
                })
            }
        }
    }

    fn get_label_address(&self, label: &str) -> usize {
        *self
            .label_map
            .get(label)
            .unwrap_or_else(|| panic!("Could not find label {}", label))
    }
}
