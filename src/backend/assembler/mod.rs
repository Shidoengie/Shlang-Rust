use crate::backend::instructions::*;
use crate::backend::vm::values::*;
use crate::collections::indexset::IndexSet;
use crate::frontend::ir::instructions::IrNode;
use crate::frontend::ir::{codegen::Ir, instructions::IrLiteral};
use crate::idents::IdentId;
use std::collections::HashMap;

pub struct Assembler {
	label_map: HashMap<String, usize>,
	ops: Vec<OpCode>,
	op_arguments: Vec<usize>,
	const_pool: Vec<Value>,

	index_map: Vec<Option<usize>>,
}

impl Assembler {
	pub fn new() -> Self {
		Self {
			label_map: HashMap::new(),
			ops: vec![],
			const_pool: vec![],
			op_arguments: vec![],
			index_map: vec![],
		}
	}
	pub fn assemble<'a>(ir: Ir<'a>) -> ByteCode<'a> {
		let mut assembler = Self {
			label_map: HashMap::new(),
			ops: Vec::with_capacity(ir.ops.len()),
			const_pool: vec![],
			op_arguments: vec![],
			index_map: vec![],
		};

		assembler.assemble_proc(ir)
	}

	pub fn assemble_proc<'a>(&mut self, ir: Ir<'a>) -> ByteCode<'a> {
		self.build_label_map(&ir.ops);

		let globals = ir
			.globals
			.into_iter()
			.map(|lit| self.literal_to_value(lit))
			.collect();

		for node in ir.ops {
			if let IrNode::Label(_) = node {
				continue;
			}
			self.assemble_node(node);
		}
		let span_map = ir.span_map.remap_op_ranges(&self.index_map, self.ops.len());
		ByteCode {
			file_id: ir.file_id,
			ops: std::mem::take(&mut self.ops).into_boxed_slice(),
			op_args: std::mem::take(&mut self.op_arguments).into_boxed_slice(),
			const_pool: std::mem::take(&mut self.const_pool).into_boxed_slice(),
			span_map,
			global_count: ir.global_count,
			local_count: ir.local_count,
			globals,
			ident_pool: ir.ident_pool,
		}
	}

	fn build_label_map(&mut self, ops: &[IrNode]) {
		let mut output_offset = 0;
		for node in ops {
			match node {
				IrNode::Label(name) => {
					self.label_map.insert(name.clone(), output_offset);
					self.index_map.push(None);
				}
				_ => {
					self.index_map.push(Some(output_offset));
					output_offset += 1;
				}
			}
		}
	}
	fn push_literal(&mut self, lit: IrLiteral) {
		let value = self.literal_to_value(lit);
		let idx = self.const_pool.len();
		self.const_pool.push(value);
		self.op_arguments.push(idx);
	}

	fn push_op(&mut self, op: OpCode) {
		self.ops.push(op);
	}
	fn push_full_op(&mut self, op: OpCode, arg: usize) {
		self.ops.push(op);
		self.op_arguments.push(arg);
	}

	fn assemble_node(&mut self, node: IrNode) {
		let code = match node {
			IrNode::NoOp => OpCode::NoOp,
			IrNode::Label(_) => unreachable!("This function should always be called after  "),
			IrNode::Push(lit) => {
				self.push_op(OpCode::Push);
				self.push_literal(lit);
				return;
			}
			IrNode::LoadLocal(idx) => {
				self.push_full_op(OpCode::LoadLocal, idx);
				return;
			}
			IrNode::StoreLocal(idx) => {
				self.push_full_op(OpCode::StoreLocal, idx);
				return;
			}
			IrNode::LoadGlobal(idx) => {
				self.push_full_op(OpCode::LoadGlobal, idx);
				return;
			}
			IrNode::StoreGlobal(idx) => {
				self.push_full_op(OpCode::StoreGlobal, idx);
				return;
			}
			IrNode::Pop => OpCode::Pop,
			IrNode::Goto(label) => {
				let address = self.get_label_address(&label);
				self.push_full_op(OpCode::Goto, address);
				return;
			}
			IrNode::Branch(label) => {
				let address = self.get_label_address(&label);
				self.push_full_op(OpCode::Branch, address);
				return;
			}
			IrNode::NotBranch(label) => {
				let address = self.get_label_address(&label);
				self.push_full_op(OpCode::NotBranch, address);
				return;
			}
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
			IrNode::SwapWith(lit) => {
				self.push_op(OpCode::SwapWith);
				self.push_literal(lit);
				return;
			}
			IrNode::SetNull => OpCode::SetNull,
			IrNode::Flush => OpCode::Flush,
			IrNode::FlushNull => OpCode::FlushNull,
			IrNode::Index => OpCode::Index,
			IrNode::IndexMut => OpCode::IndexMut,
			IrNode::Call(arity) => match arity {
				0 => OpCode::Call,
				1 => OpCode::Call1,
				2 => OpCode::Call2,
				3 => OpCode::Call3,
				4 => OpCode::Call4,
				n => {
					self.push_full_op(OpCode::CallN, n as usize);
					return;
				}
			},
			IrNode::MakeClass(Some(name)) => {
				self.push_op(OpCode::MakeClass);
				self.push_ident(name);
				return;
			}
			IrNode::MakeClass(None) => OpCode::NewAnonClass,
			IrNode::GetProperty(prop) => {
				self.push_op(OpCode::GetProperty);
				self.push_ident(prop);
				return;
			}
			IrNode::SetProperty(prop) => {
				self.push_op(OpCode::SetProperty);
				self.push_ident(prop);
				return;
			}
			IrNode::MakeList(list) => {
				self.push_full_op(OpCode::MakeList, list);
				return;
			}
		};
		self.ops.push(code);
	}
	fn push_ident(&mut self, ident: IdentId) {
		self.op_arguments.push(ident.0.get());
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
