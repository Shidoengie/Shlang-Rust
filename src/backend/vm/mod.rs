mod builtins;
mod error;
mod frame;
#[cfg(test)]
mod tests;
pub mod values;
use rayon::{iter::ParallelIterator, str::ParallelString};
use slab::Slab;
use std::{fmt::format, mem, ops::Index};
use values::*;

use crate::backend::{
	instructions::*,
	vm::{
		builtins::BUILTINS,
		error::{ErrCode, Type, VmErr},
		frame::{CallStack, Frame},
	},
};

macro_rules! impl_binary_op {
    ($self:ident, $op:tt) => {{
        let (lhs, rhs) = $self.pop_pair()?;
        $self.typecheck_pair(&lhs, &rhs)?;
        let result = match (lhs, rhs) {
            (Value::Int(left), Value::Int(right)) => Value::Int(left $op right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left $op right),
            (val, _) => return Err(ErrCode::UnsupportedOperation {
                op: stringify!($op).to_string(),
                target: val.into(),
            }.into_vmerr($self.ip)),
        };
        $self.push(result);
        $self.inc_ip();
        Ok(())
    }};
}

/// Implements a comparison operation for Ints and Floats.
macro_rules! impl_comparison_op {
    ($self:ident, $op:tt) => {{
        let (lhs, rhs) = $self.pop_pair()?;
        $self.typecheck_pair(&lhs, &rhs)?;
        let result = match (lhs, rhs) {
            (Value::Int(left), Value::Int(right)) => Value::Bool(left $op right),
            (Value::Float(left), Value::Float(right)) => Value::Bool(left $op right),
            (val, _) => return Err(ErrCode::UnsupportedOperation {
                op: stringify!($op).to_string(),
                target: val.into(),
            }.into_vmerr($self.ip)),
        };
        $self.push(result);
        $self.inc_ip();
        Ok(())
    }};
}

/// Implements a logical operation for Bools.
macro_rules! impl_logical_op {
    ($self:ident, $op:tt) => {{
        let (lhs, rhs) = $self.pop_pair()?;
        let (Value::Bool(left), Value::Bool(right)) = (lhs.clone(), rhs.clone()) else {
             return Err(ErrCode::InvalidType {
                expected: Type::Bool,
                got: if !matches!(lhs, Value::Bool(_)) { lhs.into() } else { rhs.into() },
            }.into_vmerr($self.ip));
        };
        $self.push(Value::Bool(left $op right));
        $self.inc_ip();
        Ok(())
    }};
}
#[derive(Debug, Clone)]
struct IndexOrigin<T = Value> {
	pub value: T,
	pub origin: usize,
}
impl<T> From<(T, usize)> for IndexOrigin<T> {
	fn from(value: (T, usize)) -> Self {
		Self {
			value: value.0,
			origin: value.1,
		}
	}
}
#[derive(Debug)]
pub struct ManagedObject {
	pub obj: Object,
	pub refcount: usize,
}
impl From<Object> for ManagedObject {
	fn from(value: Object) -> Self {
		Self {
			obj: value,
			refcount: 0,
		}
	}
}
#[derive(Debug, Default)]
pub struct ObjectHeap {
	items: Slab<ManagedObject>,
}
impl ObjectHeap {
	pub fn new() -> Self {
		Self::default()
	}
	pub fn add(&mut self, obj: Object) -> Value {
		let id = self.items.insert(obj.into());
		return Value::ObjectRef(id);
	}
	pub fn inc_ref(&mut self, id: usize) -> Option<Value> {
		if !self.items.contains(id) {
			return None;
		}
		self.items[id].refcount += 1;
		return Some(Value::ObjectRef(id));
	}
	pub fn dec_ref(&mut self, id: usize) -> Option<Value> {
		if !self.items.contains(id) {
			return None;
		}
		let obj = &mut self.items[id];
		if obj.refcount <= 0 {
			self.items.remove(id);
			return None;
		}
		obj.refcount -= 1;
		Some(Value::ObjectRef(id))
	}
	pub fn get(&self, id: usize) -> Option<&Object> {
		self.items.get(id).map(|m_object| &m_object.obj)
	}
	pub fn get_mut(&mut self, id: usize) -> Option<&mut Object> {
		self.items.get_mut(id).map(|m_object| &mut m_object.obj)
	}
}
impl Index<usize> for ObjectHeap {
	type Output = Object;
	fn index(&self, index: usize) -> &Self::Output {
		return &self.items[index].obj;
	}
}

#[derive(Debug)]
pub struct StackVM {
	/// Instruction pointer
	ip: usize,
	proc: Box<[OpCode]>,
	op_args: Box<[usize]>,
	arg_cursor: usize,
	call_stack: CallStack,
	is_finished: bool,
	objects: ObjectHeap,
	const_pool: Box<[Value]>,
	ident_pool: Box<[String]>,
	pub value_map: Vec<usize>,
	pub values: Vec<Value>,
	pub globals: Box<[Value]>,
}

pub type Result<T = ()> = std::result::Result<T, VmErr>;
impl StackVM {
	pub fn new(mut bytecode: ByteCode) -> Self {
		// this is done so the builtins dont colide with the user defined globals
		let mut new_globals = BUILTINS.to_vec();
		new_globals.append(&mut bytecode.globals);
		new_globals.resize(bytecode.global_count, Value::Undefined);
		let mut vm = Self {
			ip: 0,
			arg_cursor: 0,
			objects: ObjectHeap::new(),
			is_finished: false,
			call_stack: CallStack::new(),
			values: vec![],
			value_map: vec![],
			proc: bytecode.ops,
			op_args: bytecode.op_args,

			globals: new_globals.into_boxed_slice(),
			const_pool: bytecode.const_pool,
			ident_pool: bytecode.ident_pool,
		};

		let synthetic = Function {
			local_count: bytecode.local_count,
			address: 0,
			param_count: 0,
		};
		let frame = Frame::new(synthetic, 0);
		vm.call_stack = CallStack::new();
		vm.push_frame(frame)
			.expect("If this ever occurs something went wrong");
		vm
	}

	pub fn exec(&mut self) -> Result {
		while let Some(op) = self.proc.get(self.ip) {
			if self.is_finished {
				break;
			}
			self.exec_op(*op)?;
		}

		Ok(())
	}
	fn stop(&mut self) {
		self.is_finished = true;
	}
	fn inc_ip(&mut self) {
		self.ip += 1
	}

	fn expect_frame(&mut self) -> Result<&mut Frame> {
		let Some(frame) = self.call_stack.peek_mut() else {
			return Err(ErrCode::ExpectedStackFrame.into_vmerr(self.ip));
		};
		Ok(frame)
	}
	fn load_local(&mut self, id: usize) -> Result {
		let val = self
			.expect_frame()?
			.get(id)
			.cloned()
			.ok_or(ErrCode::InvalidStackIndex(id).into_vmerr(self.ip))?;
		if matches!(val, Value::Undefined) {
			return Err(ErrCode::UsedBeforeInit.into_vmerr(self.ip));
		}
		self.push(val);
		self.inc_ip();
		Ok(())
	}
	fn store_local(&mut self, id: usize) -> Result {
		let val = self.pop()?;
		// Ensure the index exists before storing
		let frame = self.expect_frame()?;
		frame[id] = val;
		self.inc_ip();
		Ok(())
	}
	fn peek(&self) -> Option<&Value> {
		self.values.last()
	}
	fn type_error<T>(&self, expected: Type, got: IndexOrigin) -> Result<T> {
		let origin = got.origin;
		Err(ErrCode::InvalidType {
			expected,
			got: got.value.into(),
		}
		.into_vmerr(origin))
	}
	fn add_object(&mut self, obj: impl Into<Object>) -> Value {
		self.objects.add(obj.into())
	}
	fn push_object(&mut self, obj: impl Into<Object>) {
		let objref = self.objects.add(obj.into());
		self.values.push(objref);
		self.value_map.push(self.ip);
	}
	fn get_args(&mut self) -> usize {
		let oparg = std::mem::take(&mut self.op_args[self.arg_cursor]);
		self.arg_cursor += 1;
		return oparg;
	}
	fn exec_op(&mut self, op: OpCode) -> Result<()> {
		match op {
			OpCode::NoOp => {
				self.inc_ip();
				Ok(())
			}
			OpCode::Push => {
				let args = self.get_args();
				self.push_const(args);
				self.inc_ip();
				Ok(())
			}
			OpCode::Pop => {
				let val = self.pop()?;
				if let Value::ObjectRef(id) = val {
					self.objects.dec_ref(id);
				}
				self.inc_ip();
				Ok(())
			}
			OpCode::Goto => {
				let position = self.get_args();
				self.ip = position;
				Ok(())
			}
			OpCode::Exit => {
				self.stop();
				Ok(())
			}
			OpCode::Flush => {
				self.values.clear();
				self.value_map.clear();
				self.inc_ip();
				Ok(())
			}
			OpCode::FlushNull => {
				self.values.clear();
				self.value_map.clear();
				self.push(Value::Null);
				self.inc_ip();
				Ok(())
			}

			// Extracted larger branches
			OpCode::Branch => self.exec_branch(false),
			OpCode::NotBranch => self.exec_branch(true),
			OpCode::StoreGlobal => self.exec_store_global(),
			OpCode::SwapWith => self.exec_swap_with(),
			OpCode::SetNull => self.exec_set_null(),

			// Already extracted or naturally complex
			OpCode::Add => self.exec_add(),
			OpCode::Sub => self.exec_sub(),
			OpCode::Mult => self.exec_mult(),
			OpCode::Div => self.exec_div(),
			OpCode::Mod => self.exec_mod(),
			OpCode::Greater => self.exec_greater(),
			OpCode::Lesser => self.exec_lesser(),
			OpCode::GreaterEq => self.exec_greater_eq(),
			OpCode::LesserEq => self.exec_lesser_eq(),
			OpCode::And => self.exec_and(),
			OpCode::Or => self.exec_or(),
			OpCode::Eq => self.exec_eq(),
			OpCode::NotEq => self.exec_not_eq(),
			OpCode::Neg => self.exec_neg(),
			OpCode::Not => self.exec_not(),
			OpCode::NullCo => self.exec_null_co(),
			OpCode::Call => self.exec_call(0),
			OpCode::Call1 => self.exec_call(1),
			OpCode::Call2 => self.exec_call(2),
			OpCode::Call3 => self.exec_call(3),
			OpCode::Call4 => self.exec_call(4),

			OpCode::CallN => {
				let args = self.get_args() as u8;
				self.exec_call(args)
			}
			OpCode::LoadGlobal => self.exec_loadglobal(),
			OpCode::LoadLocal => {
				let args = self.get_args();
				self.load_local(args)
			}
			OpCode::StoreLocal => {
				let args = self.get_args();
				self.store_local(args)
			}
			OpCode::Ret => self.exec_ret(),
			OpCode::Index => {
				self.exec_index()?;
				self.inc_ip();
				Ok(())
			}
			OpCode::IndexMut => {
				self.exec_index_mut()?;
				self.inc_ip();
				Ok(())
			}
			OpCode::MakeList => {
				let len = self.get_args();
				let chunk = self.pop_chunk(len);
				self.push_object(ListObject(chunk));
				self.inc_ip();
				Ok(())
			}

			_ => todo!(),
		}
	}

	// --- Helper logic for larger branches ---

	fn exec_branch(&mut self, jump_if_true: bool) -> Result {
		let position = self.get_args();
		let cond = self.pop_raw()?;
		let Value::Bool(b) = cond.value else {
			return self.type_error(Type::Bool, cond);
		};
		if b == jump_if_true {
			self.ip = position;
		} else {
			self.inc_ip();
		}
		Ok(())
	}

	fn exec_store_global(&mut self) -> Result {
		let index = self.get_args();
		let val = self.pop()?;
		let slot = self
			.globals
			.get_mut(index)
			.ok_or_else(|| ErrCode::InvalidStackIndex(index).into_vmerr(self.ip))?;
		*slot = val;
		self.inc_ip();
		Ok(())
	}

	fn exec_swap_with(&mut self) -> Result {
		let index = self.get_args();
		let value = self.load_const(index);
		if let Some(cur) = self.values.last_mut() {
			*cur = value;
		} else {
			self.push(value);
		}
		self.inc_ip();
		Ok(())
	}

	fn exec_set_null(&mut self) -> Result {
		if let Some(cur) = self.values.last_mut() {
			*cur = Value::Null;
		} else {
			self.push(Value::Null);
		}
		self.inc_ip();
		Ok(())
	}

	fn exec_loadglobal(&mut self) -> Result {
		let index = self.get_args();
		// Using .get() for safe access in case of invalid index from codegen
		let val = self
			.globals
			.get(index)
			.cloned()
			.ok_or(ErrCode::InvalidStackIndex(index).into_vmerr(self.ip))?;

		if matches!(val, Value::Undefined) {
			return Err(ErrCode::UsedBeforeInit.into_vmerr(self.ip));
		}
		self.push(val);
		self.inc_ip();
		Ok(())
	}
	fn pop(&mut self) -> Result<Value> {
		self.value_map.pop();
		self.values
			.pop()
			.ok_or(ErrCode::EmptyStack.into_vmerr(self.ip))
	}
	fn pop_raw(&mut self) -> Result<IndexOrigin> {
		//dbg!(&self.values);
		Ok((
			self.values
				.pop()
				.ok_or(ErrCode::EmptyStack.into_vmerr(self.ip))?,
			self.value_map
				.pop()
				.ok_or(ErrCode::EmptyStack.into_vmerr(self.ip))?,
		)
			.into())
	}

	fn load_const(&mut self, id: usize) -> Value {
		mem::replace(&mut self.const_pool[id], Value::Undefined)
	}
	fn push_const(&mut self, id: usize) {
		let value = self.load_const(id);
		if let Value::ObjectRef(id) = &value {
			let id = *id;
			self.objects.inc_ref(id);
		}
		self.values.push(value);
		self.value_map.push(self.ip);
	}
	fn push(&mut self, value: Value) {
		if let Value::ObjectRef(id) = &value {
			let id = *id;
			self.objects.inc_ref(id);
		}
		self.values.push(value);
		self.value_map.push(self.ip);
	}
	fn pop_chunk(&mut self, len: usize) -> Vec<Value> {
		self.values.drain(self.values.len() - len..).collect()
	}
	fn pop_chunk_raw(&mut self, len: usize) -> Vec<IndexOrigin> {
		self.values
			.drain(self.values.len() - len..)
			.zip(self.value_map.drain(self.value_map.len() - len..))
			.map(|value| IndexOrigin::from(value))
			.collect()
	}
	fn pop_pair(&mut self) -> Result<(Value, Value)> {
		let right = self.pop()?;
		let left = self.pop()?;
		Ok((left, right))
	}
	fn pop_pair_raw(&mut self) -> Result<(IndexOrigin, IndexOrigin)> {
		let right = self.pop_raw()?;
		let left = self.pop_raw()?;

		Ok((left, right))
	}
}
//Arithmetic ops impl
impl StackVM {
	fn typecheck_pair(&mut self, left: &Value, right: &Value) -> Result {
		if mem::discriminant(left) != mem::discriminant(right) {
			return Err(ErrCode::MixedTypes {
				first: left.clone().into(),
				last: right.clone().into(),
			}
			.into_vmerr(self.ip));
		}
		Ok(())
	}

	fn exec_add(&mut self) -> Result {
		let (lhs, rhs) = self.pop_pair()?;
		self.typecheck_pair(&lhs, &rhs)?;
		match (lhs, rhs) {
			(Value::Int(left), Value::Int(right)) => self.push(Value::Int(left + right)),
			(Value::Float(left), Value::Float(right)) => self.push(Value::Float(left + right)),
			(Value::String(left), Value::String(right)) => self.push(Value::String(left + &right)),
			(val, _) => {
				return Err(ErrCode::UnsupportedOperation {
					op: "+".to_string(),
					target: val.into(),
				}
				.into_vmerr(self.ip));
			}
		}
		self.inc_ip();
		Ok(())
	}

	fn exec_sub(&mut self) -> Result {
		impl_binary_op!(self, -)
	}
	fn exec_mult(&mut self) -> Result {
		impl_binary_op!(self, *)
	}
	fn exec_div(&mut self) -> Result {
		impl_binary_op!(self, /)
	}
	fn exec_mod(&mut self) -> Result {
		impl_binary_op!(self, %)
	}

	fn exec_greater(&mut self) -> Result {
		impl_comparison_op!(self, >)
	}
	fn exec_lesser(&mut self) -> Result {
		impl_comparison_op!(self, <)
	}
	fn exec_greater_eq(&mut self) -> Result {
		impl_comparison_op!(self, >=)
	}
	fn exec_lesser_eq(&mut self) -> Result {
		impl_comparison_op!(self, <=)
	}

	fn exec_and(&mut self) -> Result {
		impl_logical_op!(self, &&)
	}
	fn exec_or(&mut self) -> Result {
		impl_logical_op!(self, ||)
	}

	fn exec_eq(&mut self) -> Result {
		let (lhs, rhs) = self.pop_pair()?;

		// `Eq` supports more types than just numerics, so it gets a custom implementation.
		let result = match (&lhs, &rhs) {
			(left, right) if mem::discriminant(left) != mem::discriminant(right) => false,
			(Value::Int(l), Value::Int(r)) => l == r,
			(Value::Float(l), Value::Float(r)) => l == r,
			(Value::Bool(l), Value::Bool(r)) => l == r,
			(Value::String(l), Value::String(r)) => l == r,
			(Value::Null, Value::Null) => true,
			_ => unreachable!(),
		};
		self.push(Value::Bool(result));
		self.inc_ip();
		Ok(())
	}

	fn exec_not_eq(&mut self) -> Result {
		self.exec_eq()?;
		let val = self.pop()?;
		if let Value::Bool(b) = val {
			self.push(Value::Bool(!b));
		}

		Ok(())
	}

	fn exec_neg(&mut self) -> Result {
		let val = self.pop()?;
		let result = match val {
			Value::Int(i) => Value::Int(-i),
			Value::Float(f) => Value::Float(-f),
			_ => {
				return Err(ErrCode::UnsupportedOperation {
					op: "-()".to_string(),
					target: val.into(),
				}
				.into_vmerr(self.ip));
			}
		};
		self.push(result);
		self.inc_ip();
		Ok(())
	}

	fn exec_not(&mut self) -> Result {
		let val = self.pop()?;
		let Value::Bool(b) = val else {
			return Err(ErrCode::InvalidType {
				expected: Type::Bool,
				got: val.into(),
			}
			.into_vmerr(self.ip));
		};
		self.push(Value::Bool(!b));
		self.inc_ip();
		Ok(())
	}

	fn exec_null_co(&mut self) -> Result {
		let (lhs, rhs) = self.pop_pair()?;
		if matches!(lhs, Value::Null) {
			self.push(rhs);
		} else {
			self.push(lhs);
		}
		self.inc_ip();
		Ok(())
	}
}
// Functions impl
impl StackVM {
	fn exec_native_call(&mut self, func: NativeFunction, arg_len: u8) -> Result {
		if !func.is_arglen_valid(arg_len) {
			return Err(ErrCode::InvalidArgs {
				expected: arg_len,
				got: func.param_count,
			}
			.into_vmerr(self.ip));
		}
		let args = self.pop_chunk(arg_len.into());

		match (func.func)(self, &args) {
			Ok(res) => {
				self.push(res);
				self.inc_ip();
				Ok(())
			}
			Err(err) => match err {
				CallError::Major(err) => Err(err.into_vmerr(self.ip)),
				CallError::Unspecified(err) => Err(VmErr::other(self.ip, err)),
			},
		}
	}
	fn exec_func_call(&mut self, func: Function, arg_len: u8) -> Result {
		if arg_len != func.param_count {
			return Err(ErrCode::InvalidArgs {
				expected: arg_len,
				got: func.param_count,
			}
			.into_vmerr(self.ip));
		}
		let args = if func.param_count == 0 {
			vec![]
		} else {
			self.pop_chunk(arg_len as usize)
		};

		let mut frame = Frame::new(func, self.ip);
		frame.set_values(&args);
		let func_address = frame.func.address;
		self.push_frame(frame)?;
		self.ip = func_address;
		Ok(())
	}
	fn exec_ret(&mut self) -> Result {
		let value = self.pop()?;
		let Some(frame) = self.call_stack.pop() else {
			self.stop();
			return Ok(());
		};
		self.push(value);
		self.ip = frame.ret_address + 1;
		Ok(())
	}
	fn exec_call(&mut self, arg_len: u8) -> Result {
		let value = self.pop()?;

		match value {
			Value::NativeFunction(func) => self.exec_native_call(func, arg_len)?,
			Value::Function(func) => self.exec_func_call(func, arg_len)?,
			_ => {
				return Err(ErrCode::InvalidType {
					expected: Type::Function,
					got: value.into(),
				}
				.into_vmerr(self.ip));
			}
		};

		Ok(())
	}
	fn push_frame(&mut self, frame: Frame) -> Result {
		if let Err(_) = self.call_stack.push(frame) {
			return Err(ErrCode::StackOverflow.into_vmerr(self.ip));
		};
		Ok(())
	}
}
impl StackVM {
	fn exec_index(&mut self) -> Result {
		let refid = self.pop_raw()?;
		let index = self.pop_raw()?;
		if let Value::String(content) = refid.value {
			return self.index_string(content, index);
		}
		let Value::ObjectRef(refid) = refid.value else {
			return self.type_error(Type::ObjectRef, refid);
		};
		let Some(obj) = self.objects.get(refid) else {
			return Err(ErrCode::Unspecified(format!("Invalid object id")).into_vmerr(self.ip));
		};
		match obj {
			Object::Native(list) => {
				let res = list
					.lang_index(index.value)
					.map_err(|err| VmErr::new(index.origin, err))?;
				self.push(res);

				return Ok(());
			}
			_ => todo!(),
		}
	}
	fn index_string(&mut self, content: String, index: IndexOrigin) -> Result {
		let Value::Int(index) = index.value else {
			return self.type_error(Type::Int, index);
		};
		if index < 0 {
			return Err(ErrCode::IndexOutOfBounds.into_vmerr(self.ip));
		}

		if content.is_ascii() {
			let index = index as usize;
			if index as usize >= content.len() {
				return Err(ErrCode::IndexOutOfBounds.into_vmerr(self.ip));
			}
			let ch = &content[index..index + 1];
			self.push(Value::String(ch.to_owned()));
			return Ok(());
		}
		let Some((_, ch)) = content
			.par_char_indices()
			.find_first(|(idx, _)| *idx == index as usize)
		else {
			return Err(ErrCode::IndexOutOfBounds.into_vmerr(self.ip));
		};
		self.push(Value::String(ch.to_string()));
		return Ok(());
	}
	fn exec_index_mut(&mut self) -> Result {
		let target = self.pop_raw()?;
		let index = self.pop()?;
		let value = self.pop()?;
		let Value::ObjectRef(id) = target.value else {
			return self.type_error(Type::ObjectRef, target);
		};
		let Some(obj) = self.objects.get_mut(id) else {
			return Err(ErrCode::Unspecified(format!("Invalid object id")).into_vmerr(self.ip));
		};
		match obj {
			Object::Native(list) => {
				list.lang_index_mut(index, value)
					.map_err(|err| VmErr::new(self.ip, err))?;
				return Ok(());
			}
			_ => todo!(),
		}
	}
}
impl ByteCode {
	pub fn new_vm(self) -> StackVM {
		StackVM::new(self)
	}
}
impl From<ByteCode> for StackVM {
	fn from(value: ByteCode) -> Self {
		value.new_vm()
	}
}
