use std::ops::{Index, IndexMut};

use super::values::{Function, Value};
#[derive(Debug)]
pub struct Frame {
	pub func: Function,
	pub ret_address: usize,
	pub locals: Box<[Value]>,
}
impl Frame {
	pub fn new(func: Function, ret_address: usize) -> Self {
		let local_count = func.local_count;
		Self {
			func,
			ret_address,
			locals: vec![Value::Undefined; local_count].into_boxed_slice(),
		}
	}
	/// Gets a local variable from a given index
	pub fn get(&self, index: usize) -> Option<&Value> {
		self.locals.get(index)
	}
	pub fn get_mut(&mut self, index: usize) -> Option<&mut Value> {
		self.locals.get_mut(index)
	}
	pub fn set(&mut self, index: usize, value: Value) {
		self[index] = value;
	}
	pub fn set_values(&mut self, list: &[Value]) {
		for (i, val) in list.iter().enumerate() {
			self.locals[i] = val.clone();
		}
	}
}
impl Index<usize> for Frame {
	type Output = Value;
	fn index(&self, index: usize) -> &Self::Output {
		&self.locals[index]
	}
}
impl IndexMut<usize> for Frame {
	fn index_mut(&mut self, index: usize) -> &mut Self::Output {
		&mut self.locals[index]
	}
}
#[derive(Debug)]
pub struct CallStack<const MAX: usize = 10_000> {
	inner: Vec<Frame>,
}
impl<const MAX: usize> CallStack<MAX> {
	pub const MAX: usize = MAX;
	pub fn new() -> Self {
		Self {
			inner: Vec::with_capacity(MAX),
		}
	}
	pub fn push(&mut self, frame: Frame) -> Result<(), Frame> {
		if self.inner.len() == MAX {
			return Err(frame);
		}
		self.inner.push(frame);
		Ok(())
	}
	pub fn pop(&mut self) -> Option<Frame> {
		self.inner.pop()
	}
	pub fn peek(&self) -> Option<&Frame> {
		self.inner.last()
	}
	pub fn peek_mut(&mut self) -> Option<&mut Frame> {
		self.inner.last_mut()
	}
}
