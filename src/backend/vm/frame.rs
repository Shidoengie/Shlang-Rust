use std::{
    ops::{Index, IndexMut},
    sync::Arc,
};

use rayon::collections::vec_deque;

use crate::frontend::ir::instructions::{Function, Value};

pub struct Frame {
    pub func: Arc<Function>,
    pub ret_address: usize,
    pub locals: Box<[Value]>,
}
impl Frame {
    pub fn new(func: Arc<Function>, ret_address: usize) -> Self {
        let local_count = func.local_count;
        return Self {
            func,
            ret_address,
            locals: vec![Value::Null; local_count].into_boxed_slice(),
        };
    }
    /// Gets a local variable from a given index
    pub fn get(&self, index: usize) -> Option<&Value> {
        return self.locals.get(index);
    }
    pub fn get_mut(&mut self, index: usize) -> Option<&mut Value> {
        return self.locals.get_mut(index);
    }
    pub fn set(&mut self, index: usize, value: Value) {
        self[index] = value;
    }
}
impl Index<usize> for Frame {
    type Output = Value;
    fn index(&self, index: usize) -> &Self::Output {
        return &self.locals[index];
    }
}
impl IndexMut<usize> for Frame {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.locals[index]
    }
}
