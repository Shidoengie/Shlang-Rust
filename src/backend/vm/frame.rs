use std::{
    ops::{Index, IndexMut},
    sync::Arc,
};

use rayon::collections::vec_deque;

use crate::frontend::ir::instructions::{Function, Value};

pub struct Frame {
    pub func: Arc<Function>,
    pub ret_address: usize,
    pub locals: Vec<Value>,
}
impl Frame {
    pub fn new(func: Arc<Function>, ret_address: usize) -> Self {
        return Self {
            func,
            ret_address,
            locals: vec![],
        };
    }
    /// Gets a local variable from a given index
    pub fn get(&self, index: usize) -> Option<&Value> {
        return self.locals.get(index);
    }
    pub fn get_mut(&mut self, index: usize) -> Option<&mut Value> {
        return self.locals.get_mut(index);
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
