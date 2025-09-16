use std::sync::Arc;

use crate::frontend::ir::instructions::{Function, Value};

pub struct Frame {
    func: Arc<Function>,
    ret_address: usize,
    stack_start: usize,
}
