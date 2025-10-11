mod builtins;
mod error;
mod frame;
#[cfg(test)]
mod tests;

use std::{mem, sync::Arc};

use crate::backend::{
    instructions::*,
    vm::{
        builtins::BUILTINS,
        error::{ErrCode, Type, VmErr},
        frame::Frame,
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
#[derive(Debug)]
pub struct StackVM {
    /// Instruction pointer
    ip: usize,

    proc: Vec<OpCode>,
    call_stack: Vec<Frame>,
    is_finished: bool,
    /// The runtime value stack. Each value is paired with the instruction pointer
    /// that pushed it, for accurate error reporting.
    pub values: Vec<(Value, usize)>,

    pub globals: Box<[Value]>,
}

pub type Result<T = ()> = std::result::Result<T, VmErr>;
impl StackVM {
    pub fn new(proc: Vec<OpCode>, global_count: usize, local_count: usize) -> Self {
        let mut vm = Self {
            ip: 0,
            proc,
            is_finished: false,
            call_stack: vec![],
            values: vec![],
            globals: vec![Value::Null; global_count].into_boxed_slice(),
        };
        for (i, val) in BUILTINS.into_iter().enumerate() {
            vm.globals[i] = val
        }
        let synthetic = Function {
            local_count,
            address: 0,
            param_count: 0,
        };
        let frame = Frame::new(Arc::new(synthetic), 0);
        vm.call_stack = vec![frame];
        vm
    }

    pub fn exec(&mut self) -> Result {
        while let Some(op) = self.proc.get(self.ip) {
            if self.is_finished {
                break;
            }
            self.exec_op(op.clone())?;
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
        let Some(frame) = self.call_stack.last_mut() else {
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
        self.values.last().map(|(v, _)| v)
    }

    fn peek_raw(&self) -> Option<&(Value, usize)> {
        self.values.last()
    }
    fn type_error<T>(&self, expected: Type, got: Value) -> Result<T> {
        Err(ErrCode::InvalidType {
            expected,
            got: got.into(),
        }
        .into_vmerr(self.ip))
    }
    fn exec_op(&mut self, op: OpCode) -> Result<()> {
        match op {
            OpCode::NoOp => {
                self.inc_ip();
                Ok(())
            }
            OpCode::NotBranch(position) => {
                let (val, ip) = self.pop_raw()?;
                let Value::Bool(b) = val else {
                    return Err(ErrCode::InvalidType {
                        expected: Type::Bool,
                        got: val.into(),
                    }
                    .into_vmerr(ip));
                };
                // Branch if the condition is TRUE
                if b {
                    self.ip = position
                } else {
                    self.inc_ip();
                }
                Ok(())
            }
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

            OpCode::Push(val) => {
                self.push(val);
                self.inc_ip();
                Ok(())
            }
            OpCode::Call(args) => self.exec_call(args),
            OpCode::LoadGlobal(index) => {
                // Using .get() for safe access in case of invalid index from codegen
                let val = self
                    .globals
                    .get(index)
                    .cloned()
                    .ok_or(ErrCode::InvalidStackIndex(index).into_vmerr(self.ip))?;
                self.push(val);
                self.inc_ip();
                Ok(())
            }
            OpCode::StoreGlobal(index) => {
                let val = self.pop()?;
                // Ensure the index exists before storing
                if self.globals.get_mut(index).is_some() {
                    self.globals[index] = val;
                } else {
                    return Err(ErrCode::InvalidStackIndex(index).into_vmerr(self.ip));
                }
                self.inc_ip();
                Ok(())
            }
            OpCode::LoadLocal(index) => self.load_local(index),
            OpCode::StoreLocal(index) => self.store_local(index),
            OpCode::Pop => {
                self.pop()?;
                self.inc_ip();
                Ok(())
            }
            OpCode::Goto(position) => {
                self.ip = position;
                Ok(())
            }
            OpCode::Branch(position) => {
                let (val, ip) = self.pop_raw()?;
                let Value::Bool(b) = val else {
                    return Err(ErrCode::InvalidType {
                        expected: Type::Bool,
                        got: val.into(),
                    }
                    .into_vmerr(ip));
                };
                // Branch if the condition is FALSE
                if !b {
                    self.ip = position
                } else {
                    self.inc_ip();
                }
                Ok(())
            }
            OpCode::Ret => self.exec_ret(),
            _ => todo!("OpCode {:?} is not yet implemented!", op),
        }
    }
    fn pop(&mut self) -> Result<Value> {
        let (value, _) = self.pop_raw()?;
        Ok(value)
    }
    fn pop_raw(&mut self) -> Result<(Value, usize)> {
        //dbg!(&self.values);
        self.values
            .pop()
            .ok_or(ErrCode::EmptyStack.into_vmerr(self.ip))
            .inspect_err(|_| {
                dbg!(&self.ip, &self.values);
            })
    }
    fn push(&mut self, value: Value) {
        self.values.push((value, self.ip));
    }
    fn pop_chunk(&mut self, len: usize) -> Vec<Value> {
        self.values
            .drain(self.values.len() - len..)
            .map(|(value, _)| value)
            .collect()
    }
    fn pop_chunk_raw(&mut self, len: usize) -> Vec<(Value, usize)> {
        self.values.drain(self.values.len() - len..).collect()
    }
    fn pop_pair(&mut self) -> Result<(Value, Value)> {
        let ((left, _), (right, _)) = self.pop_pair_raw()?;
        Ok((left, right))
    }
    fn pop_pair_raw(&mut self) -> Result<((Value, usize), (Value, usize))> {
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

        let res = (func.func)(self, &args);
        self.push(res);
        self.inc_ip();
        Ok(())
    }
    fn exec_func_call(&mut self, func: Arc<Function>, arg_len: u8) -> Result {
        //dbg!(self.ip, &self.values);
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
        let mut frame = Frame::new(func.clone(), self.ip);
        frame.set_values(&args);
        let func_address = frame.func.address;
        self.call_stack.push(frame);
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
}
