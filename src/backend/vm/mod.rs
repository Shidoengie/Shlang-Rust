mod error;
mod frame;
#[cfg(test)]
mod tests;

use std::{collections::VecDeque, mem, sync::Arc};

use rayon::vec;

use crate::{
    backend::vm::{
        error::{ErrCode, Type, VmErr},
        frame::Frame,
    },
    frontend::ir::instructions::*,
};

// --- MACROS to reduce boilerplate for binary operations ---

/// Implements a binary operation for Ints and Floats.
/// Pops two values, checks they are the same numeric type, performs the operation,
/// and pushes the result.
macro_rules! impl_binary_op {
    ($self:ident, $op:tt) => {{
        let (lhs, rhs) = $self.pop_pair()?;
        $self.typecheck_pair(&lhs.0, &rhs.0)?;
        let result = match (lhs.0, rhs.0) {
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
/// Pops two values, checks they are the same numeric type, performs the comparison,

/// and pushes a Bool result.
macro_rules! impl_comparison_op {
    ($self:ident, $op:tt) => {{
        let (lhs, rhs) = $self.pop_pair()?;
        $self.typecheck_pair(&lhs.0, &rhs.0)?;
        let result = match (lhs.0, rhs.0) {
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
/// Pops two values, ensures they are both Bools, performs the operation,
/// and pushes the Bool result.
macro_rules! impl_logical_op {
    ($self:ident, $op:tt) => {{
        let (lhs, rhs) = $self.pop_pair()?;
        let (Value::Bool(left), Value::Bool(right)) = (lhs.0.clone(), rhs.0.clone()) else {
             return Err(ErrCode::InvalidType {
                expected: Type::Bool,
                got: if !matches!(lhs.0, Value::Bool(_)) { lhs.0.into() } else { rhs.0.into() },
            }.into_vmerr($self.ip));
        };
        $self.push(Value::Bool(left $op right));
        $self.inc_ip();
        Ok(())
    }};
}

pub struct StackVM {
    /// Instruction pointer
    ip: usize,

    call_stack: Vec<Frame>,

    /// The runtime value stack. Each value is paired with the instruction pointer
    /// that pushed it, for accurate error reporting.
    pub values: Vec<(Value, usize)>,

    pub globals: Vec<Value>,
}

pub type Result<T = ()> = std::result::Result<T, VmErr>;
impl StackVM {
    pub fn new(proc: Vec<OpCode>) -> Self {
        let mut vm = Self {
            ip: 0,
            call_stack: vec![],
            values: vec![],
            globals: vec![],
        };

        vm.add_global(
            NativeFunction::new(
                |_, args| {
                    if args.is_empty() {
                        println!();
                        return Value::Null;
                    }
                    for i in args {
                        println!("{:?}", i);
                    }

                    return Value::Null;
                },
                1,
            )
            .into(),
        );
        let synthetic = Function {
            num_locals: 0,
            proc,
            param_count: 0,
        };
        let frame = Frame::new(Arc::new(synthetic), 0);
        vm.call_stack = vec![frame];
        vm
    }
    pub fn add_global(&mut self, value: Value) {
        self.globals.push(value)
    }
    pub fn exec(&mut self) -> Result {
        // Use a while loop for safer boundary checking.
        let Some(mut frame) = self.call_stack.pop() else {
            return Ok(());
        };
        // RUN ITS THE OPS!!
        let ops = frame.func.proc.clone();
        for op in ops {
            self.exec_op(op, &mut frame);
        }
        Ok(())
    }
    fn offset_ip(&mut self, ammount: i16) -> Result {
        let new_ip = self.ip as isize + ammount as isize;
        if new_ip.is_negative() {
            return Err(ErrCode::InvalidOffset.into_vmerr(self.ip));
        }
        self.ip = new_ip as usize;
        Ok(())
    }
    fn inc_ip(&mut self) {
        self.ip += 1
    }
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
        self.typecheck_pair(&lhs.0, &rhs.0)?;
        match (lhs.0, rhs.0) {
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
        let result = match (&lhs.0, &rhs.0) {
            (left, right) if mem::discriminant(left) != mem::discriminant(right) => false,
            (Value::Int(l), Value::Int(r)) => l == r,
            (Value::Float(l), Value::Float(r)) => l == r,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r,
            (Value::Null, Value::Null) => true,
            // Allow comparison between different types if one is Null
            (Value::Null, _) | (_, Value::Null) => false,
            _ => unreachable!(),
        };
        self.push(Value::Bool(result));
        self.inc_ip();
        Ok(())
    }

    fn exec_not_eq(&mut self) -> Result {
        self.exec_eq()?; // Perform the equality check
        let (val, ip) = self.pop()?; // Pop the boolean result
        if let Value::Bool(b) = val {
            self.push(Value::Bool(!b)); // Push its negation
        }
        // `exec_eq` already incremented the IP, so we don't do it again.
        Ok(())
    }

    fn exec_neg(&mut self) -> Result {
        let (val, ip) = self.pop()?;
        let result = match val {
            Value::Int(i) => Value::Int(-i),
            Value::Float(f) => Value::Float(-f),
            _ => {
                return Err(ErrCode::UnsupportedOperation {
                    op: format!("-()"),
                    target: val.into(),
                }
                .into_vmerr(ip));
            }
        };
        self.push(result);
        self.inc_ip();
        Ok(())
    }

    fn exec_not(&mut self) -> Result {
        let (val, ip) = self.pop()?;
        let (Value::Bool(b)) = val else {
            return Err(ErrCode::InvalidType {
                expected: Type::Bool,
                got: val.into(),
            }
            .into_vmerr(ip));
        };
        self.push(Value::Bool(!b));
        self.inc_ip();
        Ok(())
    }

    fn exec_null_co(&mut self) -> Result {
        let (lhs, rhs) = self.pop_pair()?;
        if matches!(lhs.0, Value::Null) {
            self.push(rhs.0);
        } else {
            self.push(lhs.0);
        }
        self.inc_ip();
        Ok(())
    }
    fn exec_call(&mut self, arg_len: u8, frame: &mut Frame) -> Result {
        let value = self.pop()?;
        if let (Value::NativeFunction(func), _) = value {
            if arg_len as i16 != func.param_count && func.param_count != -1 {
                panic!("invalid args");
            }
            let args = self.pop_chunk(arg_len.into(), frame);

            let res = (func.func)(self, args);
            self.push(res);
            self.inc_ip();
            return Ok(());
        }
        todo!()
    }
    fn exec_op(&mut self, op: OpCode, frame: &mut Frame) -> Result<()> {
        match op {
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
            OpCode::Call(args) => self.exec_call(args, frame),
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
                let (val, _) = self.pop()?;
                // Ensure the index exists before storing
                if self.globals.get_mut(index).is_some() {
                    self.globals[index] = val;
                } else {
                    return Err(ErrCode::InvalidStackIndex(index).into_vmerr(self.ip));
                }
                self.inc_ip();
                Ok(())
            }
            OpCode::LoadLocal(index) => {
                // Using .get() for safe access in case of invalid index from codegen
                let val = frame
                    .locals
                    .get(index)
                    .cloned()
                    .ok_or(ErrCode::InvalidStackIndex(index).into_vmerr(self.ip))?;
                self.push(val);
                self.inc_ip();

                Ok(())
            }
            OpCode::StoreLocal(index) => {
                let (val, _) = self.pop()?;
                // Ensure the index exists before storing
                if frame.get_mut(index).is_some() {
                    frame[index] = val;
                } else {
                    return Err(ErrCode::InvalidStackIndex(index).into_vmerr(self.ip));
                }
                self.inc_ip();
                Ok(())
            }
            OpCode::Pop => {
                self.pop()?;
                self.inc_ip();
                Ok(())
            }
            OpCode::Goto(offset) => self.offset_ip(offset),
            OpCode::Branch(offset) => {
                let (val, ip) = self.pop()?;
                let (Value::Bool(b)) = val else {
                    return Err(ErrCode::InvalidType {
                        expected: Type::Bool,
                        got: val.into(),
                    }
                    .into_vmerr(ip));
                };
                // Branch if the condition is FALSE
                if !b {
                    self.offset_ip(offset)?
                } else {
                    self.inc_ip();
                }
                Ok(())
            }
            _ => todo!("OpCode {:?} is not yet implemented!", op),
        }
    }
    fn pop(&mut self) -> Result<(Value, usize)> {
        self.values
            .pop()
            .ok_or(ErrCode::EmptyStack.into_vmerr(self.ip))
    }
    fn push(&mut self, value: Value) {
        self.values.push((value, self.ip));
    }
    fn pop_chunk(&mut self, len: usize, frame: &mut Frame) -> Vec<Value> {
        frame.locals.drain(len - 1..).collect()
    }
    fn pop_pair(&mut self) -> Result<((Value, usize), (Value, usize))> {
        let right = self.pop()?;
        let left = self.pop()?;
        Ok((left, right))
    }
}
