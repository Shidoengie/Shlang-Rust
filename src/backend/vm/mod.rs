mod error;
#[cfg(test)]
mod tests;

use std::mem;

use crate::{
    backend::vm::error::{ErrCode, VmErr},
    frontend::ir::instructions::*,
};

pub struct StackVM {
    proc: Vec<OpCode>,
    /// Instruction pointer
    ip: usize,
    pub values: Vec<(Value, usize)>,
}

pub type Result<T = ()> = std::result::Result<T, VmErr>;
impl StackVM {
    pub fn new(proc: Vec<OpCode>) -> Self {
        Self {
            proc: proc.into(),
            ip: 0,
            values: vec![],
        }
    }
    pub fn exec(&mut self) -> Result {
        let stacklen = self.proc.len();

        loop {
            let res = self.proc[self.ip].clone();
            self.exec_op(res)?;
            if self.ip > stacklen - 1 {
                break;
            }
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
            (Value::Int(left), Value::Int(right)) => {
                self.push(Value::Int(left + right));
            }
            (Value::Float(left), Value::Float(right)) => {
                self.push(Value::Float(left + right));
            }
            (Value::String(left), Value::String(right)) => {
                self.push(Value::String(left + &right));
            }
            _ => {
                return Err(
                    ErrCode::Unspecified("Invalid OP -MAKE ME INTO AN ERROR!".to_owned())
                        .into_vmerr(self.ip),
                );
            }
        }
        self.inc_ip();
        Ok(())
    }
    fn exec_op(&mut self, op: OpCode) -> Result<()> {
        match op {
            OpCode::Add => self.exec_add(),
            OpCode::Push(val) => {
                self.values.push((val, self.ip));
                self.inc_ip();
                Ok(())
            }
            OpCode::Load(index) => {
                self.push(self.values[index].clone().0);
                self.inc_ip();
                Ok(())
            }
            OpCode::Store(index) => {
                let val = self.pop()?;
                self.values[index] = val;
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
                let res = self.pop()?;
                let (Value::Bool(b), _) = res else {
                    return Err(ErrCode::InvalidType {
                        expected: error::Type::Bool,
                        got: res.0.into(),
                    }
                    .into_vmerr(res.1));
                };
                if !b {
                    self.offset_ip(offset)?;
                }
                Ok(())
            }
            _ => todo!(),
        }
    }
    fn pop(&mut self) -> Result<(Value, usize)> {
        let Some(val) = self.values.pop() else {
            return Err(ErrCode::EmptyStack.into_vmerr(self.ip));
        };
        return Ok(val);
    }
    fn push(&mut self, value: Value) {
        self.values.push((value, self.ip));
    }
    fn pop_pair(&mut self) -> Result<((Value, usize), (Value, usize))> {
        let pair = (self.pop()?, self.pop()?);
        Ok((pair.1, pair.0))
    }
}
