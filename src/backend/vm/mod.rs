mod error;
use core::panic;

use crate::frontend::ir::instructions::*;

pub struct StackVM {
    proc: Vec<OpCode>,
    ip: usize,
    pub values: Vec<Value>,
}

pub type Result<T = ()> = std::result::Result<T, ()>;
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
        todo!()
    }
    fn offset_ip(&mut self, ammount: i16) -> Result {
        let new_ip = self.ip as isize + ammount as isize;
        if new_ip.is_negative() {
            panic!("Invalid offset")
        }
        self.ip = new_ip as usize;
        Ok(())
    }
    fn exec_op(&mut self, op: OpCode) -> Result<()> {
        match op {
            OpCode::Push(val) => {
                self.values.push(val);
                self.ip += 1;
                Ok(())
            }
            OpCode::Load(index) => {
                self.push(self.values[index].clone());
                self.ip += 1;
                Ok(())
            }
            OpCode::Store(index) => {
                let val = self.pop()?;
                self.values[index] = val;
                self.ip += 1;
                Ok(())
            }
            OpCode::Pop => {
                self.pop()?;
                self.ip += 1;
                Ok(())
            }

            OpCode::Goto(offset) => self.offset_ip(offset),
            OpCode::Branch(offset) => {
                let Value::Bool(b) = self.pop()? else {
                    panic!("Invalid type")
                };
                if b {
                    self.offset_ip(offset)?;
                }
                Ok(())
            }
            _ => todo!(),
        }
    }
    fn pop(&mut self) -> Result<Value> {
        let Some(val) = self.values.pop() else {
            todo!();
        };
        return Ok(val);
    }
    fn push(&mut self, value: Value) {
        self.values.push(value);
    }
    fn pop_pair(&mut self) -> Result<(Value, Value)> {
        let pair = (self.pop()?, self.pop()?);
        Ok((pair.1, pair.0))
    }
}
