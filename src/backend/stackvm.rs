use crate::frontend::codegen::IRgen;
use crate::frontend::nodes;
use crate::frontend::nodes::BinaryOp;
use crate::frontend::nodes::UnaryOp;
use crate::frontend::stacknodes::*;
use crate::spans::Span;
use crate::spans::Spanned;
pub struct StackVM {
    proc: Vec<Spanned<StackOp>>,
    counter: usize,
    pub values: Vec<Value>,
}
impl StackVM {
    pub fn new(proc: Vec<Spanned<StackOp>>) -> Self {
        Self {
            proc,
            counter: 0,
            values: vec![],
        }
    }
    pub fn exec_from(source: &str) -> Value {
        let mut gen = IRgen::new();
        gen.generate(source);
        Self::new(gen.stack).exec()
    }
    fn eval_unary_op(&mut self, op: UnaryOp, span: Span) {}
    pub fn exec(&mut self) -> Value {
        let stacklen = self.proc.len();

        loop {
            let res = self.proc[self.counter].clone();
            self.exec_op(res);
            if self.counter > stacklen - 1 {
                break;
            }
        }
        return self.values[0].clone();
    }
    fn float_calc(&mut self, left: f64, right: f64, op: BinaryOp, span: Span) {
        let result = match op {
            BinaryOp::Add => left + right,
            BinaryOp::Subtract => left - right,
            BinaryOp::Divide => left / right,
            BinaryOp::Multiply => left * right,
            _ => todo!(),
        };
        self.push(Value::Float(result));
        self.counter += 1;
    }
    fn int_calc(&mut self, pair: (i64, i64), op: BinaryOp, span: Span) {}
    fn eval_binary_op(&mut self, op: BinaryOp, span: Span) {
        match self.pop_pair() {
            (Value::Float(left), Value::Float(right)) => self.float_calc(left, right, op, span),
            _ => todo!(),
        }
    }

    fn exec_op(&mut self, op: Spanned<StackOp>) {
        let span = op.span;
        match op.item {
            StackOp::Push(val) => {
                println!("{:?}", &self.values);
                self.values.push(val);
                self.counter += 1;
            }
            StackOp::Load(index) => {
                self.push(self.values[index].clone());
                self.counter += 1;
            }
            StackOp::Store(index) => {
                let val = self.pop();
                self.values[index] = val;
                self.counter += 1;
            }
            StackOp::Pop => {
                self.pop();
                self.counter += 1;
            }
            StackOp::BinaryOp(binop) => self.eval_binary_op(binop, span),
            StackOp::UnaryOp(unary) => self.eval_unary_op(unary, span),
            StackOp::Goto(line, cond) => {
                if line > self.proc.len() {
                    panic!("Invalid goto line ya cunt the stack len is {} but your stupid ass supplied {line}",self.proc.len())
                }
                if cond {
                    self.counter = line;
                }
            }
        }
    }
    fn pop(&mut self) -> Value {
        self.values.pop().expect("Expected another value")
    }
    fn push(&mut self, value: Value) {
        self.values.push(value);
        println!("a");
    }
    fn pop_pair(&mut self) -> (Value, Value) {
        let pair = (self.pop(), self.pop());
        (pair.1, pair.0)
    }
}
