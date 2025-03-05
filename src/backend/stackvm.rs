use rayon::result;

use crate::frontend::nodes::BinaryOp;
use crate::frontend::nodes::UnaryOp;
use crate::frontend::stacknodes::*;
use crate::lang_errors::LangError;
use crate::spans::IntoSpanned;
use crate::spans::Span;
use crate::spans::Spanned;
type VmRes<T> = Result<T, Spanned<VmError>>;
#[derive(Debug, Clone)]
pub enum VmError {
    MixedTypes(Type, Type),
    InvalidType(Vec<Type>, Type),
    NonExistentVar(String),
    InvalidAssignment(String),
    InvalidBinary(BinaryOp, Type),
    InvalidUnary(UnaryOp, Type),
    InvalidArgSize(u32, u32),
    Unspecified(String),
}
macro_rules! unspec {
    ($range:expr, $($arg:tt)*) => {{
        let res = std::fmt::format(format_args!($($arg)*));
        Err(VmError::Unspecified(res).to_spanned($range))
    }};
    ($start:expr => $stop:expr, $($arg:tt)*) => {{
        let res = std::fmt::format(format_args!($($arg)*));
        Err(VmError::Unspecified(res).to_spanned(Span($start,$stop)))
    }}
}
impl LangError for Spanned<VmError> {
    fn msg(&self) -> String {
        match &self.item {
            VmError::MixedTypes(first, last) => format!("Mixed types: {first:?} and {last:?}"),

            VmError::InvalidType(accepted, got) => {
                let opts = String::from_iter(
                    format!("{accepted:?}")
                        .chars()
                        .filter(|c| c != &'[' && c != &']'),
                )
                .replace(',', " or ");
                if accepted.len() > 1 {
                    return format!("Invalid types expected: {opts:?} but got {got:?}");
                }
                format!("Invalid type expected: {opts:?} but got {got:?}")
            }

            VmError::NonExistentVar(name) => {
                "Couldnt find variable with name: ".to_string() + name.as_str()
            }
            VmError::InvalidAssignment(name) => {
                "Attempted to assign to non existent variable with name: ".to_string()
                    + name.as_str()
            }
            VmError::InvalidBinary(op, inv) => {
                format!("Cant do {op:?} operation with type {inv:?}")
            }
            VmError::InvalidUnary(op, inv) => format!("Cant do {op:?} operation with type {inv:?}"),
            VmError::InvalidArgSize(expected, got) => {
                let expected_txt = if expected == &1 {
                    "argument"
                } else {
                    "arguments"
                };
                let got_txt = if got == &1 { "argument" } else { "arguments" };
                format!(
                "Invalid argument size expected {expected:?} {expected_txt} but got {got:?} {got_txt}"
                )
            }

            VmError::Unspecified(msg) => msg.to_string(),
        }
    }
}

pub struct StackVM {
    proc: Vec<Spanned<StackOp>>,
    counter: usize,
    pub values: Vec<Value>,
}
impl StackVM {
    pub fn new(proc: impl Into<Vec<Spanned<StackOp>>>) -> Self {
        Self {
            proc: proc.into(),
            counter: 0,
            values: vec![],
        }
    }

    fn eval_unary_op(&mut self, op: UnaryOp, span: Span) -> VmRes<()> {
        todo!()
    }
    pub fn exec(&mut self) -> VmRes<Value> {
        let stacklen = self.proc.len();

        loop {
            let res = self.proc[self.counter].clone();
            self.exec_op(res)?;
            if self.counter > stacklen - 1 {
                break;
            }
        }
        return Ok(self.values[0].clone());
    }
    fn float_calc(&mut self, left: f64, right: f64, op: BinaryOp, span: Span) -> VmRes<()> {
        let result = match op {
            BinaryOp::Add => left + right,
            BinaryOp::Subtract => left - right,
            BinaryOp::Divide => left / right,
            BinaryOp::Multiply => left * right,
            BinaryOp::Modulo => left % right,

            _ => todo!(),
        };
        self.push(Value::Float(result));
        self.counter += 1;
        return Ok(());
    }
    fn int_calc(&mut self, left: i64, right: i64, op: BinaryOp, span: Span) -> VmRes<()> {
        let result = match op {
            BinaryOp::Add => left + right,
            BinaryOp::Subtract => left - right,
            BinaryOp::Divide => left / right,
            BinaryOp::Multiply => left * right,
            _ => todo!(),
        };
        self.push(Value::Int(result));
        self.counter += 1;
        return Ok(());
    }
    fn eval_binary_op(&mut self, op: BinaryOp, span: Span) -> VmRes<()> {
        match self.pop_pair() {
            (Value::Float(left), Value::Float(right)) => self.float_calc(left, right, op, span),
            (Value::Int(left), Value::Int(right)) => self.int_calc(left, right, op, span),
            _ => todo!(),
        }
    }

    fn exec_op(&mut self, op: Spanned<StackOp>) -> VmRes<()> {
        let span = op.span;
        match op.item {
            StackOp::Push(val) => {
                self.values.push(val);
                self.counter += 1;
                Ok(())
            }
            StackOp::Load(index) => {
                self.push(self.values[index].clone());
                self.counter += 1;
                Ok(())
            }
            StackOp::Store(index) => {
                let val = self.pop(span)?;
                self.values[index] = val;
                self.counter += 1;
                Ok(())
            }
            StackOp::Pop => {
                self.pop(span)?;
                self.counter += 1;
                Ok(())
            }
            StackOp::BinaryOp(binop) => self.eval_binary_op(binop, span),
            StackOp::UnaryOp(unary) => self.eval_unary_op(unary, span),
            StackOp::Goto(line, cond) => {
                if line > self.proc.len() {
                    return unspec!(
                        span,
                        "Invalid goto line! the stack len is {} but got {line}",
                        self.proc.len()
                    );
                }
                if cond {
                    self.counter = line;
                }
                Ok(())
            }
        }
    }
    fn pop(&mut self, span: Span) -> VmRes<Value> {
        let Some(val) = self.values.pop() else {
            return unspec!(span, "");
        };
        return Ok(val);
    }
    fn push(&mut self, value: Value) {
        self.values.push(value);
    }
    fn pop_pair(&mut self, spans: (Span, Span)) -> VmRes<(Value, Value)> {
        let pair = (self.pop(spans.0)?, self.pop(spans.1)?);
        Ok((pair.1, pair.0))
    }
}
