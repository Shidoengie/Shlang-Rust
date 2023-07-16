use std::collections::HashMap;

use crate::AstNodes;
use crate::Defaults;
use AstNodes::*;
pub struct Interpreter {
    program: Block,
    current: Scope,
}

impl Interpreter {
    pub fn new(program: Block) -> Self {
        let current = Scope {
            parent: None,
            var_map: Defaults::var_map(),
        };
        return Interpreter { program, current };
    }
    pub fn execute(&mut self) {
        self.eval_block(self.program.clone());
    }
    fn eval_block(&mut self, block: Block) -> Value {
        let cur = Some(Box::new(self.current.clone()));
        let new_scope = Scope::new(cur, HashMap::from([]));
        self.current = new_scope;
        if block.body.len() == 0 {
            return Value::Null;
        }
        let body = *block.body;
        for node in body {
            let Value::Control(result) = self.eval_node(node) else {continue;};
            let parent = self.current.parent.clone();
            self.current = *parent.expect("Invalid control outside block");
        }
        let parent = self.current.parent.clone();
        self.current = *parent.expect("Invalid control outside block");
        return Value::Null;
    }
    fn num_convert(&self, num: Value) -> (f64, bool) {
        match num {
            Value::Num(val) => (val, val != 0.0),
            Value::Bool(cond) => (cond as i8 as f64, cond),
            invalid => {
                panic!("Invalid type{invalid:?}");
            }
        }
    }

    fn num_calc(&self, kind: BinaryOp, left_val: Value, right_val: Value) -> Value {
        let (left, left_bool) = self.num_convert(left_val);
        let (right, right_bool) = self.num_convert(right_val);
        match kind {
            BinaryOp::ADD => Value::Num(left + right),
            BinaryOp::SUBTRACT => Value::Num(left - right),
            BinaryOp::MULTIPLY => Value::Num(left * right),
            BinaryOp::DIVIDE => Value::Num(left / right),
            BinaryOp::MODULO => Value::Num(left % right),
            BinaryOp::GREATER => Value::Bool(left > right),
            BinaryOp::GREATER_EQUAL => Value::Bool(left >= right),
            BinaryOp::LESSER => Value::Bool(left < right),
            BinaryOp::LESSER_EQUAL => Value::Bool(left <= right),
            BinaryOp::AND => Value::Bool(left_bool && right_bool),
            BinaryOp::OR => Value::Bool(left_bool || right_bool),
            BinaryOp::ISEQUAL => Value::Bool(left == right),
            BinaryOp::ISDIFERENT => Value::Bool(left != right),
            _ => todo!(),
        }
    }
    fn str_calc(&self, kind: BinaryOp, left_val: Value, right_val: Value) -> Value {
        let Value::Str(left) = left_val else {panic!()};
        let Value::Str(right) = right_val else {panic!()};

        match kind {
            BinaryOp::ADD => Value::Str(left + &right),
            BinaryOp::GREATER => Value::Bool(left > right),
            BinaryOp::GREATER_EQUAL => Value::Bool(left >= right),
            BinaryOp::LESSER => Value::Bool(left < right),
            BinaryOp::LESSER_EQUAL => Value::Bool(left <= right),
            BinaryOp::ISEQUAL => Value::Bool(left == right),
            BinaryOp::ISDIFERENT => Value::Bool(left != right),
            op => panic!("Cant do {op:?} operation with strings"),
        }
    }
    fn declare(&mut self, request: Declaration) {
        let init_val = self.eval_node(*request.value);
        let Value::Control(control) = init_val else {
            self.current.define(request.var_name, init_val);
            return;
        };
        let Control::Result(val) = control else {
            panic!("Unexpected controlflow node");
        };
        self.current.define(request.var_name, *val);
    }
    fn eval_binary_node(&mut self, bin_op: BinaryNode) -> Value {
        let left = self.eval_node(*bin_op.left);
        let right = self.eval_node(*bin_op.right);

        if left != right {
            let is_numerical = (matches!(left, Value::Num(_)) && matches!(right, Value::Bool(_))
                || matches!(left, Value::Bool(_)) && matches!(right, Value::Num(_)));
            match bin_op.kind {
                BinaryOp::ISDIFERENT => return Value::Bool(true),
                BinaryOp::ISEQUAL => return Value::Bool(false),
                _ => {
                    if !is_numerical {
                        panic!("mixed types: {left:?} {right:?}")
                    }
                }
            }
        }
        match left {
            Value::Num(_) | Value::Bool(_) => return self.num_calc(bin_op.kind, left, right),
            Value::Str(_) => return self.str_calc(bin_op.kind, left, right),
            invalid => panic!("Invalid type in binary operation:{invalid:?}"),
        }
    }
    fn eval_node(&mut self, expr: Node) -> Value {
        match expr {
            Node::Value(val) => *val,
            Node::Block(body) => self.eval_block(body),
            Node::Variable(var) => {
                todo!()
            }
            Node::Call(request) => {
                todo!()
            }
            Node::UnaryNode(unary_op) => {
                todo!()
            }
            Node::BinaryNode(bin_op) => self.eval_binary_node(bin_op),
            Node::Branch(branch) => {
                todo!()
            }
            Node::ReturnNode(expr) => Control::Result(Box::new(self.eval_node(*expr))).into(),
            Node::Declaration(declaration) => {
                self.declare(declaration);
                Value::Null
            }
            Node::Assignment(ass) => {
                todo!()
            }
            Node::While(obj) => {
                todo!()
            }
            Node::Loop(obj) => {
                todo!()
            }
            _ => {
                todo!()
            }
        }
    }
}
