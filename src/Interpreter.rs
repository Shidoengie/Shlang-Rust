use crate::AstNodes;
use AstNodes::*;
struct Interpreter {
    program: Block,
}

impl Interpreter {
    pub fn new(program: Block) -> Self {
        return Interpreter { program };
    }
    fn num_convert(&self, num: Value) -> (f64, bool) {
        let mut out: (f64, bool) = (0.0, false);
        match num {
            Value::Num(val) => {
                out = (val, val != 0.0);
            }
            Value::Bool(cond) => {
                out = (0.0, cond);
            }
            invalid => {
                panic!("Invalid type{invalid:?}");
            }
        };
        return out;
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
    fn eval_binary_node(&mut self, bin_op: BinaryNode) -> Value {
        let left = self.eval_node(*bin_op.left);
        let right = self.eval_node(*bin_op.right);
        if left != right {
            match bin_op.kind {
                BinaryOp::ISDIFERENT => return Value::Bool(true),
                BinaryOp::ISEQUAL => return Value::Bool(false),
                _ => panic!("mixed types: {left:?} {left:?}"),
            }
        }
        match left {
            Value::Num(_) | Value::Bool(_) => return self.num_calc(bin_op.kind, left, right),
            Value::Str(_) => return self.str_calc(bin_op.kind, left, right),
            invalid => panic!("Invalid type in binary operation:{invalid:?}")
        }
    }
    fn eval_node(&mut self, expr: Node) -> Value {
        match expr {
            Node::Value(val) => {
                return *val;
            }
            Node::Block(body) => {
                todo!()
            }
            Node::Variable(var) => {
                todo!()
            }
            Node::Call(request) => {
                todo!()
            }
            Node::UnaryNode(unary_op) => {
                todo!()
            }
            Node::BinaryNode(bin_op) => {
                self.eval_binary_node(bin_op)
            }
            Node::Branch(branch) => {
                todo!()
            }
            Node::Declaration(declare) => {
                todo!()
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
