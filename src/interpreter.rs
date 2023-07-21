use crate::ast_nodes;
use crate::defaults;
use std::collections::HashMap;
use std::*;
use ast_nodes::*;
const VOID: TypedValue = (Value::Void, Type::Void);
pub struct Interpreter {
    program: Block,
    current: Scope,
}
impl Interpreter {
    pub fn new(program: Block) -> Self {
        let current = Scope {
            parent: None,
            
            var_map: defaults::var_map(),
        };
        return Interpreter { program, current };
    }
    pub fn execute(&mut self) {
        self.eval_block(self.program.clone());
    }

    fn eval_block(&mut self, block: Block) -> TypedValue {
        let cur = Some(Box::new(self.current.clone()));
        let new_scope = Scope::new(cur, HashMap::from([]));
        self.current = new_scope;
        if block.body.len() == 0 {
            return VOID;
        }
        let body = *block.body;
        for node in body {
            let Value::Control(result) = self.eval_node(node).0 else {continue;};
            let parent = self.current.parent.clone();
            self.current = *parent.expect("Invalid control outside block");
            return (Value::Control(result), Type::Never);
        }

        let parent = self.current.parent.clone();
        self.current = *parent.expect("Invalid control outside block");
        return VOID;
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
    fn unary_calc(&self, kind: UnaryOp, target: TypedValue) -> TypedValue {
        match kind {
            UnaryOp::NEGATIVE => {
                let Value::Num(val) = target.0 else {panic!("Invalid Type expected Num but got {:?}",target.1)};
                return (Value::Num(-val),Type::Num);
            }
            UnaryOp::NOT => {
                return (Value::Bool(!self.num_convert(target.0).1),Type::Bool);
            }
        }
    }
    fn binary_calc(&self, kind: BinaryOp, left_val: Value, right_val: Value) -> Value {
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

    fn eval_binary_node(&mut self, bin_op: BinaryNode) -> TypedValue {
        let (left, left_type) = self.eval_node(*bin_op.left);
        let (right, right_type) = self.eval_node(*bin_op.right);
        if left_type != right_type {
            let are_numerical = left_type.is_numeric() && right_type.is_numeric();
            match bin_op.kind {
                BinaryOp::ISDIFERENT => return (Value::Bool(true), Type::Bool),
                BinaryOp::ISEQUAL => return (Value::Bool(true), Type::Bool),
                _ => {
                    if !are_numerical {
                        panic!("mixed types: {left:?} {right:?}")
                    }
                }
            }
        }
        match left_type {
            Type::Num | Type::Bool => {
                return (self.binary_calc(bin_op.kind, left, right), left_type)
            }
            Type::Str => return (self.str_calc(bin_op.kind, left, right), left_type),
            invalid => panic!("Invalid type in binary operation:{invalid:?}"),
        }
    }
    fn unwrap_var(&self, value: TypedValue) -> (Value, Type) {
        if value.1 == Type::Void {
            panic!("Cannot assign void to variable");
        }
        let Value::Control(control) = value.0 else {
            return value;
        };
        let Control::Result(res_val,res_type) = control else {
            panic!("Unexpected controlflow node");
        };
        if res_type == Type::Void {
            panic!("Cannot assign void to variable");
        }
        return (*res_val, res_type);
    }

    fn declare(&mut self, request: Declaration) {
        let init_val = self.eval_node(*request.value);
        self.current
            .define(request.var_name, self.unwrap_var(init_val).0);
    }
    fn assign(&mut self, request: Assignment) {
        let init_val = self.eval_node(*request.value);
        if self
            .current
            .assign(request.var_name, self.unwrap_var(init_val).0)
            .is_none()
        {
            panic!("Attempted to assign to a non existent variable")
        }
    }
    fn eval_node(&mut self, node: NodeSpan) -> TypedValue {
        let (span,expr) = (node.span,node.unspanned);
        match expr {
            Node::Value(val) => (*val.clone(), val.get_type()),
            Node::Block(body) => self.eval_block(body),
            Node::Variable(var) => {
                let result = self.current.get_var(var.name).expect("undefined variable");
                let res_type = result.get_type();
                return (result, res_type.clone());
            }
            Node::Call(request) => {
                todo!()
            }
            Node::UnaryNode(unary_op) => {
                let (target, target_type) = self.eval_node(*unary_op.object);
                self.unary_calc(unary_op.kind, (target, target_type))
            }
            Node::BinaryNode(bin_op) => self.eval_binary_node(bin_op),
            Node::Branch(branch) => {
                todo!()
            }
            Node::ReturnNode(expr) => {
                let evaluated = self.eval_node(*expr);
                let result: Value = Control::Return(Box::new(evaluated.0), evaluated.1).into();
                (result, Type::Never)
            }
            Node::ResultNode(expr) => {
                let evaluated = self.eval_node(*expr);
                let result: Value = Control::Result(Box::new(evaluated.0), evaluated.1).into();
                (result, Type::Never)
            }
            Node::BreakNode => (Control::Break.into(), Type::Never),
            Node::Declaration(declaration) => {
                self.declare(declaration);
                VOID
            }
            Node::Assignment(ass) => {
                self.assign(ass);
                VOID
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
