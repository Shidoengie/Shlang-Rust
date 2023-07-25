use crate::ast_nodes;
use crate::defaults;
use crate::lang_errors::*;
use crate::spans::*;
use ast_nodes::*;
use std::collections::HashMap;
use std::*;
const VOID: Result<TypedValue, ()> = Ok((Value::Void, Type::Void));
pub struct Interpreter {
    program: Block,
    current: Scope,
    err_out: LangError,
}
impl Interpreter {
    pub fn new(program: Block, input: String) -> Self {
        let current = Scope {
            parent: None,

            var_map: defaults::var_map(),
        };
        dbg!(current.clone());
        return Interpreter {
            program,
            current,
            err_out: LangError::new(input),
        };
    }
    fn emit_err(&self, msg: String, span: Span) {
        self.err_out.emit(msg.as_str(), span);
    }
    pub fn execute(&mut self) -> Result<TypedValue, ()> {
        return self.eval_block(self.program.clone());
    }

    fn eval_block(&mut self, block: Block) -> Result<TypedValue, ()> {
        let cur = Some(Box::new(self.current.clone()));
        let new_scope = Scope::new(cur, HashMap::from([]));
        self.current = new_scope;
        if block.body.len() == 0 {
            return VOID;
        }
        let body = *block.body;
        for node in body {
            let Value::Control(result) = self.eval_node(&node)?.0 else {continue;};
            let Some(parent) = self.current.parent.clone() else {
                self.err_out.emit("Invalid control outside block", node.span);
                return Err(());
            };
            self.current = *parent;
            return Ok((Value::Control(result), Type::Never));
        }

        let Some(parent) = self.current.parent.clone() else {
            self.err_out.emit("Invalid control outside block",(0,0));
            return Err(());
        };
        self.current = *parent;
        return VOID;
    }
    fn num_convert(&self, num: Value, span: Span) -> Result<(f64, bool), ()> {
        match num {
            Value::Num(val) => Ok((val, val != 0.0)),
            Value::Bool(cond) => Ok((cond as i8 as f64, cond)),
            invalid => {
                self.emit_err(format!("Invalid type{invalid:?}"), span);
                return Err(());
            }
        }
    }
    fn unary_calc(&self, node: UnaryNode, target: TypedValue) -> Result<TypedValue, ()> {
        match node.kind {
            UnaryOp::NEGATIVE => {
                let Value::Num(val) = target.0 else {
                    self.emit_err(
                        format!("Invalid Type expected Num but got {:?}",target.1),
                        node.object.span
                    );
                    return Err(());
                };
                return Ok((Value::Num(-val), Type::Num));
            }
            UnaryOp::NOT => {
                return Ok((
                    Value::Bool(!self.num_convert(target.0, node.object.span)?.1),
                    Type::Bool,
                ));
            }
        }
    }
    fn binary_calc(
        &self,
        node: BinaryNode,
        left_val: Value,
        right_val: Value,
    ) -> Result<Value, ()> {
        let (left, left_bool) = self.num_convert(left_val, node.left.span)?;
        let (right, right_bool) = self.num_convert(right_val, node.right.span)?;
        match node.kind {
            BinaryOp::ADD => Ok(Value::Num(left + right)),
            BinaryOp::SUBTRACT => Ok(Value::Num(left - right)),
            BinaryOp::MULTIPLY => Ok(Value::Num(left * right)),
            BinaryOp::DIVIDE => Ok(Value::Num(left / right)),
            BinaryOp::MODULO => Ok(Value::Num(left % right)),
            BinaryOp::GREATER => Ok(Value::Bool(left > right)),
            BinaryOp::GREATER_EQUAL => Ok(Value::Bool(left >= right)),
            BinaryOp::LESSER => Ok(Value::Bool(left < right)),
            BinaryOp::LESSER_EQUAL => Ok(Value::Bool(left <= right)),
            BinaryOp::AND => Ok(Value::Bool(left_bool && right_bool)),
            BinaryOp::OR => Ok(Value::Bool(left_bool || right_bool)),
            BinaryOp::ISEQUAL => Ok(Value::Bool(left == right)),
            BinaryOp::ISDIFERENT => Ok(Value::Bool(left != right)),
        }
    }
    fn str_calc(&self, node: BinaryNode, left_val: Value, right_val: Value) -> Result<Value, ()> {
        let Value::Str(left) = left_val else {return Err(());};
        let Value::Str(right) = right_val else {return Err(());};

        match node.kind {
            BinaryOp::ADD => Ok(Value::Str(left + &right)),
            BinaryOp::GREATER => Ok(Value::Bool(left > right)),
            BinaryOp::GREATER_EQUAL => Ok(Value::Bool(left >= right)),
            BinaryOp::LESSER => Ok(Value::Bool(left < right)),
            BinaryOp::LESSER_EQUAL => Ok(Value::Bool(left <= right)),
            BinaryOp::ISEQUAL => Ok(Value::Bool(left == right)),
            BinaryOp::ISDIFERENT => Ok(Value::Bool(left != right)),
            op => {
                self.emit_err(
                    format!("Cant do {op:?} operation with strings"),
                    (node.left.span.1, node.right.span.0),
                );
                Err(())
            }
        }
    }

    fn eval_binary_node(&mut self, bin_op: BinaryNode) -> Result<TypedValue, ()> {
        let (left, left_type) = self.eval_node(&*bin_op.left)?;
        let (right, right_type) = self.eval_node(&*bin_op.right)?;
        if left_type != right_type {
            let are_numerical = left_type.is_numeric() && right_type.is_numeric();
            match bin_op.kind {
                BinaryOp::ISDIFERENT => return Ok((Value::Bool(true), Type::Bool)),
                BinaryOp::ISEQUAL => return Ok((Value::Bool(false), Type::Bool)),
                _ => {
                    if !are_numerical {
                        panic!("mixed types: {left:?} {right:?}")
                    }
                }
            }
        }
        match left_type {
            Type::Num | Type::Bool => {
                let result = (self.binary_calc(bin_op, left, right)?, left_type);
                Ok(result)
            }
            Type::Str => {
                let result = (self.str_calc(bin_op, left, right)?, left_type);
                Ok(result)
            }
            invalid => {
                self.emit_err(
                    format!("Invalid type in binary operation: {invalid:?}"),
                    bin_op.left.span,
                );
                return Err(());
            }
        }
    }
    fn unwrap_var(&self, value: TypedValue, span: Span) -> Result<TypedValue, ()> {
        if value.1 == Type::Void {
            self.err_out.emit("Cannot assign void to variable", span);
            return Err(());
        }
        let Value::Control(control) = value.0 else {
            return Ok(value);
        };
        let Control::Result(res_val,res_type) = control else {
            self.err_out.emit("Unexpected control flow node",span);
            return Err(());
        };
        if res_type == Type::Void {
            self.err_out.emit("Cannot assign void to variable", span);
            return Err(());
        }
        return Ok((*res_val, res_type));
    }
    fn declare(&mut self, request: Declaration) -> Result<(), ()> {
        let init_val = self.eval_node(&*request.value)?;
        let unwrapped = self.unwrap_var(init_val, request.value.span)?;
        self.current.define(request.var_name, unwrapped.0);
        return Ok(());
    }
    fn assign(&mut self, request: Assignment) -> Result<(), ()> {
        let init_val = self.eval_node(&*request.value)?;
        if self
            .current
            .assign(
                request.var_name,
                self.unwrap_var(init_val, request.value.span)?.0,
            )
            .is_none()
        {
            self.err_out.emit(
                "Attempted to assign to a non existent variable",
                request.value.span,
            );
            return Err(());
        }
        return Ok(());
    }
    fn eval_var(&mut self, var: Variable, span: Span) -> Result<TypedValue, ()> {
        let maybe_result = self.current.get_var(var.name);
        let Some(result) = maybe_result else{
            self.emit_err("Non existing variable".to_string(), span);
            return Err(());
        };
        let res_type = result.get_type();
        return Ok((result, res_type.clone()));
    }
    fn call_builtin(
        &mut self,
        called: BuiltinFunc,
        arg_values: ValueStream,
        span: Span,
    ) -> Result<TypedValue, ()> {
        if called.arg_size != -1 && arg_values.len() as i16 != called.arg_size {
            self.emit_err(
                format!(
                    "Not enough arguments expected {:?} but got {:?}",
                    called.arg_size,
                    arg_values.len()
                ),
                span,
            );
            return Err(());
        }
        let result = (called.function)(arg_values);
        let result_type = result.get_type();
        return Ok((result, result_type));
    }
    fn call_func(
        &mut self,
        mut called: Function,
        arg_values: ValueStream,
        arg_spans: Vec<Span>,
        span: Span,
    ) -> Result<TypedValue, ()> {
        let Node::Block(mut func_block) = called.block.unspanned else{
            self.err_out.emit("HOW", span);
            return Err(());
        };
        if arg_values.len() != called.args.len() {
            self.emit_err(
                format!(
                    "Not enough arguments expected {:?} but got {:?}",
                    called.args.len(),
                    arg_values.len()
                ),
                span,
            );
            return Err(());
        }
        let mut arg_decl: NodeStream = vec![];
        for (index, name) in called.args.iter().enumerate() {
            let span = arg_spans[index];
            let var = Box::new(NodeSpan::new(arg_values[index].clone().into(), span));
            arg_decl.push(
                Declaration {
                    var_name: name.to_string(),
                    value: var,
                }
                .to_nodespan(span),
            );
        }

        arg_decl.append(&mut func_block.body);
        called.block.unspanned = Block {
            body: Box::new(arg_decl),
        }
        .into();
        let (result, result_type) = self.eval_node(&*called.block)?;
        let Value::Control(flow) = result else {
            return Ok((result,result_type));
        };
        match flow {
            Control::Break => {
                self.err_out.emit("Unexpected control flow node", span);
                return Err(());
            }
            Control::Return(val, kind) => {
                return Ok((*val, kind));
            }
            Control::Result(val, kind) => {
                return Ok((*val, kind));
            }
        }
    }
    fn eval_call(&mut self, request: Call) -> Result<TypedValue, ()> {
        let (func_val, kind) = self.eval_node(&request.callee)?;
        if kind != Type::Function {
            self.emit_err(
                format!("Invalid Function call expected a function but got {kind:?}"),
                request.callee.span,
            );
            return Err(());
        }
        let call_args = *request.args;
        let mut arg_values: ValueStream = vec![];
        let mut arg_spans: Vec<Span> = vec![];
        for a in call_args {
            arg_spans.push(a.span);
            let argument = self.eval_node(&a)?.0;
            arg_values.push(argument);
        }
        let arg_span = if !arg_spans.is_empty() {
            (request.callee.span.1 + 1, arg_spans.last().unwrap().1)
        } else {
            (request.callee.span.1 + 1, request.callee.span.1 + 2)
        };
        match func_val {
            Value::BuiltinFunc(func) => {
                return self.call_builtin(func, arg_values, arg_span);
            }
            Value::Function(called) => {
                return self.call_func(called, arg_values, arg_spans, request.callee.span);
            }
            _ => panic!(),
        }
    }
    fn eval_node(&mut self, node: &NodeSpan) -> Result<TypedValue, ()> {
        
        let (span, expr) = (node.span, node.unspanned.clone());
        match expr {
            Node::Value(val) => Ok((*val.clone(), val.get_type())),
            Node::Block(body) => self.eval_block(body),
            Node::Variable(var) => self.eval_var(var, span),
            Node::Call(request) => self.eval_call(request),
            Node::UnaryNode(unary_op) => {
                let (target, target_type) = self.eval_node(&*unary_op.object)?;
                self.unary_calc(unary_op, (target, target_type))
            }
            Node::BinaryNode(bin_op) => self.eval_binary_node(bin_op),
            Node::Branch(branch) => {
                todo!()
            }
            Node::ReturnNode(expr) => {
                let evaluated = self.eval_node(&*expr)?;
                let result: Value = Control::Return(Box::new(evaluated.0), evaluated.1).into();
                Ok((result, Type::Never))
            }
            Node::ResultNode(expr) => {
                let evaluated = self.eval_node(&*expr)?;
                let result: Value = Control::Result(Box::new(evaluated.0), evaluated.1).into();
                Ok((result, Type::Never))
            }
            Node::BreakNode => Ok((Control::Break.into(), Type::Never)),
            Node::Declaration(declaration) => {
                self.declare(declaration)?;
                VOID
            }
            Node::Assignment(ass) => {
                self.assign(ass)?;
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