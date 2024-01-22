use crate::ast_nodes;
use crate::defaults;
use crate::lang_errors::*;
use crate::spans::*;
use ast_nodes::*;
use std::collections::HashMap;
use std::*;
type IError = InterpreterError;

#[derive(Clone, Debug)]
pub enum Control {
    Return(Value),
    Result(Value),
    Value(Value),
    Break,
    Continue,
}

impl Control {
    pub fn unwrap_val(&self) -> Value {
        match self {
            Self::Return(val) | Self::Result(val) | Self::Value(val) => val.clone(),
            _ => Value::Null,
        }
    }
}
type EvalResult = Result<Control, InterpreterError>;
const VOID: EvalResult = Ok(Control::Value(Value::Void));
const NULL: EvalResult = Ok(Control::Value(Value::Null));
#[derive(Debug)]
pub struct Interpreter {
    program: NodeStream,
    heap: Vec<Value>,
}
impl Interpreter {
    pub fn new(program: NodeStream) -> Self {
        Self {
            program,
            heap: vec![],
        }
    }
    pub fn execute(&mut self) -> EvalResult {
        let mut scope = self.parse_vars()?;
        let main = Node::Variable("main".to_string()).to_spanned((0, 0));
        let call = Call {
            callee: Box::new(main),
            args: vec![],
        };
        self.eval_call(call, &mut scope.clone(), &mut scope);
        return NULL;
    }
    pub fn parse_vars(&mut self) -> Result<Scope, IError> {
        let mut scope = Scope::default();

        self.eval_block_bodge(self.program.clone(), &mut scope)?;
        Ok(scope)
    }

    pub fn execute_node(node: NodeSpan) -> EvalResult {
        Self {
            program: vec![Node::DontResult.to_spanned((0, 0))],
            heap: vec![],
        }
        .eval_node(&node, &mut Scope::new_child_in(defaults::default_scope()))
    }
    fn eval_block_bodge(&mut self, block: NodeStream, parent: &mut Scope) -> EvalResult {
        let mut new_scope = Scope::new_child_in(parent.clone());
        if block.len() == 0 {
            return NULL;
        }
        for node in block {
            let result = self.eval_node(&node, &mut new_scope)?;

            if let Control::Value(_) = result {
                continue;
            }
            if new_scope.parent.is_none() {
                return Err(IError::InvalidControl(node.span));
            };
            *parent = new_scope;
            return Ok(result);
        }

        if new_scope.parent.is_none() {
            return Err(IError::InvalidControl((0, 0)));
        };
        *parent = new_scope;
        NULL
    }
    fn eval_block(&mut self, block: NodeStream, parent: &mut Scope) -> EvalResult {
        let mut new_scope = Scope::new_child_in(parent.clone());
        if block.len() == 0 {
            return NULL;
        }
        for node in block {
            let result = self.eval_node(&node, &mut new_scope)?;

            if let Control::Value(_) = result {
                continue;
            }
            let Some(mod_parent) = new_scope.parent else {
                return Err(IError::InvalidControl(node.span));
            };
            *parent = *mod_parent;
            return Ok(result);
        }

        let Some(mod_parent) = new_scope.parent else {
            return Err(IError::InvalidControl((0,0)));
        };
        *parent = *mod_parent;
        NULL
    }

    fn num_convert(&self, num: Value, span: Span) -> Result<(f64, bool), IError> {
        match num {
            Value::Num(val) => Ok((val, val != 0.0)),
            Value::Bool(cond) => Ok((cond as i8 as f64, cond)),
            invalid => Err(IError::InvalidType(
                vec![Type::Num, Type::Bool],
                invalid.get_type(),
                span,
            )),
        }
    }
    fn unary_calc(&self, node: UnaryNode, target: Value) -> EvalResult {
        match node.kind {
            UnaryOp::NEGATIVE => {
                let Value::Num(val) = target else {
                    return Err(IError::InvalidType(vec![Type::Num],target.get_type(), node.object.span));
                };
                Ok(Control::Value(Value::Num(-val)))
            }
            UnaryOp::NOT => Ok(Control::Value(Value::Bool(
                !self.num_convert(target, node.object.span)?.1,
            ))),
        }
    }
    fn num_calc(
        &self,
        node: BinaryNode,
        left_val: Value,
        right_val: Value,
    ) -> Result<Control, IError> {
        let (left, left_bool) = self.num_convert(left_val, node.left.span)?;
        let (right, right_bool) = self.num_convert(right_val, node.right.span)?;
        let res = match node.kind {
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
        };
        Ok(Control::Value(res))
    }
    fn str_calc(
        &self,
        node: BinaryNode,
        left_val: Value,
        right_val: Value,
    ) -> Result<Control, IError> {
        let Value::Str(left) = left_val else {unreachable!()};
        let Value::Str(right) = right_val else {unreachable!()};

        let res = match node.kind {
            BinaryOp::ADD => Value::Str(left + &right),
            BinaryOp::GREATER => Value::Bool(left > right),
            BinaryOp::GREATER_EQUAL => Value::Bool(left >= right),
            BinaryOp::LESSER => Value::Bool(left < right),
            BinaryOp::LESSER_EQUAL => Value::Bool(left <= right),
            BinaryOp::ISEQUAL => Value::Bool(left == right),
            BinaryOp::ISDIFERENT => Value::Bool(left != right),
            op => {
                return Err(IError::InvalidOp(
                    op,
                    Type::Str,
                    (node.left.span.1, node.right.span.0),
                ))
            }
        };
        Ok(Control::Value(res))
    }

    fn eval_binary_node(&mut self, bin_op: BinaryNode, parent: &mut Scope) -> EvalResult {
        let left = self.eval_node(&bin_op.left, parent)?.unwrap_val();
        let right = self.eval_node(&bin_op.right, parent)?.unwrap_val();
        let left_type = left.get_type();
        let right_type = right.get_type();
        match bin_op.kind {
            BinaryOp::ISDIFERENT => return Ok(Control::Value(Value::Bool(left != right))),
            BinaryOp::ISEQUAL => return Ok(Control::Value(Value::Bool(left == right))),
            _ => {}
        }

        if left_type != right_type && !(left.is_numeric() && right.is_numeric()) {
            return Err(IError::MixedTypes(
                left_type,
                right_type,
                (bin_op.left.span.0 - 1, bin_op.right.span.1),
            ));
        }
        match left_type {
            Type::Num | Type::Bool => {
                return self.num_calc(bin_op, left, right);
            }
            Type::Str => {
                return self.str_calc(bin_op, left, right);
            }
            _ => {}
        }

        Err(IError::InvalidBinary(left_type, bin_op.left.span))
    }
    fn unwrap_var(&self, flow: Control, span: Span) -> Result<Value, IError> {
        let value = flow.unwrap_val();
        if value.is_void() {
            return Err(IError::VoidAssignment(span));
        }
        Ok(value)
    }
    fn declare(&mut self, request: Declaration, parent: &mut Scope) -> EvalResult {
        let init_val = self.eval_node(&request.value, parent)?;
        let unwrapped = self.unwrap_var(init_val, request.value.span)?;
        parent.define(request.var_name, unwrapped);
        VOID
    }

    fn assign_to_name(
        &mut self,
        name: String,
        value: Value,
        span: Span,
        target: &mut Scope,
    ) -> EvalResult {
        if target
            .assign(name.clone(), self.unwrap_var(Control::Value(value), span)?)
            .is_none()
        {
            return Err(IError::InvalidAssignment(name, span));
        }
        VOID
    }
    fn assign(&mut self, request: Assignment, parent: &mut Scope) -> EvalResult {
        let init_val = self.eval_node(&request.value, parent)?.unwrap_val();
        match request.clone().target.unspanned {
            Node::Variable(var) => self.assign_to_name(var, init_val, request.target.span, parent),

            _ => todo!(),
        }
    }
    fn default_scope(&self, name: String, span: Span) -> EvalResult {
        let maybe_result = Scope::new(None, defaults::var_map()).get_var(&name);
        let Some(result) = maybe_result else {
            return Err(IError::NonExistentVar(name, span));
        };
        Ok(Control::Value(result))
    }
    fn eval_var(&self, name: String, span: Span, env: &Scope) -> EvalResult {
        let maybe_result = env.get_var(&name);
        let Some(result) = maybe_result else {
            return self.default_scope(name, span);
        };
        Ok(Control::Value(result))
    }
    fn call_builtin(
        &mut self,
        called: BuiltinFunc,
        arg_values: ValueStream,
        span: Span,
    ) -> EvalResult {
        if called.arg_size != -1 && arg_values.len() as i16 != called.arg_size {
            return Err(IError::InvalidArgSize(
                called.arg_size as u32,
                arg_values.len() as u32,
                span,
            ));
        }
        let result = (called.function)(arg_values);
        Ok(Control::Value(result))
    }
    fn build_args(
        &mut self,
        func: &Function,
        arg_spans: Vec<Span>,
        arg_values: ValueStream,
    ) -> NodeStream {
        let mut arg_decl: NodeStream = vec![];
        let mut func_block = func.block.clone();
        for (index, name) in func.args.iter().enumerate() {
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
        arg_decl.append(&mut func_block);
        arg_decl
    }
    fn call_func(
        &mut self,
        mut called: Function,
        arg_values: ValueStream,
        arg_spans: Vec<Span>,
        span: Span,
        parent: &mut Scope,
    ) -> EvalResult {
        if arg_values.len() != called.args.len() {
            return Err(IError::InvalidArgSize(
                called.args.len() as u32,
                arg_values.len() as u32,
                span,
            ));
        }
        called.block = self.build_args(&called, arg_spans, arg_values);
        let result = self.eval_block(called.block, parent)?;
        if let Control::Break | Control::Continue = result {
            return Err(IError::InvalidControl(span));
        }
        Ok(Control::Value(result.unwrap_val()))
    }
    fn eval_call(&mut self, request: Call, base_env: &mut Scope, parent: &mut Scope) -> EvalResult {
        let func_val = self.eval_node(&request.callee, parent)?.unwrap_val();
        let func_type = func_val.get_type();
        if func_type != Type::Function {
            return Err(IError::InvalidType(
                vec![Type::Function],
                func_type,
                request.callee.span,
            ));
        }
        let call_args = request.args;
        let mut arg_values: ValueStream = vec![];
        let mut arg_spans: Vec<Span> = vec![];
        for arg in call_args {
            arg_spans.push(arg.span);
            let argument = self.eval_node(&arg, base_env)?.unwrap_val();
            arg_values.push(argument);
        }
        let arg_span = if !arg_spans.is_empty() {
            (request.callee.span.1 + 1, arg_spans.last().unwrap().1)
        } else {
            (request.callee.span.1 + 1, request.callee.span.1 + 2)
        };
        match func_val {
            Value::BuiltinFunc(func) => self.call_builtin(func, arg_values, arg_span),
            Value::Function(called) => {
                self.call_func(called, arg_values, arg_spans, request.callee.span, parent)
            }
            _ => panic!(),
        }
    }
    fn branch(&mut self, branch: Branch, parent: &mut Scope) -> EvalResult {
        let condition = self.eval_node(&branch.condition, parent)?.unwrap_val();
        let cond_val = self.num_convert(condition, branch.condition.span)?.1;
        if cond_val {
            return self.eval_block(branch.if_block, parent);
        }
        let Some(else_block) = branch.else_block else {
            return NULL;
        };
        self.eval_block(else_block, parent)
    }
    fn eval_loop(&mut self, loop_block: NodeStream, parent: &mut Scope) -> EvalResult {
        loop {
            let result = self.eval_block(loop_block.clone(), parent)?;
            match result {
                Control::Break => return NULL,
                Control::Continue | Control::Value(_) => continue,
                _ => return Ok(result),
            }
        }
    }
    fn eval_while_loop(&mut self, loop_node: While, parent: &mut Scope) -> EvalResult {
        loop {
            let condition = self.eval_node(&loop_node.condition, parent)?.unwrap_val();
            let cond_val = self.num_convert(condition, loop_node.condition.span)?.1;
            if !cond_val {
                return NULL;
            }
            let result = self.eval_block(loop_node.proc.clone(), parent)?;
            match result {
                Control::Break => return NULL,
                Control::Continue | Control::Value(_) => continue,
                _ => return Ok(result),
            }
        }
    }

    fn eval_node(&mut self, node: &NodeSpan, parent: &mut Scope) -> EvalResult {
        let (span, expr) = (node.span, node.unspanned.clone());
        match expr {
            Node::Value(val) => {
                // let kind = val.clone().get_type();
                // Ok((*val, kind))
                let kind = val.get_type();
                let Type::Ref(id) = kind else {return Ok(Control::Value(val));};
                let new_val = &self.heap[id];
                Ok(Control::Value(new_val.clone()))
            }

            Node::Variable(var) => self.eval_var(var, span, parent),
            Node::Call(request) => self.eval_call(request, &mut parent.clone(), parent),
            Node::UnaryNode(unary_op) => {
                let target = self.eval_node(&unary_op.object, parent)?;
                self.unary_calc(unary_op, target.unwrap_val())
            }
            Node::BinaryNode(bin_op) => self.eval_binary_node(bin_op, parent),
            Node::Branch(branch) => self.branch(branch, parent),
            Node::ReturnNode(expr) => {
                let evaluated = self.eval_node(&expr, parent)?.unwrap_val();
                Ok(Control::Return(evaluated))
            }
            Node::ResultNode(expr) => {
                let evaluated = self.eval_node(&expr, parent)?.unwrap_val();
                Ok(Control::Result(evaluated))
            }
            Node::BreakNode => Ok(Control::Break),
            Node::ContinueNode => Ok(Control::Continue),
            Node::Declaration(declaration) => self.declare(declaration, parent),
            Node::Assignment(ass) => self.assign(ass, parent),
            Node::While(obj) => self.eval_while_loop(obj, parent),
            Node::Loop(obj) => self.eval_loop(obj, parent),
            Node::DoBlock(block) => self.eval_block(block, parent),

            _ => {
                todo!()
            }
        }
    }
}
