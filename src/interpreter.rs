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

macro_rules! unwrap_val {
    ($val:expr) => {
        match $val {
            Control::Result(inner) | Control::Value(inner) => inner.clone(),
            Control::Return(inner) => return Ok(Control::Return(inner.clone())),
            _ => Value::Null,
        }
    };
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
    pub fn execute(&mut self) -> Result<Value, IError> {
        self.execute_with(&mut Scope::default())
    }
    pub fn execute_with(&mut self, scope: &mut Scope) -> Result<Value, IError> {
        if self.program.len() == 0 {
            return Ok(Value::Null);
        }
        let mut last = Value::Null;
        for node in self.program.clone() {
            if let Control::Value(val) = self.eval_node(&node, scope)? {
                last = val;
                continue;
            }
            return Err(IError::InvalidControl(node.span));
        }
        let main = Node::Variable("main".to_string()).to_spanned((0, 0));
        let call = Call {
            callee: Box::new(main),
            args: vec![],
        };
        self.eval_call(call, &mut scope.clone(), scope);
        Ok(last)
    }
    pub fn parse_vars(&mut self) -> Result<Scope, IError> {
        return self.program_vars(self.program.clone());
    }

    pub fn execute_node(node: NodeSpan) -> EvalResult {
        Self {
            program: vec![Node::DontResult.to_spanned((0, 0))],
            heap: vec![],
        }
        .eval_node(&node, &mut Scope::new_child_in(defaults::default_scope()))
    }

    fn program_vars(&mut self, block: NodeStream) -> Result<Scope, IError> {
        let mut new_scope = Scope::default();
        if block.len() == 0 {
            return Ok(Scope::default());
        }
        for node in block {
            if let Control::Value(_) = self.eval_node(&node, &mut new_scope)? {
                continue;
            }
            return Err(IError::InvalidControl(node.span));
        }
        Ok(new_scope)
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
        let left = unwrap_val!(self.eval_node(&bin_op.left, parent)?);
        let right = unwrap_val!(self.eval_node(&bin_op.right, parent)?);
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

    fn declare(&mut self, request: Declaration, parent: &mut Scope) -> EvalResult {
        let value = unwrap_val!(self.eval_node(&request.value, parent)?);

        if value.is_void() {
            return Err(IError::VoidAssignment(request.value.span));
        }
        parent.define(request.var_name, value);
        VOID
    }
    fn get_struct_id(&self, val: &Value) -> usize {
        let Value::StructRef(id) = val else {panic!()};
        *id
    }

    fn assign_to_access(
        &mut self,
        request: FieldAccess,
        value: Value,
        parent: &mut Scope,
    ) -> EvalResult {
        let target = unwrap_val!(self.eval_node(&request.target, parent)?);
        let id = self.get_struct_id(&target);
        let Value::Struct(mut obj) = self.heap[id].clone() else {panic!()};
        match request.requested.unspanned {
            Node::FieldAccess(access) => {
                self.assign_to_access(access, value, &mut obj.env)?;
                self.heap.insert(id, Value::Struct(obj));
                VOID
            }
            Node::Variable(var) => {
                self.assign_to_name(var, value, request.requested.span, &mut obj.env)?;
                self.heap.insert(id, Value::Struct(obj));
                VOID
            }
            _ => panic!(),
        }
    }

    fn assign_to_name(
        &mut self,
        name: String,
        value: Value,
        span: Span,
        target: &mut Scope,
    ) -> EvalResult {
        if value.is_void() {
            return Err(IError::VoidAssignment(span));
        }
        if target.assign(name.clone(), value).is_none() {
            return Err(IError::InvalidAssignment(name, span));
        }
        VOID
    }
    fn assign(&mut self, request: Assignment, parent: &mut Scope) -> EvalResult {
        let init_val = unwrap_val!(self.eval_node(&request.value, parent)?);
        match request.clone().target.unspanned {
            Node::Variable(var) => self.assign_to_name(var, init_val, request.target.span, parent),

            Node::FieldAccess(field) => {
                self.assign_to_access(field, init_val, parent)?;
                VOID
            }
            _ => todo!(),
        }
    }
    fn default_scope(&self, name: String, span: Span) -> EvalResult {
        let maybe_result = Scope::new(None, defaults::var_map(), HashMap::from([])).get_var(&name);
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
        parent: &mut Scope,
    ) -> EvalResult {
        if called.arg_size != -1 && arg_values.len() as i16 != called.arg_size {
            return Err(IError::InvalidArgSize(
                called.arg_size as u32,
                arg_values.len() as u32,
                span,
            ));
        }
        let result = (called.function)(parent.vars.clone(), arg_values);
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

        match result {
            Control::Continue | Control::Break => return Err(IError::InvalidControl(span)),
            Control::Result(val) | Control::Return(val) | Control::Value(val) => {
                return Ok(Control::Value(val))
            }
        }
    }
    fn eval_call(&mut self, request: Call, base_env: &mut Scope, parent: &mut Scope) -> EvalResult {
        let func_val = unwrap_val!(self.eval_node(&request.callee, parent)?);
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
            let argument = self.eval_node(&arg, base_env)?;
            arg_values.push(unwrap_val!(argument));
        }
        let arg_span = if !arg_spans.is_empty() {
            (request.callee.span.1 + 1, arg_spans.last().unwrap().1)
        } else {
            (request.callee.span.1 + 1, request.callee.span.1 + 2)
        };
        match func_val {
            Value::BuiltinFunc(func) => self.call_builtin(func, arg_values, arg_span, parent),
            Value::Function(called) => {
                self.call_func(called, arg_values, arg_spans, request.callee.span, parent)
            }
            _ => panic!(),
        }
    }
    fn branch(&mut self, branch: Branch, parent: &mut Scope) -> EvalResult {
        let condition = self.eval_node(&branch.condition, parent)?;
        let cond_val = self
            .num_convert(unwrap_val!(condition), branch.condition.span)?
            .1;
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
            let condition = self.eval_node(&loop_node.condition, parent)?;
            let cond_val = self
                .num_convert(unwrap_val!(condition), loop_node.condition.span)?
                .1;
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
    fn eval_access_request(
        &mut self,
        requested: NodeSpan,
        base_env: &mut Scope,
        parent: &mut Scope,
    ) -> EvalResult {
        match requested.unspanned {
            Node::FieldAccess(access) => self.eval_fieldacess(access, base_env, parent),
            Node::Call(request) => self.eval_call(request, base_env, parent),
            _ => self.eval_node(&requested, parent),
        }
    }
    fn access_struct(
        &mut self,
        id: usize,
        requested: NodeSpan,
        base_env: &mut Scope,
    ) -> EvalResult {
        let Value::Struct(mut obj) = self.heap[id].clone() else {panic!("IMPROVE ME")};
        let result = self.eval_access_request(requested, base_env, &mut obj.env)?;
        self.heap.insert(id, Value::Struct(obj));
        Ok(result)
    }
    fn eval_fieldacess(
        &mut self,
        request: FieldAccess,
        base_env: &mut Scope,
        parent: &mut Scope,
    ) -> EvalResult {
        let target = self.eval_node(&request.target, parent)?;
        match unwrap_val!(target) {
            Value::StructRef(id) => self.access_struct(id, *request.requested, base_env),
            Value::Str(val) => self.eval_access_request(
                *request.requested,
                base_env,
                &mut defaults::str_struct(val).env,
            ),
            Value::Num(val) => self.eval_access_request(
                *request.requested,
                base_env,
                &mut defaults::num_struct(val).env,
            ),

            a => panic!("{a:?}"),
        }
    }
    fn assign_fields(
        &mut self,
        target: Struct,
        params: HashMap<String, NodeSpan>,
        parent: &mut Scope,
    ) -> EvalResult {
        let mut obj = target;
        for (k, v) in params {
            if !obj.env.vars.contains_key(&k) {
                panic!("Atempted to assign to non existent struct field")
            }
            let result = self.eval_node(&v, parent)?;
            obj.env.vars.insert(k, unwrap_val!(result));
        }
        Ok(Control::Value(Value::Struct(obj.clone())))
    }
    fn eval_constructor(&mut self, constructor: Constructor, parent: &mut Scope) -> EvalResult {
        let Some(request) = parent.get_struct(&constructor.name) else {panic!("Attempted to construct a non existent struct")};
        let struct_val = self.assign_fields(request, constructor.params, parent)?;
        self.heap.push(unwrap_val!(struct_val));
        Ok(Control::Value(Value::StructRef(self.heap.len() - 1)))
    }
    fn eval_structdef(&mut self, obj: StructDef, parent: &mut Scope) -> EvalResult {
        let mut struct_env = Scope::new(None, HashMap::from([]), HashMap::from([]));
        for field in obj.fields {
            self.eval_node(&field.to_nodespan(), &mut struct_env)?;
        }
        let mut strct = Struct {
            id: obj.name.clone(),
            env: struct_env,
        };
        strct.env.structs.insert("Self".to_string(), strct.clone());
        let struct_val = Value::Struct(strct.clone());
        parent.define(obj.name, struct_val.clone());
        self.heap.push(struct_val);
        Ok(Control::Value(Value::StructRef(self.heap.len() - 1)))
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
                self.unary_calc(unary_op, unwrap_val!(target))
            }
            Node::BinaryNode(bin_op) => self.eval_binary_node(bin_op, parent),
            Node::Branch(branch) => self.branch(branch, parent),
            Node::ReturnNode(expr) => {
                let evaluated = unwrap_val!(self.eval_node(&expr, parent)?);
                Ok(Control::Return(evaluated))
            }
            Node::ResultNode(expr) => {
                let evaluated = unwrap_val!(self.eval_node(&expr, parent)?);
                Ok(Control::Result(evaluated))
            }
            Node::BreakNode => Ok(Control::Break),
            Node::ContinueNode => Ok(Control::Continue),
            Node::Declaration(declaration) => self.declare(declaration, parent),
            Node::Assignment(ass) => self.assign(ass, parent),
            Node::While(obj) => self.eval_while_loop(obj, parent),
            Node::Loop(obj) => self.eval_loop(obj, parent),
            Node::DoBlock(block) => self.eval_block(block, parent),
            Node::StructDef(obj) => self.eval_structdef(obj, parent),
            Node::Constructor(op) => self.eval_constructor(op, parent),
            Node::FieldAccess(request) => {
                self.eval_fieldacess(request, &mut parent.clone(), parent)
            }
            _ => {
                todo!()
            }
        }
    }
}
