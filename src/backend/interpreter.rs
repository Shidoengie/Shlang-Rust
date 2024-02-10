use super::defaults;
use super::defaults::eval;
use crate::frontend::nodes::*;
use crate::lang_errors::*;
use crate::spans::*;
use std::collections::HashMap;
use std::*;
pub(super) type IError = InterpreterError;

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

type EvalRes<T> = Result<T, InterpreterError>;
const VOID: EvalRes<Control> = Ok(Control::Value(Value::Void));
const NULL: EvalRes<Control> = Ok(Control::Value(Value::Null));
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
    pub fn execute(&mut self) -> EvalRes<Value> {
        self.execute_with(&mut Scope::default())
    }
    pub fn execute_with(&mut self, scope: &mut Scope) -> EvalRes<Value> {
        if self.program.len() == 0 {
            return Ok(Value::Null);
        }
        let mut last = Value::Null;
        for node in self.program.clone() {
            if let Control::Value(val) | Control::Result(val) = self.eval_node(&node, scope)? {
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
    pub fn parse_vars(&mut self) -> EvalRes<Scope> {
        return self.program_vars(self.program.clone());
    }

    pub fn execute_node(node: NodeSpan) -> EvalRes<Control> {
        Self {
            program: vec![Node::DontResult.to_spanned((0, 0))],
            heap: vec![],
        }
        .eval_node(&node, &mut Scope::new_child_in(defaults::default_scope()))
    }

    fn program_vars(&mut self, block: NodeStream) -> EvalRes<Scope> {
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
    fn eval_block(&mut self, block: NodeStream, parent: &mut Scope) -> EvalRes<Control> {
        self.eval_block_with(block, parent, Scope::default())
    }
    fn eval_block_with(
        &mut self,
        block: NodeStream,
        parent: &mut Scope,
        mut base: Scope,
    ) -> EvalRes<Control> {
        base.parent = Some(Box::new(parent.clone()));
        if block.is_empty() {
            return NULL;
        }
        for node in block {
            let result = self.eval_node(&node, &mut base)?;

            if let Control::Value(_) = result {
                continue;
            }
            let Some(mod_parent) = base.parent else {
                return Err(IError::InvalidControl(node.span));
            };
            *parent = *mod_parent;
            return Ok(result);
        }

        let Some(mod_parent) = base.parent else {
            return Err(IError::InvalidControl((0,0)));
        };
        *parent = *mod_parent;
        NULL
    }

    fn unary_calc(&self, node: UnaryNode, target: Value) -> EvalRes<Control> {
        match node.kind {
            UnaryOp::NEGATIVE => {
                let Value::Num(val) = target else {
                    return Err(IError::InvalidType(vec![Type::Num],target.get_type(), node.object.span));
                };
                Ok(Control::Value(Value::Num(-val)))
            }
            UnaryOp::NOT => {
                let Value::Bool(val) = target else {
                    return Err(IError::InvalidType(vec![Type::Bool], target.get_type(), node.object.span));
                };
                Ok(Control::Value(Value::Bool(!val)))
            }
        }
    }
    fn num_calc(&self, node: BinaryNode, left: f64, right: f64) -> EvalRes<Control> {
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
            BinaryOp::ISEQUAL => Value::Bool(left == right),
            BinaryOp::ISDIFERENT => Value::Bool(left != right),
            op => {
                return Err(IError::InvalidOp(
                    op,
                    Type::Num,
                    (node.left.span.1, node.right.span.0),
                ))
            }
        };
        Ok(Control::Value(res))
    }
    fn str_calc(&self, node: BinaryNode, left: String, right: String) -> EvalRes<Control> {
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
    fn bool_calc(&self, node: BinaryNode, left: bool, right: bool) -> EvalRes<Control> {
        let res = match node.kind {
            BinaryOp::AND => Value::Bool(left && right),
            BinaryOp::OR => Value::Bool(left || right),
            op => {
                return Err(IError::InvalidOp(
                    op,
                    Type::Bool,
                    (node.left.span.1, node.right.span.0),
                ))
            }
        };
        Ok(Control::Value(res))
    }
    fn eval_binary_node(&mut self, bin_op: BinaryNode, parent: &mut Scope) -> EvalRes<Control> {
        let left = unwrap_val!(self.eval_node(&bin_op.left, parent)?);
        let right = unwrap_val!(self.eval_node(&bin_op.right, parent)?);
        let left_type = left.get_type();
        let right_type = right.get_type();
        match bin_op.kind {
            BinaryOp::ISDIFERENT => return Ok(Control::Value(Value::Bool(left != right))),
            BinaryOp::ISEQUAL => return Ok(Control::Value(Value::Bool(left == right))),
            _ => {}
        }

        if left_type != right_type {
            return Err(IError::MixedTypes(
                left_type,
                right_type,
                (bin_op.left.span.0 - 1, bin_op.right.span.1),
            ));
        }
        match (left, right) {
            (Value::Num(left), Value::Num(right)) => {
                return self.num_calc(bin_op, left, right);
            }
            (Value::Str(left), Value::Str(right)) => {
                return self.str_calc(bin_op, left, right);
            }
            (Value::Bool(left), Value::Bool(right)) => {
                return self.bool_calc(bin_op, left, right);
            }
            _ => {}
        }
        Err(IError::InvalidBinary(left_type, bin_op.left.span))
    }

    fn declare(&mut self, request: Declaration, parent: &mut Scope) -> EvalRes<Control> {
        let value = unwrap_val!(self.eval_node(&request.value, parent)?);
        if value.is_void() {
            return Err(IError::VoidAssignment(request.value.span));
        }
        parent.define(request.var_name, value);
        VOID
    }
    fn get_struct_id(&self, val: &Value) -> usize {
        let Value::Ref(id) = val else {panic!("{val:?}")};

        *id
    }

    fn eval_fieldacess(
        &mut self,
        request: FieldAccess,
        base_env: &mut Scope,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        let target = unwrap_val!(self.eval_node(&request.target, parent)?);

        match target {
            Value::Ref(id) => self.access_struct(id, *request.requested, base_env),

            Value::Str(_) => self.eval_access_request(
                *request.requested,
                base_env,
                &mut defaults::str_struct().env,
                target,
            ),
            Value::Num(_) => self.eval_access_request(
                *request.requested,
                base_env,
                &mut defaults::num_struct().env,
                target,
            ),

            a => panic!("{a:?}"),
        }
    }
    fn access_struct(
        &mut self,
        id: usize,
        requested: NodeSpan,
        base_env: &mut Scope,
    ) -> EvalRes<Control> {
        let Value::Struct(mut obj) = self.heap[id].clone() else {panic!("IMPROVE ME")};
        let result = self.eval_access_request(requested, base_env, &mut obj.env, Value::Ref(id))?;
        Ok(result)
    }
    fn eval_access_request(
        &mut self,
        requested: NodeSpan,
        base_env: &mut Scope,
        obj_env: &mut Scope,
        obj: Value,
    ) -> EvalRes<Control> {
        match requested.unspanned {
            Node::FieldAccess(access) => self.eval_fieldacess(access, base_env, obj_env),
            Node::Call(request) => self.eval_method(request, base_env, obj_env, obj),
            _ => self.eval_node(&requested, obj_env),
        }
    }
    fn eval_method(
        &mut self,
        request: Call,
        base_env: &mut Scope,
        obj_env: &mut Scope,
        obj: Value,
    ) -> EvalRes<Control> {
        let func_val = unwrap_val!(self.eval_node(&request.callee, obj_env)?);
        let call_args = request.args;
        let mut arg_values: ValueStream = vec![obj];
        for arg in call_args.iter() {
            let argument = unwrap_val!(self.eval_node(&arg, base_env)?);
            arg_values.push(argument);
        }
        let arg_span = if !call_args.is_empty() {
            (request.callee.span.1 + 1, call_args.last().unwrap().span.1)
        } else {
            (request.callee.span.1 + 1, request.callee.span.1 + 2)
        };
        match func_val {
            Value::BuiltinFunc(func) => self.call_builtin(func, arg_values, arg_span, obj_env),
            Value::Function(called) => {
                self.call_func(called, arg_values, request.callee.span, obj_env)
            }
            _ => {
                return Err(IError::InvalidType(
                    vec![Type::Function],
                    func_val.get_type(),
                    request.callee.span,
                ))
            }
        }
    }

    fn eval_constructor(
        &mut self,
        constructor: Constructor,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        let Some(request) = parent.get_struct(&constructor.name) else {panic!("Attempted to construct a non existent struct")};
        let struct_val = self.assign_fields(request, constructor.params, parent)?;
        self.heap.push(unwrap_val!(struct_val));
        Ok(Control::Value(Value::Ref(self.heap.len() - 1)))
    }
    fn assign_fields(
        &mut self,
        target: Struct,
        params: HashMap<String, NodeSpan>,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
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
    fn assign_to_access(
        &mut self,
        request: FieldAccess,
        value: Value,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        let target = unwrap_val!(self.eval_node(&request.target, parent)?);
        let id = self.get_struct_id(&target);
        let Value::Struct(mut obj) = self.heap[id].clone() else {panic!()};

        match request.requested.unspanned {
            Node::FieldAccess(access) => {
                self.assign_to_access(access, value, &mut obj.env)?;
                self.heap[id] = Value::Struct(obj);
                VOID
            }
            Node::Variable(var) => {
                self.assign_to_name(var, value, request.requested.span, &mut obj.env)?;
                self.heap[id] = Value::Struct(obj);
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
    ) -> EvalRes<Control> {
        if value.is_void() {
            return Err(IError::VoidAssignment(span));
        }

        if target.assign(name.clone(), value).is_none() {
            return Err(IError::InvalidAssignment(name, span));
        }

        VOID
    }
    fn assign(&mut self, request: Assignment, parent: &mut Scope) -> EvalRes<Control> {
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
    fn default_scope(&self, name: String, span: Span) -> EvalRes<Control> {
        let maybe_result = Scope::new(None, defaults::var_map(), HashMap::from([])).get_var(&name);
        let Some(result) = maybe_result else {
            return Err(IError::NonExistentVar(name, span));
        };
        Ok(Control::Value(result))
    }
    fn eval_var(&self, name: String, span: Span, env: &Scope) -> EvalRes<Control> {
        let maybe_result = env.get_var(&name);
        let Some(result) = maybe_result else {
            return self.default_scope(name, span);
        };
        Ok(Control::Value(result))
    }

    fn eval_call(
        &mut self,
        request: Call,
        base_env: &mut Scope,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        let func_val = unwrap_val!(self.eval_node(&request.callee, parent)?);
        let call_args = request.args;
        let mut arg_values: ValueStream = vec![];

        for arg in call_args.iter() {
            let argument = unwrap_val!(self.eval_node(&arg, base_env)?);
            arg_values.push(argument);
        }
        let arg_span = if !call_args.is_empty() {
            (request.callee.span.1 + 1, call_args.last().unwrap().span.1)
        } else {
            (request.callee.span.1 + 1, request.callee.span.1 + 2)
        };
        match func_val {
            Value::BuiltinFunc(func) => self.call_builtin(func, arg_values, arg_span, parent),
            Value::Function(called) => {
                self.call_func(called, arg_values, request.callee.span, parent)
            }
            _ => {
                return Err(IError::InvalidType(
                    vec![Type::Function],
                    func_val.get_type(),
                    request.callee.span,
                ))
            }
        }
    }
    fn call_func(
        &mut self,
        called: Function,
        args: Vec<Value>,

        span: Span,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        if args.len() != called.args.len() {
            return Err(IError::InvalidArgSize(
                called.args.len() as u32,
                args.len() as u32,
                span,
            ));
        }
        let arg_map = self.build_args(&called, args);
        let arg_scope = Scope::new(None, arg_map, HashMap::new());
        let result = self.eval_block_with(called.block, parent, arg_scope)?;
        match result {
            Control::Continue | Control::Break => return Err(IError::InvalidControl(span)),
            Control::Result(val) | Control::Return(val) | Control::Value(val) => {
                return Ok(Control::Value(val))
            }
        }
    }
    fn build_args(&self, func: &Function, arg_values: Vec<Value>) -> HashMap<String, Value> {
        let mut arg_map = HashMap::<String, Value>::new();
        for (idx, val) in arg_values.iter().enumerate() {
            arg_map.insert(func.args[idx].clone(), val.clone());
        }
        arg_map
    }
    fn call_builtin(
        &mut self,
        called: BuiltinFunc,
        arg_values: ValueStream,
        span: Span,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        if called.arg_size != -1 && arg_values.len() as i16 != called.arg_size {
            return Err(IError::InvalidArgSize(
                called.arg_size as u32,
                arg_values.len() as u32,
                span,
            ));
        }
        let result = (called.function)(parent.vars.clone(), arg_values, self.heap.clone());
        Ok(Control::Value(result))
    }
    fn branch(&mut self, branch: Branch, parent: &mut Scope) -> EvalRes<Control> {
        let condition = unwrap_val!(self.eval_node(&branch.condition, parent)?);
        let Value::Bool(cond_val) = condition else {
            return Err(IError::InvalidType(vec![Type::Bool],condition.get_type() , branch.condition.span));
        };
        if cond_val {
            return self.eval_block(branch.if_block, parent);
        }
        let Some(else_block) = branch.else_block else {
            return NULL;
        };
        self.eval_block(else_block, parent)
    }
    fn eval_loop(&mut self, loop_block: NodeStream, parent: &mut Scope) -> EvalRes<Control> {
        loop {
            let result = self.eval_block(loop_block.clone(), parent)?;
            match result {
                Control::Break => return NULL,
                Control::Continue | Control::Value(_) => continue,
                _ => return Ok(result),
            }
        }
    }
    fn eval_while_loop(&mut self, loop_node: While, parent: &mut Scope) -> EvalRes<Control> {
        loop {
            let condition = unwrap_val!(self.eval_node(&loop_node.condition, parent)?);
            let Value::Bool(cond_val) = condition else {
                return Err(IError::InvalidType(vec![Type::Bool],condition.get_type() , loop_node.condition.span));
            };
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

    fn eval_structdef(&mut self, def: StructDef, parent: &mut Scope) -> EvalRes<Control> {
        let mut struct_env = Scope::default();
        for field in def.fields {
            self.eval_node(&field.to_nodespan(), &mut struct_env)?;
        }
        let mut obj = Struct {
            id: def.name.clone(),
            env: struct_env,
        };

        obj.env.structs.insert("Self".to_string(), obj.clone());
        let struct_val = Value::Struct(obj.clone());
        self.heap.push(struct_val);
        let struct_ref = Value::Ref(self.heap.len() - 1);
        if let Some(name) = def.name {
            parent.structs.insert(name.clone(), obj);
            parent.vars.insert(name, struct_ref.clone());
            return VOID;
        }
        Ok(Control::Value(struct_ref))
    }

    fn eval_node(&mut self, node: &NodeSpan, parent: &mut Scope) -> EvalRes<Control> {
        let (span, expr) = (node.span, node.unspanned.clone());
        match expr {
            Node::Value(val) => {
                // let kind = val.clone().get_type();
                // Ok((*val, kind))

                return Ok(Control::Value(val));
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
