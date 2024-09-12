use super::scope::Scope;
use crate::backend::defaults;
use crate::backend::defaults::default_scope;
use crate::frontend::nodes::*;
use crate::hashmap;
use crate::lang_errors::*;
use crate::spans::*;

use fmt::Display;
use slotmap::SlotMap;
use std::collections::HashMap;
use std::*;

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
            Control::Result(inner) | Control::Value(inner) => inner,
            Control::Return(inner) => return Ok(Control::Return(inner)),
            _ => Value::Null,
        }
    };
}

const VOID: EvalRes<Control> = Ok(Control::Value(Value::Void));
const NULL: EvalRes<Control> = Ok(Control::Value(Value::Null));

pub(super) type IError = InterpreterError;
pub(super) type EvalRes<T> = Result<T, Spanned<InterpreterError>>;
#[derive(Debug)]
pub struct Interpreter {
    program: NodeStream,
    pub heap: SlotMap<RefKey, Value>,
    pub envs: SlotMap<EnvKey, Scope>,
    pub functions: HashMap<String, Function>,
}

///Interface with the interpreter
impl Interpreter {
    pub fn new(program: NodeStream, functions: HashMap<String, Function>) -> Self {
        Self {
            program,
            heap: SlotMap::with_key(),
            envs: SlotMap::with_key(),
            functions,
        }
    }
    pub fn execute(&mut self) -> EvalRes<Value> {
        let mut scope = Scope::from_vars(hashmap! {
            __name__ => Value::Str("main".to_string())
        });
        self.execute_with(&mut scope)
    }
    pub fn execute_with(&mut self, scope: &mut Scope) -> EvalRes<Value> {
        if self.program.is_empty() {
            return Ok(Value::Null);
        }
        let mut last = Value::Null;
        for node in self.program.clone() {
            if let Control::Value(val) | Control::Result(val) = self.eval_node(&node, scope)? {
                last = val;
                continue;
            }
            return Err(IError::InvalidControl.to_spanned(node.span));
        }

        Ok(last)
    }

    pub fn execute_node(node: NodeSpan) -> EvalRes<Control> {
        Self {
            envs: SlotMap::with_key(),
            program: vec![Node::DontResult.to_spanned(Span(0, 0))],
            heap: SlotMap::with_key(),
            functions: HashMap::new(),
        }
        .eval_node(&node, &mut Scope::default())
    }
    pub fn execute_node_with(node: NodeSpan, parent: &mut Scope) -> EvalRes<Control> {
        Self {
            program: vec![Node::DontResult.to_spanned(Span(0, 0))],
            heap: SlotMap::with_key(),
            envs: SlotMap::with_key(),
            functions: HashMap::new(),
        }
        .eval_node(&node, parent)
    }
    pub fn execute_func_with(
        func: Function,
        parent: &mut Scope,
        args: Vec<Value>,
    ) -> EvalRes<Control> {
        Self {
            envs: SlotMap::with_key(),
            program: vec![Node::DontResult.to_spanned(Span(0, 0))],
            heap: SlotMap::with_key(),
            functions: HashMap::new(),
        }
        .call_func(func, args, Span::EMPTY, parent)
    }
    pub fn parse_vars(&mut self, mut parent: Scope) -> EvalRes<Scope> {
        if self.program.is_empty() {
            return Ok(Scope::default());
        }
        for node in self.program.clone() {
            if let Control::Value(_) = self.eval_node(&node, &mut parent)? {
                continue;
            }
            return Err(IError::InvalidControl.to_spanned(node.span));
        }
        Ok(parent)
    }
    fn eval_node(&mut self, node: &NodeSpan, parent: &mut Scope) -> EvalRes<Control> {
        let (span, expr) = (node.span, node.item.clone());
        match expr {
            Node::Value(val) => Ok(Control::Value(val)),

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
            Node::ForLoop(for_loop) => self.eval_for_loop(for_loop, parent),
            Node::BreakNode => Ok(Control::Break),
            Node::ContinueNode => Ok(Control::Continue),
            Node::Declaration(declaration) => self.declare(declaration, parent),
            Node::Assignment(ass) => self.assign(ass, parent),
            Node::While(obj) => self.eval_while_loop(obj, parent),
            Node::Loop(obj) => self.eval_loop(obj, parent),
            Node::DoBlock(block) => self.eval_block(block, parent),
            Node::StructDef(obj) => self.eval_structdef(obj, parent),
            Node::Constructor(op) => self.eval_constructor(op, parent, span),
            Node::FieldAccess(request) => {
                self.eval_fieldacess(request, &mut parent.clone(), parent)
            }

            Node::Index { target, index } => self.eval_index(*target, *index, parent),
            Node::ListLit(lit) => self.eval_list(lit, parent),
            Node::ClosureDef(cl) => self.eval_closuredef(cl, parent),
            _ => {
                todo!()
            }
        }
    }
}

///Block execution
impl Interpreter {
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
                return Err(IError::InvalidControl.to_spanned(node.span));
            };
            *parent = *mod_parent;
            return Ok(result);
        }

        let Some(mod_parent) = base.parent else {
            return Err(IError::InvalidControl.to_spanned(Span(0, 0)));
        };
        *parent = *mod_parent;
        NULL
    }
}

///Binary and unary operators
impl Interpreter {
    fn unary_calc(&self, node: UnaryNode, target: Value) -> EvalRes<Control> {
        let span = node.object.span;
        match node.kind {
            UnaryOp::NEGATIVE => {
                let Value::Num(val) = target else {
                    return type_err(Type::Num, target.get_type(), span);
                };
                Ok(Control::Value(Value::Num(-val)))
            }
            UnaryOp::NOT => {
                let Value::Bool(val) = target else {
                    return type_err(Type::Bool, target.get_type(), span);
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
                return Err(IError::InvalidOp(op, Type::Num)
                    .to_spanned(Span(node.left.span.1, node.right.span.0)))
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
                return Err(IError::InvalidOp(op, Type::Str)
                    .to_spanned(Span(node.left.span.1, node.right.span.0)))
            }
        };
        Ok(Control::Value(res))
    }
    fn bool_calc(&self, node: BinaryNode, left: bool, right: bool) -> EvalRes<Control> {
        let res = match node.kind {
            BinaryOp::AND => Value::Bool(left && right),
            BinaryOp::OR => Value::Bool(left || right),
            op => {
                return Err(IError::InvalidOp(op, Type::Bool)
                    .to_spanned(Span(node.left.span.1, node.right.span.0)))
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

        if !left.matches_typeof(&right) {
            return Err(IError::MixedTypes(left_type, right_type)
                .to_spanned(bin_op.left.span + bin_op.right.span));
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
        Err(IError::InvalidBinary(left_type).to_spanned(bin_op.left.span))
    }
}

///Control flow expression handling
impl Interpreter {
    fn branch(&mut self, branch: Branch, parent: &mut Scope) -> EvalRes<Control> {
        let condition = unwrap_val!(self.eval_node(&branch.condition, parent)?);
        let Value::Bool(cond_val) = condition else {
            return type_err(Type::Bool, condition.get_type(), branch.condition.span);
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
                return type_err(Type::Bool, condition.get_type(), loop_node.condition.span);
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
    fn eval_for_loop(&mut self, for_loop: ForLoop, parent: &mut Scope) -> EvalRes<Control> {
        let list_span = for_loop.list.span;
        let list_ref = match unwrap_val!(self.eval_node(&for_loop.list, parent)?) {
            Value::Ref(id) => id,
            invalid => return type_err(Type::List, invalid.get_type(), list_span),
        };
        let mut cursor: usize = 0;

        loop {
            let list = match &self.heap[list_ref] {
                Value::List(id) => id,
                invalid => return type_err(Type::List, invalid.get_type(), list_span),
            };
            let len = list.len();

            let result = self.eval_block_with(
                for_loop.proc.clone(),
                parent,
                Scope::new(
                    None,
                    HashMap::from_iter([(for_loop.ident.clone(), list[cursor].clone())]),
                    hashmap! {},
                ),
            )?;

            cursor += 1;
            match result {
                Control::Break => return NULL,
                Control::Continue | Control::Value(_) => {
                    if cursor == len {
                        return Ok(result);
                    }

                    continue;
                }
                _ => return Ok(result),
            }
        }
    }
}

///Variable declaration and execution
impl Interpreter {
    fn declare(&mut self, request: Declaration, parent: &mut Scope) -> EvalRes<Control> {
        let value = unwrap_val!(self.eval_node(&request.value, parent)?);
        if value.is_void() {
            return Err(IError::VoidAssignment.to_spanned(request.value.span));
        }
        parent.define(request.var_name, value);
        VOID
    }
    fn eval_var(&self, name: String, span: Span, env: &Scope) -> EvalRes<Control> {
        let maybe_result = env.get_var(&name);
        let Some(result) = maybe_result else {
            return self.default_scope(name, span);
        };
        Ok(Control::Value(result))
    }
    fn default_scope(&self, name: String, span: Span) -> EvalRes<Control> {
        let maybe_result = default_scope().get_var(&name);
        let Some(result) = maybe_result else {
            if let Some(func) = self.functions.get(&name) {
                return Ok(Control::Value(Value::Function(func.clone())));
            };
            return Err(IError::NonExistentVar(name).to_spanned(span));
        };
        Ok(Control::Value(result))
    }
}

///Variable and struct assignment and struct access
impl Interpreter {
    fn eval_fieldacess(
        &mut self,
        request: FieldAccess,
        base_env: &mut Scope,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        let target = unwrap_val!(self.eval_node(&request.target, parent)?);

        self.eval_access(target, *request.requested, base_env, parent)
    }
    fn eval_access(
        &mut self,
        target: Value,
        requested: NodeSpan,
        base_env: &mut Scope,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        match target {
            Value::Ref(id) => self.access_ref(id, requested.clone(), base_env, parent, requested),

            Value::Str(_) => self.eval_access_request(
                requested,
                base_env,
                &mut defaults::str_struct().env,
                target,
            ),
            Value::Num(_) => self.eval_access_request(
                requested,
                base_env,
                &mut defaults::num_struct().env,
                target,
            ),
            Value::Function(_) => self.eval_access_request(
                requested,
                base_env,
                &mut defaults::func_struct().env,
                target,
            ),

            Value::Null => return NULL,
            a => unspec_err(
                format!("Requested type {} has no properties", a.get_type()),
                requested.span,
            ),
        }
    }
    fn access_ref(
        &mut self,
        id: RefKey,
        requested: NodeSpan,
        base_env: &mut Scope,
        parent: &mut Scope,
        request: NodeSpan,
    ) -> EvalRes<Control> {
        match &self.heap[id] {
            Value::Struct(obj) => self.access_struct(id, requested, base_env, obj.clone()),
            Value::List(_) => self.eval_access_request(
                requested,
                base_env,
                &mut defaults::list_struct().env,
                Value::Ref(id),
            ),
            val => self.eval_access(val.clone(), request, base_env, parent),
        }
    }
    fn access_struct(
        &mut self,
        id: RefKey,
        requested: NodeSpan,
        base_env: &mut Scope,
        mut obj: Struct,
    ) -> EvalRes<Control> {
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
        match requested.item.clone() {
            Node::Call(request) => self.eval_method(request, base_env, obj_env, obj),
            Node::Variable(name) => {
                let Some(result) = obj_env.get_var(&name) else {
                    return NULL;
                };
                Ok(Control::Value(result))
            }
            _ => unimplemented!(),
        }
    }

    fn assign_to_access(
        &mut self,
        request: FieldAccess,
        value: Value,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        let target = unwrap_val!(self.eval_node(&request.target, parent)?);
        let Value::Ref(id) = target else {
            return unspec_err("Expected Reference", request.target.span);
        };
        let Value::Struct(mut obj) = self.heap[id].clone() else {
            return unspec_err("Expected Struct", request.target.span);
        };

        match request.requested.item {
            Node::FieldAccess(access) => {
                self.assign_to_access(access, value, &mut obj.env)?;
                self.heap[id] = Value::Struct(obj);
                VOID
            }
            Node::Variable(var) => {
                if obj.env.assign(var.clone(), value).is_none() {
                    return NULL;
                }
                self.heap[id] = Value::Struct(obj);
                VOID
            }
            _ => panic!(),
        }
    }
    fn assign_to_index(
        &mut self,
        val: Value,
        target_node: NodeSpan,
        index_node: NodeSpan,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        let target = unwrap_val!(self.eval_node(&target_node, parent)?);
        let index = unwrap_val!(self.eval_node(&index_node, parent)?);
        let (Value::Ref(id), Value::Num(og_idx)) = (target.clone(), index) else {
            return type_err(Type::List, target.get_type(), target_node.span);
        };
        let mut list = match &self.heap[id] {
            Value::List(ls) => ls.clone(),
            invalid => return type_err(Type::List, invalid.get_type(), index_node.span),
        };

        let idx = og_idx.floor() as usize;
        if idx >= list.len() || list.is_empty() {
            return unspec_err("Index out of bounds", index_node.span);
        }
        list[idx] = val;
        self.heap[id] = Value::List(list);
        VOID
    }

    fn assign(&mut self, request: Assignment, parent: &mut Scope) -> EvalRes<Control> {
        let init_val = unwrap_val!(self.eval_node(&request.value, parent)?);
        let span = request.target.span;
        match request.target.item {
            Node::Variable(var) => {
                parent.assign_valid(var, init_val, span)?;
            }
            Node::Index { target, index } => {
                self.assign_to_index(init_val, *target, *index, parent)?;
            }
            Node::FieldAccess(field) => {
                self.assign_to_access(field, init_val, parent)?;
            }
            _ => return unspec_err("", span),
        };
        VOID
    }
}

///Closure handling
impl Interpreter {
    fn eval_closuredef(
        &mut self,
        cl: ClosureDef,
        parent: &mut Scope,
        
    ) -> EvalRes<Control> {
        let key = self.envs.insert(parent.clone());
        return Ok(Control::Value(Closure{
            args:cl.args,
            block:cl.block,
            env:key
        }.into()));
        
    }
}
///Function and function call handling
impl Interpreter {
    fn eval_call(
        &mut self,
        request: Call,
        base_env: &mut Scope,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        let func_val = unwrap_val!(self.eval_node(&request.callee, parent)?);
        let call_args = request.args;
        let mut arg_values: Vec<Value> = vec![];

        for arg in call_args.iter() {
            let argument = unwrap_val!(self.eval_node(arg, base_env)?);
            arg_values.push(argument);
        }
        let arg_span = if !call_args.is_empty() {
            Span(request.callee.span.1 + 1, call_args.last().unwrap().span.1)
        } else {
            Span(request.callee.span.1 + 1, request.callee.span.1 + 2)
        };
        match func_val {
            Value::BuiltinFunc(func) => self.call_builtin(func, arg_values, arg_span, parent),
            Value::Function(called) => {
                self.call_func(called, arg_values, request.callee.span, parent)
            }
            Value::Ref(id) => self.call_closure(id, func_val, arg_values, request.callee.span),
            _ => return type_err(Type::Function, func_val.get_type(), request.callee.span),
        }
    }
    fn call_closure(
        &mut self,
        id: RefKey,
        func_val: Value,
        arg_values: Vec<Value>,
        span: Span,
    ) -> EvalRes<Control> {
        let got = &self.heap[id];
        if got.get_type() != Type::Closure {
            return type_err(Type::Closure, func_val.get_type(), span);
        }
        let Value::Struct(obj) = got else {
            unreachable!()
        };
        let [Some(maybe_fn), Some(maybe_env)] = obj.env.get_vars(["fn", "env"]) else {
            return unspec_err("Invalid closure object", span);
        };
        let Value::Function(func) = maybe_fn else {
            return type_err(Type::Function, maybe_fn.get_type(), span);
        };
        let Value::Ref(env_ref) = maybe_env else {
            return type_err(Type::Ref, maybe_env.get_type(), span);
        };
        let Value::Struct(ref mut env_obj) = self.heap[env_ref].clone() else {
            return type_err(Type::Struct(None), self.heap[env_ref].get_type(), span);
        };
        self.call_func(func, arg_values, span, &mut env_obj.env)
    }
    fn call_func(
        &mut self,
        called: Function,
        args: Vec<Value>,

        span: Span,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        if args.len() != called.args.len() {
            return Err(
                IError::InvalidArgSize(called.args.len() as u32, args.len() as u32)
                    .to_spanned(span),
            );
        }
        let arg_map = self.build_args(&called, args);
        let arg_scope = Scope::new(None, arg_map, HashMap::new());
        let result = self.eval_block_with(called.block, parent, arg_scope)?;
        match result {
            Control::Continue | Control::Break => {
                return Err(IError::InvalidControl.to_spanned(span))
            }
            Control::Result(val) | Control::Return(val) | Control::Value(val) => {
                if val.is_void() {
                    return unspec_err("Attempted to return void", span);
                }
                return Ok(Control::Value(val));
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
        arg_values: Vec<Value>,
        span: Span,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        check_args(called.arg_range, arg_values.len(), span)?;
        let data = FuncData {
            heap: &mut self.heap,
            global_funcs: &mut self.functions,
            parent,
            envs: &mut self.envs,
            args: arg_values,
        };
        let result = (called.function)(data);
        Ok(Control::Value(result))
    }
}

///Struct handling
impl Interpreter {
    fn eval_method(
        &mut self,
        request: Call,
        base_env: &mut Scope,
        obj_env: &mut Scope,
        obj: Value,
    ) -> EvalRes<Control> {
        let func_val = unwrap_val!(self.eval_node(&request.callee, obj_env)?);
        let call_args = request.args;
        let mut arg_values: Vec<Value> = vec![obj];
        for arg in call_args.iter() {
            let argument = unwrap_val!(self.eval_node(arg, base_env)?);
            arg_values.push(argument);
        }
        let arg_span = if !call_args.is_empty() {
            Span(request.callee.span.1 + 1, call_args.last().unwrap().span.1)
        } else {
            Span(request.callee.span.1 + 1, request.callee.span.1 + 2)
        };
        match func_val {
            Value::BuiltinFunc(func) => self.call_builtin(func, arg_values, arg_span, obj_env),
            Value::Function(called) => {
                self.call_func(called, arg_values, request.callee.span, obj_env)
            }
            _ => type_err(Type::Function, func_val.get_type(), request.callee.span),
        }
    }
    fn get_struct(&self, name: &String, span: Span, parent: &mut Scope) -> EvalRes<Struct> {
        if let Some(obj) = parent.get_struct(&name) {
            return Ok(obj);
        }
        let maybe_result = default_scope().get_struct(name);
        let Some(result) = maybe_result else {
            return unspec_err("Attempted to get a non existent struct", span);
        };
        Ok(result)
    }
    fn construct_fields(
        &mut self,
        target: Struct,
        params: HashMap<String, NodeSpan>,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        let mut obj = target;
        for (k, v) in params {
            if !obj.env.vars.contains_key(&k) {
                return unspec_err("Attempted to assign to a non existent struct field", v.span);
            }
            let result = self.eval_node(&v, parent)?;
            obj.env.vars.insert(k, unwrap_val!(result));
        }
        Ok(Control::Value(Value::Struct(obj.clone())))
    }
    fn eval_constructor(
        &mut self,
        constructor: Constructor,
        parent: &mut Scope,
        span: Span,
    ) -> EvalRes<Control> {
        let request = self.get_struct(&constructor.name, span, parent)?;
        let struct_val = self.construct_fields(request, constructor.params, parent)?;
        let key = self.heap.insert(unwrap_val!(struct_val));
        Ok(Control::Value(Value::Ref(key)))
    }

    fn eval_structdef(&mut self, def: StructDef, parent: &mut Scope) -> EvalRes<Control> {
        let mut struct_env = Scope::default();
        for field in def.fields {
            self.eval_node(&field.to_node(), &mut struct_env)?;
        }
        let mut obj = Struct {
            id: def.name.clone(),
            env: struct_env,
        };

        obj.env.structs.insert("Self".to_string(), obj.clone());
        let struct_val = Value::Struct(obj.clone());
        let key = self.heap.insert(struct_val);
        let struct_ref = Value::Ref(key);
        if let Some(name) = def.name {
            parent.structs.insert(name.clone(), obj);
            parent.vars.insert(name, struct_ref.clone());
            return VOID;
        }
        Ok(Control::Value(struct_ref))
    }
}

///List handling
impl Interpreter {
    fn eval_index(
        &mut self,
        target_node: NodeSpan,
        index_node: NodeSpan,
        parent: &mut Scope,
    ) -> EvalRes<Control> {
        let target = unwrap_val!(self.eval_node(&target_node, parent)?);
        let index = unwrap_val!(self.eval_node(&index_node, parent)?);
        match (target, index) {
            (Value::Str(txt), Value::Num(og_idx)) => {
                let idx = og_idx.floor() as usize;
                let Some(out) = txt.get(idx..idx + 1) else {
                    return unspec_err("Index out of Bounds", index_node.span);
                };

                Ok(Control::Value(Value::Str(out.to_owned())))
            }
            (Value::Ref(id), Value::Num(og_idx)) => {
                let list = match &self.heap[id] {
                    Value::List(ls) => ls,
                    invalid => return type_err(Type::List, invalid.get_type(), target_node.span),
                };

                let idx = og_idx.floor() as usize;
                let Some(out) = list.get(idx) else {
                    return unspec_err("Index out of Bounds", index_node.span);
                };
                Ok(Control::Value(out.clone()))
            }
            _ => {
                todo!()
            }
        }
    }
    fn eval_list(&mut self, lit: Vec<NodeSpan>, parent: &mut Scope) -> EvalRes<Control> {
        let mut list: Vec<Value> = vec![];
        for node in lit {
            list.push(unwrap_val!(self.eval_node(&node, parent)?));
        }
        let val = self.heap.insert(Value::List(list));
        Ok(Control::Value(Value::Ref(val)))
    }
}
fn unspec_err<T>(msg: impl Display, span: Span) -> EvalRes<T> {
    return Err(IError::Unspecified(msg.to_string()).to_spanned(span));
}
fn type_err<T>(expected: Type, got: Type, span: Span) -> EvalRes<T> {
    return Err(IError::InvalidType(vec![expected], got).to_spanned(span));
}
fn check_args(arg_range: Option<(u8, u8)>, given_size: usize, span: Span) -> EvalRes<()> {
    let Some(range) = arg_range else {
        return Ok(());
    };
    if range.1 == 0 && given_size != 0 {
        return unspec_err(format!("Expected no arguments but got {given_size}"), span);
    }

    if given_size > range.1.into() {
        return unspec_err(format!("Given arguments are greater then expected; Expected atmost {max} but got {given_size}",max=range.1), span);
    }
    if given_size < range.0.into() {
        return unspec_err(format!("Given arguments are lesser then expected; Expected atleast {min} but got {given_size}",min=range.0), span);
    }
    return Ok(());
}
