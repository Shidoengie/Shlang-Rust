use super::scope::Scope;
use super::values::*;
use crate::backend::{
    defaults::{
        self, default_scope,
        natives::{self, GlobalMethods},
    },
    values::Value,
};

use crate::catch;
use crate::frontend::nodes::*;
use crate::hashmap;
use crate::lang_errors::*;
use crate::spans::*;

use fmt::Display;
use slotmap::SlotMap;
use std::collections::HashMap;
use std::{any::TypeId, *};

macro_rules! unwrap_val {
    ($val:expr) => {
        match $val {
            Control::Result(inner) | Control::Value(inner) => inner,
            Control::Return(inner) => return Ok(Control::Return(inner)),
            _ => Value::Null,
        }
    };
}

const VOID: EvalRes = Ok(Control::Value(Value::Void));
const NULL: EvalRes = Ok(Control::Value(Value::Null));

pub(super) type IError = InterpreterError;
pub(super) type EvalRes<T = Control> = Result<T, Spanned<InterpreterError>>;
#[derive(Debug)]
pub struct Interpreter {
    program: NodeStream,
    pub heap: SlotMap<RefKey, Value>,
    pub envs: SlotMap<EnvKey, Scope>,
    pub functions: HashMap<String, Function>,
    pub native_constructors: HashMap<String, NativeConstructor>,
}

///Interface with the interpreter
impl Interpreter {
    pub fn new(program: NodeStream, functions: HashMap<String, Function>) -> Self {
        Self {
            program,
            functions,
            native_constructors: natives::native_constructors(),
            heap: SlotMap::with_key(),
            envs: SlotMap::with_key(),
        }
    }
    pub fn create(
        program: NodeStream,
        functions: HashMap<String, Function>,
        heap: SlotMap<RefKey, Value>,
        envs: SlotMap<EnvKey, Scope>,
        native_constructors: HashMap<String, NativeConstructor>,
    ) -> Self {
        Self {
            program,
            functions,
            heap,
            envs,
            native_constructors,
        }
    }
    pub fn execute(&mut self) -> EvalRes<Value> {
        let mut scope = Scope::from_vars(hashmap! {
            __name => Value::Str("main".to_string())
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
            return IError::InvalidControl.to_spanned(node.span).err();
        }

        Ok(last)
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

    fn eval_node(&mut self, node: &NodeSpan, parent: &mut Scope) -> EvalRes {
        let (span, expr) = (node.span, node.item.clone());
        match expr {
            Node::Bool(val) => Ok(Control::Value(Value::Bool(val))),
            Node::Str(val) => Ok(Control::Value(Value::Str(val))),
            Node::Number(val) => Ok(Control::Value(Value::Num(val))),
            Node::Null => Ok(Control::Value(Value::Null)),
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
            Node::Declaration(declaration) => self.declare(&declaration, parent, false),
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
            Node::FuncDef(func) => self.eval_funcdef(func, parent),
            _ => {
                todo!()
            }
        }
    }
}

///Block execution
impl Interpreter {
    fn eval_block(&mut self, block: NodeStream, parent: &mut Scope) -> EvalRes {
        self.eval_block_with(block, parent, Scope::default())
    }
    fn eval_block_with(
        &mut self,
        block: NodeStream,
        parent: &mut Scope,
        mut base: Scope,
    ) -> EvalRes {
        base.parent = Some(Box::new(parent.clone()));
        if block.is_empty() {
            return NULL;
        }

        for (i, node) in block.iter().enumerate() {
            let result = match (self.eval_node(node, &mut base)?, &node.item) {
                (c @ Control::Result(_), Node::ResultNode(_)) => {
                    if i != block.len() - 1 {
                        continue;
                    }
                    c
                }
                (Control::Value(_), _) => {
                    continue;
                }
                (Control::Result(_), _) => {
                    continue;
                }
                (c, _) => c,
            };

            let result = match result {
                Control::Result(inner) => Control::Result(self.capture_fn(inner, &mut base)),
                Control::Return(inner) => Control::Return(self.capture_fn(inner, &mut base)),
                _ => result,
            };
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
    fn unary_calc(&self, node: UnaryNode, target: Value) -> EvalRes {
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
    fn num_calc(&self, node: BinaryNode, left: f64, right: f64) -> EvalRes {
        let res = match node.kind {
            BinaryOp::Add => Value::Num(left + right),
            BinaryOp::Subtract => Value::Num(left - right),
            BinaryOp::Multiply => Value::Num(left * right),
            BinaryOp::Divide => Value::Num(left / right),
            BinaryOp::Modulo => Value::Num(left % right),
            BinaryOp::Greater => Value::Bool(left > right),
            BinaryOp::GreaterOrEqual => Value::Bool(left >= right),
            BinaryOp::Lesser => Value::Bool(left < right),
            BinaryOp::LesserOrEqual => Value::Bool(left <= right),
            BinaryOp::IsEqual => Value::Bool(left == right),
            BinaryOp::IsDifferent => Value::Bool(left != right),
            op => {
                return Err(IError::InvalidOp(op, Type::Num)
                    .to_spanned(Span(node.left.span.1, node.right.span.0)));
            }
        };
        Ok(Control::Value(res))
    }
    fn str_calc(&self, node: BinaryNode, left: String, right: String) -> EvalRes {
        let res = match node.kind {
            BinaryOp::Add => Value::Str(left + &right),
            BinaryOp::Greater => Value::Bool(left > right),
            BinaryOp::GreaterOrEqual => Value::Bool(left >= right),
            BinaryOp::Lesser => Value::Bool(left < right),
            BinaryOp::LesserOrEqual => Value::Bool(left <= right),
            BinaryOp::IsEqual => Value::Bool(left == right),
            BinaryOp::IsDifferent => Value::Bool(left != right),
            op => {
                return Err(IError::InvalidOp(op, Type::Str)
                    .to_spanned(Span(node.left.span.1, node.right.span.0)));
            }
        };
        Ok(Control::Value(res))
    }
    fn bool_calc(&mut self, node: BinaryNode, left: bool, parent: &mut Scope) -> EvalRes {
        match node.kind {
            BinaryOp::And => {
                if !left {
                    return Ok(Control::Value(Value::Bool(false)));
                }
                return self.eval_node(&node.right, parent);
            }
            BinaryOp::OR => {
                if left {
                    return Ok(Control::Value(Value::Bool(true)));
                }
                return self.eval_node(&node.right, parent);
            }
            op => {
                return Err(IError::InvalidOp(op, Type::Bool)
                    .to_spanned(Span(node.left.span.1, node.right.span.0)));
            }
        };
    }
    fn null_coalescing(&mut self, left: Value, node: BinaryNode, parent: &mut Scope) -> EvalRes {
        if left != Value::Null {
            return Ok(Control::Value(left));
        }
        return self.eval_node(&node.right, parent);
    }
    fn eval_binary_node(&mut self, bin_op: BinaryNode, parent: &mut Scope) -> EvalRes {
        let left = unwrap_val!(self.eval_node(&bin_op.left, parent)?);
        if bin_op.kind == BinaryOp::NullCoalescing {
            return self.null_coalescing(left, bin_op, parent);
        }
        if let Value::Bool(val) = left {
            return self.bool_calc(bin_op, val, parent);
        }
        let right = unwrap_val!(self.eval_node(&bin_op.right, parent)?);
        let left_type = left.get_type();
        let right_type = right.get_type();
        match bin_op.kind {
            BinaryOp::IsDifferent => return Ok(Control::Value(Value::Bool(left != right))),
            BinaryOp::IsEqual => return Ok(Control::Value(Value::Bool(left == right))),
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

            _ => {}
        }
        Err(IError::InvalidBinary(left_type).to_spanned(bin_op.left.span))
    }
}

///Control flow expression handling
impl Interpreter {
    fn branch(&mut self, branch: Branch, parent: &mut Scope) -> EvalRes {
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
    fn eval_loop(&mut self, loop_block: NodeStream, parent: &mut Scope) -> EvalRes {
        loop {
            let result = self.eval_block(loop_block.clone(), parent)?;
            match result {
                Control::Break => return NULL,
                Control::Continue | Control::Value(_) | Control::Result(_) => continue,
                _ => return Ok(result),
            }
        }
    }
    fn eval_while_loop(&mut self, loop_node: While, parent: &mut Scope) -> EvalRes {
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
                Control::Continue | Control::Value(_) | Control::Result(_) => continue,
                _ => return Ok(result),
            }
        }
    }
    fn eval_for_loop(&mut self, for_loop: ForLoop, parent: &mut Scope) -> EvalRes {
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
                Control::Continue | Control::Value(_) | Control::Result(_) => {
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
    fn declare(&mut self, request: &Declaration, parent: &mut Scope, in_struct: bool) -> EvalRes {
        let value = match unwrap_val!(self.eval_node(&request.value, parent)?) {
            Value::Closure(cl) => {
                let scope = &mut self.envs[cl.env];
                scope.define(request.var_name.clone(), Value::Closure(cl.clone()));
                Value::Closure(cl)
            }
            Value::Function(func)
                if in_struct && (request.var_name == "__repr" && func.args.len() != 1) =>
            {
                return unspec_err(
                    "__repr should accept exactly 1 argument",
                    request.value.span,
                );
            }

            Value::Void => return Err(IError::VoidAssignment.to_spanned(request.value.span)),
            value => value,
        };

        parent.define(request.var_name.to_owned(), value);

        VOID
    }
    fn eval_var(&self, name: String, span: Span, env: &Scope) -> EvalRes {
        let maybe_result = env.get_var(&name);
        let Some(result) = maybe_result else {
            return self.default_scope(name, span);
        };
        Ok(Control::Value(result))
    }
    fn default_scope(&self, name: String, span: Span) -> EvalRes {
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
    ) -> EvalRes {
        let target = unwrap_val!(self.eval_node(&request.target, parent)?);

        self.eval_access(target, *request.requested, base_env, parent)
    }
    fn eval_access(
        &mut self,
        target: Value,
        requested: NodeSpan,
        base_env: &mut Scope,
        parent: &mut Scope,
    ) -> EvalRes {
        let val = target.clone();
        match target {
            Value::Ref(id) => self.access_ref(id, requested, base_env, parent),

            Value::Str(str) => {
                self.access_primitive(val, NativeObject::new("String", str), requested, base_env)
            }
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
            Value::Closure(_) => self.eval_access_request(
                requested,
                base_env,
                &mut defaults::closure_obj().env,
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
    ) -> EvalRes {
        match &self.heap[id] {
            Value::Struct(obj) => self.access_struct(id, requested, base_env, obj.clone()),
            Value::List(list) => self.access_native_object(
                id,
                NativeObject::new("List", list.clone()),
                requested,
                base_env,
            ),
            Value::NativeObject(obj) => {
                self.access_native_object(id, obj.clone(), requested, base_env)
            }
            val => self.eval_access(val.clone(), requested, base_env, parent),
        }
    }
    fn access_primitive(
        &mut self,
        val: Value,
        mut obj: NativeObject,
        requested: NodeSpan,
        base_env: &mut Scope,
    ) -> EvalRes {
        match requested.item.clone() {
            Node::Call(request) => {
                let res = self.eval_primitive_method(&mut obj, request.clone(), base_env);
                let res = match res {
                    Ok(res) => res,
                    Err(err) => {
                        if obj.inner.get_type_id() == TypeId::of::<GlobalMethods>() {
                            return Err(err);
                        };
                        if !matches!(err.item, IError::MethodNotFound(_, _)) {
                            return Err(err);
                        }
                        let mut methods_obj =
                            NativeObject::new("GlobalMethods", GlobalMethods(val));
                        self.eval_primitive_method(&mut methods_obj, request, base_env)?
                    }
                };
                Ok(res)
            }
            Node::Variable(name) => {
                let Some(val) = obj.inner.lang_get(&name, self) else {
                    return Err(IError::NonExistentVar(name.to_owned()).to_spanned(requested.span));
                };
                return Ok(Control::Value(val));
            }
            _ => unimplemented!(),
        }
    }
    fn access_native_object(
        &mut self,
        id: RefKey,
        obj: NativeObject,
        requested: NodeSpan,
        base_env: &mut Scope,
    ) -> EvalRes {
        match requested.item.clone() {
            Node::Call(request) => self.eval_native_method(request, id, obj, base_env),
            Node::Variable(name) => {
                self.native_object_get(&name, id, obj, base_env, requested.span)
            }
            _ => unimplemented!(),
        }
    }
    fn access_struct(
        &mut self,
        id: RefKey,
        requested: NodeSpan,
        base_env: &mut Scope,
        mut obj: Struct,
    ) -> EvalRes {
        let result = self.eval_access_request(requested, base_env, &mut obj.env, Value::Ref(id))?;
        Ok(result)
    }
    fn eval_access_request(
        &mut self,
        requested: NodeSpan,
        base_env: &mut Scope,
        obj_env: &mut Scope,
        obj: Value,
    ) -> EvalRes {
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
    ) -> EvalRes {
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
    ) -> EvalRes {
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

    fn assign(&mut self, request: Assignment, parent: &mut Scope) -> EvalRes {
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
            _ => return unspec_err("Unassignable item", span),
        };
        VOID
    }
}

///Function and function call handling
impl Interpreter {
    fn eval_funcdef(&mut self, func: FuncDef, parent: &mut Scope) -> EvalRes {
        if !func.captures {
            return Ok(Control::Value(
                Function {
                    args: func.args,
                    block: func.block,
                }
                .into(),
            ));
        }
        let key = self.envs.insert(parent.clone());

        return Ok(Control::Value(
            Closure {
                args: func.args,
                block: func.block,
                env: key,
            }
            .into(),
        ));
    }
    pub(crate) fn call_closure(
        &mut self,
        closure: Closure,
        arg_values: &[Value],
        span: Span,
    ) -> EvalRes<Value> {
        let id = closure.env;
        let mut env = self.envs[id].clone();
        let result = self.call_func(closure.into(), arg_values, span, &mut env)?;

        self.envs[id] = env;

        return Ok(result);
    }

    fn capture_fn(&mut self, val: Value, parent: &mut Scope) -> Value {
        if let Value::Function(func) = val {
            return Closure {
                args: func.args,
                block: func.block,
                env: self.envs.insert(parent.clone()),
            }
            .into();
        }
        return val;
    }
    fn eval_call(&mut self, request: Call, base_env: &mut Scope, parent: &mut Scope) -> EvalRes {
        let func_val = unwrap_val!(self.eval_node(&request.callee, parent)?);
        let call_args = request.args;
        let mut arg_values: Vec<Value> = vec![];
        for arg in call_args.iter() {
            let argument = unwrap_val!(self.eval_node(arg, base_env)?);
            arg_values.push(self.capture_fn(argument, parent));
        }
        let arg_span = if !call_args.is_empty() {
            Span(request.callee.span.1 + 1, call_args.last().unwrap().span.1)
        } else {
            Span(request.callee.span.1 + 1, request.callee.span.1 + 2)
        };

        match func_val {
            Value::BuiltinFunc(func) => self.call_builtin(func, arg_values, arg_span, parent),
            Value::Function(called) => {
                let res = self.call_func(called, &arg_values, request.callee.span, parent)?;
                Ok(res.into())
            }
            Value::Closure(id) => {
                let res = self.call_closure(id, &arg_values, request.callee.span)?;
                Ok(res.into())
            }
            _ => return type_err(Type::Function, func_val.get_type(), request.callee.span),
        }
    }

    pub(crate) fn call_func(
        &mut self,
        called: Function,
        args: &[Value],
        span: Span,
        parent: &mut Scope,
    ) -> EvalRes<Value> {
        if args.len() != called.args.len() {
            return Err(
                IError::InvalidArgSize(called.args.len() as u32, args.len() as u32)
                    .to_spanned(span),
            );
        }
        let arg_map = self.build_args(&called, args);
        let arg_scope = Scope::from_vars(arg_map);
        let result = self.eval_block_with(called.block, parent, arg_scope)?;
        match result {
            Control::Continue | Control::Break => {
                return Err(IError::InvalidControl.to_spanned(span));
            }
            Control::Result(val) | Control::Return(val) | Control::Value(val) => {
                if val.is_void() {
                    return unspec_err("Attempted to return void", span);
                }
                return Ok(val);
            }
        }
    }
    fn build_args(&self, func: &Function, arg_values: &[Value]) -> HashMap<String, Value> {
        let mut arg_map = HashMap::<String, Value>::new();
        for (idx, val) in arg_values.iter().enumerate() {
            arg_map.insert(func.args[idx].clone(), val.clone());
        }
        arg_map
    }
    fn handle_panic(&mut self, err: Value, span: Span) -> EvalRes<Value> {
        let val = if let Value::Ref(key) = err {
            self.heap[key].clone()
        } else {
            err
        };
        match &val {
            Value::Struct(obj) => {
                let Some(ref id) = obj.id else {
                    return Err(IError::Panic(val.to_string()).to_spanned(span));
                };
                if id != "Error" {
                    return Err(IError::Panic(val.to_string()).to_spanned(span));
                }
                let msg = obj.env.get_var("msg").unwrap();
                return Err(IError::Panic(msg.to_string()).to_spanned(span));
            }
            _ => return Err(IError::Panic(val.to_string()).to_spanned(span)),
        }
    }
    fn handle_func_results(&mut self, result: FuncResult, span: Span) -> EvalRes<Value> {
        Ok(catch! { err {
            use CallError as Ce;

            match err {
                Ce::Major(val) => return Err(val.to_spanned(span)),
                Ce::Panic(val) => return self.handle_panic(val, span),
                Ce::Unspecified(err) =>return Err(IError::Unspecified(err).to_spanned(span))
            }

            } in result
        })
    }
    fn call_builtin(
        &mut self,
        called: BuiltinFunc,

        arg_values: Vec<Value>,
        span: Span,
        parent: &mut Scope,
    ) -> EvalRes {
        check_args(called.arg_range, arg_values.len(), span)?;
        let data = FuncData {
            args: arg_values,
            span,
            parent,
        };
        let result = (called.function)(data, self);
        Ok(Control::Value(self.handle_func_results(result, span)?))
    }
}
///Native object handling
impl Interpreter {
    fn construct_native_object(
        &mut self,
        constructor: Constructor,

        parent: &mut Scope,
        span: Span,
    ) -> EvalRes {
        if !self.native_constructors.contains_key(&constructor.name) {
            return unspec_err("Attempted to get a non existent struct", span);
        }
        let mut params = HashMap::<String, Value>::new();
        for (k, v) in constructor.params {
            let result = self.eval_node(&v, parent)?;
            params.insert(k, unwrap_val!(result));
        }
        let native_constructor = self.native_constructors.get(&constructor.name).unwrap();

        let constructor_data = NativeConstructorData {
            arguments: params,
            parent,
            span,
        };

        let obj = catch!( err {
            return unspec_err(err, span);
        } in native_constructor(constructor_data,self));
        let key = self.heap.insert(Value::NativeObject(obj));
        Ok(Control::Value(Value::Ref(key)))
    }
    fn eval_constructor(
        &mut self,
        constructor: Constructor,
        parent: &mut Scope,
        span: Span,
    ) -> EvalRes {
        let Ok(request) = self.get_struct(&constructor.name, span, parent) else {
            return self.construct_native_object(constructor, parent, span);
        };
        let struct_val = self.construct_fields(request, constructor.params, parent)?;
        let key = self.heap.insert(unwrap_val!(struct_val));
        Ok(Control::Value(Value::Ref(key)))
    }
    fn native_method_results(
        &mut self,
        result: NativeFuncResult,
        requested: &str,
        target: &str,
        span: Span,
    ) -> EvalRes<Value> {
        Ok(catch! { err {
            use NativeCallError as Ce;

            match err {
                Ce::Major(val) => return Err(val.to_spanned(span)),
                Ce::MethodNotFound => {
                        return IError::MethodNotFound(
                            requested.to_owned(),Some(target.to_owned())
                        ).to_spanned(span).err()
                },
                Ce::Unspecified(msg) => return Err(IError::Unspecified(msg).to_spanned(span)),
                Ce::Panic(val) => return self.handle_panic(val, span),
            }

            } in result
        })
    }
    fn native_object_get(
        &mut self,
        name: &str,
        id: RefKey,
        mut obj: NativeObject,
        base_env: &mut Scope,
        span: Span,
    ) -> EvalRes {
        let Some(val) = obj.inner.lang_get(&name, self) else {
            return Err(IError::NonExistentVar(name.to_owned()).to_spanned(span));
        };
        self.heap[id] = Value::NativeObject(obj);
        return Ok(val.into());
    }
    fn eval_primitive_method(
        &mut self,
        obj: &mut NativeObject,
        request: Call,
        base_env: &mut Scope,
    ) -> EvalRes {
        let Node::Variable(method) = &request.callee.item else {
            unimplemented!()
        };
        let call_args = request.args;
        let mut args: Vec<Value> = vec![];
        for arg in call_args.iter() {
            let argument = unwrap_val!(self.eval_node(arg, base_env)?);

            args.push(self.capture_fn(argument, base_env));
        }
        let arg_span = if !call_args.is_empty() {
            Span(request.callee.span.1 + 1, call_args.last().unwrap().span.1)
        } else {
            Span(request.callee.span.1, request.callee.span.1 + 2)
        };
        let data = FuncData {
            args,
            span: arg_span,
            parent: base_env,
        };
        let raw_res = obj.inner.call_native_method(&method, self, data);
        let res =
            self.native_method_results(raw_res, &method, &obj.id, request.callee.span + arg_span)?;
        return Ok(Control::Value(res));
    }
    fn eval_native_method(
        &mut self,
        request: Call,
        id: RefKey,
        mut obj: NativeObject,
        base_env: &mut Scope,
    ) -> EvalRes {
        let res = self.eval_primitive_method(&mut obj, request.clone(), base_env);
        let res = match res {
            Ok(res) => res,
            Err(err) => {
                if obj.inner.get_type_id() == TypeId::of::<GlobalMethods>() {
                    return Err(err);
                };
                if !matches!(err.item, IError::MethodNotFound(_, _)) {
                    return Err(err);
                }
                let mut methods_obj = NativeObject::new(
                    "GlobalMethods",
                    GlobalMethods(Value::NativeObject(obj.clone())),
                );
                self.eval_primitive_method(&mut methods_obj, request, base_env)?
            }
        };
        self.heap[id] = Value::NativeObject(obj);
        return Ok(res);
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
    ) -> EvalRes {
        let Node::Variable(method) = &request.callee.item else {
            unimplemented!()
        };
        let Some(func_val) = obj_env.get_var(method) else {
            return self.eval_primitive_method(
                &mut NativeObject::new("GlobalMethods", natives::GlobalMethods(obj)),
                request,
                base_env,
            );
        };

        let call_args = request.args;

        let mut arg_values: Vec<Value> = vec![obj];
        match func_val {
            // This is done so as to when a method has no arguments we wont be passing implicit self
            Value::BuiltinFunc(ref func) => {
                if func.arg_range.is_some_and(|range| range == (0, 0)) {
                    arg_values = vec![];
                }
            }
            Value::Function(ref called) => {
                if called.args.is_empty() {
                    arg_values = vec![];
                }
            }
            Value::Closure(ref called) => {
                if called.args.is_empty() {
                    arg_values = vec![];
                }
            }
            _ => return type_err(Type::Function, func_val.get_type(), request.callee.span),
        };
        for arg in call_args.iter() {
            let argument = unwrap_val!(self.eval_node(arg, base_env)?);

            arg_values.push(self.capture_fn(argument, base_env));
        }
        let arg_span = if !call_args.is_empty() {
            Span(request.callee.span.1 + 1, call_args.last().unwrap().span.1)
        } else {
            Span(request.callee.span.1, request.callee.span.1 + 2)
        };
        match func_val {
            Value::BuiltinFunc(func) => self.call_builtin(func, arg_values, arg_span, obj_env),
            Value::Function(called) => {
                let res = self.call_func(called, &arg_values, request.callee.span, obj_env)?;
                Ok(res.into())
            }
            Value::Closure(called) => {
                let res = self.call_closure(called, &arg_values, request.callee.span)?;
                Ok(res.into())
            }
            _ => type_err(Type::Function, func_val.get_type(), request.callee.span),
        }
    }
    fn get_struct(&self, name: &String, span: Span, parent: &mut Scope) -> EvalRes<Struct> {
        if let Some(obj) = parent.get_struct(name) {
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
    ) -> EvalRes {
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

    fn eval_structdef(&mut self, def: StructDef, parent: &mut Scope) -> EvalRes {
        let mut struct_env = Scope::default();
        for field in def.fields {
            if let Field::Declaration(decl) = &field.item {
                self.declare(decl, &mut struct_env, true)?;
                continue;
            }
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
    ) -> EvalRes {
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
            v => unspec_err(
                format!("Cant index type {}", v.0.get_type()),
                index_node.span,
            ),
        }
    }
    fn eval_list(&mut self, lit: Vec<NodeSpan>, parent: &mut Scope) -> EvalRes {
        let mut list: Vec<Value> = vec![];
        for node in lit {
            let val = unwrap_val!(self.eval_node(&node, parent)?);
            list.push(self.capture_fn(val, parent));
        }
        let val = self.heap.insert(Value::List(list));
        Ok(Control::Value(Value::Ref(val)))
    }
}
fn unspec_err<T>(msg: impl Display, span: Span) -> EvalRes<T> {
    return IError::Unspecified(msg.to_string()).to_spanned(span).err();
}
fn type_err<T>(expected: Type, got: Type, span: Span) -> EvalRes<T> {
    return IError::InvalidType(vec![expected], got)
        .to_spanned(span)
        .err();
}
fn check_args(arg_range: Option<(u8, u8)>, given_size: usize, mut span: Span) -> EvalRes<()> {
    let Some(range) = arg_range else {
        return Ok(());
    };
    if given_size == 0 {
        span.0 -= 1;
    }
    if range.1 == 0 && given_size != 0 {
        return unspec_err(format!("Expected no arguments but got {given_size}"), span);
    }

    if given_size > range.1.into() {
        return unspec_err(
            format!(
                "Given arguments are greater then expected; Expected atmost {max} but got {given_size}",
                max = range.1
            ),
            span,
        );
    }
    if given_size < range.0.into() {
        return unspec_err(
            format!(
                "Given arguments are lesser then expected; Expected atleast {min} but got {given_size}",
                min = range.0
            ),
            span,
        );
    }
    return Ok(());
}
