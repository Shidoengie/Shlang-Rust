use crate::ast_nodes;
use crate::defaults;
use crate::lang_errors::*;
use crate::spans::*;
use ast_nodes::*;
use std::collections::HashMap;
use std::*;
type IError = InterpreterError;
type EvalResult = Result<TypedValue, InterpreterError>;
const VOID: EvalResult = Ok((Value::Void, Type::Void));
const NULL: EvalResult = Ok((Value::Null, Type::Null));
#[derive(Debug)]
pub struct Interpreter {
    program: NodeStream,
    heap: HashMap<u32, Value>,
}
impl Interpreter {
    pub fn new(program: NodeStream) -> Self {
        Self {
            program,
            heap: HashMap::from([]),
        }
    }
    pub fn execute(&mut self) -> EvalResult {
        self.eval_block(self.program.clone(), &mut defaults::default_scope())
    }
    pub fn execute_node(node:NodeSpan) -> EvalResult {
        Self{
            program:vec![Node::DontResult.to_spanned((0,0))],
            heap:HashMap::from([]),
        }.eval_node(&node, &mut Scope::new_child_in(defaults::default_scope()))
        
    }
    
    fn eval_block(&mut self, block: NodeStream, parent: &mut Scope) -> EvalResult {
        let mut new_scope = Scope::new_child_in(parent.clone());
        if block.len() == 0 {
            return NULL;
        }
        let body = block;
        for node in body {
            let Value::Control(result) = self.eval_node(&node,&mut new_scope)?.0 else {continue;};
            let Some(mod_parent) = new_scope.parent else {
                
                return Err(IError::InvalidControl(node.span));
            };
            *parent = *mod_parent;
            return Ok((Value::Control(result), Type::Never));
        }

        let Some(mod_parent) = new_scope.parent else {
            return Err(IError::InvalidControl((0,0)));
        };
        *parent = *mod_parent;
        NULL
    }
    fn heap_define(&mut self, value: Value) -> u32 {
        let ref_id = self
            .heap
            .len();
        self.heap.insert(ref_id.try_into().unwrap(), value);
        ref_id as u32
    }
    fn num_convert(&self, num: Value, span: Span) -> Result<(f64, bool), IError> {
        match num {
            Value::Num(val) => Ok((val, val != 0.0)),
            Value::Bool(cond) => Ok((cond as i8 as f64, cond)),
            invalid => Err(IError::InvalidType(vec![Type::Num,Type::Bool],invalid.get_type(),span)),
        }
    }
    fn unary_calc(&self, node: UnaryNode, target: TypedValue) -> EvalResult {
        match node.kind {
            UnaryOp::NEGATIVE => {
                let Value::Num(val) = target.0 else {
                    return Err(IError::InvalidType(vec![Type::Num],target.1, node.object.span));
                };
                Ok((Value::Num(-val), Type::Num))
            }
            UnaryOp::NOT => Ok((
                Value::Bool(!self.num_convert(target.0, node.object.span)?.1),
                Type::Bool,
            ))
        }
    }
    fn binary_calc(
        &self,
        node: BinaryNode,
        left_val: Value,
        right_val: Value,
    ) -> Result<Value, IError> {
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
    fn str_calc(&self, node: BinaryNode, left_val: Value, right_val: Value) -> Result<Value, IError> {
        let Value::Str(left) = left_val else {unreachable!()};
        let Value::Str(right) = right_val else {unreachable!()};

        match node.kind {
            BinaryOp::ADD => Ok(Value::Str(left + &right)),
            BinaryOp::GREATER => Ok(Value::Bool(left > right)),
            BinaryOp::GREATER_EQUAL => Ok(Value::Bool(left >= right)),
            BinaryOp::LESSER => Ok(Value::Bool(left < right)),
            BinaryOp::LESSER_EQUAL => Ok(Value::Bool(left <= right)),
            BinaryOp::ISEQUAL => Ok(Value::Bool(left == right)),
            BinaryOp::ISDIFERENT => Ok(Value::Bool(left != right)),
            op => Err(IError::InvalidOp(op, Type::Str, (node.left.span.1, node.right.span.0)))
        }
    }

    fn eval_binary_node(&mut self, bin_op: BinaryNode, parent: &mut Scope) -> EvalResult {
        let left = self.eval_node(&bin_op.left, parent)?.unwrap_result();
        if left.1 == Type::Never {
            return Ok(left);
        }
        let right = self.eval_node(&bin_op.right, parent)?.unwrap_result();
        if right.1 == Type::Never {
            return Ok(right);
        }
        match bin_op.kind {
            BinaryOp::ISDIFERENT => return Ok((Value::Bool(left != right), Type::Bool)),
            BinaryOp::ISEQUAL => return Ok((Value::Bool(left == right), Type::Bool)),
            _ => {}
        }
        if left.1 != right.1 && !(left.1.is_numeric() && right.1.is_numeric()) {
            return Err(IError::MixedTypes(left.1, right.1,(bin_op.left.span.0,bin_op.right.span.1)));
        }
        match left.1 {
            Type::Num | Type::Bool => {
                let result = (self.binary_calc(bin_op, left.0, right.0)?, left.1);
                return Ok(result);
            }
            Type::Str => {
                let result = (self.str_calc(bin_op, left.0, right.0)?, left.1);
                return Ok(result);
            }
            _ => {}
        }
        
        Err(IError::InvalidBinary(left.1, bin_op.left.span))
    }
    fn unwrap_var(&self, value: TypedValue, span: Span) -> EvalResult {
        let err = Err(IError::VoidAssignment(span));
        if value.1 == Type::Void {
            return err;
        }
        let Value::Control(Control::Result(res_val,res_type)) = value.0 else {
            return Ok(value);
        };
        if res_type == Type::Void {
            return err;
        }
        Ok((*res_val, res_type))
    }
    fn declare(&mut self, request: Declaration, parent: &mut Scope) -> EvalResult {
        let init_val = self.eval_node(&request.value, parent)?;
        let unwrapped = self.unwrap_var(init_val, request.value.span)?;
        parent.define(request.var_name, unwrapped.0);
        VOID
    }
    fn get_struct_id(&self, val: &Value) -> u32 {
        let Value::StructRef(id) = val else {panic!()};
        *id
    }
    fn struct_from_heap(&self, id: u32) -> Struct {
        let Value::Struct(obj) = self.heap.get(&id).unwrap().clone() else {panic!()};
        obj
    }
    fn assign_to_access(
        &mut self,
        request: FieldAccess,
        value: TypedValue,
        parent: &mut Scope,
    ) -> EvalResult {
        let target = self.eval_node(&request.target, parent)?.0;
        let id = self.get_struct_id(&target);
        let mut obj = self.struct_from_heap(id);
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
        value: TypedValue,
        span: Span,
        target: &mut Scope,
    ) -> EvalResult {
        if target
            .assign(name.clone(), self.unwrap_var(value, span)?.0)
            .is_none()
        {
            return Err(IError::InvalidAssignment(name, span));
        }
        VOID
    }
    fn assign(&mut self, request: Assignment, parent: &mut Scope) -> EvalResult {
        let init_val = self.eval_node(&request.value, parent)?;
        match request.clone().target.unspanned {
            Node::Variable(var) => 
                self.assign_to_name(var, init_val, request.target.span, parent),
            
            Node::FieldAccess(field) => {
                self.assign_to_access(field, init_val, parent)?;
                VOID
            }
            _ => todo!(),
        }
    }
    fn default_scope(&self, name: String, span: Span) -> EvalResult {
        let maybe_result =
            Scope::new(None, defaults::var_map(), HashMap::from([])).get_var(&name);
        let Some(result) = maybe_result else {
            return Err(IError::NonExistentVar(name, span));
        };
        let res_type = result.get_type();
        Ok((result, res_type))
    }
    fn eval_var(&self, name: String, span: Span, env: &Scope) -> EvalResult {
        let maybe_result = env.get_var(&name);
        let Some(result) = maybe_result else {
            return self.default_scope(name, span);
        };
        let res_type = result.get_type();
        Ok((result, res_type))
    }
    fn call_builtin(
        &mut self,
        called: BuiltinFunc,
        arg_values: ValueStream,
        span: Span,
        parent: &mut Scope,
    ) -> EvalResult {
        if called.arg_size != -1 && arg_values.len() as i16 != called.arg_size {
            return Err(IError::InvalidArgSize(called.arg_size as u32, arg_values.len() as u32, span));
        }
        let result = (called.function)(parent.vars.clone(), arg_values);
        let result_type = result.get_type();
        Ok((result, result_type))
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
            return Err(IError::InvalidArgSize(called.args.len() as u32, arg_values.len() as u32, span));
        }
        called.block = self.build_args(&called, arg_spans, arg_values);
        let (result, result_type) = self.eval_block(called.block, parent)?;
        let Value::Control(flow) = result else {
            return Ok((result,result_type));
        };
        match flow {
            Control::Return(val, kind) => Ok((*val, kind)),
            Control::Result(val, kind) => Ok((*val, kind)),
            _ => Err(IError::InvalidControl(span)),
            
        }
    }
    fn eval_call(&mut self, request: Call, base_env: &mut Scope, parent: &mut Scope) -> EvalResult {
        let (func_val, kind) = self.eval_node(&request.callee, parent)?;
        if kind != Type::Function {
            return Err(IError::InvalidType(vec![Type::Function], kind, request.callee.span,));
        }
        let call_args = *request.args;
        let mut arg_values: ValueStream = vec![];
        let mut arg_spans: Vec<Span> = vec![];
        for arg in call_args {
            arg_spans.push(arg.span);
            let argument = self.eval_node(&arg, base_env)?.0;
            arg_values.push(argument);
        }
        let arg_span = if !arg_spans.is_empty() {
            (request.callee.span.1 + 1, arg_spans.last().unwrap().1)
        } else {
            (request.callee.span.1 + 1, request.callee.span.1 + 2)
        };
        match func_val {
            Value::BuiltinFunc(func) => self.call_builtin(func, arg_values, arg_span, parent),
            Value::Function(called) => self.call_func(called, arg_values, arg_spans, request.callee.span, parent),
            _ => panic!(),
        }
    }
    fn branch(&mut self, branch: Branch, parent: &mut Scope) -> EvalResult {
        
        let condition = self.eval_node(&branch.condition, parent)?;
        let cond_val = self.num_convert(condition.0, branch.condition.span)?.1;

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
            let (result, result_type) = self.eval_block(loop_block.clone(), parent)?;
            let Value::Control(flow) = result.clone() else {continue;};
            match flow {
                Control::Break => return NULL,
                Control::Continue => continue,
                _ => return Ok((result, result_type)),
            }
        }
    }
    fn eval_while_loop(&mut self, loop_node: While, parent: &mut Scope) -> EvalResult {
        loop {
            let condition = self.eval_node(&loop_node.condition, parent)?;
            let cond_val = self.num_convert(condition.0, loop_node.condition.span)?.1;
            if !cond_val {
                return NULL;
            }
            let (result, result_type) = self.eval_block(loop_node.proc.clone(), parent)?;
            let Value::Control(flow) = result.clone() else {continue;};
            match flow {
                Control::Break => return NULL,
                Control::Continue => continue,
                _ => return Ok((result, result_type)),
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
    fn access_struct(&mut self, id: u32, requested: NodeSpan, base_env: &mut Scope) -> EvalResult {
        let Value::Struct(mut obj) = self.heap.get(&id).unwrap().clone() else {panic!("IMPROVE ME")};
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
        match target.0 {
            Value::StructRef(id) => self.access_struct(
                id, 
                *request.requested, 
                base_env
            ),
            Value::Str(val) => self.eval_access_request(
                *request.requested, 
                base_env, 
                &mut defaults::str_struct(val).env
            ),
            Value::Num(val) => self.eval_access_request(
                *request.requested, 
                base_env, 
                &mut defaults::num_struct(val).env
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
            let result = self.eval_node(&v, parent)?.0;
            obj.env.vars.insert(k, result);
        }
        Ok((Value::Struct(obj.clone()), Type::UserDefined(obj.id)))
    }
    fn eval_constructor(&mut self, constructor: Constructor, parent: &mut Scope) -> EvalResult {
        let Some(request) = parent.get_struct(&constructor.name) else {panic!("Attempted to construct a non existent struct")};
        let struct_val = self
            .assign_fields(request, constructor.params, parent)?
            .0;
        let id = self.heap_define(struct_val);
        Ok((Value::StructRef(id), Type::Ref(id)))
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
        let id = self.heap_define(struct_val);
        Ok((Value::StructRef(id), Type::Ref(id)))
    }

    fn eval_node(&mut self, node: &NodeSpan, parent: &mut Scope) -> EvalResult {
        let (span, expr) = (node.span, node.unspanned.clone());
        match expr {
            Node::Value(val) => {
                // let kind = val.clone().get_type();
                // Ok((*val, kind))
                let kind = val.get_type();
                let Type::Ref(id) = kind else {return Ok((val, kind));};
                let new_val = self.heap.get(&id).unwrap();
                let new_kind = val.get_type();
                Ok((new_val.clone(), new_kind))
            }
            
            Node::Variable(var) => self.eval_var(var, span, parent),
            Node::Call(request) => self.eval_call(request, &mut parent.clone(), parent),
            Node::UnaryNode(unary_op) => {
                let (target, target_type) = self.eval_node(&unary_op.object, parent)?;
                self.unary_calc(unary_op, (target, target_type))
            }
            Node::BinaryNode(bin_op) => self.eval_binary_node(bin_op, parent),
            Node::Branch(branch) => self.branch(branch, parent),
            Node::ReturnNode(expr) => {
                let evaluated = self.eval_node(&expr, parent)?;
                let result: Value = Control::Return(Box::new(evaluated.0), evaluated.1).into();
                Ok((result, Type::Never))
            }
            Node::ResultNode(expr) => {
                let evaluated = self.eval_node(&expr, parent)?;
                let result: Value = Control::Result(Box::new(evaluated.0), evaluated.1).into();
                Ok((result, Type::Never))
            }
            Node::BreakNode => Ok((Control::Break.into(), Type::Never)),
            Node::ContinueNode => Ok((Control::Continue.into(),Type::Never)),
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
