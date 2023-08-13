use crate::ast_nodes;
use crate::defaults;
use crate::lang_errors::*;
use crate::spans::*;
use ast_nodes::*;
use std::collections::HashMap;
use std::*;
const VOID: Result<TypedValue, ()> = Ok((Value::Void, Type::Void));
const NULL: Result<TypedValue, ()> = Ok((Value::Null, Type::Null));
pub struct Interpreter {
    program: BlockSpan,
    err_out: ErrorBuilder,
}
impl Interpreter {
    pub fn new(program: BlockSpan, input: String) -> Self {
        return Interpreter {
            program,
            err_out: ErrorBuilder::new(input),
        };
    }
    fn emit_err(&self, msg: String, span: Span) {
        self.err_out.emit(msg.as_str(), span);
    }
    pub fn execute(&mut self) -> Result<TypedValue, ()> {
        return self.eval_block(self.program.clone(), &mut defaults::default_scope());
    }

    fn eval_block(&mut self, block: BlockSpan, parent: &mut Scope) -> Result<TypedValue, ()> {
        let mut new_scope = Scope::new_child_in(*parent);
        if block.unspanned.body.len() == 0 {
            return NULL;
        }
        let body = *block.unspanned.body;
        for node in body {
            let Value::Control(result) = self.eval_node(&node,&mut new_scope)?.0 else {
                continue;
            };
            if new_scope.parent.is_none() {
                self.err_out
                    .emit("Invalid control outside block", node.span);
                return Err(());
            }
            return Ok((Value::Control(result), Type::Never));
        }

        if new_scope.parent.is_none() {
            self.err_out.emit("Invalid control outside block", (0, 0));
            return Err(());
        }
        return NULL;
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

    fn eval_binary_node(
        &mut self,
        bin_op: BinaryNode,
        parent: &mut Scope,
    ) -> Result<TypedValue, ()> {
        let left = self.eval_node(&*bin_op.left, parent)?.unwrap_result();
        if left.1 == Type::Never {
            return Ok(left);
        }
        let right = self.eval_node(&*bin_op.right, parent)?.unwrap_result();
        if right.1 == Type::Never {
            return Ok(right);
        }
        match bin_op.kind {
            BinaryOp::ISDIFERENT => return Ok((Value::Bool(left != right), Type::Bool)),
            BinaryOp::ISEQUAL => return Ok((Value::Bool(left == right), Type::Bool)),
            _ => {}
        }
        if left.1 != right.1 && !(left.1.is_numeric() && right.1.is_numeric()) {
            panic!("mixed types: {left:?} {right:?}")
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
        self.emit_err(
            format!("Invalid type in binary operation: {:?}", left.1),
            bin_op.left.span,
        );
        return Err(());
    }
    fn unwrap_var(&self, value: TypedValue, span: Span) -> Result<TypedValue, ()> {
        if value.1 == Type::Void {
            self.err_out.emit("Cannot assign void to variable", span);
            return Err(());
        }
        let Value::Control(Control::Result(res_val,res_type)) = value.0 else {
            return Ok(value);
        };
        if res_type == Type::Void {
            self.err_out.emit("Cannot assign void to variable", span);
            return Err(());
        }
        return Ok((*res_val, res_type));
    }
    fn declare(&mut self, request: Declaration, parent: &mut Scope) -> Result<TypedValue, ()> {
        let init_val = self.eval_node(&*request.value, parent)?;
        let unwrapped = self.unwrap_var(init_val, request.value.span)?;
        parent.define(request.var_name, unwrapped.0);
        return VOID;
    }
    fn assign_to_access(
        &mut self,
        request: FieldAccess,
        value: TypedValue,
        parent: &mut Scope,
    ) -> Result<TypedValue, ()> {
        todo!()
    }

    fn assign_to_name(
        &mut self,
        name: String,
        value: TypedValue,
        span: Span,
        target: &mut Scope,
    ) -> Result<TypedValue, ()> {
        if target
            .assign(name, self.unwrap_var(value, span)?.0)
            .is_none()
        {
            self.err_out
                .emit("Attempted to assign to a non existent variable", span);
            return Err(());
        }
        return VOID;
    }
    fn assign(&mut self, request: Assignment, parent: &mut Scope) -> Result<TypedValue, ()> {
        let init_val = self.eval_node(&*request.value, parent)?;
        match request.clone().target.unspanned {
            Node::Variable(var) => {
                return self.assign_to_name(var.name, init_val, request.target.span, parent);
            }
            Node::FieldAccess(field) => {
                self.assign_to_access(field, init_val.clone(), parent)?;
                VOID
            }
            _ => todo!(),
        }
    }
    fn default_scope(&self, var: Variable, span: Span) -> Result<TypedValue, ()> {
        let maybe_result =
            Scope::new(None, defaults::var_map(), HashMap::from([])).get_var(&var.name);
        let Some(result) = maybe_result else {
            self.emit_err("Non existing variable".to_string(), span);
            return Err(());
        };
        let res_type = result.get_type();
        return Ok((result, res_type.clone()));
    }
    fn eval_var(&self, var: Variable, span: Span, env: &Scope) -> Result<TypedValue, ()> {
        let maybe_result = env.get_var(&var.name);
        let Some(result) = maybe_result else {
            return self.default_scope(var, span);
        };
        let res_type = result.get_type();
        return Ok((result, res_type.clone()));
    }
    fn call_builtin(
        &mut self,
        called: BuiltinFunc,
        arg_values: ValueStream,
        span: Span,
        parent: &mut Scope,
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
        let result = (called.function)(parent.vars.clone(), arg_values);
        let result_type = result.get_type();
        return Ok((result, result_type));
    }
    fn build_args(
        &mut self,
        func: &Function,
        arg_spans: Vec<Span>,
        arg_values: ValueStream,
    ) -> BlockSpan {
        let mut arg_decl: NodeStream = vec![];
        let mut func_block = func.block.unspanned.clone();
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
        arg_decl.append(&mut func_block.body);
        arg_decl.to_blockspan(func.block.span)
    }
    fn call_func(
        &mut self,
        mut called: Function,
        arg_values: ValueStream,
        arg_spans: Vec<Span>,
        span: Span,
        parent: &mut Scope,
    ) -> Result<TypedValue, ()> {
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
        called.block = self.build_args(&called, arg_spans, arg_values).boxed();
        let (result, result_type) = self.eval_block(*called.block, parent)?;
        let Value::Control(flow) = result else {
            return Ok((result,result_type));
        };
        match flow {
            Control::Return(val, kind) => {
                return Ok((*val, kind));
            }
            Control::Result(val, kind) => {
                return Ok((*val, kind));
            }
            _ => {
                self.err_out.emit("Unexpected control flow node", span);
                return Err(());
            }
        }
    }
    fn eval_call(
        &mut self,
        request: Call,
        base_env: &mut Scope,
        parent: &mut Scope,
    ) -> Result<TypedValue, ()> {
        let (func_val, kind) = self.eval_node(&request.callee, parent)?;
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
            Value::BuiltinFunc(func) => {
                return self.call_builtin(func, arg_values, arg_span, parent);
            }
            Value::Function(called) => {
                return self.call_func(called, arg_values, arg_spans, request.callee.span, parent);
            }
            _ => panic!(),
        }
    }
    fn branch(&mut self, branch: Branch, parent: &mut Scope) -> Result<TypedValue, ()> {
        let if_block = *branch.if_block;
        let condition = self.eval_node(&*branch.condition, parent)?;
        let cond_val = self.num_convert(condition.0, branch.condition.span)?.1;

        if cond_val {
            return self.eval_block(if_block, parent);
        }
        let Some(else_block) = branch.else_block else {
            return NULL;
        };
        return self.eval_block(*else_block, parent);
    }
    fn eval_loop(&mut self, loop_node: Loop, parent: &mut Scope) -> Result<TypedValue, ()> {
        loop {
            let (result, result_type) = self.eval_block(*loop_node.proc.clone(), parent)?;
            let Value::Control(flow) = result.clone() else {continue;};
            match flow {
                Control::Break => return NULL,
                Control::Continue => continue,
                _ => return Ok((result, result_type)),
            }
        }
    }
    fn eval_while_loop(&mut self, loop_node: While, parent: &mut Scope) -> Result<TypedValue, ()> {
        loop {
            let condition = self.eval_node(&*loop_node.condition, parent)?;
            let cond_val = self.num_convert(condition.0, loop_node.condition.span)?.1;
            if !cond_val {
                return NULL;
            }
            let (result, result_type) = self.eval_block(*loop_node.proc.clone(), parent)?;
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
    ) -> Result<TypedValue, ()> {
        match requested.unspanned {
            Node::FieldAccess(access) => return self.eval_fieldacess(access, base_env, parent),
            Node::Call(request) => return self.eval_call(request, base_env, parent),
            _ => return self.eval_node(&requested, parent),
        }
    }
    fn access_struct(
        &mut self,
        obj: &mut Struct,
        requested: NodeSpan,
        base_env: &mut Scope,
    ) -> Result<TypedValue, ()> {
        let result = self.eval_access_request(requested, base_env, &mut obj.env)?;
        return Ok(result);
    }
    fn eval_fieldacess(
        &mut self,
        request: FieldAccess,
        base_env: &mut Scope,
        parent: &mut Scope,
    ) -> Result<TypedValue, ()> {
        let target = self.eval_node(&request.target, parent)?;
        match target.0 {
            Value::Struct(mut obj) => {
                return self.access_struct(&mut obj, *request.requested, base_env)
            }
            a => panic!("{a:?}"),
        }
    }
    fn assign_fields(
        &mut self,
        target: Struct,
        params: HashMap<String, NodeSpan>,
        parent: &mut Scope,
    ) -> Result<TypedValue, ()> {
        let mut obj = target.clone();
        for (k, v) in params {
            if !obj.env.vars.contains_key(&k) {
                panic!("Atempted to assign to non existent struct field")
            }
            let result = self.eval_node(&v, parent)?.0;
            obj.env.vars.insert(k, result);
        }
        return Ok((Value::Struct(obj.clone()), Type::UserDefined(obj.id)));
    }
    fn eval_constructor(
        &mut self,
        constructor: Constructor,
        parent: &mut Scope,
    ) -> Result<TypedValue, ()> {
        let Some(request) = parent.structs.get(&constructor.name) else {panic!("Attempted to construct a non existent struct")};
        self.assign_fields(request.clone(), constructor.params, parent)
    }
    fn eval_structdef(&mut self, obj: StructDef) -> Result<TypedValue, ()> {
        let mut struct_env = Scope::new(None, HashMap::from([]), HashMap::from([]));
        for field in obj.fields {
            self.eval_node(&field.to_nodespan(), &mut struct_env)?;
        }
        let struct_val = Value::Struct(Struct {
            id: obj.name.clone(),
            env: struct_env,
        });
        return Ok((struct_val, Type::UserDefined(obj.name)));
    }

    fn eval_node(&mut self, node: &NodeSpan, parent: &mut Scope) -> Result<TypedValue, ()> {
        let (span, expr) = (node.span, node.unspanned.clone());
        match expr {
            Node::Value(val) => {
                let kind = val.clone().get_type();
                Ok((*val, kind))
            }
            Node::Block(body) => self.eval_block(body, parent),
            Node::Variable(var) => self.eval_var(var, span, parent),
            Node::Call(request) => self.eval_call(request, &mut parent.clone(), parent),
            Node::UnaryNode(unary_op) => {
                let (target, target_type) = self.eval_node(&*unary_op.object, parent)?;
                self.unary_calc(unary_op, (target, target_type))
            }
            Node::BinaryNode(bin_op) => self.eval_binary_node(bin_op, parent),
            Node::Branch(branch) => self.branch(branch, parent),
            Node::ReturnNode(expr) => {
                let evaluated = self.eval_node(&*expr, parent)?;
                let result: Value = Control::Return(Box::new(evaluated.0), evaluated.1).into();
                Ok((result, Type::Never))
            }
            Node::ResultNode(expr) => {
                let evaluated = self.eval_node(&*expr, parent)?;
                let result: Value = Control::Result(Box::new(evaluated.0), evaluated.1).into();
                Ok((result, Type::Never))
            }
            Node::BreakNode => Ok((Control::Break.into(), Type::Never)),
            Node::Declaration(declaration) => self.declare(declaration, parent),
            Node::Assignment(ass) => self.assign(ass, parent),
            Node::While(obj) => self.eval_while_loop(obj, parent),
            Node::Loop(obj) => self.eval_loop(obj, parent),
            Node::DoBlock(block) => self.eval_block(*block.body, parent),
            Node::StructDef(obj) => self.eval_structdef(obj),
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
