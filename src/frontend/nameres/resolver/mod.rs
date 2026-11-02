mod error;
use crate::{
	collections::spans::{IntoSpanned, OkToSpanned, Span, Spanned},
	frontend::{
		FileStore,
		ast::{
			nodes::{self as ast, Constructor, Node as AstNode},
			parser::GLOBAL_NAME_MAP,
		},
		nameres::{
			resolved_nodes::*,
			scope::{Scope, ScopeInfo, VarInfo},
		},
	},
	hashmap,
	lang_errors::{ErrorBox, ToErrorBox},
	symbols::SymbolId,
};
pub use error::NameErr;

use std::collections::HashMap;
pub type Result<T = RNodeSpan> = std::result::Result<T, ErrorBox<NameErr>>;
use ResolvedNode as RNode;

trait MapToRnodespan<T>
where
	T: IntoRNodespan,
{
	fn ok_to_rnodespan(self, span: Span) -> Result<Spanned<ResolvedNode>>;
}

impl<T> MapToRnodespan<T> for Result<T>
where
	T: IntoRNodespan,
{
	fn ok_to_rnodespan(self, span: Span) -> Result<Spanned<ResolvedNode>> {
		Ok(self?.to_rnodespan(span))
	}
}
pub struct NameRes<'store> {
	symbol_counter: usize,
	scopes: Scope,
	file_store: &'store mut FileStore,
	file_id: crate::collections::FileId,
	scope_locals_stack: Vec<usize>,
}
///resolver entry points
impl<'store> NameRes<'store> {
	pub fn new(file_store: &'store mut FileStore) -> Self {
		Self {
			file_store,
			file_id: crate::collections::FileId::ANON,
			scopes: Scope::default(),
			symbol_counter: 0,
			scope_locals_stack: vec![],
		}
	}
	pub fn resolve<'a>(&mut self, ast: ast::Program<'a>) -> Result<ResolvedAst<'a>> {
		self.file_id = ast.file_id;
		self.push_var_counter();
		let decls = self.resolve_toplevel(ast.proc)?;
		let local_count = self.pop_var_counter();
		Ok(ResolvedAst {
			proc: decls,
			symbol_pool: ast.symbol_pool,
			global_count: self.scopes.globals.len(),
			local_count,
			file_id: ast.file_id,
		})
	}
	pub fn resolve_expr<'a>(&mut self, expr: ast::Ast<'a>) -> Result<ResolvedAstNode<'a>> {
		self.file_id = expr.file_id;
		let start_symbol = self.symbol_counter;
		self.push_var_counter();
		let node = self.resolve_node(expr.node)?;
		let local_count = self.pop_var_counter();
		self.symbol_counter = start_symbol;
		Ok(ResolvedAstNode::new(
			node,
			self.scopes.globals.len(),
			local_count,
			expr.symbol_pool,
			expr.file_id,
		))
	}
}
///scope management
impl NameRes<'_> {
	fn push_var_counter(&mut self) {
		self.scope_locals_stack.push(0);
	}
	fn pop_var_counter(&mut self) -> usize {
		self.scope_locals_stack.pop().unwrap_or(0)
	}
	fn update_var_counter(&mut self) {
		if let Some(max) = self.scope_locals_stack.last_mut() {
			*max = self.symbol_counter;
		}
	}
	fn enter_scope(&mut self) {
		self.scopes.enter_scope();
	}
	fn enter_scope_with(&mut self, info: ScopeInfo) {
		self.scopes.enter_scope_with(info);
	}
	fn leave_scope(&mut self) {
		self.scopes.leave_scope();
	}
	fn gen_name(&mut self) -> usize {
		let old_count = self.symbol_counter;
		self.symbol_counter += 1;
		self.update_var_counter();
		old_count
	}

	fn get_var(&self, name: SymbolId, span: Span) -> Result<&VarInfo> {
		self.scopes
			.get_var(name)
			.ok_or(NameErr::UndefinedVar(name).to_errorbox(span, self.file_id))
	}
}

//toplevel resolution
impl NameRes<'_> {
	pub fn resolve_toplevel(&mut self, exprs: Vec<ast::NodeSpan>) -> Result<Vec<RNodeSpan>> {
		self.enter_scope();
		self.predeclare_toplevel(&exprs);
		let res = self.resolve_toplevel_body(exprs);
		self.leave_scope();
		res
	}

	/// Register top-level names before resolving any expressions so forward references work.
	fn predeclare_toplevel(&mut self, exprs: &[ast::NodeSpan]) {
		for val in exprs {
			let span = val.span;
			let mut modifier_span: Option<Span> = None;
			let name = match &val.item {
				AstNode::Decl(decl) if decl.hoisted => {
					modifier_span = decl.modifier_span;
					decl.name
				}
				AstNode::ClassLit(class) if class.name.is_some() => class.name.unwrap().item,
				_ => continue,
			};
			let mut info = VarInfo::new_global(name).with_span(span);
			info.modifier_span = modifier_span;
			info.define_in(&mut self.scopes);
		}
	}

	/// Resolve each top-level expression after all hoisted names have been registered.
	fn resolve_toplevel_body(&mut self, exprs: Vec<ast::NodeSpan>) -> Result<Vec<RNodeSpan>> {
		let mut local_decls = vec![];
		let mut hoisted_decls = vec![];
		let base_locals = 0usize;
		self.push_var_counter();
		for node in exprs {
			self.resolve_toplevel_expr(&mut local_decls, &mut hoisted_decls, node)?;
		}
		let inner_max = self.pop_var_counter();
		self.symbol_counter = base_locals;
		if let Some(max) = self.scope_locals_stack.last_mut() {
			*max = (*max).max(inner_max);
		}
		let mut decls = hoisted_decls;
		decls.append(&mut local_decls);
		Ok(decls)
	}

	fn resolve_toplevel_expr(
		&mut self,
		local_decls: &mut Vec<Spanned<ResolvedNode>>,
		hoisted_decls: &mut Vec<Spanned<ResolvedNode>>,
		node: Spanned<AstNode>,
	) -> Result<()> {
		match node.item {
			AstNode::Decl(decl) if decl.hoisted => {
				let expr = self.resolve_node(decl.expr.deref_item())?.box_item();
				let global = &self.scopes.globals[&decl.name];
				hoisted_decls.push(
					Decl {
						id: global.id,
						expr,
						is_global: true,
					}
					.to_rnodespan(node.span),
				);
			}
			AstNode::ClassLit(class) => {
				todo!()
				// self.scopes.enter_class();
				// let class = self.resolve_class(class);
			}

			_ => {
				let node = self.resolve_node(node)?;
				local_decls.push(node);
			}
		}
		Ok(())
	}
}

///declaration and collection resolution
impl NameRes<'_> {
	fn resolve_var_decl(&mut self, decl: ast::Decl, span: Span) -> Result<Decl> {
		let expr = self.resolve_node(decl.expr.deref_item())?.box_item();

		let id = self.gen_name();
		let mut info = VarInfo::new(decl.name, id)
			.with_readonly(decl.readonly)
			.with_item(decl.is_item)
			.with_span(span);

		info.modifier_span = decl.modifier_span;
		info.define_in(&mut self.scopes);
		Ok(Decl {
			expr,
			id,
			is_global: false,
		})
	}
	fn resolve_list(&mut self, list: Vec<Spanned<AstNode>>) -> Result<Vec<RNodeSpan>> {
		let mut new = vec![];
		for node in list {
			new.push(self.resolve_node(node)?)
		}
		Ok(new)
	}
	fn resolve_assignment(
		&mut self,
		target: Spanned<Box<AstNode>>,
		value: Spanned<Box<AstNode>>,

		span: Span,
	) -> Result {
		let target = target.deref_item();
		let Spanned {
			item: AstNode::Variable(ref name),
			span: var_span,
		} = target
		else {
			let target = self.resolve_node(target)?.box_item();
			let value = self.resolve_node(value.deref_item())?.box_item();
			return Ok(RNode::Assignment { target, value }.to_spanned(span));
		};
		let value = self.resolve_node(value.deref_item())?.box_item();
		let info = self.get_var(*name, span)?;
		if info.readonly {
			let mut span = span;
			span.start = var_span.end + 1;
			return Err(NameErr::AssignmentToReadonly {
				is_item: info.is_item,
				decl_span: info.span,
				modifier_span: info.modifier_span,
			}
			.to_errorbox(span, self.file_id));
		}
		Ok(RNode::Assignment {
			target: RNode::Variable {
				id: info.id,
				is_global: info.global,
			}
			.to_spanned(target.span)
			.box_item(),
			value,
		}
		.to_spanned(span))
	}
	fn resolve_field(&mut self, ast_field: ast::Field) -> Result<Field> {
		let default = if let Some(field) = ast_field.default {
			Some(self.resolve_node(field)?)
		} else {
			None
		};
		Ok(Field {
			default,
			readonly: ast_field.readonly,
			name_span: ast_field.name_span,
			modifier_span: ast_field.modifier_span,
			private: ast_field.private,
		})
	}
}

///node resolution
impl NameRes<'_> {
	fn resolve_class(&mut self, class: ast::ClassLit) -> Result<ClassLit> {
		let mut fields = hashmap!();
		for (key, field) in class.fields.into_iter() {
			let field = self.resolve_field(field.item)?.to_spanned(field.span);
			fields.insert(key, field);
		}
		let mut methods = hashmap!();
		for (key, method) in class.methods.into_iter() {
			let method = self.resolve_method(method.item)?.to_spanned(method.span);
			methods.insert(key, method);
		}
		Ok(ClassLit {
			methods,
			fields,
			name: class.name,
		})
	}
	fn resolve_method(&mut self, method: ast::Method) -> Result<Method> {
		let old_symbol_counter = self.symbol_counter;
		// Starts at one due to self being desugared to the first element
		self.symbol_counter = 1;
		let mut func_scope = ScopeInfo::default();
		let mut args = vec![];
		for arg in method.args {
			let new_id = self.gen_name();
			VarInfo::new(arg.item, new_id)
				.with_span(arg.span)
				.define_in(&mut func_scope);
			args.push(new_id);
		}
		self.push_var_counter();
		let block = self.resolve_block_with(method.block, func_scope)?;
		let local_count = self.pop_var_counter();
		self.symbol_counter = old_symbol_counter;
		Ok(Method {
			args,
			local_count,
			block,
			name_span: method.name_span,
			modifier_span: method.modifier_span,
			private: method.private,
		})
	}
	fn resolve_function_lit(&mut self, func: ast::FunctionLit) -> Result<FunctionLit> {
		let old_symbol_counter = self.symbol_counter;
		self.symbol_counter = 0;
		let mut func_scope = ScopeInfo::default();
		let mut args = vec![];
		for arg in func.args {
			let new_id = self.gen_name();
			VarInfo::new(arg.item, new_id)
				.with_span(arg.span)
				.define_in(&mut func_scope);
			args.push(new_id);
		}
		self.push_var_counter();
		let block = self.resolve_block_with(func.block, func_scope)?;
		let local_count = self.pop_var_counter();
		self.symbol_counter = old_symbol_counter;
		Ok(FunctionLit {
			symbols: args,
			block,
			captures: func.captures,
			local_count,
		})
	}
	fn resolve_branch(&mut self, branch: ast::Branch) -> Result<Branch> {
		let condition = self.resolve_node(branch.condition.deref_item())?.box_item();
		let if_block = self.resolve_block(branch.if_block)?;
		let else_block = branch
			.else_block
			.map(|else_block| self.resolve_block(else_block))
			.transpose()?;
		Ok(Branch {
			condition,
			if_block,
			else_block,
		})
	}
	fn resolve_for_loop(&mut self, forloop: ast::ForLoop) -> Result<ResolvedNode> {
		let loop_var = self.gen_name();
		let mut base = ScopeInfo::default();
		VarInfo::new(forloop.symbol, loop_var)
			.with_span(forloop.symbol_span)
			.define_in(&mut base);
		let list = self.resolve_node(forloop.list.deref_item())?.box_item();
		let block = self.resolve_block_with(forloop.proc, base)?;
		Ok(RNode::ForLoop {
			loop_var,
			list,
			block,
		})
	}
	fn resolve_field_access(&mut self, field: ast::FieldAccess) -> Result<ResolvedNode> {
		let target = self.resolve_node(field.target.deref_item())?.box_item();
		let requested = match field.requested.item {
			ast::AccessType::Method {
				args,
				arg_span,
				callee,
				callee_span,
			} => AccessType::Method {
				callee,
				callee_span,
				arg_span,
				args: self.resolve_list(args)?,
			},
			ast::AccessType::Property(prop) => AccessType::Property(*prop),
		}
		.to_spanned(field.requested.span);
		Ok(RNode::FieldAccess(target, requested))
	}
	fn resolve_constructor(&mut self, con: ast::Constructor) -> Result<ResolvedNode> {
		let target = self.resolve_node(con.target.deref_item())?.box_item();
		let mut params = HashMap::new();

		for (name, value) in con.params {
			params.insert(name, self.resolve_node(value)?);
		}
		Ok(RNode::Constructor { target, params })
	}

	fn resolve_call(&mut self, call: ast::Call) -> Result<ResolvedNode> {
		let callee = self.resolve_node(call.callee.deref_item())?.box_item();
		let mut args = vec![];
		for node in call.args {
			args.push(self.resolve_node(node)?);
		}
		Ok(RNode::Call { args, callee })
	}
	fn resolve_record_lit(
		&mut self,
		obj: HashMap<SymbolId, ast::NodeSpan>,
	) -> Result<ResolvedNode> {
		let mut fields = hashmap!();
		for (name, node) in obj {
			fields.insert(name, self.resolve_node(node)?.box_item());
		}
		Ok(RNode::RecordLit(fields))
	}
	fn resolve_node(&mut self, node: ast::NodeSpan) -> Result<RNodeSpan> {
		use ResolvedNode as RNode;
		let span = node.span;
		match node.item {
			AstNode::Decl(decl) => self.resolve_var_decl(decl, span).ok_to_rnodespan(span),
			AstNode::ClassLit(class) => {
				todo!()
				// self.scopes.enter_class(value);
				// if let Some(ref name) = class.name.clone() {
				// 	parent.define(name.item, VarInfo::new(name.item));
				// }
				// self.resolve_class(class).ok_to_rnodespan(span)
			}

			AstNode::SelfValue => Ok(RNode::Variable {
				id: 0,
				is_global: false,
			}
			.to_spanned(span)),
			AstNode::SelfTy => todo!(),
			AstNode::Assignment { target, value } => self.resolve_assignment(target, value, span),
			AstNode::Variable(name) => {
				let info = self.get_var(name, span)?;
				Ok(RNode::Variable {
					id: info.id,
					is_global: info.global,
				}
				.to_spanned(span))
			}

			AstNode::FunctionLit(func) => self.resolve_function_lit(func).ok_to_rnodespan(span),
			AstNode::While(node) => {
				let condition = self.resolve_node(node.condition.deref_item())?.box_item();
				let block = self.resolve_block(node.proc)?;
				Ok(RNode::While { block, condition }.to_spanned(span))
			}
			AstNode::Loop(block) => {
				let block = self.resolve_block(block)?;
				Ok(RNode::Loop(block).to_spanned(span))
			}
			AstNode::Branch(branch) => self.resolve_branch(branch).ok_to_rnodespan(span),

			AstNode::DoBlock(block) => {
				let block = self.resolve_block(block)?;
				Ok(RNode::DoBlock(block).to_spanned(span))
			}
			AstNode::Return(node) => {
				let node = self.resolve_node(node.deref_item())?.box_item();
				Ok(RNode::Return(node).to_spanned(span))
			}
			AstNode::Result(node) => {
				let node = self.resolve_node(node.deref_item())?.box_item();
				Ok(RNode::Result(node).to_spanned(span))
			}

			AstNode::BinaryNode(bin) => {
				let left = self.resolve_node(bin.left.deref_item())?.box_item();
				let right = self.resolve_node(bin.right.deref_item())?.box_item();
				Ok(RNode::BinaryNode {
					left,
					right,
					kind: bin.kind,
				}
				.to_spanned(span))
			}
			AstNode::ForLoop(forloop) => self.resolve_for_loop(forloop).ok_to_spanned(span),
			AstNode::Index { target, index } => {
				let target = self.resolve_node(target.deref_item())?.box_item();
				let index = self.resolve_node(index.deref_item())?.box_item();
				Ok(RNode::Index { target, index }.to_spanned(span))
			}

			AstNode::Call(call) => self.resolve_call(call).ok_to_spanned(span),
			AstNode::FieldAccess(field) => self.resolve_field_access(field).ok_to_spanned(span),

			AstNode::RecordLit(obj) => self.resolve_record_lit(obj).ok_to_spanned(span),
			AstNode::ListLit(list) => {
				let list = self.resolve_list(list)?;
				Ok(RNode::ListLit(list).to_spanned(span))
			}
			AstNode::UnaryNode(un) => {
				let target = self.resolve_node(un.target.deref_item())?.box_item();
				Ok(RNode::UnaryNode(un.kind, target).to_spanned(span))
			}
			AstNode::Constructor(con) => self.resolve_constructor(con).ok_to_spanned(span),

			AstNode::ContinueNode => Ok(RNode::Continue.to_spanned(span)),
			AstNode::BreakNode => Ok(RNode::Break.to_spanned(span)),
			AstNode::Bool(v) => Ok(RNode::Bool(v).to_spanned(span)),
			AstNode::Float(v) => Ok(RNode::Float(v).to_spanned(span)),
			AstNode::Null => Ok(RNode::Null.to_spanned(span)),
			AstNode::Str(v) => Ok(RNode::String(v).to_spanned(span)),
			AstNode::Int(v) => Ok(RNode::Int(v).to_spanned(span)),
			AstNode::DontResult => Err(NameErr::UnexpectedSemi.to_errorbox(span, self.file_id)),
		}
	}
}

///block resolution
impl NameRes<'_> {
	fn resolve_block_with(&mut self, ast: ast::Block, base: ScopeInfo) -> Result<Block> {
		let ast_span = ast.span;
		self.scopes.enter_scope_with(base);
		let mut buffer = vec![];
		let base_locals = self.symbol_counter;
		self.push_var_counter();

		for node in ast {
			let resolved = self.resolve_node(node)?;
			buffer.push(resolved);
		}
		let inner_max = self.pop_var_counter();
		self.symbol_counter = base_locals;
		// Propagate the inner scope's max to the parent scope
		if let Some(max) = self.scope_locals_stack.last_mut() {
			*max = (*max).max(inner_max);
		}
		self.scopes.leave_scope();
		Ok(buffer.to_spanned(ast_span))
	}
	fn resolve_block(&mut self, ast: ast::Block) -> Result<Block> {
		self.resolve_block_with(ast, ScopeInfo::default())
	}
}
