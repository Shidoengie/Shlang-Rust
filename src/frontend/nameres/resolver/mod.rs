mod error;
use crate::{
	collections::spans::{IntoSpanned, Span, Spanned},
	frontend::{
		FileStore,
		ast::{
			nodes::{self as ast, Node as AstNode},
			parser::GLOBAL_NAME_MAP,
		},
		nameres::{
			resolved_nodes::*,
			scope::{Scope, VarInfo},
		},
	},
	hashmap,
	idents::IdentId,
	lang_errors::{ErrorBox, ToErrorBox},
};
pub use error::NameErr;

use std::collections::HashMap;
pub type Result<T = RNodeSpan> = std::result::Result<T, ErrorBox<NameErr>>;
use ResolvedNode as RNode;
#[derive(Debug)]
struct GlobalEntry {
	pub span: Option<Span>,
	pub modifier_span: Option<Span>,
	pub id: usize,
}

impl From<usize> for GlobalEntry {
	fn from(value: usize) -> Self {
		Self {
			span: None,
			modifier_span: None,
			id: value,
		}
	}
}
impl GlobalEntry {
	fn new(id: usize, span: Span, modifier_span: Span) -> Self {
		Self {
			span: Some(span),
			id,
			modifier_span: Some(modifier_span),
		}
	}
}

#[derive(Default)]
pub struct NameRes {
	ident_counter: usize,
	globals: HashMap<IdentId, GlobalEntry>,
	pub(crate) file_store: FileStore,
	file_id: crate::collections::FileId,
	scope_locals_stack: Vec<usize>,
}
///resolver entry points
impl NameRes {
	pub fn new(file_store: FileStore) -> Self {
		Self {
			file_store,
			file_id: crate::collections::FileId::ANON,
			globals: HashMap::from_iter(
				(0..GLOBAL_NAME_MAP.len())
					.into_iter()
					.map(|idx| (unsafe { IdentId::new_unchecked(idx) }, idx.into())),
			),
			..Default::default()
		}
	}
	pub fn resolve<'a>(&mut self, ast: ast::Program<'a>) -> Result<ResolvedAst<'a>> {
		self.file_id = ast.file_id;
		self.push_scope();
		let decls = self.resolve_toplevel(ast.proc)?;
		let local_count = self.pop_scope();
		Ok(ResolvedAst {
			proc: decls,
			ident_pool: ast.ident_pool,
			global_count: self.globals.len(),
			local_count,
			file_id: ast.file_id,
		})
	}
	pub fn resolve_expr<'a>(&mut self, expr: ast::Ast<'a>) -> Result<ResolvedAstNode<'a>> {
		self.file_id = expr.file_id;
		let start_ident = self.ident_counter;
		self.push_scope();
		let node = self.resolve_node(expr.node, &mut Scope::default())?;
		let local_count = self.pop_scope();
		self.ident_counter = start_ident;
		Ok(ResolvedAstNode::new(
			node,
			self.globals.len(),
			local_count,
			expr.ident_pool,
			expr.file_id,
		))
	}
}

///toplevel resolution
impl NameRes {
	fn add_global(&mut self, name: IdentId) {
		if self.globals.contains_key(&name) {
			return;
		}
		let id = self.globals.len();
		self.globals.insert(name, GlobalEntry::from(id));
	}

	pub fn resolve_toplevel(&mut self, exprs: Vec<ast::NodeSpan>) -> Result<Vec<RNodeSpan>> {
		for val in exprs.iter() {
			let span = val.span;
			let AstNode::Decl(decl) = &val.item else {
				continue;
			};
			if !decl.hoisted {
				continue;
			}

			let name = decl.name.to_owned();
			let id = self.globals.len();
			self.globals.insert(
				name,
				GlobalEntry {
					id,
					span: Some(span),
					modifier_span: decl.modifier_span,
				},
			);
		}

		let mut root = Scope::default();
		let mut local_decls = vec![];
		let mut hoisted_decls = vec![];
		let base_locals = 0usize;
		self.push_scope();
		for node in exprs {
			let AstNode::Decl(decl) = &node.item else {
				let node = self.resolve_node(node, &mut root)?;
				local_decls.push(node);
				continue;
			};
			if !decl.hoisted {
				let node = self.resolve_node(node, &mut root)?;
				local_decls.push(node);
				continue;
			}
			let expr = self
				.resolve_node(decl.expr.clone().deref_item(), &mut root)?
				.box_item();
			let global = &self.globals[&decl.name];
			hoisted_decls.push(
				Decl {
					id: global.id,
					expr,
					is_global: true,
				}
				.to_rnodespan(node.span),
			);
		}
		let inner_max = self.pop_scope();
		self.ident_counter = base_locals;
		if let Some(max) = self.scope_locals_stack.last_mut() {
			*max = (*max).max(inner_max);
		}
		let mut decls = hoisted_decls;
		decls.append(&mut local_decls);
		Ok(decls)
	}
}

///scope management
impl NameRes {
	fn push_scope(&mut self) {
		self.scope_locals_stack.push(0);
	}
	fn pop_scope(&mut self) -> usize {
		self.scope_locals_stack.pop().unwrap_or(0)
	}
	fn update_scope_locals(&mut self) {
		if let Some(max) = self.scope_locals_stack.last_mut() {
			*max = self.ident_counter;
		}
	}
	fn gen_name(&mut self) -> usize {
		let old_count = self.ident_counter;
		self.ident_counter += 1;
		self.update_scope_locals();
		old_count
	}
	fn get_var(&mut self, name: IdentId, parent: &mut Scope, span: Span) -> Result<VarInfo> {
		if let Some(info) = parent.get_var(name) {
			return Ok(info);
		}
		let Some(info) = parent.get_var(name).or_else(|| {
			self.globals.get(&name).map(|global| VarInfo {
				name: name.to_owned(),
				global: true,
				id: global.id,
				readonly: true,
				span: global.span,
				modifier_span: global.modifier_span,
				is_item: true,
			})
		}) else {
			return Err(NameErr::UndefinedVar(name).to_errorbox(span, self.file_id));
		};
		Ok(info)
	}
}

///declaration and collection resolution
impl NameRes {
	fn resolve_var_decl(
		&mut self,
		decl: ast::Decl,
		parent: &mut Scope,
		span: Span,
	) -> Result<Decl> {
		let expr = self
			.resolve_node(decl.expr.deref_item(), parent)?
			.box_item();

		let id = self.gen_name();
		let mut info = VarInfo::new(decl.name, id)
			.with_readonly(decl.readonly)
			.with_item(decl.is_item)
			.with_span(span);

		info.modifier_span = decl.modifier_span;
		info.define_in(parent);
		Ok(Decl {
			expr,
			id,
			is_global: false,
		})
	}
	fn resolve_list(
		&mut self,
		list: Vec<Spanned<AstNode>>,
		parent: &mut Scope,
	) -> Result<Vec<RNodeSpan>> {
		let mut new = vec![];
		for node in list {
			new.push(self.resolve_node(node, parent)?)
		}
		Ok(new)
	}
	fn resolve_assignment(
		&mut self,
		target: Spanned<Box<AstNode>>,
		value: Spanned<Box<AstNode>>,
		parent: &mut Scope,
		span: Span,
	) -> Result {
		let target = target.deref_item();
		let Spanned {
			item: AstNode::Variable(ref name),
			span: var_span,
		} = target
		else {
			let target = self.resolve_node(target, parent)?.box_item();
			let value = self.resolve_node(value.deref_item(), parent)?.box_item();
			return Ok(RNode::Assignment { target, value }.to_spanned(span));
		};
		let info = self.get_var(*name, parent, span)?;
		let value = self.resolve_node(value.deref_item(), parent)?.box_item();
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
	fn resolve_field(&mut self, ast_field: ast::Field, parent: &mut Scope) -> Result<Field> {
		let default = if let Some(field) = ast_field.default {
			Some(self.resolve_node(field, parent)?)
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
impl NameRes {
	fn resolve_class(
		&mut self,
		class: ast::ClassLit,
		parent: &mut Scope,
		span: Span,
	) -> Result<RNodeSpan> {
		let mut fields = hashmap!();
		for (key, field) in class.fields.into_iter() {
			let field = self
				.resolve_field(field.item, parent)?
				.to_spanned(field.span);
			fields.insert(key, field);
		}
		let mut methods = hashmap!();
		for (key, method) in class.methods.into_iter() {
			let method = self
				.resolve_method(method.item, parent)?
				.to_spanned(method.span);
			methods.insert(key, method);
		}
		Ok(ClassLit {
			methods,
			fields,
			name: class.name,
		}
		.to_rnodespan(span))
	}
	fn resolve_method(&mut self, method: ast::Method, parent: &mut Scope) -> Result<Method> {
		let old_ident_counter = self.ident_counter;
		// Starts at one due to self being desugared to the first element
		self.ident_counter = 1;
		let mut func_scope = Scope::default();
		let mut args = vec![];
		for arg in method.args {
			let new_id = self.gen_name();
			VarInfo::new(arg.item, new_id)
				.with_span(arg.span)
				.define_in(&mut func_scope);
			args.push(new_id);
		}
		self.push_scope();
		let block = self.resolve_block_with(method.block, parent, func_scope)?;
		let local_count = self.pop_scope();
		self.ident_counter = old_ident_counter;
		Ok(Method {
			args,
			local_count,
			block,
			name_span: method.name_span,
			modifier_span: method.modifier_span,
			private: method.private,
		})
	}
	fn resolve_function_lit(
		&mut self,
		func: ast::FunctionLit,
		parent: &mut Scope,
		span: Span,
	) -> Result<RNodeSpan> {
		let old_ident_counter = self.ident_counter;
		self.ident_counter = 0;
		let mut func_scope = Scope::default();
		let mut args = vec![];
		for arg in func.args {
			let new_id = self.gen_name();
			VarInfo::new(arg.item, new_id)
				.with_span(arg.span)
				.define_in(&mut func_scope);
			args.push(new_id);
		}
		self.push_scope();
		let block = self.resolve_block_with(func.block, parent, func_scope)?;
		let local_count = self.pop_scope();
		self.ident_counter = old_ident_counter;
		Ok(RNode::FunctionLit(FunctionLit {
			idents: args,
			block,
			captures: func.captures,
			local_count,
		})
		.to_spanned(span))
	}
	fn resolve_branch(
		&mut self,
		branch: ast::Branch,
		parent: &mut Scope,
		span: Span,
	) -> Result<RNodeSpan> {
		let condition = self
			.resolve_node(branch.condition.deref_item(), parent)?
			.box_item();
		let if_block = self.resolve_block(branch.if_block, parent)?;
		let else_block = branch
			.else_block
			.map(|else_block| self.resolve_block(else_block, parent))
			.transpose()?;
		Ok(Branch {
			condition,
			if_block,
			else_block,
		}
		.to_rnodespan(span))
	}
	fn resolve_for_loop(
		&mut self,
		forloop: ast::ForLoop,
		parent: &mut Scope,
		span: Span,
	) -> Result<RNodeSpan> {
		let mut base = Scope::default();
		let loop_var = self.gen_name();
		VarInfo::new(forloop.ident, loop_var)
			.with_span(forloop.ident_span)
			.define_in(&mut base);
		let list = self
			.resolve_node(forloop.list.deref_item(), parent)?
			.box_item();
		let block = self.resolve_block_with(forloop.proc, parent, base)?;
		Ok(RNode::ForLoop {
			loop_var,
			list,
			block,
		}
		.to_spanned(span))
	}
	fn resolve_field_access(
		&mut self,
		field: ast::FieldAccess,
		parent: &mut Scope,
		span: Span,
	) -> Result<RNodeSpan> {
		let target = self
			.resolve_node(field.target.deref_item(), parent)?
			.box_item();
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
				args: self.resolve_list(args, parent)?,
			},
			ast::AccessType::Property(prop) => AccessType::Property(*prop),
		}
		.to_spanned(field.requested.span);
		Ok(RNode::FieldAccess(target, requested).to_spanned(span))
	}
	fn resolve_constructor(
		&mut self,
		con: ast::Constructor,
		parent: &mut Scope,
		span: Span,
	) -> Result<RNodeSpan> {
		let target = self
			.resolve_node(con.target.deref_item(), parent)?
			.box_item();
		let mut params = HashMap::new();
		for (name, value) in con.params {
			params.insert(name, self.resolve_node(value, parent)?);
		}
		Ok(RNode::Constructor { target, params }.to_spanned(span))
	}
	fn resolve_struct_lit(
		&mut self,
		obj: HashMap<IdentId, ast::NodeSpan>,
		parent: &mut Scope,
		span: Span,
	) -> Result<RNodeSpan> {
		let mut fields = hashmap!();
		for (name, node) in obj {
			fields.insert(name, self.resolve_node(node, parent)?.box_item());
		}
		Ok(RNode::StructDef(fields).to_spanned(span))
	}
	fn resolve_call(
		&mut self,
		call: ast::Call,
		parent: &mut Scope,
		span: Span,
	) -> Result<RNodeSpan> {
		let callee = self
			.resolve_node(call.callee.deref_item(), parent)?
			.box_item();
		let mut args = vec![];
		for node in call.args {
			args.push(self.resolve_node(node, parent)?);
		}
		Ok(RNode::Call { args, callee }.to_spanned(span))
	}
	fn resolve_record_lit(
		&mut self,
		obj: HashMap<IdentId, ast::NodeSpan>,
		parent: &mut Scope,
		span: Span,
	) -> Result<RNodeSpan> {
		let mut fields = hashmap!();
		for (name, node) in obj {
			fields.insert(name, self.resolve_node(node, parent)?.box_item());
		}
		Ok(RNode::RecordLit(fields).to_spanned(span))
	}
	fn resolve_node(&mut self, node: ast::NodeSpan, parent: &mut Scope) -> Result<RNodeSpan> {
		use ResolvedNode as RNode;
		let span = node.span;
		match node.item {
			AstNode::Decl(decl) => {
				let decl = self.resolve_var_decl(decl, parent, span)?;
				Ok(RNode::Decl(decl).to_spanned(span))
			}
			AstNode::ClassLit(class) => self.resolve_class(class, parent, span),

			AstNode::SelfValue => Ok(RNode::Variable {
				id: 0,
				is_global: false,
			}
			.to_spanned(span)),
			AstNode::SelfTy => todo!(),
			AstNode::Assignment { target, value } => {
				self.resolve_assignment(target, value, parent, span)
			}
			AstNode::Variable(name) => {
				let info = self.get_var(name, parent, span)?;
				Ok(RNode::Variable {
					id: info.id,
					is_global: info.global,
				}
				.to_spanned(span))
			}

			AstNode::FunctionLit(func) => self.resolve_function_lit(func, parent, span),
			AstNode::While(node) => {
				let condition = self
					.resolve_node(node.condition.deref_item(), parent)?
					.box_item();
				let block = self.resolve_block(node.proc, parent)?;
				Ok(RNode::While { block, condition }.to_spanned(span))
			}
			AstNode::Loop(block) => {
				let block = self.resolve_block(block, parent)?;
				Ok(RNode::Loop(block).to_spanned(span))
			}
			AstNode::Branch(branch) => self.resolve_branch(branch, parent, span),

			AstNode::DoBlock(block) => {
				let block = self.resolve_block(block, parent)?;
				Ok(RNode::DoBlock(block).to_spanned(span))
			}
			AstNode::Return(node) => {
				let node = self.resolve_node(node.deref_item(), parent)?.box_item();
				Ok(RNode::Return(node).to_spanned(span))
			}
			AstNode::Result(node) => {
				let node = self.resolve_node(node.deref_item(), parent)?.box_item();
				Ok(RNode::Result(node).to_spanned(span))
			}

			AstNode::BinaryNode(bin) => {
				let left = self.resolve_node(bin.left.deref_item(), parent)?.box_item();
				let right = self
					.resolve_node(bin.right.deref_item(), parent)?
					.box_item();
				Ok(RNode::BinaryNode {
					left,
					right,
					kind: bin.kind,
				}
				.to_spanned(span))
			}
			AstNode::ForLoop(forloop) => self.resolve_for_loop(forloop, parent, span),
			AstNode::Index { target, index } => {
				let target = self.resolve_node(target.deref_item(), parent)?.box_item();
				let index = self.resolve_node(index.deref_item(), parent)?.box_item();
				Ok(RNode::Index { target, index }.to_spanned(span))
			}
			AstNode::StructLit(obj) => self.resolve_struct_lit(obj, parent, span),
			AstNode::Call(call) => self.resolve_call(call, parent, span),
			AstNode::FieldAccess(field) => self.resolve_field_access(field, parent, span),

			AstNode::RecordLit(obj) => self.resolve_record_lit(obj, parent, span),
			AstNode::ListLit(list) => {
				let list = self.resolve_list(list, parent)?;
				Ok(RNode::ListLit(list).to_spanned(span))
			}
			AstNode::UnaryNode(un) => {
				let target = self
					.resolve_node(un.target.deref_item(), parent)?
					.box_item();
				Ok(RNode::UnaryNode(un.kind, target).to_spanned(span))
			}
			AstNode::Constructor(con) => self.resolve_constructor(con, parent, span),

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
impl NameRes {
	fn resolve_block_with(
		&mut self,
		ast: ast::Block,
		parent: &mut Scope,
		mut base: Scope,
	) -> Result<Block> {
		let ast_span = ast.span;
		base.parent = Some(Box::new(parent.clone()));
		let mut buffer = vec![];
		let base_locals = self.ident_counter;
		self.push_scope();
		for node in ast {
			let resolved = self.resolve_node(node, &mut base)?;
			buffer.push(resolved);
		}
		let inner_max = self.pop_scope();
		self.ident_counter = base_locals;
		// Propagate the inner scope's max to the parent scope
		if let Some(max) = self.scope_locals_stack.last_mut() {
			*max = (*max).max(inner_max);
		}
		let Some(mod_parent) = base.parent else {
			unimplemented!("Parent should always exist");
		};
		*parent = *mod_parent;
		Ok(buffer.to_spanned(ast_span))
	}
	fn resolve_block(&mut self, ast: ast::Block, parent: &mut Scope) -> Result<Block> {
		self.resolve_block_with(ast, parent, Scope::default())
	}
}
