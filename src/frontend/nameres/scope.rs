use std::collections::HashMap;

use crate::{collections::spans::Span, symbols::SymbolId};

pub trait Definer {
	fn define(&mut self, value: VarInfo);
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarInfo {
	pub name: SymbolId,
	pub id: usize,
	pub global: bool,
	pub readonly: bool,
	pub span: Option<Span>,
	pub modifier_span: Option<Span>,
	pub is_item: bool,
}
#[allow(unused)]
impl VarInfo {
	#[inline(always)]
	pub const fn new(name: SymbolId, id: usize) -> Self {
		Self {
			name,
			id,
			global: false,
			readonly: false,
			span: None,
			modifier_span: None,
			is_item: false,
		}
	}
	pub const fn new_global(name: SymbolId) -> Self {
		Self {
			name,
			id: 0,
			global: true,
			readonly: false,
			span: None,
			modifier_span: None,
			is_item: false,
		}
	}
	#[inline(always)]
	pub fn as_global(self) -> Self {
		Self {
			global: true,
			..self
		}
	}

	#[inline(always)]
	pub fn as_readonly(self) -> Self {
		Self {
			readonly: true,
			..self
		}
	}
	#[inline(always)]
	pub fn with_global(self, global: bool) -> Self {
		Self { global, ..self }
	}

	#[inline(always)]
	pub fn with_readonly(self, readonly: bool) -> Self {
		Self { readonly, ..self }
	}
	#[inline(always)]
	pub fn with_span(self, span: Span) -> Self {
		Self {
			span: Some(span),
			..self
		}
	}
	#[inline(always)]
	pub fn with_modifier_span(self, modifier_span: Span) -> Self {
		Self {
			modifier_span: Some(modifier_span),
			..self
		}
	}
	#[inline(always)]
	pub fn with_item(self, is_item: bool) -> Self {
		Self { is_item, ..self }
	}
	pub fn define_in(self, scope: &mut impl Definer) {
		scope.define(self);
	}
}

#[derive(Debug, Clone)]
pub struct ClassInfo {
	pub name: Option<SymbolId>,
	pub id: usize,
	pub global: bool,
}

#[derive(Debug, Clone, Default)]
pub struct ScopeInfo {
	pub vars: HashMap<SymbolId, VarInfo>,
}
#[derive(Clone, Debug, Default)]
pub struct Scope {
	scope_stack: Vec<ScopeInfo>,
	global_counter: usize,
	pub globals: HashMap<SymbolId, VarInfo>,
	pub class_stack: Vec<ClassInfo>,
}

impl Scope {
	fn inc_globals(&mut self) {
		self.global_counter += 1;
	}
	pub fn get_var(&self, var_name: SymbolId) -> Option<&VarInfo> {
		if let Some(key) = self.globals.get(&var_name) {
			return Some(key);
		}
		if self.scope_stack.len() < 1 {
			return None;
		}
		let mut cursor = self.scope_stack.len() - 1;
		while let Some(info) = self.scope_stack.get(cursor) {
			if let Some(var) = info.vars.get(&var_name) {
				return Some(var);
			}
			if cursor == 0 {
				break;
			}
			cursor -= 1;
		}
		None
	}
	pub fn get_vars<const U: usize>(&self, vars: [SymbolId; U]) -> [Option<&VarInfo>; U] {
		const ARRAY_REPEAT_VALUE: Option<&VarInfo> = None;
		let mut out: [Option<&VarInfo>; U] = [ARRAY_REPEAT_VALUE; U];
		for (i, v) in vars.iter().copied().enumerate() {
			out[i] = self.get_var(v);
		}
		out
	}
	pub fn enter_scope(&mut self) {
		self.scope_stack.push(ScopeInfo::default());
	}
	pub fn enter_scope_with(&mut self, info: ScopeInfo) {
		self.scope_stack.push(info);
	}
	pub fn leave_scope(&mut self) {
		self.scope_stack.pop();
	}
	fn current_scope(&mut self) -> &mut ScopeInfo {
		if self.scope_stack.len() < 1 {
			self.scope_stack.push(ScopeInfo::default());
		}
		return unsafe { self.scope_stack.last_mut().unwrap_unchecked() };
	}
	pub fn enter_local_class(&mut self, name: Option<SymbolId>, id: usize) {
		let info = ClassInfo {
			name,
			id,
			global: false,
		};
		self.class_stack.push(info);
	}
	pub fn enter_global_class(&mut self, name: Option<SymbolId>) {
		let id = self.global_counter;
		self.inc_globals();
		let info = ClassInfo {
			global: true,
			id,
			name,
		};
		self.class_stack.push(info);
	}
	pub fn leave_class(&mut self) {
		self.class_stack.pop();
	}
}
impl Definer for Scope {
	fn define(&mut self, value: VarInfo) {
		if value.global {
			let mut value = value;
			value.id = self.global_counter;
			self.globals.insert(value.name, value);
			self.inc_globals();
		} else {
			self.current_scope().vars.insert(value.name, value);
		}
	}
}
impl Definer for ScopeInfo {
	fn define(&mut self, value: VarInfo) {
		self.vars.insert(value.name, value);
	}
}
