use std::collections::HashMap;

use crate::{collections::spans::Span, idents::IdentId};
#[derive(Clone, Debug, PartialEq)]
pub struct VarInfo {
	pub name: IdentId,
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
	pub const fn new(name: IdentId, id: usize) -> Self {
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
	pub fn define_in(self, scope: &mut Scope) {
		scope.define(self.name, self);
	}
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Scope {
	pub parent: Option<Box<Scope>>,
	pub vars: HashMap<IdentId, VarInfo>,
}

impl Scope {
	pub fn get_var(&self, var_name: IdentId) -> Option<VarInfo> {
		if let Some(var) = self.vars.get(&var_name) {
			return Some(var.clone());
		}
		if let Some(parent) = &self.parent {
			return parent.get_var(var_name);
		}
		None
	}
	pub fn get_vars<const U: usize>(&self, vars: [IdentId; U]) -> [Option<VarInfo>; U] {
		const ARRAY_REPEAT_VALUE: Option<VarInfo> = None;
		let mut out: [Option<VarInfo>; U] = [ARRAY_REPEAT_VALUE; U];
		for (i, v) in vars.iter().copied().enumerate() {
			out[i] = self.get_var(v);
		}
		out
	}
	pub fn define(&mut self, var_name: IdentId, value: VarInfo) {
		self.vars.insert(var_name, value);
	}
}
