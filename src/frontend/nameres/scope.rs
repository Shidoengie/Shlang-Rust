use std::{collections::HashMap, fmt::Debug};

use nonmax::NonMaxU32;

use crate::{
	collections::{sorted_debug_map, spans::Span},
	frontend::ast::parser::GLOBAL_NAME_MAP,
	symbols::SymbolId,
};

#[derive(Clone, Debug, PartialEq)]
pub struct VarInfo {
	pub name: SymbolId,
	pub id: usize,
	pub global: bool,
	pub readonly: bool,
	pub span: Option<Span>,
	pub modifier_span: Option<Span>,
	pub is_class: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarBuilder {
	pub name: SymbolId,
	pub global: bool,
	pub readonly: bool,
	pub span: Option<Span>,
	pub modifier_span: Option<Span>,
	pub is_class: bool,
}
#[allow(unused)]
impl VarBuilder {
	#[inline(always)]
	pub const fn new(name: SymbolId) -> Self {
		Self {
			name,
			global: false,
			readonly: false,
			span: None,
			modifier_span: None,
			is_class: false,
		}
	}
	pub const fn new_global(name: SymbolId) -> Self {
		Self {
			name,
			global: true,
			readonly: false,
			span: None,
			modifier_span: None,
			is_class: false,
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
	pub fn as_class(self) -> Self {
		Self {
			is_class: true,
			..self
		}
	}
	#[inline(always)]
	pub fn with_class(self, is_class: bool) -> Self {
		Self { is_class, ..self }
	}
	pub fn finish(self, id: usize) -> VarInfo {
		VarInfo {
			name: self.name,
			id,
			global: self.global,
			readonly: self.readonly,
			span: self.span,
			modifier_span: self.modifier_span,
			is_class: self.is_class,
		}
	}
}

#[derive(Debug, Clone)]
pub struct ClassInfo {
	pub name: Option<SymbolId>,
	pub id: usize,
	pub global: bool,
}

#[derive(Clone, Default)]
pub struct ScopeInfo {
	pub vars: HashMap<SymbolId, VarInfo>,
}
impl Debug for ScopeInfo {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("ScopeInfo")
			.field("vars", &sorted_debug_map(&self.vars))
			.finish()
	}
}
impl ScopeInfo {
	pub fn define(&mut self, info: VarInfo) {
		self.vars.insert(info.name, info);
	}
}
#[derive(Debug, Clone, Default)]
pub struct Frame {
	pub local_counter: usize,
	pub max_local_count: usize,
	scope: ScopeInfo,
	child_scopes: Vec<ScopeInfo>,
	scope_local_count_stack: Vec<usize>,
}
impl Frame {
	pub fn enter_scope(&mut self) {
		self.scope_local_count_stack.push(self.local_counter);
		self.child_scopes.push(ScopeInfo::default());
	}
	pub fn enter_scope_with(&mut self, info: ScopeInfo) {
		self.scope_local_count_stack.push(self.local_counter);
		self.reserve_locals(&info);
		self.child_scopes.push(info);
	}
	fn leave_scope(&mut self) -> Option<ScopeInfo> {
		let scope = self.child_scopes.pop()?;
		if let Some(local_count) = self.scope_local_count_stack.pop() {
			self.local_counter = local_count;
		}
		Some(scope)
	}
	fn reserve_locals(&mut self, scope: &ScopeInfo) {
		let next_local = scope
			.vars
			.values()
			.filter(|var| !var.global)
			.map(|var| var.id.saturating_add(1))
			.max()
			.unwrap_or(0);
		self.reserve_local_slots(next_local);
	}
	fn reserve_local_slots(&mut self, local_count: usize) {
		self.local_counter = self.local_counter.max(local_count);
		self.max_local_count = self.max_local_count.max(self.local_counter);
	}
	fn current_scope_mut(&mut self) -> &mut ScopeInfo {
		self.child_scopes.last_mut().unwrap_or(&mut self.scope)
	}
	fn current_scope(&self) -> &ScopeInfo {
		self.child_scopes.last().unwrap_or(&self.scope)
	}
	fn define(&mut self, info: VarBuilder) -> usize {
		assert!(
			!info.global,
			"Global values should never be defined inside the frame"
		);
		let id = self.local_counter;
		self.local_counter += 1;
		self.max_local_count = self.max_local_count.max(self.local_counter);
		let info = info.finish(id);
		let scope = self.current_scope_mut();
		scope.define(info);
		id
	}
	fn define_anon_symbol(&mut self) -> usize {
		let id = self.local_counter;
		self.local_counter += 1;
		self.max_local_count = self.max_local_count.max(self.local_counter);
		id
	}
	fn get_var(&self, var_name: SymbolId) -> Option<&VarInfo> {
		self.child_scopes
			.iter()
			.rev()
			.find_map(|info| info.vars.get(&var_name))
			.or_else(|| self.scope.vars.get(&var_name))
	}
}
#[derive(Clone, Default)]
pub struct Scope {
	main_frame: Frame,
	child_frames: Vec<Frame>,
	global_counter: usize,
	pub globals: HashMap<SymbolId, VarInfo>,
	pub class_stack: Vec<ClassInfo>,
}
impl Debug for Scope {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Scope")
			.field("main_frame", &self.main_frame)
			.field("child_frames", &self.child_frames)
			.field("global_counter", &self.global_counter)
			.field("globals", &sorted_debug_map(&self.globals))
			.field("class_stack", &self.class_stack)
			.finish()
	}
}
#[allow(unused)]
impl Scope {
	pub fn new() -> Self {
		Self::default()
	}
	/// This function is temporary and will be removed in the future
	pub fn with_defaults() -> Self {
		fn map_globals(id: usize) -> (SymbolId, VarInfo) {
			let symbol_id = SymbolId(NonMaxU32::new(id as u32).unwrap());
			let info = VarBuilder::new_global(symbol_id).finish(id);
			(symbol_id, info)
		}
		let globals = (0..GLOBAL_NAME_MAP.len()).into_iter().map(map_globals);
		Self::with_globals(HashMap::from_iter(globals))
	}
	pub fn with_globals(globals: HashMap<SymbolId, VarInfo>) -> Self {
		let global_counter = globals
			.values()
			.map(|var| var.id.saturating_add(1))
			.max()
			.unwrap_or(0);
		Self {
			globals,
			global_counter,
			..Default::default()
		}
	}
}
impl Scope {
	fn inc_globals(&mut self) {
		self.global_counter += 1;
	}
	pub fn get_var(&self, var_name: SymbolId) -> Option<&VarInfo> {
		if let Some(key) = self.globals.get(&var_name) {
			return Some(key);
		}
		self.child_frames
			.iter()
			.rev()
			.find_map(|frame| frame.get_var(var_name))
			.or_else(|| self.main_frame.get_var(var_name))
	}
	pub fn get_vars<const U: usize>(&self, vars: [SymbolId; U]) -> [Option<&VarInfo>; U] {
		const ARRAY_REPEAT_VALUE: Option<&VarInfo> = None;
		let mut out: [Option<&VarInfo>; U] = [ARRAY_REPEAT_VALUE; U];
		for (i, v) in vars.iter().copied().enumerate() {
			out[i] = self.get_var(v);
		}
		out
	}
	pub fn count_locals(&self) -> usize {
		self.current_scope().vars.len()
	}
	pub fn enter_scope(&mut self) {
		self.current_frame_mut().enter_scope();
	}
	pub fn enter_scope_with(&mut self, info: ScopeInfo) {
		self.current_frame_mut().enter_scope_with(info);
	}
	pub fn leave_scope(&mut self) -> Option<ScopeInfo> {
		self.current_frame_mut().leave_scope()
	}
	pub fn leave_frame(&mut self) -> usize {
		if let Some(frame) = self.child_frames.pop() {
			return frame.max_local_count;
		}
		self.main_frame.max_local_count
	}
	pub fn enter_frame(&mut self) {
		self.child_frames.push(Frame::default());
	}
	pub fn enter_frame_with(&mut self, info: ScopeInfo) {
		let mut frame = Frame::default();
		frame.reserve_locals(&info);
		frame.scope = info;
		self.child_frames.push(frame);
	}
	pub fn reserve_local_slots(&mut self, local_count: usize) {
		self.current_frame_mut().reserve_local_slots(local_count);
	}
	pub fn next_local_id(&self) -> usize {
		self.current_frame().local_counter
	}

	fn current_frame(&self) -> &Frame {
		self.child_frames.last().unwrap_or(&self.main_frame)
	}
	fn current_frame_mut(&mut self) -> &mut Frame {
		self.child_frames.last_mut().unwrap_or(&mut self.main_frame)
	}
	fn current_scope(&self) -> &ScopeInfo {
		self.current_frame().current_scope()
	}
	fn current_scope_mut(&mut self) -> &mut ScopeInfo {
		self.current_frame_mut().current_scope_mut()
	}
	pub fn enter_local_class(&mut self, name: Option<SymbolId>) -> ClassInfo {
		let id = if let Some(name) = name {
			self.define(VarBuilder::new(name).as_class().as_readonly())
		} else {
			self.current_frame_mut().define_anon_symbol()
		};
		let info = ClassInfo {
			name,
			id,
			global: false,
		};
		self.class_stack.push(info.clone());
		info
	}
	pub fn enter_global_class(&mut self, name: Option<SymbolId>, id: Option<usize>) -> ClassInfo {
		let id = id.unwrap_or_else(|| {
			let id = self.global_counter;
			self.inc_globals();
			id
		});
		let info = ClassInfo {
			global: true,
			id,
			name,
		};
		self.class_stack.push(info.clone());
		info
	}
	pub fn leave_class(&mut self) {
		self.class_stack.pop();
	}
	pub fn global_count(&self) -> usize {
		self.global_counter
	}
	pub fn define(&mut self, value: VarBuilder) -> usize {
		if value.global {
			let id = self.global_counter;
			self.globals.insert(value.name, value.finish(id));
			self.inc_globals();
			id
		} else {
			self.current_frame_mut().define(value)
		}
	}
}

#[cfg(test)]
mod tests {
	use nonmax::NonMaxU32;

	use super::*;

	fn symbol(id: u32) -> SymbolId {
		SymbolId(NonMaxU32::new(id).unwrap())
	}

	#[test]
	fn local_ids_reuse_slots_after_scope_and_track_high_water_mark() {
		let mut scope = Scope::new();
		scope.enter_frame();
		scope.define(VarBuilder::new(symbol(0)));

		scope.enter_scope();
		scope.define(VarBuilder::new(symbol(1)));
		scope.enter_scope();
		scope.define(VarBuilder::new(symbol(2)));
		assert_eq!(scope.get_var(symbol(2)).unwrap().id, 2);
		scope.define(VarBuilder::new(symbol(2)));
		assert_eq!(scope.get_var(symbol(2)).unwrap().id, 3);
		assert!(scope.leave_scope().is_some());

		scope.define(VarBuilder::new(symbol(3)));
		assert_eq!(scope.get_var(symbol(3)).unwrap().id, 2);
		assert!(scope.leave_scope().is_some());

		scope.define(VarBuilder::new(symbol(4)));
		assert_eq!(scope.get_var(symbol(4)).unwrap().id, 1);
		assert_eq!(scope.leave_frame(), 4);
	}

	#[test]
	fn local_ids_and_maximum_reset_for_each_frame() {
		let mut scope = Scope::new();

		scope.enter_frame();
		scope.define(VarBuilder::new(symbol(0)));
		assert_eq!(scope.leave_frame(), 1);

		scope.enter_frame();
		scope.define(VarBuilder::new(symbol(1)));
		assert_eq!(scope.leave_frame(), 1);
	}

	#[test]
	fn nested_frames_can_resolve_enclosing_locals() {
		let mut scope = Scope::new();
		scope.enter_frame();
		scope.define(VarBuilder::new(symbol(0)));

		scope.enter_frame();
		scope.define(VarBuilder::new(symbol(1)));
		assert_eq!(scope.get_var(symbol(0)).unwrap().id, 0);
		assert_eq!(scope.get_var(symbol(1)).unwrap().id, 0);
		assert_eq!(scope.leave_frame(), 1);

		assert_eq!(scope.get_var(symbol(0)).unwrap().id, 0);
		assert_eq!(scope.leave_frame(), 1);
	}

	#[test]
	fn redeclarations_consume_distinct_local_ids() {
		let mut scope = Scope::new();
		scope.enter_frame();

		scope.define(VarBuilder::new(symbol(0)));
		scope.define(VarBuilder::new(symbol(0)));

		assert_eq!(scope.get_var(symbol(0)).unwrap().id, 1);
		assert_eq!(scope.leave_frame(), 2);
	}

	#[test]
	fn prepopulated_scope_ids_advance_frame_local_counter() {
		let mut scope = Scope::new();
		let mut args = ScopeInfo::default();
		args.define(VarBuilder::new(symbol(0)).finish(0));

		scope.enter_frame();
		scope.enter_scope_with(args);
		scope.define(VarBuilder::new(symbol(1)));

		assert_eq!(scope.get_var(symbol(0)).unwrap().id, 0);
		assert_eq!(scope.get_var(symbol(1)).unwrap().id, 1);
		assert!(scope.leave_scope().is_some());
		assert_eq!(scope.leave_frame(), 2);
	}

	#[test]
	fn enter_frame_with_initializes_frame_scope_without_child_scope() {
		let mut scope = Scope::new();
		let mut args = ScopeInfo::default();
		args.define(VarBuilder::new(symbol(0)).finish(0));

		scope.enter_frame_with(args);
		scope.define(VarBuilder::new(symbol(1)));

		assert_eq!(scope.get_var(symbol(0)).unwrap().id, 0);
		assert_eq!(scope.get_var(symbol(1)).unwrap().id, 1);
		assert_eq!(scope.leave_frame(), 2);
	}

	#[test]
	fn get_var_without_child_scopes_checks_frame_scope() {
		let mut scope = Scope::new();
		scope.define(VarBuilder::new(symbol(0)));

		assert_eq!(scope.get_var(symbol(0)).unwrap().id, 0);
	}
}
