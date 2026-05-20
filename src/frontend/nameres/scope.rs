use std::collections::HashMap;

use crate::spans::Span;
#[derive(Clone, Debug, PartialEq)]
pub struct VarInfo {
    pub name: String,
    pub id: usize,
    pub global: bool,
    pub readonly: bool,
    pub span: Option<Span>,
    pub modifier_span: Option<Span>,
}
#[allow(unused)]
impl VarInfo {
    #[inline(always)]
    pub const fn new(name: String, id: usize) -> Self {
        Self {
            name,
            id,
            global: false,
            readonly: false,
            span: None,
            modifier_span: None,
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
    pub fn define_in(self, scope: &mut Scope) {
        scope.define(self.name.clone(), self);
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub vars: HashMap<String, VarInfo>,
}

impl Scope {
    pub fn get_var(&self, var_name: impl AsRef<str>) -> Option<VarInfo> {
        if let Some(var) = self.vars.get(var_name.as_ref()) {
            return Some(var.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.get_var(var_name);
        }
        None
    }
    pub fn get_vars<const U: usize>(&self, vars: [impl AsRef<str>; U]) -> [Option<VarInfo>; U] {
        const ARRAY_REPEAT_VALUE: Option<VarInfo> = None;
        let mut out: [Option<VarInfo>; U] = [ARRAY_REPEAT_VALUE; U];
        for (i, v) in vars.iter().enumerate() {
            out[i] = self.get_var(v);
        }
        out
    }
    pub fn define(&mut self, var_name: String, value: VarInfo) {
        self.vars.insert(var_name, value);
    }

    pub fn new(parent: Option<Box<Scope>>, vars: HashMap<String, VarInfo>) -> Self {
        Scope { parent, vars }
    }
    pub fn from_vars(vars: HashMap<String, VarInfo>) -> Self {
        Scope { parent: None, vars }
    }
    pub fn new_child_in(parent: Scope) -> Self {
        Scope {
            parent: Some(Box::new(parent)),
            vars: HashMap::from([]),
        }
    }
}
