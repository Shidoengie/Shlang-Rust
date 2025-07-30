use crate::spans::{IntoSpanned, Span};
use std::{collections::HashMap, fmt::Display};
#[derive(Clone, Debug, PartialEq, Default)]
pub struct VarInfo {
    name: String,
}
impl VarInfo {
    pub fn new(name: impl Display) -> Self {
        Self {
            name: name.to_string(),
        }
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
    pub fn define(&mut self, var_name: impl Display, value: VarInfo) {
        self.vars.insert(var_name.to_string(), value);
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
