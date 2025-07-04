use super::interpreter::{EvalRes, IError};

use crate::{
    backend::values::*,
    frontend::nodes::{self, *},
    spans::{IntoSpanned, Span},
};
use std::{collections::HashMap, fmt::Display};
#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub vars: HashMap<String, Value>,
    pub structs: HashMap<String, Struct>,
}

impl Scope {
    pub fn get_var(&self, var_name: impl AsRef<str>) -> Option<Value> {
        if let Some(var) = self.vars.get(var_name.as_ref()) {
            return Some(var.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.get_var(var_name);
        }
        None
    }
    pub fn get_vars<const U: usize>(&self, vars: [impl AsRef<str>; U]) -> [Option<Value>; U] {
        const ARRAY_REPEAT_VALUE: Option<Value> = None;
        let mut out: [Option<Value>; U] = [ARRAY_REPEAT_VALUE; U];
        for (i, v) in vars.iter().enumerate() {
            out[i] = self.get_var(v);
        }
        out
    }
    pub fn assign_valid(&mut self, name: String, value: Value, span: Span) -> EvalRes<Value> {
        if value.is_void() {
            return Err(IError::VoidAssignment.to_spanned(span));
        }

        if self.assign(name.clone(), value).is_none() {
            return Err(IError::InvalidAssignment(name).to_spanned(span));
        }
        return Ok(Value::Void);
    }
    pub fn get_struct(&self, struct_name: &String) -> Option<Struct> {
        if let Some(obj) = self.structs.get(struct_name) {
            return Some(obj.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.get_struct(struct_name);
        }
        None
    }
    pub fn define(&mut self, var_name: impl Display, value: Value) {
        if let Value::Struct(obj) = &value {
            self.structs.insert(var_name.to_string(), obj.clone());
        }
        self.vars.insert(var_name.to_string(), value);
    }

    pub fn new(
        parent: Option<Box<Scope>>,
        vars: HashMap<String, Value>,
        structs: HashMap<String, Struct>,
    ) -> Self {
        Scope {
            parent,
            vars,
            structs,
        }
    }
    pub fn from_vars(vars: HashMap<String, Value>) -> Self {
        Scope {
            parent: None,
            vars,
            structs: HashMap::new(),
        }
    }
    pub fn new_child_in(parent: Scope) -> Self {
        Scope {
            parent: Some(Box::new(parent)),
            vars: HashMap::from([]),
            structs: HashMap::from([]),
        }
    }
    pub fn assign(&mut self, var_name: String, value: Value) -> Option<Value> {
        if let Some(var) = self.vars.get_mut(&var_name) {
            *var = value;
            return Some(var.clone());
        }
        if let Some(parent) = &mut self.parent {
            return parent.assign(var_name, value);
        }
        None
    }
}
impl Default for Scope {
    fn default() -> Self {
        Scope {
            parent: None,
            vars: HashMap::new(),
            structs: HashMap::new(),
        }
    }
}
