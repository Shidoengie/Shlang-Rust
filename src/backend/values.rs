use dyn_clone::DynClone;

use slotmap::{SlotMap, new_key_type};
use std::{
    any::Any,
    collections::HashMap,
    fmt::{Debug, Display},
};

use crate::{
    Interpreter,
    backend::{
        interpreter::{EvalRes, IError},
        scope::Scope,
    },
    frontend::nodes::NodeStream,
    lang_errors::InterpreterError,
    spans::{IntoSpanned, Span},
};
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Closure(Closure),
    Null,
    Void,
    Num(f64),
    Bool(bool),
    Str(String),
    Function(Function),
    BuiltinFunc(BuiltinFunc),
    Struct(Struct),
    Ref(RefKey),
    List(Vec<Value>),
    NativeObject(NativeObject),
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Bool(_) => Type::Bool,
            Value::BuiltinFunc(_) | Value::Function(_) => Type::Function,
            Value::Null => Type::Null,
            Value::Void => Type::Void,
            Value::Str(_) => Type::Str,
            Value::Num(_) => Type::Num,
            Value::List(_) => Type::List,
            Value::Struct(obj) => match &obj.id {
                Some(id) => Type::Struct(id.clone()),
                None => Type::AnonStruct,
            },
            Value::NativeObject(obj) => Type::Struct(obj.id.clone()),
            Value::Closure(_) => Type::Closure,
            Value::Ref(_) => Type::Ref,
        }
    }

    pub fn is_void(&self) -> bool {
        self == &Self::Void
    }
    pub fn matches_typeof(&self, val: &Self) -> bool {
        self.get_type() == val.get_type()
    }
}

pub type TypedValue = (Value, Type);

pub fn list_join(list: &[Value], seperator: &str) -> String {
    if list.is_empty() {
        return "[]".to_owned();
    }
    let out = String::from_iter(list.iter().enumerate().map(|(i, v)| {
        if i == list.len() - 1 {
            return v.to_string();
        }
        v.to_string() + seperator
    }));
    format!("[{out}]")
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            Self::Num(num) => num.to_string(),
            Self::Bool(cond) => cond.to_string(),
            Self::Str(txt) => txt.to_string(),
            Self::Null => "null".to_string(),
            Self::Void => "void".to_string(),
            Self::List(list) => list_join(list, ","),
            Self::Struct(obj) => obj.to_string(),
            Self::Function(func) => func.to_string(),
            Self::Closure(cl) => cl.to_string(),
            Self::NativeObject(obj) => {
                let repr = obj.inner.lang_repr();
                if repr.is_empty() {
                    format!("{}{{native}}", obj.id)
                } else {
                    repr
                }
            }
            Self::Ref(id) => format!("ref {:?}", id),
            _ => "unnamed".to_string(),
        };
        write!(f, "{out}")
    }
}

#[derive(Clone, Debug)]
pub enum Control {
    Return(Value),
    Result(Value),
    Value(Value),
    Break,
    Continue,
}
impl From<Value> for Control {
    fn from(value: Value) -> Self {
        Self::Value(value)
    }
}
impl Control {
    pub fn into_val(self) -> Value {
        match self {
            Control::Result(v) | Control::Return(v) | Control::Value(v) => v,
            _ => panic!("Control node doesnt have a value"),
        }
    }
}
#[macro_export]
macro_rules! arg_range {
    () => {
        Some((0, 0))
    };
    (Inf) => {
        None
    };
    ($size:expr) => {
        Some(($size, $size))
    };
    ($start:expr => $stop:expr) => {
        Some(($start, $stop))
    };
}
#[derive(Debug)]
pub struct NativeConstructorData<'a> {
    pub arguments: HashMap<String, Value>,
    pub span: Span,
    pub parent: &'a mut Scope,
}
impl NativeConstructorData<'_> {
    pub fn validate(&self, params: &[String]) -> Result<(), String> {
        if self.arguments.keys().eq(params) {
            return Err("Malformed Constructor".to_string());
        }
        Ok(())
    }
}
/// The result of the Native Constructor
///
/// - Ok(NativeObject) Object was constructed correctly
/// - Err(String) Object was not constructed correctly,
pub type NativeConstructorResult = Result<NativeObject, String>;

pub type NativeConstructor = fn(NativeConstructorData, &mut Interpreter) -> NativeConstructorResult;
pub type NativeFuncResult<T = Value> = Result<T, NativeCallError>;
#[derive(Debug)]
pub enum NativeCallError {
    MethodNotFound,
    Unspecified(String),
    Panic(Value),
    Major(InterpreterError),
}
impl From<CallError> for NativeCallError {
    fn from(value: CallError) -> Self {
        match value {
            CallError::Major(e) => Self::Major(e),
            CallError::Unspecified(e) => Self::Unspecified(e),
            CallError::Panic(e) => Self::Panic(e),
        }
    }
}
pub trait NativeTraitID {
    fn get_obj_id() -> &'static str;
    fn get_obj_id_from_val(&self) -> &'static str {
        Self::get_obj_id()
    }
}
pub trait NativeTrait: Debug + Any + DynClone {
    fn call_native_method(
        &mut self,
        name: &str,
        ctx: &mut Interpreter,
        data: FuncData,
    ) -> NativeFuncResult {
        Err(NativeCallError::MethodNotFound)
    }
    fn lang_get(&mut self, name: &str, ctx: &mut Interpreter) -> Option<Value> {
        None
    }
    fn lang_repr(&self) -> String {
        String::new()
    }
    fn as_type_id(&self) -> std::any::TypeId {
        self.type_id()
    }
}

dyn_clone::clone_trait_object!(NativeTrait);

#[derive(Clone, Debug)]
pub struct NativeObject {
    pub id: String,
    pub inner: Box<dyn NativeTrait>,
}
impl NativeObject {
    pub fn new(id: impl Display, inner: impl NativeTrait) -> Self {
        Self {
            id: id.to_string(),
            inner: Box::new(inner),
        }
    }
}
impl<T: NativeTrait + NativeTraitID> From<T> for NativeObject {
    fn from(value: T) -> Self {
        Self::new(value.get_obj_id_from_val(), value)
    }
}
impl<T: NativeTrait + NativeTraitID> From<T> for Value {
    fn from(value: T) -> Self {
        Value::NativeObject(NativeObject::new(value.get_obj_id_from_val(), value))
    }
}
impl From<NativeObject> for Value {
    fn from(value: NativeObject) -> Self {
        Self::NativeObject(value)
    }
}
impl PartialEq for NativeObject {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
#[derive(Clone, Debug, PartialEq, Default)]
pub struct Struct {
    pub id: Option<String>,
    pub env: Scope,
}
impl Struct {
    pub fn method(
        &mut self,
        name: impl ToString,
        arg_range: Option<(u8, u8)>,
        func: FuncPtr,
    ) -> &mut Self {
        self.prop(
            name.to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: func,
                arg_range,
                id: name.to_string(),
            }),
        )
    }
    pub fn is_instance_of(&self, name: impl ToString) -> bool {
        self.id.as_ref().is_some_and(|id| id == &name.to_string())
    }
    pub fn get_prop(&self, prop: impl AsRef<str>) -> Option<Value> {
        self.env.get_var(prop)
    }
    pub fn get_method(&self, prop: impl AsRef<str>) -> Option<Function> {
        match self.env.get_var(prop)? {
            Value::Function(func) => Some(func),
            _ => None,
        }
    }
    pub fn new(name: impl ToString) -> Self {
        let name = name.to_string();
        Self {
            id: if name.is_empty() { None } else { Some(name) },
            env: Scope::default(),
        }
    }
    pub fn set_props(&mut self, env: VarMap) -> &mut Self {
        self.env = Scope::from_vars(env);
        self
    }
    pub fn prop(&mut self, name: impl ToString, value: Value) -> &mut Self {
        self.env.define(name.to_string(), value);
        self
    }

    pub fn call_method(
        &self,
        name: impl Display,
        data: FuncData,
        ctx: &mut Interpreter,
        key: RefKey,
    ) -> EvalRes<Value> {
        let Some(method) = self.get_method(name.to_string()) else {
            return Err(
                IError::MethodNotFound(name.to_string(), self.id.to_owned()).to_spanned(data.span)
            );
        };
        let mut args = vec![Value::Ref(key)];
        args.extend_from_slice(&data.args);
        ctx.call_func(method, &args, data.span, data.parent)
    }
    pub fn insert(&self, heap: &mut SlotMap<RefKey, Value>) -> Value {
        let id = heap.insert(Value::Struct(self.clone()));
        Value::Ref(id)
    }
    pub fn has_method(&self, name: impl Display) -> bool {
        let Some(value) = self.env.get_var(name.to_string()) else {
            return false;
        };
        value.get_type() == Type::Function
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = if let Some(name) = &self.id {
            name.to_owned()
        } else {
            String::new()
        };
        buffer += "{";
        let vars = &self.env.vars;
        for (index, (k, v)) in vars.iter().enumerate() {
            buffer += k;
            buffer += ":";
            buffer += &v.to_string();
            if index < vars.len() - 1 {
                buffer += ", "
            }
        }
        buffer += "}";
        write!(f, "{buffer}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub block: NodeStream,
    pub args: Vec<String>,
    pub env: EnvKey,
}

impl From<Closure> for Value {
    fn from(x: Closure) -> Self {
        Value::Closure(x)
    }
}
impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = String::from("@(");
        for i in &self.args {
            buffer += i;
            if self.args.last().unwrap() != i {
                buffer += ", "
            }
        }
        buffer += ") ";
        write!(f, "{buffer}")
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub block: NodeStream,
    pub args: Vec<String>,
}
impl From<Closure> for Function {
    fn from(value: Closure) -> Self {
        Self {
            args: value.args,
            block: value.block,
        }
    }
}
impl Function {
    pub fn new(block: NodeStream, args: Vec<String>) -> Self {
        Self { block, args }
    }
}
impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = String::from("func(");
        for i in &self.args {
            buffer += i;
            if self.args.last().unwrap() != i {
                buffer += ", "
            }
        }
        buffer += ") ";
        write!(f, "{buffer}")
    }
}
impl From<Function> for Value {
    fn from(x: Function) -> Self {
        Value::Function(x)
    }
}
new_key_type! {
    /// Key type used for the heap
    pub struct RefKey;
    /// Key type used for storing closure environments
    pub struct EnvKey;
}

pub struct FuncData<'a> {
    pub args: Vec<Value>,
    pub span: Span,
    pub parent: &'a mut Scope,
}

#[derive(Debug)]
pub enum CallError {
    Major(InterpreterError),
    Panic(Value),
    Unspecified(String),
}
impl From<NativeCallError> for CallError {
    fn from(value: NativeCallError) -> Self {
        match value {
            NativeCallError::Major(e) => Self::Major(e),
            NativeCallError::Unspecified(e) => Self::Unspecified(e),
            NativeCallError::Panic(e) => Self::Panic(e),
            NativeCallError::MethodNotFound => Self::Unspecified("Non existent method".to_string()),
        }
    }
}
pub type FuncResult<T = Value> = Result<T, CallError>;

type FuncPtr = fn(FuncData, &mut Interpreter) -> FuncResult;

#[derive(Clone)]
pub struct BuiltinFunc {
    pub function: FuncPtr,
    pub arg_range: Option<(u8, u8)>,
    pub id: String,
}

impl Debug for BuiltinFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFunc")
            .field("id", &self.id)
            .field("arg_size", &self.arg_range)
            .finish()
    }
}
impl PartialEq for BuiltinFunc {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl BuiltinFunc {
    pub fn new(id: String, function: FuncPtr, arg_range: Option<(u8, u8)>) -> Self {
        Self {
            function,
            arg_range,
            id,
        }
    }
}
impl From<BuiltinFunc> for Value {
    fn from(x: BuiltinFunc) -> Self {
        Value::BuiltinFunc(x)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Null,
    Void,
    Num,
    Closure,
    Bool,
    Str,
    Function,
    List,
    AnonStruct,
    Struct(String),
    Ref,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let txt = match self {
            Self::Void => "void",
            Self::Null => "null",
            Self::Function => "func",
            Self::Bool => "bool",
            Self::Num => "num",
            Self::Str => "str",
            Self::List => "list",
            Self::Closure => "closure",
            Self::Struct(id) => id,
            Self::AnonStruct => "struct",
            Self::Ref => "ref",
        };
        write!(f, "{txt}")
    }
}
pub type VarMap = HashMap<String, Value>;
