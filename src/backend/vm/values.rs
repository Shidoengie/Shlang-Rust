use std::{
    any::Any,
    fmt::{self, Write},
};

use derive_more::From;

use crate::backend::vm::{
    StackVM,
    error::{ErrCode, Type},
};

#[derive(Debug, Clone, Default)]
#[repr(u8)]
pub enum Value {
    #[default]
    Undefined = 0,
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Function(Function),
    NativeFunction(NativeFunction),
    ObjectRef(usize),
}
impl Value {
    pub fn lang_debug_fmt(&self, f: &mut impl Write) -> std::fmt::Result {
        match self {
            Self::Bool(v) => write!(f, "{v}"),
            Self::ObjectRef(v) => write!(f, "<object@{v}>"),
            Self::Int(v) => write!(f, "{v}i"),
            Self::Float(v) => write!(f, "{v}f"),
            Self::String(v) => write!(f, "\"{v}\""),
            Self::Null => write!(f, "null"),
            Self::Undefined => write!(f, "undefined"),
            Self::Function(v) => write!(f, "<function@{}>", v.address),
            Self::NativeFunction(_) => write!(f, "<nativefunction>"),
        }
    }
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(v) => write!(f, "{v}"),
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::String(v) => write!(f, "{v}"),
            Self::ObjectRef(v) => write!(f, "<object@{v}>"),
            Self::Null => write!(f, "null"),
            Self::Undefined => write!(f, "undefined"),
            Self::Function(v) => write!(f, "<function@{}>", v.address),
            Self::NativeFunction(_) => write!(f, "<nativefunction>"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Function {
    pub address: usize,
    pub local_count: usize,
    pub param_count: u8,
}
impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Self::Function(value)
    }
}

#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub func: FuncPtr,
    pub param_count: u8,
}
impl NativeFunction {
    pub const VARIADIC_VALUE: u8 = u8::MAX;
    pub const fn new(func: FuncPtr, param_count: u8) -> Self {
        Self { func, param_count }
    }
    pub const fn new_variadic(func: FuncPtr) -> Self {
        Self {
            func,
            param_count: Self::VARIADIC_VALUE,
        }
    }
    ///Determines if a given parameter length is the accepted parameter count
    pub const fn is_arglen_valid(&self, arg_len: u8) -> bool {
        self.is_variadic() || arg_len == self.param_count
    }
    pub const fn is_variadic(&self) -> bool {
        self.param_count == Self::VARIADIC_VALUE
    }
}
impl From<NativeFunction> for Value {
    fn from(value: NativeFunction) -> Self {
        Self::NativeFunction(value)
    }
}
pub type FuncPtr = fn(ctx: &mut StackVM, args: &[Value]) -> FuncResult;
pub type FuncResult = Result<Value, CallError>;

pub type NativeFuncResult<T = Value> = Result<T, NativeCallError>;
pub enum CallError {
    Unspecified(String),
    Major(ErrCode),
}
pub enum NativeCallError {
    MethodNotFound,
    Unspecified(String),
    Major(ErrCode),
}

impl From<CallError> for NativeCallError {
    fn from(value: CallError) -> Self {
        match value {
            CallError::Major(e) => Self::Major(e),
            CallError::Unspecified(e) => Self::Unspecified(e),
        }
    }
}
impl From<NativeCallError> for CallError {
    fn from(value: NativeCallError) -> Self {
        match value {
            NativeCallError::Major(e) => Self::Major(e),
            NativeCallError::Unspecified(e) => Self::Unspecified(e),
            NativeCallError::MethodNotFound => Self::Unspecified("Non existent method".to_owned()),
        }
    }
}
impl From<NativeCallError> for ErrCode {
    fn from(value: NativeCallError) -> Self {
        match value {
            NativeCallError::Major(e) => e,
            NativeCallError::Unspecified(e) => Self::Unspecified(e),
            NativeCallError::MethodNotFound => Self::Unspecified("Non existent method".to_owned()),
        }
    }
}
impl From<CallError> for ErrCode {
    fn from(value: CallError) -> Self {
        match value {
            CallError::Major(e) => e,
            CallError::Unspecified(e) => Self::Unspecified(e),
        }
    }
}
impl<T> From<ErrCode> for NativeFuncResult<T> {
    fn from(value: ErrCode) -> Self {
        return Err(NativeCallError::Major(value));
    }
}
impl From<ErrCode> for FuncResult {
    fn from(value: ErrCode) -> Self {
        return Err(CallError::Major(value));
    }
}

pub trait NativeTrait: std::fmt::Debug + Any {
    fn call_native_method(
        &mut self,
        name: &str,
        ctx: &mut StackVM,
        data: Vec<Value>,
    ) -> NativeFuncResult;
    #[allow(unused_variables)]
    fn lang_get(&mut self, name: &str, ctx: &mut StackVM) -> Option<Value> {
        return None;
    }
    #[inline(always)]
    fn get_typename(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
    fn lang_index(&self, key: Value) -> NativeFuncResult {
        return ErrCode::UnsupportedOperation {
            op: format!("[x]"),
            target: Type::Custom(self.get_typename().to_owned()),
        }
        .into();
    }
    fn lang_index_mut(&mut self, key: Value, value: Value) -> NativeFuncResult<()> {
        return ErrCode::UnsupportedOperation {
            op: format!("x[y] = z"),
            target: Type::Custom(self.get_typename().to_owned()),
        }
        .into();
    }
    fn lang_repr(&self) -> String {
        return String::new();
    }
    fn get_type_id(&self) -> std::any::TypeId {
        return self.type_id();
    }
}
#[derive(Debug)]
pub enum Object {
    Native(Box<dyn NativeTrait>),
}
impl<T: NativeTrait> From<T> for Object {
    fn from(value: T) -> Self {
        return Self::Native(Box::new(value));
    }
}
#[derive(Debug)]
pub struct ListObject(pub Vec<Value>);
impl NativeTrait for ListObject {
    fn call_native_method(
        &mut self,
        name: &str,
        ctx: &mut StackVM,
        data: Vec<Value>,
    ) -> NativeFuncResult {
        todo!()
    }
    fn get_typename(&self) -> &'static str {
        return "List";
    }
    fn lang_index_mut(&mut self, key: Value, value: Value) -> NativeFuncResult<()> {
        let Value::Int(key) = key else {
            return ErrCode::InvalidType {
                expected: Type::Int,
                got: key.into(),
            }
            .into();
        };
        if key < 0 || key as usize >= self.0.len() {
            return ErrCode::IndexOutOfBounds.into();
        }
        let key = key as usize;
        self.0[key] = value;
        Ok(())
    }
    fn lang_index(&self, key: Value) -> NativeFuncResult {
        let Value::Int(key) = key else {
            return ErrCode::InvalidType {
                expected: Type::Int,
                got: key.into(),
            }
            .into();
        };
        if key < 0 {
            return ErrCode::IndexOutOfBounds.into();
        }
        let key = key as usize;
        let Some(val) = self.0.get(key) else {
            return ErrCode::IndexOutOfBounds.into();
        };
        return Ok(val.clone());
    }
}
