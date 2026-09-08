use crate::backend::vm::{StackVM, values::*};

impl<T: NativeTrait> From<T> for Object {
	fn from(value: T) -> Self {
		return Self::Native(Box::new(value));
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

#[derive(Debug, Clone)]
pub struct NativeFunction(usize);

impl NativeFunction {
	pub const VARIADIC_VALUE: u8 = u8::MAX;
	pub const MAX_PTR_SIZE: usize = usize::MAX << 8;
	#[inline(always)]
	pub fn new(func: FuncPtr, param_count: u8) -> Result<Self, ()> {
		let ptr_value: usize = func as usize;

		if ptr_value > Self::MAX_PTR_SIZE {
			return Err(());
		}
		let packed_value = param_count as usize | ptr_value << 8;
		Ok(Self(packed_value))
	}
	#[inline(always)]
	unsafe fn get_func_ptr(&self) -> FuncPtr {
		let ptr = (self.0 >> 8) as *const ();
		return unsafe { std::mem::transmute(ptr) };
	}
	pub const fn get_param_count(&self) -> u8 {
		return (self.0 & !Self::MAX_PTR_SIZE) as u8;
	}
	#[inline(always)]
	pub fn call(&self, ctx: &mut StackVM, args: &[Value]) -> FuncResult {
		let fn_ptr = unsafe { self.get_func_ptr() };
		(fn_ptr)(ctx, args)
	}
	#[inline(always)]
	pub fn new_variadic(func: FuncPtr) -> Result<Self, ()> {
		Self::new(func, Self::VARIADIC_VALUE)
	}
	///Determines if a given parameter length is the accepted parameter count
	pub const fn is_arglen_valid(&self, arg_len: u8) -> bool {
		self.is_variadic() || arg_len == self.get_param_count()
	}

	pub const fn is_variadic(&self) -> bool {
		self.get_param_count() == Self::VARIADIC_VALUE
	}
}
impl From<NativeFunction> for Value {
	fn from(value: NativeFunction) -> Self {
		Self::NativeFunction(value)
	}
}
