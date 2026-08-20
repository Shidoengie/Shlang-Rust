use std::io::{self, Write};

use crate::backend::vm::values::FuncPtr;

///! This module is temporary, as it is not the ideal solution
///! It will get replaced with FFI
use super::values::{NativeFunction, Value};

pub fn get_builtins() -> Vec<Value> {
	vec![
		NativeFunction::new_variadic(NATIVE_INPUT).unwrap().into(),
		NativeFunction::new_variadic(NATIVE_PRINTLN).unwrap().into(),
		NativeFunction::new(STR_LEN, 1).unwrap().into(),
	]
}
pub const STR_LEN: FuncPtr = |_, args| {
	let Value::String(string) = &args[0] else {
		return Ok(Value::Null);
	};
	return Ok(Value::Int(string.len() as i64));
};
pub const NATIVE_PRINTLN: FuncPtr = |_, args| {
	if args.is_empty() {
		println!();
		return Ok(Value::Null);
	}
	for value in args {
		print!("{} ", value);
	}
	println!();
	Ok(Value::Null)
};
pub const NATIVE_INPUT: FuncPtr = |_, args| {
	if !args.is_empty() {
		for value in args {
			print!("{} ", value);
		}

		io::stdout().flush().unwrap();
	}
	let mut result = String::new();
	let read = io::stdin().read_line(&mut result);
	if read.is_err() {
		result = "".to_string()
	}
	result = result.trim().to_owned();
	Ok(Value::String(result))
};
