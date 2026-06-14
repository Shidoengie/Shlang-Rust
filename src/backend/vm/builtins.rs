use std::io::{self, Write};

///! This module is temporary, as it is not the ideal solution
///! It will get replaced with FFI
use super::values::{NativeFunction, Value};

pub const BUILTINS: [Value; 3] = [
	Value::NativeFunction(NATIVE_PRINTLN),
	Value::NativeFunction(NATIVE_INPUT),
	Value::NativeFunction(STR_LEN),
];
pub const STR_LEN: NativeFunction = NativeFunction::new(
	|_, args| {
		let Value::String(string) = &args[0] else {
			return Ok(Value::Null);
		};
		return Ok(Value::Int(string.len() as i64));
	},
	1,
);
pub const NATIVE_PRINTLN: NativeFunction = NativeFunction::new_variadic(|_, args| {
	if args.is_empty() {
		println!();
		return Ok(Value::Null);
	}
	for value in args {
		print!("{} ", value);
	}
	println!();
	Ok(Value::Null)
});
pub const NATIVE_INPUT: NativeFunction = NativeFunction::new_variadic(|_, args| {
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
});
