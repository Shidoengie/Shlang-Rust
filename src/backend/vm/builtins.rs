use std::io::{self, Read, Write};

///! This module is temporary, as it is not the ideal solution
///! It will get replaced with FFI
use crate::backend::instructions::{NativeFunction, Value};

pub const BUILTINS: [Value; 2] = [
    Value::NativeFunction(NATIVE_PRINTLN),
    Value::NativeFunction(NATIVE_INPUT),
];
pub const NATIVE_PRINTLN: NativeFunction = NativeFunction::new_variadic(|_, args| {
    if args.is_empty() {
        println!();
        return Value::Null;
    }
    for value in args {
        print!("{} ", value);
    }
    println!();
    Value::Null
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
    return Value::String(result);
});
