use std::{collections::HashMap, fmt::Display};

type Ce = NativeCallError;
use crate::{backend::values::*, hashmap, spans::Span, Interpreter};
/// The result of the Native Constructor
///
/// - Ok(NativeObject) Object was constructed correctly
/// - Err(String) Object was not constructed correctly,
pub type NativeConstructorResult = Result<NativeObject, String>;

pub type NativeConstructor = fn(NativeConstructorData, &mut Interpreter) -> NativeConstructorResult;

pub fn native_constructors() -> HashMap<String, NativeConstructor> {
    let map = hashmap![];
    map
}

impl NativeTrait for i64 {
    fn lang_call(&mut self, name: &str, ctx: &mut Interpreter, data: FuncData) -> NativeFuncResult {
        match name {
            "as_num" => {
                check_args(Some((0, 0)), data.args.len())?;
                return Ok(Value::Num(*self as f64));
            }
            _ => return Err(Ce::MethodNotFound),
        }
    }
}
fn check_args(arg_range: Option<(u8, u8)>, given_size: usize) -> NativeFuncResult<()> {
    let Some(range) = arg_range else {
        return Ok(());
    };

    if range.1 == 0 && given_size != 0 {
        return unspec_err(format!("Expected no arguments but got {given_size}"));
    }

    if given_size > range.1.into() {
        return unspec_err(format!(
            "Given arguments are greater then expected; Expected atmost {max} but got {given_size}",
            max = range.1
        ));
    }
    if given_size < range.0.into() {
        return unspec_err(format!(
            "Given arguments are lesser then expected; Expected atleast {min} but got {given_size}",
            min = range.0
        ));
    }
    return Ok(());
}
fn unspec_err<T>(msg: impl Display) -> NativeFuncResult<T> {
    return Err(Ce::Unspecified(msg.to_string()));
}
