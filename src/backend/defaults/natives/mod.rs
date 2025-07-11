use std::collections::HashMap;
use std::fmt::Display;

use crate::arg_range;
use crate::backend::values::NativeCallError;
use crate::backend::values::NativeConstructor;
use crate::backend::values::NativeFuncResult;
use crate::hashmap;
mod hashmap;
mod strings;
pub use strings::*;
type Ce = NativeCallError;
pub fn native_constructors() -> HashMap<String, NativeConstructor> {
    hashmap![Map => hashmap::make_hashmap as NativeConstructor]
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
const NO_ARGS: Option<(u8, u8)> = arg_range!();
