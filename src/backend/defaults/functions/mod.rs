use crate::backend::defaults::*;
use crate::backend::scope::Scope;
use crate::backend::values::*;
use crate::frontend::nodes::*;
use crate::{assert_params, catch, get_params, vars, Interpreter};
use crate::{lang_errors::LangError, Parser};
mod filesys;
mod math;
mod misc;
mod panics;
mod repr;
pub use filesys::*;
pub use math::*;
pub use misc::*;
pub use panics::*;
pub use repr::*;

const NULL: FuncResult = Ok(Value::Null);
