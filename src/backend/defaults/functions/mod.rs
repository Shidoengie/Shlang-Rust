use crate::backend::defaults::*;
use crate::backend::scope::Scope;
use crate::backend::values::*;
use crate::frontend::nodes::*;
use crate::{Interpreter, assert_params, catch, get_params, vars};
use crate::{Parser, lang_errors::LangError};
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
