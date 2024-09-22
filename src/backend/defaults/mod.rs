mod closures;
mod func_methods;
mod functions;
mod global_methods;
mod lists;
mod numbers;
mod strings;

use crate::backend::scope::Scope;
use crate::frontend::nodes::*;
use crate::vars;
use crate::vars_internal;
pub use closures::*;

pub use func_methods::*;
use functions::*;
pub use global_methods::*;
pub use lists::*;
pub use numbers::*;
use slotmap::SlotMap;
use std::collections::HashMap;
use std::f64::consts::{E, PI, TAU};
use std::fmt::Display;
pub use strings::*;

const NULL: FuncResult = Ok(Value::Null);

pub fn default_scope() -> Scope {
    let vars = vars![
        noice => Value::Num(69.0),
        PI => Value::Num(PI),
        E => Value::Num(E),
        TAU => Value::Num(TAU),
        print_err(print_err,1),
        wait(wait_builtin,1),
        time(unix_time),

        input(input_builtin,Inf),
        println(println_builtin,Inf),
        print(print_builtin,Inf),
        parse_num(1),
        to_str(1),
        stringify(stringify_vals,Inf),
        typeof(typeof_node,1),
        eval(1),
        max(2),
        min(2),
        sqrt(1),
        sin(1),
        cos(1),
        tan(1),
        pow(2),
        log(2),
        import(import_var,1 => 2),
        del(delete_var,1 => 2),
        range(1 => 3),
        floor(1),
        round(1),
        clone(clone_val,1),
        capture_env(),
        rand_num(1 => 2),
        error(err_func,1),
        panic(emit_panic,1),
        assert(2),
        assert_type(2),
        try(try_eval,1),
        open_dir(1),
        delete_dir(1),
        delete_file(1),
        paths_in_dir(1),
        open_file(open_textfile,1),
        create_file(1),
        create_dir(1),
        write_file(2)
    ];
    let structs = vars! {
        Error => error_struct(String::new()),

    };

    Scope {
        parent: None,
        vars,
        structs,
    }
}

fn error_struct(msg: String) -> Struct {
    let props = vars![
        msg => Value::Str(msg),
        panic(emit_panic,1)
    ];
    Struct {
        id: Some("Error".to_string()),
        env: Scope::from_vars(props),
    }
}

fn create_err(msg: impl Display, heap: &mut SlotMap<RefKey, Value>) -> Value {
    let err_obg = error_struct(msg.to_string());
    let id = heap.insert(Value::Struct(err_obg));
    return Value::Ref(id);
}
fn type_err_obj(expected: &Type, got: &Type, at: i32, heap: &mut SlotMap<RefKey, Value>) -> Value {
    create_err(
        format!("Invalid type expected {expected} but got {got} at argument:{at}"),
        heap,
    )
}
#[macro_export]
macro_rules! get_params {
    ($param1:pat = $type1:expr ; $data:expr, $state:expr) => {
        let $param1 = &$data.args[0] else {
            return Ok(type_err_obj(
                &$type1,
                &$data.args[0].get_type(),
                1,
                &mut $state.heap,
            ));
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr ; $data:expr, $state:expr) => {
        let $param1 = &$data.args[0] else {
            return Ok(type_err_obj(
                &$type1,
                &$data.args[0].get_type(),
                1,
                &mut $state.heap,
            ));
        };
        let $param2 = &$data.args[1] else {
            return Ok(type_err_obj(
                &$type2,
                &$data.args[1].get_type(),
                2,
                &mut $state.heap,
            ));
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr,$param3:pat = $type3:expr  ; $data:expr, $state:expr) => {
        let $param1 = &$data.args[0] else {
            return Ok(type_err_obj(
                &$type1,
                &$data.args[0].get_type(),
                1,
                &mut $state.heap,
            ));
        };
        let $param2 = &$data.args[1] else {
            return Ok(type_err_obj(
                &$type2,
                &$data.args[1].get_type(),
                2,
                &mut $state.heap,
            ));
        };
        let $param3 = &$data.args[2] else {
            return Ok(type_err_obj(
                &$type3,
                &$data.args[2].get_type(),
                3,
                &mut $state.heap,
            ));
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr,$param3:pat = $type3:expr ,$param4:pat = $type4:expr  ; $data:expr, $state:expr) => {
        let $param1 = &$data.args[0] else {
            return Ok(type_err_obj(
                &$type1,
                &$data.args[0].get_type(),
                1,
                &mut $state.heap,
            ));
        };
        let $param2 = &$data.args[1] else {
            return Ok(type_err_obj(
                &$type2,
                &$data.args[1].get_type(),
                2,
                &mut $state.heap,
            ));
        };
        let $param3 = &$data.args[2] else {
            return Ok(type_err_obj(
                &$type3,
                &$data.args[2].get_type(),
                3,
                &mut $state.heap,
            ));
        };
        let $param4 = &$data.args[3] else {
            return Ok(type_err_obj(
                &$type4,
                &$data.args[3].get_type(),
                4,
                &mut $state.heap,
            ));
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr,$param3:pat = $type3:expr ,$param4:pat = $type4:expr ,$param5:pat = $type5:expr ; $data:expr, $state:expr) => {
        let $param1 = &$data.args[0] else {
            return Ok(type_err_obj(
                &$type1,
                &$data.args[0].get_type(),
                1,
                &mut $state.heap,
            ));
        };
        let $param2 = &$data.args[1] else {
            return Ok(type_err_obj(
                &$type2,
                &$data.args[1].get_type(),
                2,
                &mut $state.heap,
            ));
        };
        let $param3 = &$data.args[2] else {
            return Ok(type_err_obj(
                &$type3,
                &$data.args[2].get_type(),
                3,
                &mut $state.heap,
            ));
        };
        let $param4 = &$data.args[3] else {
            return Ok(type_err_obj(
                &$type4,
                &$data.args[3].get_type(),
                4,
                &mut $state.heap,
            ));
        };
        let $param5 = &$data.args[4] else {
            return Ok(type_err_obj(
                &$type5,
                &$data.args[4].get_type(),
                5,
                &mut $state.heap,
            ));
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr,$param3:pat = $type3:expr ,$param4:pat = $type4:expr ,$param5:pat = $type5:expr, $param6:pat = $type6:expr; $data:expr, $state:expr) => {
        let $param1 = &$data.args[0] else {
            return Ok(type_err_obj(
                &$type1,
                &$data.args[0].get_type(),
                1,
                &mut $state.heap,
            ));
        };
        let $param2 = &$data.args[1] else {
            return Ok(type_err_obj(
                &$type2,
                &$data.args[1].get_type(),
                2,
                &mut $state.heap,
            ));
        };
        let $param3 = &$data.args[2] else {
            return Ok(type_err_obj(
                &$type3,
                &$data.args[2].get_type(),
                3,
                &mut $state.heap,
            ));
        };
        let $param4 = &$data.args[3] else {
            return Ok(type_err_obj(
                &$type4,
                &$data.args[3].get_type(),
                4,
                &mut $state.heap,
            ));
        };
        let $param5 = &$data.args[4] else {
            return Ok(type_err_obj(
                &$type5,
                &$data.args[4].get_type(),
                5,
                &mut $state.heap,
            ));
        };
        let $param6 = &$data.args[5] else {
            return Ok(type_err_obj(
                &$type6,
                &$data.args[5].get_type(),
                6,
                &mut $state.heap,
            ));
        };
    };
}
#[macro_export]
macro_rules! assert_params {
    ($param1:pat = $type1:expr ; $data:expr, $state:expr) => {
        let $param1 = &$data.args[0] else {
            return Err(type_err_obj(
                &$type1,
                &$data.args[0].get_type(),
                1,
                &mut $state.heap,
            ));
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr ; $data:expr, $state:expr) => {
        let $param1 = &$data.args[0] else {
            return Err(type_err_obj(
                &$type1,
                &$data.args[0].get_type(),
                1,
                &mut $state.heap,
            ));
        };
        let $param2 = &$data.args[1] else {
            return Err(type_err_obj(
                &$type2,
                &$data.args[1].get_type(),
                2,
                &mut $state.heap,
            ));
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr,$param3:pat = $type3:expr  ; $data:expr, $state:expr) => {
        let $param1 = &$data.args[0] else {
            return Err(type_err_obj(
                &$type1,
                &$data.args[0].get_type(),
                1,
                &mut $state.heap,
            ));
        };
        let $param2 = &$data.args[1] else {
            return Err(type_err_obj(
                &$type2,
                &$data.args[1].get_type(),
                2,
                &mut $state.heap,
            ));
        };
        let $param3 = &$data.args[2] else {
            return Err(type_err_obj(
                &$type3,
                &$data.args[2].get_type(),
                3,
                &mut $state.heap,
            ));
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr,$param3:pat = $type3:expr ,$param4:pat = $type4:expr  ; $data:expr, $state:expr) => {
        let $param1 = &$data.args[0] else {
            return Err(type_err_obj(
                &$type1,
                &$data.args[0].get_type(),
                1,
                &mut $state.heap,
            ));
        };
        let $param2 = &$data.args[1] else {
            return Err(type_err_obj(
                &$type2,
                &$data.args[1].get_type(),
                2,
                &mut $state.heap,
            ));
        };
        let $param3 = &$data.args[2] else {
            return Err(type_err_obj(
                &$type3,
                &$data.args[2].get_type(),
                3,
                &mut $state.heap,
            ));
        };
        let $param4 = &$data.args[3] else {
            return Err(type_err_obj(
                &$type4,
                &$data.args[3].get_type(),
                4,
                &mut $state.heap,
            ));
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr,$param3:pat = $type3:expr ,$param4:pat = $type4:expr ,$param5:pat = $type5:expr ; $data:expr, $state:expr) => {
        let $param1 = &$data.args[0] else {
            return Err(type_err_obj(
                &$type1,
                &$data.args[0].get_type(),
                1,
                &mut $state.heap,
            ));
        };
        let $param2 = &$data.args[1] else {
            return Err(type_err_obj(
                &$type2,
                &$data.args[1].get_type(),
                2,
                &mut $state.heap,
            ));
        };
        let $param3 = &$data.args[2] else {
            return Err(type_err_obj(
                &$type3,
                &$data.args[2].get_type(),
                3,
                &mut $state.heap,
            ));
        };
        let $param4 = &$data.args[3] else {
            return Err(type_err_obj(
                &$type4,
                &$data.args[3].get_type(),
                4,
                &mut $state.heap,
            ));
        };
        let $param5 = &$data.args[4] else {
            return Err(type_err_obj(
                &$type5,
                &$data.args[4].get_type(),
                5,
                &mut $state.heap,
            ));
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr,$param3:pat = $type3:expr ,$param4:pat = $type4:expr ,$param5:pat = $type5:expr, $param6:pat = $type6:expr; $data:expr, $state:expr) => {
        let $param1 = &$data.args[0] else {
            return Err(type_err_obj(
                &$type1,
                &$data.args[0].get_type(),
                1,
                &mut $state.heap,
            ));
        };
        let $param2 = &$data.args[1] else {
            return Err(type_err_obj(
                &$type2,
                &$data.args[1].get_type(),
                2,
                &mut $state.heap,
            ));
        };
        let $param3 = &$data.args[2] else {
            return Err(type_err_obj(
                &$type3,
                &$data.args[2].get_type(),
                3,
                &mut $state.heap,
            ));
        };
        let $param4 = &$data.args[3] else {
            return Err(type_err_obj(
                &$type4,
                &$data.args[3].get_type(),
                4,
                &mut $state.heap,
            ));
        };
        let $param5 = &$data.args[4] else {
            return Err(type_err_obj(
                &$type5,
                &$data.args[4].get_type(),
                5,
                &mut $state.heap,
            ));
        };
        let $param6 = &$data.args[5] else {
            return Err(type_err_obj(
                &$type6,
                &$data.args[5].get_type(),
                6,
                &mut $state.heap,
            ));
        };
    };
}
#[macro_export]
macro_rules! vars_internal {
    ([$($acc:tt)*]) => {
        HashMap::from([$($acc)*])
    };

    ([$($acc:tt)*] $key:ident => $val:expr $(, $($left:tt)*)? ) => {
        vars_internal!([$($acc)* (stringify!($key).to_string(),$val),] $($($left)*)? )
    };
    ([$($acc:tt)*] $name:ident($func:ident) $(, $($left:tt)*)? ) => {
        vars_internal!([$($acc)* (
            stringify!($name).to_string(),
            Value::BuiltinFunc(BuiltinFunc::new(
                stringify!($name).to_string(),
                $func,Some((0,0))
            ))
        ),] $($($left)*)? )
    };
    ([$($acc:tt)*] $name:ident($func:ident, Inf) $(, $($left:tt)*)? ) => {
        vars_internal!([$($acc)* (
            stringify!($name).to_string(),
            Value::BuiltinFunc(BuiltinFunc::new(
                stringify!($name).to_string(),
                $func,None
            ))
        ),] $($($left)*)? )
    };
    ([$($acc:tt)*] $name:ident($func:ident,$size:expr) $(, $($left:tt)*)? ) => {
        vars_internal!([$($acc)* (
            stringify!($name).to_string(),
            Value::BuiltinFunc(BuiltinFunc::new(
                stringify!($name).to_string(),
                $func,Some(($size,$size))
            ))
        ),] $($($left)*)? )
    };
    ([$($acc:tt)*] $name:ident($func:ident,$start:expr => $stop:expr) $(, $($left:tt)*)? ) => {
        vars_internal!([$($acc)* (
            stringify!($name).to_string(),
            Value::BuiltinFunc(BuiltinFunc::new(
                stringify!($name).to_string(),
                $func,Some(($start,$stop))
            ))
        ),] $($($left)*)? )
    };

    ([$($acc:tt)*] $name:ident() $(, $($left:tt)*)? ) => {
        vars_internal!([$($acc)* (
            stringify!($name).to_string(),
            Value::BuiltinFunc(BuiltinFunc::new(
                stringify!($name).to_string(),
                $name,Some((0,0))
            ))
        ),] $($($left)*)? )
    };

    ([$($acc:tt)*] $name:ident($size:expr) $(, $($left:tt)*)? ) => {
        vars_internal!([$($acc)* (
            stringify!($name).to_string(),
            Value::BuiltinFunc(BuiltinFunc::new(
                stringify!($name).to_string(),
                $name,Some(($size,$size))
            ))
        ),] $($($left)*)? )
    };
    ([$($acc:tt)*] $name:ident($start:expr => $stop:expr) $(, $($left:tt)*)? ) => {
        vars_internal!([$($acc)* (
            stringify!($name).to_string(),
            Value::BuiltinFunc(BuiltinFunc::new(
                stringify!($name).to_string(),
                $name,Some(($start,$stop))
            ))
        ),] $($($left)*)? )
    };
}
#[macro_export]
macro_rules! vars {
    () => {
        HashMap::new()
    };

    ($($t:tt)*) => {
        vars_internal!([] $($t)*)
    };
}
fn get_error_obj<'a>(val: &Value, heap: &'a mut SlotMap<RefKey, Value>) -> Option<&'a Struct> {
    let Value::Ref(id) = val else {
        return None;
    };
    let Value::Struct(obj) = &heap[*id] else {
        return None;
    };
    if obj.id.as_ref().is_some_and(|id| id != "Error") {
        return None;
    }
    return Some(obj);
}
