mod closures;
mod func_methods;
mod functions;
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
pub use lists::*;
pub use numbers::*;
use slotmap::SlotMap;
use std::collections::HashMap;
use std::f64::consts::{E, PI, TAU};
use std::fmt::Display;
pub use strings::*;

const NULL: Value = Value::Null;

pub fn default_scope() -> Scope {
    let vars = vars![
        noice => Value::Num(69.0),
        PI => Value::Num(PI),
        TAU => Value::Num(TAU),
        print_err(print_err,Inf),
        wait(wait_builtin,1),
        time(unix_time),
        open_file(open_textfile,1),
        input(input_builtin,Inf),
        println(println_builtin,Inf),
        print(print_builtin,Inf),
        parse_num(parse_num,1),
        to_str(to_str,1),
        stringify(stringify_vals,Inf),
        typeof(typeof_node,1),
        eval(eval,1),
        max(max,2),
        min(min,2),
        sqrt(sqrt,1),
        sin(sin,1),
        cos(cos,1),
        tan(tan,1),
        pow(pow,2),
        import(import_var,1),
        del(delete_var,1),
        range(range,1 => 3),
        floor(floor,1),
        round(round,1),
        clone(clone_val,1),
        capture_env(capture_env)
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
        msg => Value::Str(msg)
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
    ($param1:pat = $type1:expr ; $data:expr) => {
        let $param1 = &$data.args[0] else {
            return type_err_obj(&$type1, &$data.args[0].get_type(), 1, $data.heap);
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr ; $data:expr) => {
        let $param1 = &$data.args[0] else {
            return type_err_obj(&$type1, &$data.args[0].get_type(), 1, $data.heap);
        };
        let $param2 = &$data.args[1] else {
            return type_err_obj(&$type2, &$data.args[1].get_type(), 2, $data.heap);
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr,$param3:pat = $type3:expr  ; $data:expr) => {
        let $param1 = &$data.args[0] else {
            return type_err_obj(&$type1, &$data.args[0].get_type(), 1, $data.heap);
        };
        let $param2 = &$data.args[1] else {
            return type_err_obj(&$type2, &$data.args[1].get_type(), 2, $data.heap);
        };
        let $param3 = &$data.args[2] else {
            return type_err_obj(&$type3, &$data.args[2].get_type(), 3, $data.heap);
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr,$param3:pat = $type3:expr ,$param4:pat = $type4:expr  ; $data:expr) => {
        let $param1 = &$data.args[0] else {
            return type_err_obj(&$type1, &$data.args[0].get_type(), 1, $data.heap);
        };
        let $param2 = &$data.args[1] else {
            return type_err_obj(&$type2, &$data.args[1].get_type(), 2, $data.heap);
        };
        let $param3 = &$data.args[2] else {
            return type_err_obj(&$type3, &$data.args[2].get_type(), 3, $data.heap);
        };
        let $param4 = &$data.args[3] else {
            return type_err_obj(&$type4, &$data.args[3].get_type(), 4, $data.heap);
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr,$param3:pat = $type3:expr ,$param4:pat = $type4:expr ,$param5:pat = $type5:expr ; $data:expr) => {
        let $param1 = &$data.args[0] else {
            return type_err_obj(&$type1, &$data.args[0].get_type(), 1, $data.heap);
        };
        let $param2 = &$data.args[1] else {
            return type_err_obj(&$type2, &$data.args[1].get_type(), 2, $data.heap);
        };
        let $param3 = &$data.args[2] else {
            return type_err_obj(&$type3, &$data.args[2].get_type(), 3, $data.heap);
        };
        let $param4 = &$data.args[3] else {
            return type_err_obj(&$type4, &$data.args[3].get_type(), 4, $data.heap);
        };
        let $param5 = &$data.args[4] else {
            return type_err_obj(&$type5, &$data.args[4].get_type(), 5, $data.heap);
        };
    };
    ($param1:pat = $type1:expr,$param2:pat = $type2:expr,$param3:pat = $type3:expr ,$param4:pat = $type4:expr ,$param5:pat = $type5:expr, $param6:pat = $type6:expr; $data:expr) => {
        let $param1 = &$data.args[0] else {
            return type_err_obj(&$type1, &$data.args[0].get_type(), 1, $data.heap);
        };
        let $param2 = &$data.args[1] else {
            return type_err_obj(&$type2, &$data.args[1].get_type(), 2, $data.heap);
        };
        let $param3 = &$data.args[2] else {
            return type_err_obj(&$type3, &$data.args[2].get_type(), 3, $data.heap);
        };
        let $param4 = &$data.args[3] else {
            return type_err_obj(&$type4, &$data.args[3].get_type(), 4, $data.heap);
        };
        let $param5 = &$data.args[4] else {
            return type_err_obj(&$type5, &$data.args[4].get_type(), 5, $data.heap);
        };
        let $param6 = &$data.args[5] else {
            return type_err_obj(&$type6, &$data.args[5].get_type(), 6, $data.heap);
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
