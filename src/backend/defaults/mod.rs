mod closures;
mod func_methods;
mod functions;

mod global_methods;

pub mod natives;
mod numbers;

use crate::backend::scope::Scope;

use crate::vars;
use crate::vars_internal;

use crate::Interpreter;
use crate::backend::values::*;

use functions::*;

pub use closures::*;
pub use func_methods::*;
pub use global_methods::*;

pub use numbers::*;

use slotmap::SlotMap;
use std::collections::HashMap;
use std::f64::consts::{E, PI, TAU};
use std::fmt::Display;
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
        write_file(2),
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
    let mut obj = Struct::new("Error");
    obj.set_props(vars![
        msg => Value::Str(msg),
        panic(emit_panic,1)
    ]);
    obj
}

fn create_err(msg: impl Display, heap: &mut SlotMap<RefKey, Value>) -> Value {
    let err_obg = error_struct(msg.to_string());

    return err_obg.insert(heap);
}
fn type_err_obj(
    expected: &Type,
    got: &Type,
    at: usize,
    heap: &mut SlotMap<RefKey, Value>,
) -> Value {
    create_err(
        format!("Invalid type expected {expected} but got {got} at argument:{at}"),
        heap,
    )
}
#[macro_export]
macro_rules! get_params {
    ($($param:pat = $type:expr),+ ; $data:expr, $state:expr) => {
        let mut _idx = 0;
        $(
            let $param = &$data.args[_idx] else {
                return Ok(type_err_obj(
                    &$type,
                    &$data.args[_idx].get_type(),
                    _idx + 1,
                    &mut $state.heap,
                ));
            };

            _idx += 1;
        )*
    };
}
#[macro_export]
macro_rules! assert_params {
    ($($param:pat = $type:expr),+ ; $data:expr, $state:expr) => {
        let mut _assert_idx = 0;
        $(
    let $param = &$data.args[_assert_idx] else {
            return Err(CallError::Panic(type_err_obj(
                &$type,
                &$data.args[_assert_idx].get_type(),
                _assert_idx + 1,
                &mut $state.heap,
            )));
        };
        _assert_idx += 1;
        )*
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
macro_rules! get_list {
    ($key:expr, $state:expr) => {{
        let Value::List(list) = $state.heap.get(*$key).unwrap().clone() else {
            return Ok(type_err_obj(
                &Type::List,
                &$state.heap.get(*$key).unwrap().get_type(),
                1,
                &mut $state.heap,
            ));
        };
        list
    }};
    ($key:expr, ref $state:expr) => {{
        let Value::List(list) = $state.heap.get(*$key).unwrap() else {
            return Ok(type_err_obj(
                &Type::List,
                &$state.heap.get(*$key).unwrap().get_type(),
                1,
                &mut $state.heap,
            ));
        };
        list
    }};
    (ref $key:expr, $state:expr) => {{
        let Value::List(list) = $state.heap.get(*$key).unwrap() else {
            return Ok(type_err_obj(
                &Type::List,
                &$state.heap.get(*$key).unwrap().get_type(),
                1,
                &mut $state.heap,
            ));
        };
        list
    }};
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
pub fn deref_val(val: Value, heap: &SlotMap<RefKey, Value>) -> Value {
    if let Value::Ref(id) = val {
        return heap[id].clone();
    }
    val
}
///ONLY USE IT IN METHODS!
pub fn get_args(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let args = match &data.args[0] {
        Value::Function(func) => &func.args,
        Value::Closure(cl) => &cl.args,
        _ => unimplemented!(),
    };
    let list: Vec<Value> = args.iter().map(|s| Value::Str(s.to_owned())).collect();
    return Ok(Value::Ref(state.heap.insert(Value::List(list))));
}
///ONLY USE IT IN METHODS!
pub fn count_args(data: FuncData, _: &mut Interpreter) -> FuncResult {
    match &data.args[0] {
        Value::Function(func) => Ok(Value::Num(func.args.len() as f64)),
        Value::Closure(cl) => Ok(Value::Num(cl.args.len() as f64)),
        _ => unimplemented!(),
    }
}
