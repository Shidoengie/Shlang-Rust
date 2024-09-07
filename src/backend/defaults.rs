use crate::frontend::nodes::*;
use crate::lang_errors::LangError;
use crate::spans::Span;
use crate::Interpreter;
use crate::Parser;
use slotmap::SlotMap;

use std::collections::HashMap;

use std::f64::consts::{E, PI, TAU};
use std::fmt::Display;
use std::fs;

use functions::*;
use std::io;
use std::io::Write;
use std::thread;
use std::time;
use std::time::Duration;
use std::time::SystemTime;

use super::interpreter::IError;
use super::scope::Scope;
const NULL: Value = Value::Null;
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
        let $param5 = &$data.args[3] else {
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
        let $param5 = &$data.args[3] else {
            return type_err_obj(&$type5, &$data.args[4].get_type(), 5, $data.heap);
        };
        let $param6 = &$data.args[3] else {
            return type_err_obj(&$type6, &$data.args[5].get_type(), 6, $data.heap);
        };
    };
}

macro_rules! vars {
    () => {
        HashMap::new()
    };

    ($($t:tt)*) => {
        vars_internal!([] $($t)*)
    };
}

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
        round(round,1)
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

pub fn num_struct() -> Struct {
    let env = vars![
        to_str(to_str, 1),
        max(max, 2),
        min(min, 2),
        sqrt(sqrt, 1),
        sin(sin, 1),
        cos(cos, 1),
        tan(tan, 1),
        pow(pow, 2),
        floor(floor, 1),
        round(round, 1)
    ];

    Struct {
        id: None,
        env: Scope::from_vars(env),
    }
}
pub fn list_struct() -> Struct {
    use list_methods::*;
    let env = vars![
        len(list_len, 1),
        push(list_push, 2),
        append(list_append, 2),
        pop(list_pop, 1),
        remove(list_remove, 2),
        pop_at(list_pop_at, 2),
        join(join, 2),
        has(list_has, 2)
    ];
    Struct {
        id: None,
        env: Scope::from_vars(env),
    }
}
mod list_methods {
    use super::*;
    pub fn list_pop(data: FuncData) -> Value {
        get_params!(
            Value::Ref(key) = Type::Ref
        ;data);

        let Value::List(mut list) = data.heap.get(*key).unwrap().clone() else {
            return NULL;
        };
        let Some(value) = list.pop() else {
            return NULL;
        };
        data.heap[*key] = Value::List(list);
        return value;
    }
    pub fn list_remove(data: FuncData) -> Value {
        get_params!(
            Value::Ref(key) = Type::Ref
        ;data);
        let Value::List(list) = data.heap.get(*key).unwrap().clone() else {
            return NULL;
        };
        let old_len = list.len();
        let new_list: Vec<Value> = list.into_iter().filter(|v| v != &data.args[1]).collect();
        let new_len = new_list.len();
        data.heap[*key] = Value::List(new_list);
        Value::Num((old_len - new_len) as f64)
    }
    pub fn list_len(data: FuncData) -> Value {
        get_params!(
            Value::Ref(key) = Type::Ref
        ;data);
        let Value::List(list) = data.heap.get(*key).unwrap() else {
            return NULL;
        };
        Value::Num(list.len() as f64)
    }
    pub fn list_pop_at(data: FuncData) -> Value {
        get_params!(
            Value::Ref(key) = Type::Ref,
            Value::Num(og_index) = Type::Num
        ;data);
        let index = og_index.floor() as usize;
        let Value::List(mut list) = data.heap.get(*key).unwrap().clone() else {
            return type_err_obj(
                &Type::List,
                &data.heap.get(*key).unwrap().get_type(),
                1,
                data.heap,
            );
        };
        list.remove(index);
        data.heap[*key] = Value::List(list);
        Value::Ref(*key)
    }
    pub fn list_append(data: FuncData) -> Value {
        get_params!(
            Value::Ref(list_id) = Type::Ref,
            Value::Ref(pushed_id) = Type::Ref
        ;data);
        let Value::List(mut list) = data.heap.get(*list_id).unwrap().clone() else {
            return type_err_obj(
                &Type::List,
                &data.heap.get(*list_id).unwrap().get_type(),
                1,
                data.heap,
            );
        };
        let Value::List(mut pushed) = data.heap.get(*pushed_id).unwrap().clone() else {
            return type_err_obj(
                &Type::List,
                &data.heap.get(*pushed_id).unwrap().get_type(),
                1,
                data.heap,
            );
        };
        list.append(&mut pushed);
        data.heap[*list_id] = Value::List(list);
        Value::Ref(*list_id)
    }
    pub fn list_push(data: FuncData) -> Value {
        get_params!(
            Value::Ref(key) = Type::Ref
        ;data);
        let Value::List(mut list) = data.heap.get(*key).unwrap().clone() else {
            return type_err_obj(
                &Type::List,
                &data.heap.get(*key).unwrap().get_type(),
                1,
                data.heap,
            );
        };
        list.push(data.args[1].clone());
        data.heap[*key] = Value::List(list);
        Value::Ref(*key)
    }

    pub fn join(data: FuncData) -> Value {
        get_params!(
            Value::Ref(key) = Type::Ref,
            Value::Str(seperator) = Type::Str
        ;data);
        let Value::List(list) = data.heap.get(*key).unwrap() else {
            return type_err_obj(
                &Type::List,
                &data.heap.get(*key).unwrap().get_type(),
                1,
                data.heap,
            );
        };
        let mut buffer = String::new();
        for i in list {
            buffer += &i.to_string();
            buffer += seperator.as_str();
        }
        Value::Str(buffer)
    }
    pub fn list_has(data: FuncData) -> Value {
        get_params!(
            Value::Ref(key) = Type::Ref
        ;data);
        let Value::List(list) = data.heap.get(*key).unwrap() else {
            return type_err_obj(
                &Type::List,
                &data.heap.get(*key).unwrap().get_type(),
                1,
                data.heap,
            );
        };

        Value::Bool(list.contains(&data.args[1]))
    }
}
pub fn str_struct() -> Struct {
    use string_methods::*;
    let env = vars![
        parse_num(parse_num, 1),
        substr(substr, 3),
        len(len, 1),
        remove(remove, 2),
        replace(replace, 2),
        char_at(char_at, 2),
        split(split, 1 => 2),
        to_upper(to_upper, 1),
        to_lower(to_lower, 1),
        has(has_val, 2)
    ];
    Struct {
        id: None,
        env: Scope::new(None, env, HashMap::from([])),
    }
}
mod string_methods {
    use super::*;
    pub fn has_val(data: FuncData) -> Value {
        get_params!(
            Value::Str(value) = Type::Str,
            Value::Str(pat) = Type::Str
        ;data);
        Value::Bool(value.contains(pat))
    }
    pub fn remove(data: FuncData) -> Value {
        get_params!(
            Value::Str(val) = Type::Str
        ;data);
        let mut value = val.clone();
        let target = &data.args[1];
        match target {
            Value::Num(index) => {
                if index < &0.0 || index >= &(value.len() as f64) {
                    return create_err("Index out of bounds", data.heap);
                }
                value.remove(*index as usize);
                return Value::Str(value.to_string());
            }
            Value::Str(ref pattern) => return Value::Str(value.replace(pattern, "")),
            _ => NULL,
        }
    }

    pub fn replace(data: FuncData) -> Value {
        get_params!(
            Value::Str(value) = Type::Str,
            Value::Str(target) = Type::Str,
            Value::Str(filler) = Type::Str
        ;data);
        Value::Str(value.replace(target, filler))
    }
    pub fn substr(data: FuncData) -> Value {
        get_params!(
            Value::Str(value) = Type::Str,
            Value::Num(start) = Type::Num,
            Value::Num(end) = Type::Num
        ;data);
        let start_index = start.floor() as usize;
        let end_index = end.floor() as usize;
        let sub = &value[start_index..end_index];
        Value::Str(sub.to_string())
    }
    pub fn to_upper(data: FuncData) -> Value {
        get_params!(
            Value::Str(value) = Type::Str
        ;data);
        Value::Str(value.to_uppercase())
    }
    pub fn to_lower(data: FuncData) -> Value {
        get_params!(
            Value::Str(value) = Type::Str
        ;data);
        Value::Str(value.to_lowercase())
    }
    pub fn len(data: FuncData) -> Value {
        get_params!(
            Value::Str(value) = Type::Str
        ;data);
        Value::Num(value.len() as f64)
    }

    pub fn char_at(data: FuncData) -> Value {
        get_params!(
            Value::Str(value) = Type::Str,
            Value::Num(index) = Type::Num
        ;data);

        value
            .chars()
            .nth(*index as usize)
            .map(|c| Value::Str(c.to_string()))
            .unwrap_or(Value::Null)
    }
    pub fn split(data: FuncData) -> Value {
        get_params!(
            Value::Str(value) = Type::Str
        ;data);
        match data.args.len() {
            1 => {
                let split_str = value
                    .split(' ')
                    .map(|v| Value::Str(String::from(v)))
                    .collect();
                let val = data.heap.insert(Value::List(split_str));
                Value::Ref(val)
            }
            _ => {
                let Value::Str(seperator) = &data.args[1] else {
                    return type_err_obj(&Type::Str, &data.args[1].get_type(), 1, data.heap);
                };
                if seperator.is_empty() {
                    return create_err("Seperator must not be empty", data.heap);
                }
                let split_str = value
                    .split(seperator)
                    .map(|v| Value::Str(String::from(v)))
                    .collect();
                let val = data.heap.insert(Value::List(split_str));
                Value::Ref(val)
            }
        }
    }
}
mod functions {
    use colored::Colorize;

    use crate::catch;

    use super::*;
    pub fn delete_var(data: FuncData) -> Value {
        get_params!(Value::Str(val) = Type::Str;data);
        if !data.parent.vars.contains_key(val) {
            return NULL;
        }
        data.parent.vars.remove(val);
        NULL
    }

    pub fn println_builtin(data: FuncData) -> Value {
        if data.args.is_empty() {
            println!();
        }
        let mut out = "".to_string();
        for val in data.args {
            let derefed = deref_val(val, data.heap);
            out += format!(" {derefed}").as_str();
        }
        out = out.trim().to_string();
        println!("{out}");
        Value::Null
    }
    fn print_err_msg(msg: impl Display) {
        println!("{} {msg}", "ERROR!".red());
    }
    pub fn print_err(data: FuncData) -> Value {
        let val = deref_val(data.args[0].clone(), data.heap);
        match &val {
            Value::Struct(obj) => {
                let Some(ref id) = obj.id else {
                    print_err_msg(val);
                    return NULL;
                };
                if id != "Error" {
                    print_err_msg(val);
                    return NULL;
                }
                let msg = obj.env.get_var("msg").unwrap();
                print_err_msg(msg);
            }
            _ => print_err_msg(val),
        };
        NULL
    }
    pub fn print_builtin(data: FuncData) -> Value {
        if data.args.is_empty() {
            print!("");
            io::stdout().flush().unwrap();
            return Value::Null;
        }
        let mut out = "".to_string();
        for val in data.args {
            out += format!(" {}", deref_val(val, data.heap)).as_str();
        }
        out = out.trim().to_string();
        print!("{out}");
        io::stdout().flush().unwrap();
        NULL
    }
    pub fn open_textfile(data: FuncData) -> Value {
        get_params!(Value::Str(path) = Type::Str;data);
        let Ok(contents) = fs::read_to_string(path) else {
            return create_err("Failed to open file", data.heap);
        };
        return Value::Str(contents);
    }
    pub fn typeof_node(data: FuncData) -> Value {
        let val = if let Value::Ref(id) = data.args[0].clone() {
            &data.heap[id]
        } else {
            &data.args[0]
        };
        let out = val.get_type().to_string();
        Value::Str(out)
    }
    pub fn parse_num(data: FuncData) -> Value {
        get_params!(Value::Str(input) = Type::Str;data);
        let Ok(val) = input.parse() else {
            return create_err("Invalid float format", data.heap);
        };
        Value::Num(val)
    }
    pub fn min(data: FuncData) -> Value {
        get_params!(
            Value::Num(val1) = Type::Num,
            Value::Num(val2) = Type::Num
        ;data);
        Value::Num(val1.min(*val2))
    }
    pub fn max(data: FuncData) -> Value {
        get_params!(
            Value::Num(val1) = Type::Num,
            Value::Num(val2) = Type::Num
        ;data);
        Value::Num(val1.max(*val2))
    }
    pub fn pow(data: FuncData) -> Value {
        get_params!(
            Value::Num(val1) = Type::Num,
            Value::Num(val2) = Type::Num
        ;data);
        Value::Num(val1.powf(*val2))
    }
    pub fn cos(data: FuncData) -> Value {
        get_params!(
            Value::Num(val1) = Type::Num
        ;data);
        Value::Num(val1.cos())
    }
    pub fn tan(data: FuncData) -> Value {
        get_params!(
            Value::Num(val1) = Type::Num
        ;data);
        Value::Num(val1.tan())
    }
    pub fn sin(data: FuncData) -> Value {
        get_params!(
            Value::Num(val1) = Type::Num
        ;data);
        Value::Num(val1.sin())
    }
    pub fn sqrt(data: FuncData) -> Value {
        get_params!(
            Value::Num(val1) = Type::Num
        ;data);
        Value::Num(val1.sqrt())
    }
    pub fn floor(data: FuncData) -> Value {
        get_params!(
            Value::Num(val1) = Type::Num
        ;data);
        Value::Num(val1.floor())
    }
    pub fn round(data: FuncData) -> Value {
        get_params!(
            Value::Num(val1) = Type::Num
        ;data);
        Value::Num(val1.round())
    }
    pub fn deref_val(val: Value, heap: &SlotMap<RefKey, Value>) -> Value {
        if let Value::Ref(id) = val {
            return heap[id].clone();
        }
        val
    }
    pub fn to_str(data: FuncData) -> Value {
        Value::Str(data.args[0].to_string())
    }
    pub fn stringify_vals(data: FuncData) -> Value {
        if data.args.is_empty() {
            return Value::Str("".to_owned());
        }
        let mut out = String::new();
        for val in data.args {
            out += (val.to_string() + "").as_str();
        }
        Value::Str(out.trim().to_owned())
    }
    pub fn unix_time(_: FuncData) -> Value {
        Value::Num(
            SystemTime::now()
                .duration_since(time::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        )
    }
    pub fn wait_builtin(data: FuncData) -> Value {
        get_params!(
            Value::Num(val1) = Type::Num
        ;data);
        thread::sleep(Duration::from_millis((val1 * 1000.0).floor() as u64));
        NULL
    }
    pub fn input_builtin(data: FuncData) -> Value {
        if !data.args.is_empty() {
            print!("{}", data.args[0]);
            io::stdout().flush().unwrap();
        }
        let mut result = String::new();
        let read = io::stdin().read_line(&mut result);
        if read.is_err() {
            result = "".to_string()
        }
        return Value::Str(String::from(result.trim()));
    }
    pub fn eval(data: FuncData) -> Value {
        get_params!(
            Value::Str(source) = Type::Str
        ;data);
        let mut parser = Parser::new(source.as_str());
        let ast_result = parser.parse();
        let Ok((ast, functions)) = ast_result else {
            return Value::Null;
        };
        let mut inter = Interpreter::new(ast, functions);
        inter.heap.clone_from(data.heap);
        let Ok(result) = inter.execute_with(&mut data.parent.clone()) else {
            return Value::Null;
        };

        result
    }
    pub fn import_var(data: FuncData) -> Value {
        get_params!(
            Value::Str(path) = Type::Str
        ;data);
        let Ok(source) = fs::read_to_string(path) else {
            return create_err("Failed to read file", data.heap);
        };
        let mut parser = Parser::new(source.as_str());

        let (ast, functions) = catch!(
            err {
                return create_err(err.msg(), data.heap);
            } in parser.parse()
        );
        let mut inter = Interpreter::new(ast, functions);
        let base = Scope::from_vars(vars!(
            __name__ => Value::Str("lib".to_string())
        ));
        let scope = catch!(
            err {
                return create_err(err.msg(), data.heap);
            } in inter.parse_vars(base)
        );
        let heapstuff = |(name, val): (&String, &Value)| -> (String, Value) {
            let mut new_val = val.to_owned();
            if let Value::Ref(id) = val {
                let derefed = &inter.heap[*id];
                let key = data.heap.insert(derefed.clone());
                new_val = Value::Ref(key);
            }
            (name.to_owned(), new_val)
        };
        let new_vars: HashMap<_, _> = data
            .parent
            .vars
            .iter()
            .chain(&scope.vars)
            .map(heapstuff)
            .collect();

        let new_structs: HashMap<_, _> = data
            .parent
            .structs
            .iter()
            .chain(&scope.structs)
            .map(|(k, v)| (k.to_owned(), v.to_owned()))
            .collect();
        let new_functions: HashMap<_, _> = data
            .parent
            .structs
            .iter()
            .chain(&scope.structs)
            .map(|(k, v)| (k.to_owned(), v.to_owned()))
            .collect();

        data.parent.vars = new_vars;
        data.parent.structs = new_structs;
        Value::Null
    }
    fn gen_range(from: f64, to: f64, inc: f64) -> Vec<Value> {
        let capacity = from.round().abs() as usize + to.round().abs() as usize;
        let mut buffer: Vec<Value> = Vec::with_capacity(capacity);
        let mut current_num = from;
        if to > 0.0 {
            while current_num < to {
                buffer.push(Value::Num(current_num));
                current_num += inc;
            }
        } else {
            while current_num > to {
                buffer.push(Value::Num(current_num));
                current_num += inc;
            }
        }
        buffer
    }
    pub fn range(data: FuncData) -> Value {
        match data.args.len() {
            0 => {
                return create_err("Expected at least 1 argument", data.heap);
            }
            1 => {
                get_params!(
                    Value::Num(to) = Type::Num
                ;data);
                let new_range = gen_range(0.0, *to, 1.0);
                let val = data.heap.insert(Value::List(new_range));
                Value::Ref(val)
            }
            2 => {
                get_params!(
                    Value::Num(from) = Type::Num,
                    Value::Num(to) = Type::Num
                ;data);
                if from == &0.0 && to == &0.0 {
                    return create_err("Invalid range", data.heap);
                }
                let new_range = gen_range(*from, *to, 1.0);
                let val = data.heap.insert(Value::List(new_range));
                Value::Ref(val)
            }
            _ => {
                get_params!(
                    Value::Num(mut from) = Type::Num,
                    Value::Num(mut to) = Type::Num,
                    Value::Num(mut inc) = Type::Num
                ;data);

                if inc == 0.0 {
                    return create_err("Increment cant be bellow 0", data.heap);
                }
                if from == 0.0 && to == 0.0 {
                    return create_err("Invalid range", data.heap);
                }
                if from >= 0.0 && inc > 0.0 && to < 0.0 {
                    return create_err("Invalid range", data.heap);
                }
                if from <= 0.0 && inc < 0.0 && to > 0.0 {
                    return create_err("Invalid range", data.heap);
                }
                let new_range = gen_range(from, to, inc);
                let val = data.heap.insert(Value::List(new_range));
                Value::Ref(val)
            }
        }
    }
}
