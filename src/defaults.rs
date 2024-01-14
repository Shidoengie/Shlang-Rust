use crate::ast_nodes;
use crate::interpreter::Control;
use crate::Interpreter;
use crate::Parser;
use ast_nodes::*;
use std::collections::HashMap;
use std::f64::consts::{E, PI, TAU};
use std::fs;

use std::io;

use std::io::Write;
use std::thread;
use std::time;
use std::time::Duration;
use std::time::SystemTime;

const NULL: Value = Value::Null;
pub fn default_scope() -> Scope {
    Scope {
        parent: None,
        vars: var_map(),
        structs: HashMap::from([]),
    }
}
pub fn var_map() -> VarMap {
    HashMap::from([
        ("noice".to_string(), Value::Num(69.0)),
        ("PI".to_string(), Value::Num(PI)),
        ("TAU".to_string(), Value::Num(TAU)),
        ("E".to_string(), Value::Num(E)),
        (
            "wait".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: wait_builtin,
                arg_size: 1,
            }),
        ),
        (
            "time".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: unix_time,
                arg_size: 0,
            }),
        ),
        (
            "open_file".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: open_textfile,
                arg_size: 1,
            }),
        ),
        (
            "input".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: input_builtin,
                arg_size: -1,
            }),
        ),
        (
            "println".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: println_builtin,
                arg_size: -1,
            }),
        ),
        (
            "print".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: print_builtin,
                arg_size: -1,
            }),
        ),
        (
            "parse_num".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: parse_num,
                arg_size: 1,
            }),
        ),
        (
            "to_str".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: to_str,
                arg_size: 1,
            }),
        ),
        (
            "typeof".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: typeof_node,
                arg_size: 1,
            }),
        ),
        (
            "eval".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: eval,
                arg_size: 1,
            }),
        ),
        (
            "min".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: min,
                arg_size: 2,
            }),
        ),
        (
            "max".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: max,
                arg_size: 2,
            }),
        ),
        (
            "sqrt".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: sqrt,
                arg_size: 1,
            }),
        ),
        (
            "cos".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: cos,
                arg_size: 1,
            }),
        ),
        (
            "sin".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: sin,
                arg_size: 1,
            }),
        ),
        (
            "tan".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: tan,
                arg_size: 1,
            }),
        ),
        (
            "pow".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: pow,
                arg_size: 2,
            }),
        ),
    ])
}
pub fn str_struct(val: String) -> Struct {
    let env = str_structmap(val);
    Struct {
        id: "str".to_string(),
        env: Scope::new(None, env, HashMap::from([])),
    }
}

pub fn num_struct(val: f64) -> Struct {
    let env = num_structmap(val);

    Struct {
        id: "num".to_string(),
        env: Scope::new(None, env, HashMap::from([])),
    }
}
pub fn num_structmap(val: f64) -> VarMap {
    HashMap::from([
        ("v".to_string(), Value::Num(val)),
        (
            "sqrt".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: sqrt_method,
                arg_size: 0,
            }),
        ),
        (
            "to_string".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: num_to_str,
                arg_size: 0,
            }),
        ),
    ])
}
pub fn str_structmap(val: String) -> VarMap {
    HashMap::from([
        ("v".to_string(), Value::Str(val)),
        (
            "parse_num".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: parse_num_method,
                arg_size: 0,
            }),
        ),
        (
            "substr".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: substr_method,
                arg_size: 2,
            }),
        ),
        (
            "len".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: len_method,
                arg_size: 0,
            }),
        ),
        (
            "remove".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: str_remove,
                arg_size: 1,
            }),
        ),
        (
            "replace".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: str_replace,
                arg_size: 2,
            }),
        ),
        (
            "char_at".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: char_at_method,
                arg_size: 1,
            }),
        ),
    ])
}
pub fn str_remove(scope: VarMap, args: ValueStream) -> Value {
    let Some(Value::Str(v)) = scope.get("v") else {unreachable!()};
    let mut value = v.clone();
    let target = &args[0];
    match target {
        Value::Num(index) => {
            if index < &0.0 || index >= &(value.len() as f64) {
                return NULL;
            }
            value.remove(*index as usize);
            return Value::Str(value.to_string());
        }
        Value::Str(ref pattern) => return Value::Str(value.replace(pattern, "")),
        _ => NULL,
    }
}

pub fn str_replace(scope: VarMap, args: ValueStream) -> Value {
    let Some(Value::Str(value)) = scope.get("v") else {unreachable!()};
    let [Value::Str(ref target),Value::Str(ref filler)] = args[..2] else { return NULL; };
    Value::Str(value.replace(target, filler))
}
pub fn parse_num_method(scope: VarMap, _: ValueStream) -> Value {
    let Some(Value::Str(value)) = scope.get("v") else {unreachable!()};
    let parsed: Result<f64, _> = String::from_iter(value.chars().filter(|&c| c != '_')).parse();
    let Ok(result) = parsed else {return NULL;};

    Value::Num(result)
}
pub fn num_to_str(scope: VarMap, _: ValueStream) -> Value {
    let Some(Value::Num(value)) = scope.get("v") else {unreachable!()};
    Value::Str(format!("{value}"))
}
fn substr_method(scope: VarMap, args: ValueStream) -> Value {
    let Some(Value::Str(value)) = scope.get("v") else { unreachable!() };
    let [Value::Num(start), Value::Num(end)] = args[..2] else { return NULL; };
    let sub = &value[start as usize..end as usize];
    Value::Str(sub.to_string())
}
fn len_method(scope: VarMap, _: ValueStream) -> Value {
    let Some(Value::Str(value)) = scope.get("v") else { unreachable!()};
    Value::Num(value.len() as f64)
}

fn char_at_method(scope: VarMap, args: ValueStream) -> Value {
    let Some(Value::Str(value)) = scope.get("v") else {unreachable!() };
    let Value::Num(index) = &args[0] else {return NULL;};

    value
        .chars()
        .nth(*index as usize)
        .map(|c| Value::Str(c.to_string()))
        .unwrap_or(Value::Null)
}

pub fn val_to_str(val: &Value) -> String {
    match val {
        Value::Num(num) => num.to_string(),
        Value::Bool(cond) => cond.to_string(),
        Value::Str(txt) => txt.to_string(),
        Value::Null => "null".to_string(),
        Value::Void => "void".to_string(),

        _ => "unnamed".to_string(),
    }
}

pub fn println_builtin(_: VarMap, args: ValueStream) -> Value {
    if args.is_empty() {
        println!();
    }
    let mut out = "".to_string();
    for val in args {
        out += format!(" {}", val_to_str(&val)).as_str();
    }
    out = out.trim().to_string();
    println!("{out}");
    Value::Null
}
pub fn print_builtin(_: VarMap, args: ValueStream) -> Value {
    if args.is_empty() {
        print!("");
        io::stdout().flush().unwrap();
        return Value::Null;
    }
    let mut out = "".to_string();
    for val in args {
        out += format!(" {}", val_to_str(&val)).as_str();
    }
    out = out.trim().to_string();
    print!("{out}");
    io::stdout().flush().unwrap();
    NULL
}
pub fn open_textfile(_: VarMap, args: ValueStream) -> Value {
    let Value::Str(path) = &args[0] else {return NULL;};
    let Ok(contents) = fs::read_to_string(path)  else {return NULL;};
    return Value::Str(contents);
}
pub fn typeof_node(_: VarMap, args: ValueStream) -> Value {
    let out = match args[0].get_type() {
        Type::Void => "void",
        Type::Null => "null",
        Type::Function => "func",
        Type::Bool => "bool",
        Type::Num => "num",
        Type::Str => "str",
        Type::UserDefined(id) => return Value::Str(id),
        Type::Ref(_) => "ref",
    }
    .to_string();
    Value::Str(out)
}
pub fn parse_num(_: VarMap, args: ValueStream) -> Value {
    let Value::Str(input) = &args[0] else {return NULL;};
    Value::Num(input.parse().unwrap())
}
fn min(_: VarMap, args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    let Value::Num(val2) = &args[1] else {return NULL;};
    Value::Num(val1.min(*val2))
}
fn max(_: VarMap, args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    let Value::Num(val2) = &args[1] else {return NULL;};
    Value::Num(val1.max(*val2))
}
fn pow(_: VarMap, args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    let Value::Num(val2) = &args[1] else {return NULL;};
    Value::Num(val1.powf(*val2))
}
fn cos(_: VarMap, args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    Value::Num(val1.cos())
}
fn tan(_: VarMap, args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    Value::Num(val1.tan())
}
fn sin(_: VarMap, args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    Value::Num(val1.sin())
}
fn sqrt(_: VarMap, args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    Value::Num(val1.sqrt())
}
fn sqrt_method(scope: VarMap, _: ValueStream) -> Value {
    let Some(Value::Num(value)) = scope.get("v") else {unreachable!()};
    Value::Num(value.sqrt())
}
pub fn to_str(_: VarMap, args: ValueStream) -> Value {
    Value::Str(val_to_str(&args[0]))
}
// this needs more space
pub fn unix_time(_: VarMap, _: ValueStream) -> Value {
    Value::Num(
        SystemTime::now()
            .duration_since(time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )
}
pub fn wait_builtin(_: VarMap, args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    thread::sleep(Duration::from_millis((val1 * 1000.0).floor() as u64));
    NULL
}
pub fn input_builtin(_: VarMap, args: ValueStream) -> Value {
    if !args.is_empty() {
        let message = &args[0];
        print!("{}", val_to_str(message));
        io::stdout().flush().unwrap();
    }
    let mut result = String::new();
    let read = io::stdin().read_line(&mut result);
    if read.is_err() {
        result = "".to_string()
    }
    return Value::Str(String::from(result.trim()));
}
pub fn eval(_: VarMap, args: ValueStream) -> Value {
    let Value::Str(source) = &args[0] else {return Value::Str("Expected a string".to_string());};
    let mut parser = Parser::new(source.as_str());
    let ast_result = parser.batch_parse_expr();
    let Ok(ast) = ast_result else {
        return Value::Null;
    };

    let Ok(result) = Interpreter::new(ast).execute() else {return Value::Null;};

    match result {
        Control::Result(val) | Control::Return(val) | Control::Value(val) => val,
        _ => Value::Null,
    }
}
