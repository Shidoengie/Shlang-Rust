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
        (
            "import_var".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: import_var,
                arg_size: 2,
            }),
        ),
    ])
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

pub fn println_builtin(args: ValueStream) -> Value {
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
pub fn print_builtin(args: ValueStream) -> Value {
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
fn open_textfile(args: ValueStream) -> Value {
    let Value::Str(path) = &args[0] else {unreachable!()};
    let Ok(contents) = fs::read_to_string(path)  else {unreachable!()};
    return Value::Str(contents);
}
pub fn typeof_node(args: ValueStream) -> Value {
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
pub fn parse_num(args: ValueStream) -> Value {
    let Value::Str(input) = &args[0] else {unreachable!()};
    Value::Num(input.parse().unwrap())
}
fn min(args: ValueStream) -> Value {
    let [Value::Num(val1),Value::Num(val2)] = &args[..2] else {unreachable!()};
    Value::Num(val1.min(*val2))
}
fn max(args: ValueStream) -> Value {
    let [Value::Num(val1),Value::Num(val2)] = &args[..2] else {unreachable!()};
    Value::Num(val1.max(*val2))
}
fn pow(args: ValueStream) -> Value {
    let [Value::Num(val1),Value::Num(val2)] = &args[..2] else {unreachable!()};
    Value::Num(val1.powf(*val2))
}
fn cos(args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    Value::Num(val1.cos())
}
fn tan(args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    Value::Num(val1.tan())
}
fn sin(args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    Value::Num(val1.sin())
}
fn sqrt(args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    Value::Num(val1.sqrt())
}

pub fn to_str(args: ValueStream) -> Value {
    Value::Str(val_to_str(&args[0]))
}
// this needs more space
fn unix_time(_: ValueStream) -> Value {
    Value::Num(
        SystemTime::now()
            .duration_since(time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )
}
fn wait_builtin(args: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    thread::sleep(Duration::from_millis((val1 * 1000.0).floor() as u64));
    NULL
}
fn input_builtin(args: ValueStream) -> Value {
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
fn eval(args: ValueStream) -> Value {
    let Value::Str(source) = &args[0] else {return NULL};
    let mut parser = Parser::new(source.as_str());
    let ast_result = parser.parse();
    let Ok(ast) = ast_result else {
        return Value::Null;
    };

    let Ok(result) = Interpreter::new(ast).execute() else {return Value::Null;};

    match result {
        Control::Result(val) | Control::Return(val) | Control::Value(val) => val,
        _ => Value::Null,
    }
}
fn import_var(args: ValueStream) -> Value {
    let [Value::Str(path), Value::Str(var)] = &args[..2] else {unreachable!()};
    let Ok(source) = fs::read_to_string(path) else {return NULL;};
    let mut parser = Parser::new(source.as_str());
    let Ok(ast) = parser.parse() else {return NULL;};
    let Ok(scope) = Interpreter::new(ast).parse_vars() else {return NULL;};
    let Some(var) = scope.get_var(var) else {return NULL;};
    var
}
