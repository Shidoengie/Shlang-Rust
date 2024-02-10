use crate::frontend::nodes::*;

use crate::Interpreter;
use crate::Parser;

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

macro_rules! vars {
    [] => {
        HashMap::new()
    };

    [$($key:ident => $val:expr),*] => {
        HashMap::from([
        $(
            (stringify!($key).to_string(),$val.into()),
        )*
        ])
    };


}

pub fn var_map() -> VarMap {
    vars![
        noice => Value::Num(69.0),
        PI => Value::Num(PI),
        TAU => Value::Num(TAU),
        wait  => BuiltinFunc::new(wait_builtin,1),
        time => BuiltinFunc::new(unix_time, 0),
        open_file => BuiltinFunc::new(open_textfile, 1),
        input => BuiltinFunc::new(input_builtin,-1),
        println => BuiltinFunc::new(println_builtin,-1,),
        print => BuiltinFunc::new(print_builtin,-1),
        parse_num => BuiltinFunc::new(parse_num,1),
        to_str => BuiltinFunc::new(to_str,1),
        typeof => BuiltinFunc::new(typeof_node,1),
        eval => BuiltinFunc::new(eval,1),
        max => BuiltinFunc::new(max,2),
        min => BuiltinFunc::new(min,2),
        sqrt => BuiltinFunc::new(sqrt,1),
        sin => BuiltinFunc::new(sin,1),
        cos => BuiltinFunc::new(cos,1),
        tan => BuiltinFunc::new(tan,1),
        pow => BuiltinFunc::new(pow,2),
        import_var => BuiltinFunc::new(import_var,2)
    ]
}

pub fn num_struct() -> Struct {
    let env = vars![
        to_str => BuiltinFunc::new(to_str,1),
        max => BuiltinFunc::new(max,2),
        min => BuiltinFunc::new(min,2),
        sqrt => BuiltinFunc::new(sqrt,1),
        sin => BuiltinFunc::new(sin,1),
        cos => BuiltinFunc::new(cos,1),
        tan => BuiltinFunc::new(tan,1),
        pow => BuiltinFunc::new(pow,2)
    ];

    Struct {
        id: None,
        env: Scope::new(None, env, HashMap::from([])),
    }
}
pub fn str_struct() -> Struct {
    let env = vars![
        parse_num => BuiltinFunc::new(parse_num,1),
        substr => BuiltinFunc::new(substr_method,3),
        len => BuiltinFunc::new(len_method,1),
        remove => BuiltinFunc::new(str_remove,2),
        replace => BuiltinFunc::new(str_replace,2),
        char_at => BuiltinFunc::new(char_at_method,2)
    ];
    Struct {
        id: None,
        env: Scope::new(None, env, HashMap::from([])),
    }
}
fn str_remove(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let Value::Str(ref mut value) = args[0].clone() else {unreachable!()};
    let target = &args[1];
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

fn str_replace(scope: VarMap, args: ValueStream, heap: ValueStream) -> Value {
    let [Value::Str(ref value),Value::Str(ref target),Value::Str(ref filler)] = args[..3] else { return NULL; };
    Value::Str(value.replace(target, filler))
}
fn substr_method(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let [Value::Str(value),Value::Num(start), Value::Num(end)] = &args[..3] else { return NULL; };
    let sub = &value[start.clone() as usize..end.clone() as usize];
    Value::Str(sub.to_string())
}
fn len_method(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let Value::Str(value) = &args[0] else { unreachable!()};
    Value::Num(value.len() as f64)
}

fn char_at_method(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let Value::Str(value) = &args[0] else { unreachable!()};
    let Value::Num(index) = &args[1] else { unreachable!()};

    value
        .chars()
        .nth(*index as usize)
        .map(|c| Value::Str(c.to_string()))
        .unwrap_or(Value::Null)
}
fn println_builtin(_: VarMap, args: ValueStream, heap: ValueStream) -> Value {
    if args.is_empty() {
        println!();
    }
    let mut out = "".to_string();
    for val in args {
        out += format!(" {}", val.to_string()).as_str();
    }
    out = out.trim().to_string();
    println!("{out}");
    Value::Null
}
fn print_builtin(_: VarMap, args: ValueStream, heap: ValueStream) -> Value {
    if args.is_empty() {
        print!("");
        io::stdout().flush().unwrap();
        return Value::Null;
    }
    let mut out = "".to_string();
    for val in args {
        out += format!(" {}", val.to_string()).as_str();
    }
    out = out.trim().to_string();
    print!("{out}");
    io::stdout().flush().unwrap();
    NULL
}
fn open_textfile(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let Value::Str(path) = &args[0] else {unreachable!()};
    let Ok(contents) = fs::read_to_string(path)  else {unreachable!()};
    return Value::Str(contents);
}
fn typeof_node(_: VarMap, args: ValueStream, heap: ValueStream) -> Value {
    let val = if let Value::Ref(id) = args[0].clone() {
        &heap[id]
    } else {
        &args[0]
    };
    let out = val.get_type().to_string();
    Value::Str(out)
}
fn parse_num(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let Value::Str(input) = &args[0] else {unreachable!()};
    Value::Num(input.parse().unwrap())
}
fn min(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let [Value::Num(val1),Value::Num(val2)] = &args[..2] else {unreachable!()};
    Value::Num(val1.min(*val2))
}
fn max(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let [Value::Num(val1),Value::Num(val2)] = &args[..2] else {unreachable!()};
    Value::Num(val1.max(*val2))
}
fn pow(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let [Value::Num(val1),Value::Num(val2)] = &args[..2] else {unreachable!()};
    Value::Num(val1.powf(*val2))
}
fn cos(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {unreachable!()};
    Value::Num(val1.cos())
}
fn tan(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {unreachable!()};
    Value::Num(val1.tan())
}
fn sin(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {unreachable!()};
    Value::Num(val1.sin())
}
fn sqrt(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {unreachable!()};
    Value::Num(val1.sqrt())
}
pub fn to_str(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    Value::Str(args[0].to_string())
}
fn unix_time(_: VarMap, _: ValueStream, _: ValueStream) -> Value {
    Value::Num(
        SystemTime::now()
            .duration_since(time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )
}
fn wait_builtin(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    thread::sleep(Duration::from_millis((val1 * 1000.0).floor() as u64));
    NULL
}
fn input_builtin(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    if !args.is_empty() {
        print!("{}", args[0].to_string());
        io::stdout().flush().unwrap();
    }
    let mut result = String::new();
    let read = io::stdin().read_line(&mut result);
    if read.is_err() {
        result = "".to_string()
    }
    return Value::Str(String::from(result.trim()));
}
pub fn eval(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let Value::Str(source) = &args[0] else {return NULL};
    let mut parser = Parser::new(source.as_str());
    let ast_result = parser.parse();
    let Ok(ast) = ast_result else {
        return Value::Null;
    };

    let Ok(result) = Interpreter::new(ast).execute() else {return Value::Null;};

    result
}
fn import_var(_: VarMap, args: ValueStream, _: ValueStream) -> Value {
    let [Value::Str(path), Value::Str(var)] = &args[..2] else {unreachable!()};
    let Ok(source) = fs::read_to_string(path) else {return NULL;};
    let mut parser = Parser::new(source.as_str());
    let Ok(ast) = parser.parse() else {return NULL;};
    let Ok(scope) = Interpreter::new(ast).parse_vars() else {return NULL;};
    let Some(var) = scope.get_var(var) else {return NULL;};
    var
}
