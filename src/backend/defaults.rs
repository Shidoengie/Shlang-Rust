use crate::frontend::nodes::*;

use crate::Interpreter;
use crate::Parser;

use std::collections::HashMap;
use std::env::vars;
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

    ([$($acc:tt)*] $name:ident($func:ident,$size:expr) $(, $($left:tt)*)? ) => {
        vars_internal!([$($acc)* (
            stringify!($name).to_string(),
            Value::BuiltinFunc(BuiltinFunc::new(
                stringify!($name).to_string(),
                $func,$size
            ))
        ),] $($($left)*)? )
    };
}
pub fn var_map() -> VarMap {
    vars![
        noice => Value::Num(69.0),
        PI => Value::Num(PI),
        TAU => Value::Num(TAU),
        wait(wait_builtin,1),
        time(unix_time,0),
        open_file(open_textfile,1),
        input(input_builtin,-1),
        println(println_builtin,-1),
        print(print_builtin,-1),
        parse_num(parse_num,1),
        to_str(to_str,1),
        stringify(stringify_vals,-1),
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
        del(delete_var,1)
    ]
}
fn delete_var(scope: &mut Scope, args: Vec<Value>, heap: &mut Vec<Value>) -> Value {
    let Value::Str(val) = &args[0] else {return NULL;};
    if !scope.vars.contains_key(val) {
        return NULL;
    }
    scope.vars.remove(val);
    NULL
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
        pow(pow, 2)
    ];

    Struct {
        id: None,
        env: Scope::new(None, env, HashMap::from([])),
    }
}
pub fn str_struct() -> Struct {
    let env = vars![
        parse_num(parse_num, 1),
        substr(substr_method, 3),
        len(len_method, 1),
        remove(str_remove, 2),
        replace(str_replace, 2),
        char_at(char_at_method, 2)
    ];
    Struct {
        id: None,
        env: Scope::new(None, env, HashMap::from([])),
    }
}
fn str_remove(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let Value::Str(ref mut value) = args[0].clone() else {return NULL;};
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

fn str_replace(scope: &mut Scope, args: Vec<Value>, heap: &mut Vec<Value>) -> Value {
    let [Value::Str(ref value),Value::Str(ref target),Value::Str(ref filler)] = args[..3] else { return NULL; };
    Value::Str(value.replace(target, filler))
}
fn substr_method(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let [Value::Str(value),Value::Num(start), Value::Num(end)] = &args[..3] else { return NULL; };
    let sub = &value[start.clone() as usize..end.clone() as usize];
    Value::Str(sub.to_string())
}
fn len_method(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let Value::Str(value) = &args[0] else { return NULL;};
    Value::Num(value.len() as f64)
}

fn char_at_method(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let Value::Str(value) = &args[0] else { return NULL;};
    let Value::Num(index) = &args[1] else { return NULL;};

    value
        .chars()
        .nth(*index as usize)
        .map(|c| Value::Str(c.to_string()))
        .unwrap_or(Value::Null)
}
fn println_builtin(_: &mut Scope, args: Vec<Value>, heap: &mut Vec<Value>) -> Value {
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
fn print_builtin(_: &mut Scope, args: Vec<Value>, heap: &mut Vec<Value>) -> Value {
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
fn open_textfile(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let Value::Str(path) = &args[0] else {return NULL;};
    let Ok(contents) = fs::read_to_string(path)  else {return NULL;};
    return Value::Str(contents);
}
fn typeof_node(_: &mut Scope, args: Vec<Value>, heap: &mut Vec<Value>) -> Value {
    let val = if let Value::Ref(id) = args[0].clone() {
        &heap[id]
    } else {
        &args[0]
    };
    let out = val.get_type().to_string();
    Value::Str(out)
}
fn parse_num(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let Value::Str(input) = &args[0] else {return NULL;};
    Value::Num(input.parse().unwrap())
}
fn min(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let [Value::Num(val1),Value::Num(val2)] = &args[..2] else {return NULL;};
    Value::Num(val1.min(*val2))
}
fn max(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let [Value::Num(val1),Value::Num(val2)] = &args[..2] else {return NULL;};
    Value::Num(val1.max(*val2))
}
fn pow(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let [Value::Num(val1),Value::Num(val2)] = &args[..2] else {return NULL;};
    Value::Num(val1.powf(*val2))
}
fn cos(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    Value::Num(val1.cos())
}
fn tan(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    Value::Num(val1.tan())
}
fn sin(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    Value::Num(val1.sin())
}
fn sqrt(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    Value::Num(val1.sqrt())
}
pub fn to_str(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    Value::Str(args[0].to_string())
}
fn stringify_vals(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    if args.is_empty() {
        return Value::Str("".to_owned());
    }
    let mut out = String::new();
    for val in args {
        out += (val.to_string() + "").as_str();
    }
    Value::Str(out.trim().to_owned())
}
fn unix_time(_: &mut Scope, _: Vec<Value>, _: &mut Vec<Value>) -> Value {
    Value::Num(
        SystemTime::now()
            .duration_since(time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    )
}
fn wait_builtin(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
    let Value::Num(val1) = &args[0] else {return NULL;};
    thread::sleep(Duration::from_millis((val1 * 1000.0).floor() as u64));
    NULL
}
fn input_builtin(_: &mut Scope, args: Vec<Value>, _: &mut Vec<Value>) -> Value {
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
pub fn eval(scope: &mut Scope, args: Vec<Value>, heap: &mut Vec<Value>) -> Value {
    let Value::Str(source) = &args[0] else {return NULL};
    let mut parser = Parser::new(source.as_str());
    let ast_result = parser.parse();
    let Ok(ast) = ast_result else {
        return Value::Null;
    };
    let mut inter = Interpreter::new(ast);
    inter.heap = heap.clone();
    let Ok(result) = inter.execute_with(&mut scope.clone()) else {return Value::Null;};

    result
}
fn import_var(parent: &mut Scope, args: Vec<Value>, heap: &mut Vec<Value>) -> Value {
    let Value::Str(path) = &args[0] else {return NULL;};
    let Ok(source) = fs::read_to_string(path) else {return NULL;};
    let mut parser = Parser::new(source.as_str());
    let Ok(ast) = parser.parse() else {return NULL;};
    let mut inter = Interpreter::new(ast);
    let base = Scope::from_vars(vars!(
        __name__ => Value::Str("lib".to_string())
    ));
    let Ok(scope) = inter.parse_vars(base) else {return NULL;};
    let heapstuff = |(name, val): (&String, &Value)| -> (String, Value) {
        let mut new_val = val.to_owned();
        if let Value::Ref(id) = val {
            let derefed = &inter.heap[*id];
            heap.push(derefed.clone());
            new_val = Value::Ref(heap.len() - 1);
        }
        (name.to_owned(), new_val)
    };
    let new_vars: HashMap<_, _> = parent
        .vars
        .iter()
        .chain(&scope.vars)
        .map(heapstuff)
        .collect();
    let new_structs: HashMap<_, _> = parent
        .structs
        .iter()
        .chain(&scope.structs)
        .map(|(k, v)| (k.to_owned(), v.to_owned()))
        .collect();
    parent.vars = new_vars;
    parent.structs = new_structs;
    Value::Void
}
