use crate::ast_nodes;
use ast_nodes::*;
use std::collections::HashMap;
use std::io;
use std::io::Write;


pub fn var_map() -> HashMap<String, Value> {
    let map = HashMap::from([
        ("nice".to_string(), Value::Num(69.0)),
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
    ]);
    map
}

fn val_to_str(val:&Value)->String{
    match val {
        Value::Num(num) => num.to_string(),
        Value::Bool(cond) => cond.to_string(),
        Value::Str(txt) => txt.to_string(),
        Value::Null => "null".to_string(),
        Value::Void => "void".to_string(),
        Value::Control(val) => format!("{val:?}"),
        _ => "unnamed".to_string(),
    }
}
pub fn println_builtin(args: ValueStream) -> Value {
    if args.is_empty() {
        println!("");
    }
    let mut out = "".to_string();
    for val in args {
        out += format!(" {}",val_to_str(&val)).as_str();
    }
    println!("{out}");
    return Value::Void;
}
pub fn print_builtin(args: ValueStream) -> Value {
    if args.is_empty() {
        print!("");
        io::stdout().flush().unwrap();
        return Value::Void;
    }
    let mut out = "".to_string();
    for val in args {
        out += format!(" {}",val_to_str(&val)).as_str();
    }
    print!("{out}");
    io::stdout().flush().unwrap();
    return Value::Void;
}

pub fn parse_num(args: ValueStream)->Value{
    let Value::Str(input) = &args[0] else {panic!()};
    Value::Num(input.parse().unwrap())
}
pub fn to_str(args: ValueStream)->Value{
    return Value::Str(val_to_str(&args[0]));
}
pub fn input_builtin(args: ValueStream) -> Value {
    if !args.is_empty(){
        let message = &args[0];
        print!("{}",val_to_str(&message));
        io::stdout().flush().unwrap();
    }
    let mut result = String::new();
    let read = io::stdin().read_line(&mut result);
    if read.is_err() {
        result = "".to_string()
    }
    return Value::Str(String::from(result.trim()));
}
