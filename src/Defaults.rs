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
                arg_size: 1,
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
            "str_to_num".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: str_to_num,
                arg_size: 1,
            }),

        ),
        (
            "num_to_str".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: num_to_str,
                arg_size: 1,
            }),
            
        ),
    ]);
    map
}
pub fn print_builtin(args: ValueStream) -> Value {
    if args.is_empty() {
        println!("");

    }
    for val in args {
        match val {
            Value::Num(num) => println!("{num}"),
            Value::Bool(cond) => println!("{cond}"),
            Value::Str(txt) => println!("{txt}"),
            Value::Null => println!("null"),
            Value::Void => println!("void"),
            Value::Control(_) => println!("!"),
            _ => println!("todo"),
        }
    }
    return Value::Void;
}
pub fn str_to_num(args: ValueStream)->Value{
    let Value::Str(input) = &args[0] else {panic!()};
    Value::Num(input.parse().unwrap())
}
pub fn num_to_str(args: ValueStream)->Value{
    let Value::Num(input) = &args[0] else {panic!()};
    Value::Str(input.to_string())
}
pub fn input_builtin(args: ValueStream) -> Value {
    let message = &args[0];
    print!("{message:?}");
    io::stdout().flush().unwrap();
    let mut result = String::new();
    let read = io::stdin().read_line(&mut result);
    if read.is_err() {
        result = "".to_string()
    }
    return Value::Str(String::from(result.trim()));
}
