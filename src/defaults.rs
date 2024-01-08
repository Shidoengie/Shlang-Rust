use crate::ast_nodes;
use crate::Interpreter;
use crate::Parser;
use ast_nodes::*;
use std::collections::HashMap;
use std::f64::consts::PI;
use std::io;
use std::io::Write;
use std::thread;
use std::time::Duration;
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
        ("pi".to_string(), Value::Num(PI)),
        (
            "coma".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: coma_builtin,
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
            "char_at".to_string(),
            Value::BuiltinFunc(BuiltinFunc {
                function: char_at_method,
                arg_size: 1,
            }),
        ),
    ])
}

pub fn parse_num_method(scope: VarMap, _: ValueStream) -> Value {
    let Some(Value::Str(value)) = scope.get("v") else {panic!()};
    let parsed: Result<f64, _> = String::from_iter(value.chars().filter(|&c| c != '_')).parse();
    let Ok(result) = parsed else {return NULL;};
    Value::Num(result)
}
pub fn num_to_str(scope: VarMap, _: ValueStream) -> Value {
    let Some(Value::Num(value)) = scope.get("v") else {panic!()};
    Value::Str(format!("{value}"))
}
fn substr_method(scope: VarMap, args: ValueStream) -> Value {
    let Some(Value::Str(value)) = scope.get("v") else { panic!() };
    let [Value::Num(start), Value::Num(end)] = args[..2] else { return NULL; };
    let sub = &value[start as usize..end as usize];
    Value::Str(sub.to_string())
}
fn len_method(scope: VarMap, _: ValueStream) -> Value {
    let Some(Value::Str(value)) = scope.get("v") else { panic!() };
    Value::Num(value.len() as f64)
}

fn char_at_method(scope: VarMap, args: ValueStream) -> Value {
    let Some(Value::Str(value)) = scope.get("v") else { panic!() };
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
        Value::Control(val) => format!("{val:?}"),
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
    Value::Null
}

pub fn typeof_node(_: VarMap, args: ValueStream) -> Value {
    use Type::*;
    let out = match args[0].get_type() {
        Void => "void",
        Null => "null",
        Function => "func",
        Never => "!",
        Bool => "bool",
        Num => "num",

        Str => "str",
        UserDefined(id) => return Value::Str(id),
        Ref(_) => "ref",
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
pub fn to_str(_: VarMap, args: ValueStream) -> Value {
    Value::Str(val_to_str(&args[0]))
}



// this needs more space
pub fn coma_builtin(_: VarMap, args: ValueStream) -> Value {
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
    let Value::Control(nevah) = result.0 else {return result.0;};

    match nevah {
        Control::Result(val, _) => *val,
        Control::Return(val, _) => *val,
        _ => Value::Null,
    }
}
