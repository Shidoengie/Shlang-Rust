use super::*;
use crate::catch;
use crate::get_params;
use crate::lang_errors::LangError;
use crate::Interpreter;
use crate::Parser;
use colored::Colorize;
use rand::Rng;
use rayon::iter::IntoParallelRefIterator;
use rayon::iter::ParallelIterator;
use std::fs;
use std::io;
use std::io::Write;
use std::thread;

use std::time;
use std::time::Duration;
use std::time::SystemTime;

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
    for (i, val) in data.args.iter().enumerate() {
        if i == 0 {
            out += &deref_val(val.clone(), data.heap).to_string();
            continue;
        }
        out += &format!(" {}", deref_val(val.clone(), data.heap));
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
    for (i, val) in data.args.iter().enumerate() {
        if i == 0 {
            out += &deref_val(val.clone(), data.heap).to_string();
            continue;
        }
        out += &format!(" {}", deref_val(val.clone(), data.heap));
    }

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
pub fn log(data: FuncData) -> Value {
    get_params!(
            Value::Num(val1) = Type::Num,
            Value::Num(val2) = Type::Num
        ;data);
    Value::Num(val1.log(*val2))
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
    inter.envs = data.envs.clone();
    let base = Scope::from_vars(vars!(
        __name => Value::Str("lib".to_string())
    ));
    let scope = catch!(
        err {
            return create_err(err.msg(), data.heap);
        } in inter.parse_vars(base)
    );
    data.envs.clone_from(&inter.envs);
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

    for (name, func) in inter.functions {
        data.global_funcs.insert(name, func);
    }
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
pub fn clone_val(data: FuncData) -> Value {
    let Value::Ref(id) = data.args[0] else {
        return data.args[0].clone();
    };
    let derefed = data.heap[id].clone();
    return Value::Ref(data.heap.insert(derefed));
}
pub fn capture_env(data: FuncData) -> Value {
    let obj = Struct {
        env: data.parent.clone(),
        id: None,
    };
    return Value::Ref(data.heap.insert(Value::Struct(obj)));
}
pub fn rand_num(data: FuncData) -> Value {
    match data.args.len() {
        1 => {
            get_params!(Value::Num(to) = Type::Num;data);
            let num: f64 = rand::thread_rng().gen_range(0.0..*to);
            return Value::Num(num);
        }
        2 => {
            get_params!(Value::Num(from) = Type::Num,Value::Num(to) = Type::Num;data);
            if (*from..*to).is_empty() {
                return create_err("Invalid Range", data.heap);
            }
            let num: f64 = rand::thread_rng().gen_range(*from..*to);
            return Value::Num(num);
        }
        _ => unimplemented!(),
    }
}
pub fn err_func(data: FuncData) -> Value {
    get_params!(Value::Str(msg) = Type::Str;data);
    return create_err(msg, data.heap);
}
///ONLY USE IT IN METHODS!
pub fn count_args(data: FuncData) -> Value {
    match &data.args[0] {
        Value::Function(func) => Value::Num(func.args.len() as f64),
        Value::Closure(cl) => Value::Num(cl.args.len() as f64),
        _ => unimplemented!(),
    }
}

///ONLY USE IT IN METHODS!
pub fn get_args(data: FuncData) -> Value {
    let args = match &data.args[0] {
        Value::Function(func) => &func.args,
        Value::Closure(cl) => &cl.args,
        _ => unimplemented!(),
    };
    let list: Vec<Value> = args.par_iter().map(|s| Value::Str(s.to_owned())).collect();
    return Value::Ref(data.heap.insert(Value::List(list)));
}
