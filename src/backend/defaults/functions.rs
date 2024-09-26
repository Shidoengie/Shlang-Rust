use super::*;
use crate::assert_params;
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

pub fn delete_var(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let (var, will_panic) = match data.args.len() {
        0 => {
            assert_params!(
            Value::Str(var) = Type::Str
        ;data,state);
            (var, true)
        }
        1 => {
            assert_params!(
            Value::Str(var) = Type::Str,
            Value::Bool(will_panic) = Type::Bool
        ;data,state);
            (var, will_panic.clone())
        }
        _ => unimplemented!(),
    };
    if data.parent.vars.contains_key(var) {
        data.parent.vars.remove(var);
        return NULL;
    }
    let err = create_err("Undefined Variable", &mut state.heap);
    if will_panic {
        return Err(err);
    } else {
        return Ok(err);
    }
}
pub fn emit_panic(data: FuncData, state: &mut Interpreter) -> FuncResult {
    return Err(data.args[0].clone());
}
pub fn assert(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let [val1, val2] = &data.args[..2] else {
        unimplemented!()
    };
    if val1 == val2 {
        return NULL;
    }
    return Err(Value::Str(format!(
        "Assert failed; First value was {val1} and second value was {val2}"
    )));
}
pub fn try_eval(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let val = &data.args[0];
    let Some(obj) = get_error_obj(val, &mut state.heap) else {
        return Ok(val.clone());
    };
    let msg = obj.env.get_var("msg").unwrap();
    return Err(msg);
}
pub fn assert_type(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let [val1, val2] = [
        deref_val(data.args[0].clone(), &state.heap),
        deref_val(data.args[1].clone(), &state.heap),
    ];
    if val1.matches_typeof(&val2) {
        return NULL;
    }
    return Err(Value::Str(format!(
        "Type assert failed; First value had type {t1} and second value had type {t2}",
        t1 = val1.get_type(),
        t2 = val2.get_type()
    )));
}
pub fn println_builtin(data: FuncData, state: &mut Interpreter) -> FuncResult {
    if data.args.is_empty() {
        println!();
    }
    let mut out = "".to_string();
    for (i, val) in data.args.iter().enumerate() {
        if i == 0 {
            out += &deref_val(val.clone(), &state.heap).to_string();
            continue;
        }
        out += &format!(" {}", deref_val(val.clone(), &state.heap));
    }
    out = out.trim().to_string();
    println!("{out}");
    Ok(Value::Null)
}
fn print_err_msg(msg: impl Display) {
    println!("{} {msg}", "ERROR!".red());
}
pub fn print_err(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let val = deref_val(data.args[0].clone(), &state.heap);
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
pub fn print_builtin(data: FuncData, state: &mut Interpreter) -> FuncResult {
    if data.args.is_empty() {
        print!("");
        io::stdout().flush().unwrap();
        return Ok(Value::Null);
    }
    let mut out = "".to_string();
    for (i, val) in data.args.iter().enumerate() {
        if i == 0 {
            out += &deref_val(val.clone(), &state.heap).to_string();
            continue;
        }
        out += &format!(" {}", deref_val(val.clone(), &state.heap));
    }

    print!("{out}");
    io::stdout().flush().unwrap();
    NULL
}

pub fn typeof_node(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let val = if let Value::Ref(id) = data.args[0].clone() {
        &state.heap[id]
    } else {
        &data.args[0]
    };
    let out = val.get_type().to_string();
    Ok(Value::Str(out))
}
pub fn parse_num(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(Value::Str(input) = Type::Str;data,state);
    let Ok(val) = input.parse() else {
        return Ok(create_err("Invalid float format", &mut state.heap));
    };
    Ok(Value::Num(val))
}
pub fn min(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num,
            Value::Num(val2) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.min(*val2)))
}
pub fn max(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num,
            Value::Num(val2) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.max(*val2)))
}
pub fn pow(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num,
            Value::Num(val2) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.powf(*val2)))
}
pub fn cos(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.cos()))
}
pub fn tan(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.tan()))
}
pub fn sin(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.sin()))
}
pub fn sqrt(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.sqrt()))
}
pub fn floor(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.floor()))
}
pub fn round(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.round()))
}
pub fn log(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num,
            Value::Num(val2) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.log(*val2)))
}
pub fn deref_val(val: Value, heap: &SlotMap<RefKey, Value>) -> Value {
    if let Value::Ref(id) = val {
        return heap[id].clone();
    }
    val
}
pub fn to_str(data: FuncData, state: &mut Interpreter) -> FuncResult {
    Ok(Value::Str(data.args[0].to_string()))
}
pub fn stringify_vals(data: FuncData, state: &mut Interpreter) -> FuncResult {
    if data.args.is_empty() {
        return Ok(Value::Str("".to_owned()));
    }
    let mut out = String::new();
    for val in data.args {
        out += (val.to_string() + "").as_str();
    }
    Ok(Value::Str(out.trim().to_owned()))
}
pub fn unix_time(_: FuncData, _: &mut Interpreter) -> FuncResult {
    Ok(Value::Num(
        SystemTime::now()
            .duration_since(time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    ))
}
pub fn wait_builtin(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    thread::sleep(Duration::from_millis((val1 * 1000.0).floor() as u64));
    NULL
}
pub fn input_builtin(data: FuncData, state: &mut Interpreter) -> FuncResult {
    if !data.args.is_empty() {
        print!("{}", data.args[0]);
        io::stdout().flush().unwrap();
    }
    let mut result = String::new();
    let read = io::stdin().read_line(&mut result);
    if read.is_err() {
        result = "".to_string()
    }
    return Ok(Value::Str(String::from(result.trim())));
}
pub fn eval(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Str(source) = Type::Str
        ;data,state);
    let mut parser = Parser::new(source.as_str());

    let (ast, functions) = catch!(
        err {
            return Ok(create_err(err.msg(), &mut state.heap));
        } in parser.parse()
    );
    let mut inter = Interpreter::new(ast, functions);
    inter.heap.clone_from(&state.heap);
    let result = catch! {
        err {
            return Ok(create_err(err.msg(),&mut state.heap));
        } in inter.execute_with(&mut data.parent.clone())
    };

    Ok(result)
}
pub fn import_var(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let (path, will_panic) = match data.args.len() {
        0 => {
            assert_params!(
            Value::Str(path) = Type::Str
            ;data,state);
            (path, true)
        }
        1 => {
            assert_params!(
            Value::Str(path) = Type::Str,
            Value::Bool(will_panic) = Type::Bool
        ;data,state);
            (path, *will_panic)
        }
        _ => unimplemented!(),
    };

    let Ok(source) = fs::read_to_string(path) else {
        return Ok(create_err("Failed to read file", &mut state.heap));
    };
    let mut parser = Parser::new(source.as_str());

    let (ast, functions) = catch!(
        err {
            let err = create_err(err.msg(), &mut state.heap);
            if will_panic{return Err(err)} else {return Ok(err);}

        } in parser.parse()
    );
    let mut inter = Interpreter::new(ast, functions);
    inter.envs.clone_from(&state.envs);
    let base = Scope::from_vars(vars!(
        __name => Value::Str("lib".to_string())
    ));
    let scope = catch!(
        err {
            let err = create_err(err.msg(), &mut state.heap);
            if will_panic{return Err(err)} else {return Ok(err);}
        } in inter.parse_vars(base)
    );
    state.envs.clone_from(&inter.envs);
    let heapstuff = |(name, val): (&String, &Value)| -> (String, Value) {
        let mut new_val = val.to_owned();
        if let Value::Ref(id) = val {
            let derefed = &inter.heap[*id];
            let key = state.heap.insert(derefed.clone());
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
        state.functions.insert(name, func);
    }
    data.parent.vars = new_vars;
    data.parent.structs = new_structs;

    NULL
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
pub fn range(data: FuncData, state: &mut Interpreter) -> FuncResult {
    match data.args.len() {
        0 => {
            return Ok(create_err("Expected at least 1 argument", &mut state.heap));
        }
        1 => {
            get_params!(
                    Value::Num(to) = Type::Num
                ;data,state);
            let new_range = gen_range(0.0, *to, 1.0);
            let val = state.heap.insert(Value::List(new_range));
            Ok(Value::Ref(val))
        }
        2 => {
            get_params!(
                    Value::Num(from) = Type::Num,
                    Value::Num(to) = Type::Num
                ;data,state);
            if from == &0.0 && to == &0.0 {
                return Ok(create_err("Invalid range", &mut state.heap));
            }
            let new_range = gen_range(*from, *to, 1.0);
            let val = state.heap.insert(Value::List(new_range));
            Ok(Value::Ref(val))
        }
        _ => {
            get_params!(
                    Value::Num(mut from) = Type::Num,
                    Value::Num(mut to) = Type::Num,
                    Value::Num(mut inc) = Type::Num
                ;data,state);

            if inc == 0.0 {
                return Ok(create_err("Increment cant be bellow 0", &mut state.heap));
            }
            if from == 0.0 && to == 0.0 {
                return Ok(create_err("Invalid range", &mut state.heap));
            }
            if from >= 0.0 && inc > 0.0 && to < 0.0 {
                return Ok(create_err("Invalid range", &mut state.heap));
            }
            if from <= 0.0 && inc < 0.0 && to > 0.0 {
                return Ok(create_err("Invalid range", &mut state.heap));
            }
            let new_range = gen_range(from, to, inc);
            let val = state.heap.insert(Value::List(new_range));
            Ok(Value::Ref(val))
        }
    }
}
pub fn clone_val(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let Value::Ref(id) = data.args[0] else {
        return Ok(data.args[0].clone());
    };
    let derefed = state.heap[id].clone();
    return Ok(Value::Ref(state.heap.insert(derefed)));
}
pub fn capture_env(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let obj = Struct {
        env: data.parent.clone(),
        id: None,
    };
    return Ok(Value::Ref(state.heap.insert(Value::Struct(obj))));
}
pub fn rand_num(data: FuncData, state: &mut Interpreter) -> FuncResult {
    match data.args.len() {
        1 => {
            get_params!(Value::Num(to) = Type::Num;data,state);
            let num: f64 = rand::thread_rng().gen_range(0.0..*to);
            return Ok(Value::Num(num));
        }
        2 => {
            get_params!(Value::Num(from) = Type::Num,Value::Num(to) = Type::Num;data,state);
            if (*from..*to).is_empty() {
                return Ok(create_err("Invalid Range", &mut state.heap));
            }
            let num: f64 = rand::thread_rng().gen_range(*from..*to);
            return Ok(Value::Num(num));
        }
        _ => unimplemented!(),
    }
}
pub fn err_func(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(Value::Str(msg) = Type::Str;data,state);
    return Ok(create_err(msg, &mut state.heap));
}
///ONLY USE IT IN METHODS!
pub fn count_args(data: FuncData, state: &mut Interpreter) -> FuncResult {
    match &data.args[0] {
        Value::Function(func) => Ok(Value::Num(func.args.len() as f64)),
        Value::Closure(cl) => Ok(Value::Num(cl.args.len() as f64)),
        _ => unimplemented!(),
    }
}

///ONLY USE IT IN METHODS!
pub fn get_args(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let args = match &data.args[0] {
        Value::Function(func) => &func.args,
        Value::Closure(cl) => &cl.args,
        _ => unimplemented!(),
    };
    let list: Vec<Value> = args.par_iter().map(|s| Value::Str(s.to_owned())).collect();
    return Ok(Value::Ref(state.heap.insert(Value::List(list))));
}
pub fn open_textfile(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(Value::Str(path) = Type::Str;data,state);
    let Ok(contents) = fs::read_to_string(path) else {
        return Ok(create_err("Failed to open file", &mut state.heap));
    };
    return Ok(Value::Str(contents));
}
pub fn open_dir(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(Value::Str(path) = Type::Str;data,state);
    let contents = catch!(err {
        return Ok(create_err(err, &mut state.heap));
    } in fs::read_dir(path));
    let mut files: Vec<Value> = vec![];
    for query in contents {
        match query {
            Ok(entry) => {
                let Ok(file) = fs::read_to_string(entry.path()) else {
                    unimplemented!()
                };
                files.push(Value::Str(file));
            }
            Err(err) => {
                return Ok(create_err(err.to_string(), &mut state.heap));
            }
        }
    }
    return Ok(Value::Ref(state.heap.insert(Value::List(files))));
}
pub fn paths_in_dir(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(Value::Str(path) = Type::Str;data,state);
    let contents = catch!(err {
        return Ok(create_err(err, &mut state.heap));
    } in fs::read_dir(path));
    let mut paths: Vec<Value> = vec![];
    for query in contents {
        match query {
            Ok(entry) => {
                paths.push(Value::Str(entry.path().to_str().unwrap().to_owned()));
            }
            Err(err) => {
                return Ok(create_err(err.to_string(), &mut state.heap));
            }
        }
    }
    return Ok(Value::Ref(state.heap.insert(Value::List(paths))));
}
pub fn create_file(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(path) = Type::Str
    ;data,state);
    let Ok(_) = fs::File::create(path) else {
        return Ok(create_err("Failed to create file", &mut state.heap));
    };
    return NULL;
}
pub fn create_dir(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(path) = Type::Str
    ;data,state);
    let Ok(_) = fs::create_dir(path) else {
        return Ok(create_err("Failed to create directory", &mut state.heap));
    };
    return NULL;
}
pub fn delete_dir(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(path) = Type::Str
    ;data,state);
    if let Err(err) = fs::remove_dir_all(path) {
        return Ok(create_err(err.to_string(), &mut state.heap));
    }
    return NULL;
}
pub fn delete_file(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(path) = Type::Str
    ;data,state);
    if let Err(err) = fs::remove_file(path) {
        return Ok(create_err(err.to_string(), &mut state.heap));
    }
    return NULL;
}
pub fn write_file(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(path) = Type::Str,
        Value::Str(contents) = Type::Str
    ;data,state);
    let Ok(_) = fs::write(path, contents) else {
        return Ok(create_err("Failed to write file", &mut state.heap));
    };
    return NULL;
}
