use rand::Rng;

use crate::backend::defaults::natives::cmd::CommandWrapper;

use super::*;
use std::{
    error::Error,
    fs, io,
    process::Command,
    thread,
    time::{self, Duration, SystemTime},
};

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
        return Err(CallError::Panic(err));
    } else {
        return Ok(err);
    }
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
    let parsed_funcs = HashMap::from_iter(functions.iter().map(|(name, value)| {
        let Node::FuncDef(func) = value else {
            unimplemented!("All Nodes should be the variant func, please check the parser.");
        };
        (
            name.clone(),
            Function::new(func.block.clone(), func.args.clone()).into(),
        )
    }));
    let mut inter = Interpreter::new(ast, parsed_funcs);
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
            if will_panic{return Err(CallError::Panic(err))} else {return Ok(err);}

        } in parser.parse()
    );
    let parsed_funcs = HashMap::from_iter(functions.iter().map(|(name, value)| {
        let Node::FuncDef(func) = value else {
            unimplemented!("All Nodes should be the variant func, please check the parser.");
        };
        (
            name.clone(),
            Function::new(func.block.clone(), func.args.clone()).into(),
        )
    }));
    let mut inter = Interpreter::new(ast, parsed_funcs);
    inter.envs.clone_from(&state.envs);
    let base = Scope::from_vars(vars!(
        __name => Value::Str("lib".to_string())
    ));
    let scope = catch!(
        err {
            let err = create_err(err.msg(), &mut state.heap);
            if will_panic{return Err(CallError::Panic(err));} else {return Ok(err);}
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
                    &Value::Num(from) = Type::Num,
                    &Value::Num(to) = Type::Num,
                    &Value::Num(inc) = Type::Num
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
pub fn cmd(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(Value::Str(process) = Type::Str;data,state);
    return Ok(Value::Ref(
        state
            .heap
            .insert(CommandWrapper::new(process.clone()).into()),
    ));
}
