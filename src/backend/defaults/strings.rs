use super::*;
use crate::backend::scope::*;
use crate::frontend::nodes::*;
use crate::{get_params, Interpreter};
use rayon::prelude::*;
use std::collections::HashMap;

pub fn str_struct() -> Struct {
    let env = vars![
        parse_num(parse_num, 1),
        substr(3),
        len(1),
        remove(2),
        replace(3),
        char_at(2),
        split(split, 1 => 2),
        to_upper(1),
        to_lower(1),
        has(has_val, 2),
        as_error(into_err,1),
        repeat(2)
    ];
    Struct {
        id: None,
        env: Scope::new(None, env, HashMap::from([])),
    }
}
fn repeat(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(value) = Type::Str,
        Value::Num(times) = Type::Num
    ;data,state);
    let n = times.floor() as usize;
    return Ok(Value::Str(value.repeat(n)));
}

fn has_val(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Str(value) = Type::Str,
            Value::Str(pat) = Type::Str
        ;data,state);
    Ok(Value::Bool(value.contains(pat)))
}
fn remove(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Str(val) = Type::Str
        ;data,state);
    let mut value = val.clone();
    let target = &data.args[1];
    match target {
        Value::Num(index) => {
            if index < &0.0 || index >= &(value.len() as f64) {
                return Ok(create_err("Index out of bounds", &mut state.heap));
            }
            value.remove(*index as usize);
            return Ok(Value::Str(value.to_string()));
        }
        Value::Str(ref pattern) => return Ok(Value::Str(value.replace(pattern, ""))),
        _ => NULL,
    }
}

fn replace(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Str(value) = Type::Str,
            Value::Str(target) = Type::Str,
            Value::Str(filler) = Type::Str
        ;data,state);
    Ok(Value::Str(value.replace(target, filler)))
}
fn substr(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Str(value) = Type::Str,
            Value::Num(start) = Type::Num,
            Value::Num(end) = Type::Num
        ;data,state);
    let start_index = start.floor() as usize;
    let end_index = end.floor() as usize;
    let sub = &value[start_index..end_index];
    Ok(Value::Str(sub.to_string()))
}
fn to_upper(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Str(value) = Type::Str
        ;data,state);
    Ok(Value::Str(value.to_uppercase()))
}
fn to_lower(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Str(value) = Type::Str
        ;data,state);
    Ok(Value::Str(value.to_lowercase()))
}
fn len(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Str(value) = Type::Str
        ;data,state);
    Ok(Value::Num(value.len() as f64))
}

fn char_at(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Str(value) = Type::Str,
            Value::Num(og_index) = Type::Num
        ;data,state);
    let index = if *og_index < 0.0 {
        return Ok(create_err("Index out of bounds", &mut state.heap));
    } else {
        og_index.floor() as usize
    };

    if index >= value.len() {
        return Ok(create_err("Index out of bounds", &mut state.heap));
    }
    let (_, val) = value
        .as_parallel_string()
        .par_char_indices()
        .skip_any(index)
        .find_first(|(i, _)| *i == index)
        .unwrap();
    return Ok(Value::Str(val.to_string()));
}
fn into_err(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Str(value) = Type::Str
    ;data,state);
    return Ok(create_err(value, &mut state.heap));
}
fn split(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Str(value) = Type::Str
        ;data,state);
    match data.args.len() {
        1 => {
            let split_str = value
                .split(' ')
                .map(|v| Value::Str(String::from(v)))
                .collect();
            let val = state.heap.insert(Value::List(split_str));
            Ok(Value::Ref(val))
        }
        _ => {
            let Value::Str(seperator) = &data.args[1] else {
                return Ok(type_err_obj(
                    &Type::Str,
                    &data.args[1].get_type(),
                    1,
                    &mut state.heap,
                ));
            };
            if seperator.is_empty() {
                return Ok(create_err("Seperator must not be empty", &mut state.heap));
            }
            let split_str = value
                .split(seperator)
                .map(|v| Value::Str(String::from(v)))
                .collect();
            let val = state.heap.insert(Value::List(split_str));
            Ok(Value::Ref(val))
        }
    }
}
