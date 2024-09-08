use super::*;
use crate::backend::scope::*;
use crate::frontend::nodes::*;
use crate::get_params;
use rayon::prelude::*;
use std::collections::HashMap;

pub fn str_struct() -> Struct {
    let env = vars![
        parse_num(parse_num, 1),
        substr(substr, 3),
        len(len, 1),
        remove(remove, 2),
        replace(replace, 2),
        char_at(char_at, 2),
        split(split, 1 => 2),
        to_upper(to_upper, 1),
        to_lower(to_lower, 1),
        has(has_val, 2)
    ];
    Struct {
        id: None,
        env: Scope::new(None, env, HashMap::from([])),
    }
}

fn has_val(data: FuncData) -> Value {
    get_params!(
            Value::Str(value) = Type::Str,
            Value::Str(pat) = Type::Str
        ;data);
    Value::Bool(value.contains(pat))
}
fn remove(data: FuncData) -> Value {
    get_params!(
            Value::Str(val) = Type::Str
        ;data);
    let mut value = val.clone();
    let target = &data.args[1];
    match target {
        Value::Num(index) => {
            if index < &0.0 || index >= &(value.len() as f64) {
                return create_err("Index out of bounds", data.heap);
            }
            value.remove(*index as usize);
            return Value::Str(value.to_string());
        }
        Value::Str(ref pattern) => return Value::Str(value.replace(pattern, "")),
        _ => NULL,
    }
}

fn replace(data: FuncData) -> Value {
    get_params!(
            Value::Str(value) = Type::Str,
            Value::Str(target) = Type::Str,
            Value::Str(filler) = Type::Str
        ;data);
    Value::Str(value.replace(target, filler))
}
fn substr(data: FuncData) -> Value {
    get_params!(
            Value::Str(value) = Type::Str,
            Value::Num(start) = Type::Num,
            Value::Num(end) = Type::Num
        ;data);
    let start_index = start.floor() as usize;
    let end_index = end.floor() as usize;
    let sub = &value[start_index..end_index];
    Value::Str(sub.to_string())
}
fn to_upper(data: FuncData) -> Value {
    get_params!(
            Value::Str(value) = Type::Str
        ;data);
    Value::Str(value.to_uppercase())
}
fn to_lower(data: FuncData) -> Value {
    get_params!(
            Value::Str(value) = Type::Str
        ;data);
    Value::Str(value.to_lowercase())
}
fn len(data: FuncData) -> Value {
    get_params!(
            Value::Str(value) = Type::Str
        ;data);
    Value::Num(value.len() as f64)
}

fn char_at(data: FuncData) -> Value {
    get_params!(
            Value::Str(value) = Type::Str,
            Value::Num(og_index) = Type::Num
        ;data);
    let index = if *og_index < 0.0 {
        return create_err("Index out of bounds", data.heap);
    } else {
        og_index.floor() as usize
    };

    if index >= value.len() {
        return create_err("Index out of bounds", data.heap);
    }
    let (_, val) = value
        .as_parallel_string()
        .par_char_indices()
        .skip_any(index)
        .find_first(|(i, _)| *i == index)
        .unwrap();
    return Value::Str(val.to_string());
}
fn split(data: FuncData) -> Value {
    get_params!(
            Value::Str(value) = Type::Str
        ;data);
    match data.args.len() {
        1 => {
            let split_str = value
                .split(' ')
                .map(|v| Value::Str(String::from(v)))
                .collect();
            let val = data.heap.insert(Value::List(split_str));
            Value::Ref(val)
        }
        _ => {
            let Value::Str(seperator) = &data.args[1] else {
                return type_err_obj(&Type::Str, &data.args[1].get_type(), 1, data.heap);
            };
            if seperator.is_empty() {
                return create_err("Seperator must not be empty", data.heap);
            }
            let split_str = value
                .split(seperator)
                .map(|v| Value::Str(String::from(v)))
                .collect();
            let val = data.heap.insert(Value::List(split_str));
            Value::Ref(val)
        }
    }
}
