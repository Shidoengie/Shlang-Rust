use super::*;
use crate::{backend::interpreter::Control, get_params, lang_errors::LangError};
macro_rules! get_list {
    ($key:expr, $data:expr) => {{
        let Value::List(list) = $data.heap.get(*$key).unwrap().clone() else {
            return type_err_obj(
                &Type::List,
                &$data.heap.get(*$key).unwrap().get_type(),
                1,
                $data.heap,
            );
        };
        list
    }};
    (ref $key:expr, $data:expr) => {{
        let Value::List(list) = $data.heap.get(*$key).unwrap() else {
            return type_err_obj(
                &Type::List,
                &$data.heap.get(*$key).unwrap().get_type(),
                1,
                $data.heap,
            );
        };
        list
    }};
}
pub fn list_struct() -> Struct {
    let env = vars![
        len(list_len, 1),
        push(list_push, 2),
        append(list_append, 2),
        pop(list_pop, 1),
        remove(list_remove, 2),
        pop_at(list_pop_at, 2),
        join(join, 2),
        has(list_has, 2),
        clone(clone_val, 1),
        map(list_map, 2)
    ];
    Struct {
        id: None,
        env: Scope::from_vars(env),
    }
}
fn list_pop(data: FuncData) -> Value {
    get_params!(
        Value::Ref(key) = Type::Ref
    ;data);

    let mut list = get_list!(key, data);
    let Some(value) = list.pop() else {
        return NULL;
    };
    data.heap[*key] = Value::List(list);
    return value;
}
fn list_remove(data: FuncData) -> Value {
    get_params!(
        Value::Ref(key) = Type::Ref
    ;data);
    let list = get_list!(key, data);
    let old_len = list.len();
    let new_list: Vec<Value> = list.into_iter().filter(|v| v != &data.args[1]).collect();
    let new_len = new_list.len();
    data.heap[*key] = Value::List(new_list);
    Value::Num((old_len - new_len) as f64)
}
fn list_len(data: FuncData) -> Value {
    get_params!(
        Value::Ref(key) = Type::Ref
    ;data);
    let list = get_list!(ref key, data);
    Value::Num(list.len() as f64)
}
fn list_pop_at(data: FuncData) -> Value {
    get_params!(
        Value::Ref(key) = Type::Ref,
        Value::Num(og_index) = Type::Num
    ;data);
    let index = og_index.floor() as usize;
    let mut list = get_list!(key, data);
    list.remove(index);
    data.heap[*key] = Value::List(list);
    Value::Ref(*key)
}
fn list_append(data: FuncData) -> Value {
    get_params!(
        Value::Ref(list_id) = Type::Ref,
        Value::Ref(pushed_id) = Type::Ref
    ;data);
    let mut list = get_list!(list_id, data);
    let mut pushed = get_list!(pushed_id, data);
    list.append(&mut pushed);
    data.heap[*list_id] = Value::List(list);
    Value::Ref(*list_id)
}
fn list_push(data: FuncData) -> Value {
    get_params!(
        Value::Ref(key) = Type::Ref
    ;data);
    let mut list = get_list!(key, data);
    list.push(data.args[1].clone());
    data.heap[*key] = Value::List(list);
    Value::Ref(*key)
}

fn join(data: FuncData) -> Value {
    get_params!(
        Value::Ref(key) = Type::Ref,
        Value::Str(seperator) = Type::Str
    ;data);
    let list = get_list!(ref key, data);
    Value::Str(list_join(&list, &seperator))
}
fn list_has(data: FuncData) -> Value {
    get_params!(
        Value::Ref(key) = Type::Ref
    ;data);
    let list = get_list!(ref key, data);

    Value::Bool(list.contains(&data.args[1]))
}
fn list_map(data: FuncData) -> Value {
    get_params!(
        Value::Ref(key) = Type::Ref,
        Value::Closure(cl) = Type::Closure
    ;data);
    let mut cl = cl.clone();
    let list = get_list!(ref key, data);
    let mut buffer: Vec<Value> = vec![];
    for v in list {
        let mapped = match cl.exec(vec![v.to_owned()], data.envs) {
            Err(e) => {
                return create_err(e.msg(), data.heap);
            }
            Ok(Control::Value(val)) | Ok(Control::Return(val)) | Ok(Control::Result(val)) => val,
            _ => unimplemented!(),
        };
        buffer.push(mapped);
    }
    return Value::Ref(data.heap.insert(Value::List(buffer)));
}