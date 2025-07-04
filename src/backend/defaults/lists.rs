use super::*;
use crate::{backend::values::Control, catch, get_params, lang_errors::LangError, Interpreter};
macro_rules! get_list {
    ($key:expr, $state:expr) => {{
        let Value::List(list) = $state.heap.get(*$key).unwrap().clone() else {
            return Ok(type_err_obj(
                &Type::List,
                &$state.heap.get(*$key).unwrap().get_type(),
                1,
                &mut $state.heap,
            ));
        };
        list
    }};
    ($key:expr, ref $state:expr) => {{
        let Value::List(list) = $state.heap.get(*$key).unwrap() else {
            return Ok(type_err_obj(
                &Type::List,
                &$state.heap.get(*$key).unwrap().get_type(),
                1,
                &mut $state.heap,
            ));
        };
        list
    }};
    (ref $key:expr, $state:expr) => {{
        let Value::List(list) = $state.heap.get(*$key).unwrap() else {
            return Ok(type_err_obj(
                &Type::List,
                &$state.heap.get(*$key).unwrap().get_type(),
                1,
                &mut $state.heap,
            ));
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
fn list_pop(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Ref(key) = Type::Ref
    ;data,state);

    let mut list = get_list!(key, state);
    let Some(value) = list.pop() else {
        return NULL;
    };
    state.heap[*key] = Value::List(list);
    return Ok(value);
}
fn list_remove(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Ref(key) = Type::Ref
    ;data,state);
    let list = get_list!(key, state);
    let old_len = list.len();
    let new_list: Vec<Value> = list.into_iter().filter(|v| v != &data.args[1]).collect();
    let new_len = new_list.len();
    state.heap[*key] = Value::List(new_list);
    Ok(Value::Num((old_len - new_len) as f64))
}
fn list_len(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Ref(key) = Type::Ref
    ;data,state);
    let list = get_list!(ref key, state);
    Ok(Value::Num(list.len() as f64))
}
fn list_pop_at(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Ref(key) = Type::Ref,
        Value::Num(og_index) = Type::Num
    ;data,state);
    let index = og_index.floor() as usize;
    let mut list = get_list!(key, state);
    list.remove(index);
    state.heap[*key] = Value::List(list);
    Ok(Value::Ref(*key))
}
fn list_append(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Ref(list_id) = Type::Ref,
        Value::Ref(pushed_id) = Type::Ref
    ;data,state);
    let mut list = get_list!(list_id, state);
    let mut pushed = get_list!(pushed_id, state);
    list.append(&mut pushed);
    state.heap[*list_id] = Value::List(list);
    Ok(Value::Ref(*list_id))
}
fn list_push(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Ref(key) = Type::Ref
    ;data,state);
    let mut list = get_list!(key, state);
    list.push(data.args[1].clone());
    state.heap[*key] = Value::List(list);
    Ok(Value::Ref(*key))
}

fn join(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Ref(key) = Type::Ref,
        Value::Str(seperator) = Type::Str
    ;data,state);
    let list = get_list!(ref key, state);
    Ok(Value::Str(list_join(list, seperator)))
}
fn list_has(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Ref(key) = Type::Ref
    ;data,state);
    let list = get_list!(ref key, state);

    Ok(Value::Bool(list.contains(&data.args[1])))
}
fn list_map(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Ref(key) = Type::Ref,
        Value::Closure(cl) = Type::Closure
    ;data,state);
    let list = get_list!(key, state);
    let mut buffer: Vec<Value> = vec![];
    for v in list {
        let mapped = catch!(err {
            return Ok(create_err(err.msg(), &mut state.heap));
        } in state.call_closure(cl.clone(), vec![v.clone()], data.span));
        let Control::Value(val) = mapped else {
            unimplemented!()
        };
        buffer.push(val);
    }
    return Ok(Value::Ref(state.heap.insert(Value::List(buffer))));
}
