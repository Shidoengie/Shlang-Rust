use crate::{
    backend::interpreter::Control, catch, get_params, lang_errors::LangError, Interpreter,
};

use super::*;

pub fn closure_obj() -> Struct {
    let props = vars![
        call(call_closure, 2),
        arg_len(count_args, 1),
        args(get_args, 1),
        get_env(get_env, 1),
        set_env(set_env, 2)
    ];
    Struct {
        id: None,
        env: Scope::from_vars(props),
    }
}
fn get_env(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Closure(closure) = Type::Closure
    ;data,state);
    let env_obj = Struct {
        env: state.envs[closure.env].clone(),
        id: None,
    };
    Ok(Value::Ref(state.heap.insert(Value::Struct(env_obj))))
}
fn set_env(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Closure(closure) = Type::Closure,
        Value::Ref(key) = Type::Ref
    ;data,state);

    let Value::Struct(obj) = state.heap.get(*key).unwrap() else {
        return Ok(type_err_obj(
            &Type::AnonStruct,
            &state.heap.get(*key).unwrap().get_type(),
            1,
            &mut state.heap,
        ));
    };
    let scope = obj.env.clone();
    state.envs[closure.env] = scope;
    NULL
}
fn call_closure(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Closure(closure) = Type::Closure,
        Value::Ref(list_ref) = Type::Ref
    ;data,state);

    let Value::List(ref list) = state.heap[*list_ref] else {
        return Ok(type_err_obj(
            &Type::List,
            &state.heap[*list_ref].get_type(),
            1,
            &mut state.heap,
        ));
    };
    let env_key = closure.env;
    let env = &mut state.envs[env_key];

    let res = catch!( err {
        return Ok(create_err(err.msg(), &mut state.heap));
    } in Interpreter::execute_func_with(closure.to_owned().into(), env, list.to_vec()));
    return Ok(res);
}
