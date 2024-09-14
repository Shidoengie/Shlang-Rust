use crate::{
    backend::interpreter::Control, catch, get_params, lang_errors::LangError, Interpreter,
};

use super::*;

pub fn closure_obj() -> Struct {
    let props = vars![
        call(call_closure, 2),
        arg_len(count_args, 1),
        args(get_args, 1)
    ];
    Struct {
        id: None,
        env: Scope::from_vars(props),
    }
}

fn call_closure(data: FuncData) -> Value {
    get_params!(
        Value::Closure(closure) = Type::Closure,
        Value::Ref(list_ref) = Type::Ref
    ;data);

    let Value::List(ref list) = data.heap[*list_ref] else {
        return type_err_obj(&Type::List, &data.heap[*list_ref].get_type(), 1, data.heap);
    };
    let env_key = closure.env;
    let env = &mut data.envs[env_key];

    let res = catch!( err {
        return create_err(err.msg(), data.heap);
    } in Interpreter::execute_func_with(closure.to_owned().into(), env, list.to_vec()));
    match res {
        Control::Return(v) | Control::Result(v) | Control::Value(v) => v,
        _ => NULL,
    }
}
