use super::*;
use crate::backend::interpreter::Control;
use crate::catch;
use crate::lang_errors::LangError;
use crate::{get_params, Interpreter};

pub fn func_struct() -> Struct {
    let env = vars![
        call_with(call_func_with,2=>3),
        arg_len(count_args, 1),
        args(get_args,1)
    ];

    Struct {
        id: None,
        env: Scope::from_vars(env),
    }
}
fn call_func_with(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
        Value::Function(func) = Type::Function,
        Value::Ref(env_id) = Type::Ref;
        data,state
    );

    let Value::Struct(ref mut env_obj) = state.heap[*env_id].clone() else {
        return Ok(type_err_obj(
            &Type::AnonStruct,
            &state.heap[*env_id].get_type(),
            1,
            &mut state.heap,
        ));
    };
    let args = if let Some(Value::Ref(id)) = data.args.get(2) {
        let Value::List(ls) = state.heap[*id].clone() else {
            return Ok(type_err_obj(
                &Type::AnonStruct,
                &state.heap[*env_id].get_type(),
                1,
                &mut state.heap,
            ));
        };
        ls
    } else {
        vec![]
    };
    let res = catch!( err {
        return Ok(create_err(err.msg(), &mut state.heap));
    } in Interpreter::execute_func_with(func.clone(), &mut env_obj.env, args));
    return Ok(res);
}
