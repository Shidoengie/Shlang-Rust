use super::*;

use crate::catch;
use crate::lang_errors::LangError;
use crate::{Interpreter, get_params};

pub fn func_struct() -> Struct {
    let mut fnstr = Struct::default();
    fnstr.set_props(vars![
        call_with(call_func_with,2=>3),
        arg_len(count_args, 1),
        args(get_args,1)
    ]);
    fnstr
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
    } in state.call_func(func.clone(),&args,data.span ,&mut env_obj.env));
    return Ok(res);
}
