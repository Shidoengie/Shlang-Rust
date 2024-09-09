use crate::{
    backend::interpreter::Control, catch, get_params, lang_errors::LangError, Interpreter,
};

use super::*;

pub fn closure_obj() -> Struct {
    let props = vars![
        env => NULL,
        fn => NULL,
        call(call_closure,2)
    ];
    Struct {
        id: Some("Closure".to_string()),
        env: Scope::from_vars(props),
    }
}
fn call_closure(data: FuncData) -> Value {
    get_params!(
        Value::Ref(obj_ref) = Type::Ref,
        Value::Ref(list_ref) = Type::Ref
    ;data);
    let Value::Struct(ref obj) = data.heap[*obj_ref] else {
        return type_err_obj(
            &Type::Closure,
            &data.heap[*obj_ref].get_type(),
            1,
            data.heap,
        );
    };
    if obj.id.as_ref().is_some_and(|id| id != "Closure") || obj.id.is_none() {
        return type_err_obj(
            &Type::Closure,
            &data.heap[*obj_ref].get_type(),
            1,
            data.heap,
        );
    }
    let Value::List(ref list) = data.heap[*list_ref] else {
        return type_err_obj(&Type::List, &data.heap[*list_ref].get_type(), 1, data.heap);
    };
    let Some(Value::Function(func)) = obj.env.get_var("fn") else {
        return create_err("expected fn to be a function", data.heap);
    };
    let Some(Value::Ref(env_ref)) = obj.env.get_var("env") else {
        return create_err("expected env to be a ref", data.heap);
    };
    let Value::Struct(ref mut env_obj) = data.heap[env_ref].clone() else {
        return type_err_obj(
            &Type::Struct(None),
            &data.heap[*obj_ref].get_type(),
            1,
            data.heap,
        );
    };
    let res = catch!( err {
        return create_err(err.msg(), data.heap);
    } in Interpreter::execute_func_with(func.clone(), &mut env_obj.env, list.to_vec()));
    match res {
        Control::Return(v) | Control::Result(v) | Control::Value(v) => v,
        _ => NULL,
    }
}
