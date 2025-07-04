use super::*;
use crate::backend::values::Control;
use crate::catch;
use crate::lang_errors::LangError;
use crate::{get_params, Interpreter};

pub fn global_methods() -> Scope {
    Scope::from_vars(vars![
        try(try_eval,1),
        catch(catch_err,2)
    ])
}
fn catch_err(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let val = &data.args[0];

    let func = match &data.args[1] {
        Value::Closure(cl) => cl,
        v => {
            return Err(CallError::Panic(Value::Str(format!(
                "Expected type Closure but got type {}",
                v.get_type()
            ))))
        }
    };
    if let Value::Ref(id) = val {
        let Value::Struct(obj) = &state.heap[*id] else {
            return Ok(val.clone());
        };
        if obj.id.as_ref().is_some_and(|id| id != "Error") {
            return Ok(val.clone());
        }
    } else {
        return Ok(val.clone());
    };

    let res = catch!(
        err {
            return Err(CallError::Panic(create_err(err.msg(), &mut state.heap)));
        } in state.call_closure(func.to_owned(), vec![val.clone()], data.span)
    );
    let Control::Value(val) = res else {
        unimplemented!()
    };
    return Ok(val);
}
