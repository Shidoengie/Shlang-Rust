use super::*;
pub fn emit_panic(data: FuncData, state: &mut Interpreter) -> FuncResult {
    return Err(CallError::Panic(data.args[0].clone()));
}
pub fn assert(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let [val1, val2] = &data.args[..2] else {
        unimplemented!()
    };
    if val1 == val2 {
        return NULL;
    }
    return Err(CallError::Panic(Value::Str(format!(
        "Assert failed; First value was {val1} and second value was {val2}"
    ))));
}
pub fn try_eval(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let val = &data.args[0];
    let Some(obj) = get_error_obj(val, &mut state.heap) else {
        return Ok(val.clone());
    };
    let msg = obj.env.get_var("msg").unwrap();
    return Err(CallError::Panic(msg));
}
pub fn assert_type(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let [val1, val2] = [
        deref_val(data.args[0].clone(), &state.heap),
        deref_val(data.args[1].clone(), &state.heap),
    ];
    if val1.matches_typeof(&val2) {
        return NULL;
    }
    return Err(CallError::Panic(Value::Str(format!(
        "Type assert failed; First value had type {t1} and second value had type {t2}",
        t1 = val1.get_type(),
        t2 = val2.get_type()
    ))));
}
