use super::*;
pub fn math_obj() -> Struct {
    let mut obj = Struct::new("Math");
    obj.set_props(vars!(
        max(2),
        min(2),
        sqrt(1),
        sin(1),
        cos(1),
        tan(1),
        pow(2),
        log(2),
    ));
    obj
}
pub fn min(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num,
            Value::Num(val2) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.min(*val2)))
}
pub fn max(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num,
            Value::Num(val2) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.max(*val2)))
}
pub fn pow(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num,
            Value::Num(val2) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.powf(*val2)))
}
pub fn cos(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.cos()))
}
pub fn tan(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.tan()))
}
pub fn sin(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.sin()))
}
pub fn sqrt(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.sqrt()))
}
pub fn floor(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.floor()))
}
pub fn round(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.round()))
}
pub fn log(data: FuncData, state: &mut Interpreter) -> FuncResult {
    get_params!(
            Value::Num(val1) = Type::Num,
            Value::Num(val2) = Type::Num
        ;data,state);
    Ok(Value::Num(val1.log(*val2)))
}
