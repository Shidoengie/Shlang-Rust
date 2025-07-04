use colored::Colorize;
use std::fmt::Display;
use std::fmt::Write as _;
use std::io;
use std::io::Write;

use crate::backend::interpreter::IError;
use crate::spans::Span;

use super::*;
pub fn print_err_msg(msg: impl Display) {
    println!("{} {msg}", "ERROR!".red());
}
fn stringfy_args(
    args: &[Value],
    span: Span,
    parent: &mut Scope,
    state: &mut Interpreter,
) -> FuncResult<String> {
    let mut out = "".to_string();
    for (i, val) in args.iter().enumerate() {
        let repr = get_print_repr(val.clone(), span, parent, state)?;
        if i == 0 {
            write!(out, "{repr}").expect("Failed to write to String");
            continue;
        }
        write!(out, " {repr}").expect("Failed to write to String");
    }
    out = out.trim().to_string();
    Ok(out)
}
pub fn print_err(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let val = deref_val(data.args[0].clone(), &state.heap);
    match &val {
        Value::Struct(obj) => {
            let Some(ref id) = obj.id else {
                print_err_msg(val);
                return NULL;
            };
            if id != "Error" {
                print_err_msg(val);
                return NULL;
            }
            let msg = obj.env.get_var("msg").unwrap();
            print_err_msg(msg);
        }
        _ => print_err_msg(val),
    };
    NULL
}
pub fn get_print_repr(
    val: Value,
    span: Span,
    parent: &mut Scope,
    state: &mut Interpreter,
) -> FuncResult<String> {
    let Value::Ref(key) = val else {
        return Ok(val.to_string());
    };
    let derefed = &state.heap[key];
    match derefed.to_owned() {
        Value::Struct(obj) => match obj.call_method(
            "__print",
            FuncData {
                args: vec![],
                span,
                parent,
            },
            state,
            key,
        ) {
            Ok(res) => {
                return Ok(res.to_string());
            }
            Err(item) => {
                if let IError::MethodNotFound(_, _) = item.item {
                    return Ok(obj.to_string());
                };
                return Err(CallError::Major(item.item));
            }
        },
        _ => return Ok(val.to_string()),
    }
}
pub fn println_builtin(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let out = stringfy_args(&data.args, data.span, data.parent, state)?;
    println!("{out}");
    Ok(Value::Null)
}
pub fn print_builtin(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let out = stringfy_args(&data.args, data.span, data.parent, state)?;
    print!("{out}");
    io::stdout().flush().unwrap();
    NULL
}
pub fn input_builtin(data: FuncData, state: &mut Interpreter) -> FuncResult {
    if !data.args.is_empty() {
        print!("{}", data.args[0]);
        io::stdout().flush().unwrap();
    }
    let mut result = String::new();
    let read = io::stdin().read_line(&mut result);
    if read.is_err() {
        result = "".to_string()
    }
    return Ok(Value::Str(String::from(result.trim())));
}
pub fn to_str(data: FuncData, state: &mut Interpreter) -> FuncResult {
    Ok(Value::Str(data.args[0].to_string()))
}
pub fn stringify_vals(data: FuncData, state: &mut Interpreter) -> FuncResult {
    let out = stringfy_args(&data.args, data.span, data.parent, state)?;
    Ok(Value::Str(out))
}
