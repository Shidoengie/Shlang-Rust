use super::*;
use crate::backend::defaults::{create_err, type_err_obj};
use crate::backend::values::*;

use crate::{Interpreter, check_args, get_params};

use rayon::prelude::*;

impl NativeTrait for String {
    fn get_id(&self) -> &str {
        return "String";
    }
    fn call_native_method(
        &mut self,
        name: &str,
        ctx: &mut Interpreter,
        data: FuncData,
    ) -> NativeFuncResult {
        let len = data.args.len();
        match name {
            "len" => {
                check_args(arg_range!(), len)?;
                return Ok(Value::Num(self.chars().count() as f64));
            }
            "byte_len" => {
                check_args(arg_range!(), len)?;
                return Ok(Value::Num(self.len() as f64));
            }
            "parse_num" => {
                check_args!(len)?;
                let Ok(num) = self.parse::<f64>() else {
                    return Ok(create_err("Failed to parse number", &mut ctx.heap));
                };
                return Ok(Value::Num(num));
            }
            "repeat" => {
                check_args!(1, len)?;
                get_params!(
                    Value::Num(times) = Type::Num
                ;data,ctx);
                let n = times.floor() as usize;
                return Ok(Value::Str(self.repeat(n)));
            }
            "has" => {
                check_args(arg_range!(1), len)?;
                get_params!(
                    Value::Str(pat) = Type::Str
                ;data,ctx);
                Ok(Value::Bool(self.contains(pat)))
            }
            "remove" => {
                check_args(arg_range!(1), len)?;
                let target = &data.args[1];
                match target {
                    Value::Num(index) => {
                        if index < &0.0 || index >= &(self.len() as f64) {
                            return Ok(create_err("Index out of bounds", &mut ctx.heap));
                        }
                        let mut value = self.clone();
                        value.remove(index.floor() as usize);
                        return Ok(Value::Str(value));
                    }
                    Value::Str(pattern) => return Ok(Value::Str(self.replace(pattern, ""))),
                    _ => Ok(Value::Null),
                }
            }
            "replace" => {
                check_args(arg_range!(2), len)?;
                get_params!(
                    Value::Str(target) = Type::Str,
                    Value::Str(filler) = Type::Str
                ;data,ctx);

                Ok(Value::Str(self.replace(target, filler)))
            }
            "byte_substring" => {
                check_args!(2, len)?;
                get_params!(
                    Value::Num(start) = Type::Num,
                    Value::Num(end) = Type::Num
                ;data,ctx);

                if *start < 0.0 {
                    return Ok(create_err(
                        "Start index must be a positive integer",
                        &mut ctx.heap,
                    ));
                }
                if *start >= self.len() as f64 {
                    return Ok(create_err(
                        "Start index must not be greater then the strings length",
                        &mut ctx.heap,
                    ));
                }
                if *end < 0.0 {
                    return Ok(create_err(
                        "End index must be a positive integer",
                        &mut ctx.heap,
                    ));
                }
                if *end >= self.len() as f64 {
                    return Ok(create_err(
                        "End index must not be greater then the strings length",
                        &mut ctx.heap,
                    ));
                }
                if *end >= *start {
                    return Ok(create_err(
                        "End index must be greater then the start index",
                        &mut ctx.heap,
                    ));
                }
                let start_index = start.floor() as usize;
                let end_index = end.floor() as usize;
                return Ok(Value::Str(self[start_index..end_index].to_owned()));
            }
            "substring" => {
                check_args!(2, len)?;
                get_params!(
                    Value::Num(start) = Type::Num,
                    Value::Num(end) = Type::Num
                ;data,ctx);

                let chars = self.char_indices();
                let char_len = chars.clone().count();
                if *start < 0.0 {
                    return Ok(create_err(
                        "Start index must be a positive integer",
                        &mut ctx.heap,
                    ));
                }
                if *start >= char_len as f64 {
                    return Ok(create_err(
                        "Start index must not be greater then the strings length",
                        &mut ctx.heap,
                    ));
                }
                if *end < 0.0 {
                    return Ok(create_err(
                        "End index must be a positive integer",
                        &mut ctx.heap,
                    ));
                }
                if *end >= char_len as f64 {
                    return Ok(create_err(
                        "End index must not be greater then the strings length",
                        &mut ctx.heap,
                    ));
                }
                if *end >= *start {
                    return Ok(create_err(
                        "End index must be greater then the start index",
                        &mut ctx.heap,
                    ));
                }
                let start_index = start.floor() as usize;
                let end_index = end.floor() as usize;
                let chars = chars.skip_while(|(idx, _)| *idx != start_index);
                let mut sub = String::new();
                for (idx, char) in chars {
                    sub.push(char);
                    if idx == end_index {
                        break;
                    }
                }
                Ok(Value::Str(sub.to_string()))
            }
            "to_upper" => {
                check_args(NO_ARGS, len)?;
                return Ok(Value::Str(self.to_uppercase()));
            }
            "char_at" => {
                check_args(arg_range!(1), len)?;
                get_params!(
                    Value::Str(value) = Type::Str,
                    Value::Num(og_index) = Type::Num
                ;data,ctx);
                let index = if *og_index < 0.0 {
                    return Ok(create_err("Index out of bounds", &mut ctx.heap));
                } else {
                    og_index.floor() as usize
                };

                if index >= value.len() {
                    return Ok(create_err("Index out of bounds", &mut ctx.heap));
                }
                let (_, val) = value
                    .as_parallel_string()
                    .par_char_indices()
                    .skip_any(index)
                    .find_first(|(i, _)| *i == index)
                    .unwrap();
                return Ok(Value::Str(val.to_string()));
            }
            "to_lower" => {
                check_args(NO_ARGS, len)?;
                return Ok(Value::Str(self.to_lowercase()));
            }
            "into_err" => {
                return Ok(create_err(self, &mut ctx.heap));
            }
            "split" => {
                check_args(arg_range!(0 => 1), len)?;
                if data.args.is_empty() {
                    let split_str = self
                        .split(' ')
                        .map(|v| Value::Str(String::from(v)))
                        .collect();
                    let val = ctx.heap.insert(Value::List(split_str));
                    return Ok(Value::Ref(val));
                }
                let Value::Str(seperator) = &data.args[1] else {
                    return Ok(type_err_obj(
                        &Type::Str,
                        &data.args[1].get_type(),
                        1,
                        &mut ctx.heap,
                    ));
                };
                if seperator.is_empty() {
                    return Ok(create_err("Seperator must not be empty", &mut ctx.heap));
                }
                let split_str = self
                    .split(seperator)
                    .map(|v| Value::Str(String::from(v)))
                    .collect();
                let val = ctx.heap.insert(Value::List(split_str));
                Ok(Value::Ref(val))
            }
            _ => return Err(Ce::MethodNotFound),
        }
    }
}
