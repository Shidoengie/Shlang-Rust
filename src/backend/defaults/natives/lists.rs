use super::Ce;
use crate::{
    Interpreter,
    backend::{
        defaults::create_err,
        values::*,
    },
    catch, check_args, get_list, get_native_params,
};

impl NativeTraitID for Vec<Value> {
    fn get_obj_id() -> &'static str {
        "List"
    }
}

impl NativeTrait for Vec<Value> {
    fn call_native_method(
        &mut self,
        name: &str,
        ctx: &mut Interpreter,
        data: FuncData,
    ) -> NativeFuncResult {
        let given_size = data.args.len();
        match name {
            "pop" => {
                check_args!(given_size)?;
                let Some(value) = self.pop() else {
                    return Ok(Value::Null);
                };
                Ok(value)
            }
            "len" => {
                check_args!(given_size)?;
                Ok(Value::Num(self.len() as f64))
            }
            "remove" => {
                check_args!(1, given_size)?;

                let old_len = self.len();
                let new_list: Vec<Value> = self
                    .clone()
                    .into_iter()
                    .filter(|v| v != &data.args[1])
                    .collect();
                let new_len = new_list.len();
                *self = new_list;
                Ok(Value::Num((old_len - new_len) as f64))
            }
            "pop_at" => {
                get_native_params!(
                    Value::Num(og_index) = Type::Num
                ;data,ctx);
                if *og_index >= self.len() as f64 {
                    return Ok(create_err("Index out of bounds", &mut ctx.heap));
                }
                if *og_index < 0.0 {
                    return Ok(create_err("Index out of bounds", &mut ctx.heap));
                }
                let index = og_index.floor() as usize;
                self.remove(index);
                Ok(Value::Null)
            }
            "append" => {
                get_native_params!(
                    Value::Ref(pushed_id) = Type::Ref
                ;data,ctx);
                let mut pushed = get_list!(pushed_id, ctx);
                self.append(&mut pushed);
                Ok(Value::Null)
            }
            "push" => {
                check_args!(1, given_size)?;
                self.push(data.args[1].clone());
                Ok(Value::Null)
            }
            "join" => {
                get_native_params!(
                    Value::Str(seperator) = Type::Str
                ;data,ctx);
                Ok(Value::Str(list_join(self, seperator)))
            }
            "has" => {
                check_args!(1, given_size)?;
                Ok(Value::Bool(self.contains(&data.args[1])))
            }
            "map" => {
                get_native_params!(
                    Value::Closure(cl) = Type::Closure
                ;data,ctx);
                let mut buffer: Vec<Value> = vec![];
                for v in self {
                    let mapped = catch!(err {
                        return Err(Ce::Major(err.item));
                    } in ctx.call_closure(cl.clone(), &[v.clone()], data.span));

                    buffer.push(mapped);
                }
                Ok(Value::Ref(ctx.heap.insert(Value::List(buffer))))
            }

            "filter" => {
                get_native_params!(
                    Value::Closure(cl) = Type::Closure
                ;data,ctx);
                let mut buffer: Vec<Value> = vec![];
                for v in self.iter().cloned() {
                    let mapped = catch!(err {
                        return Err(Ce::Major(err.item));
                    } in ctx.call_closure(cl.clone(), &[v.clone()], data.span));

                    let Value::Bool(skip) = mapped else {
                        return Ok(create_err(
                            format!(
                                "Expected return type to be bool but got {}",
                                mapped.get_type()
                            ),
                            &mut ctx.heap,
                        ));
                    };
                    if skip {
                        buffer.push(v);
                    }
                }
                Ok(Value::Ref(ctx.heap.insert(Value::List(buffer))))
            }
            _ => Err(Ce::MethodNotFound),
        }
    }
}
