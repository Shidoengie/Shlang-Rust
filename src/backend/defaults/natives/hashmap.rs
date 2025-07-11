use std::collections::HashMap;

use crate::{
    Interpreter, arg_range,
    backend::{
        defaults::{create_err, natives::check_args, type_err_obj},
        values::*,
    },
    get_list, get_params,
};
pub fn make_hashmap(data: NativeConstructorData, ctx: &mut Interpreter) -> NativeConstructorResult {
    if !data.arguments.is_empty() {
        return Err(format!("This struct doesnt accept any arguments"));
    }
    let obj = NativeObject::new("Map", HashMap::<String, Value>::new());
    return Ok(obj);
}
type Ce = NativeCallError;
impl NativeTrait for HashMap<String, Value> {
    fn call_native_method(
        &mut self,
        name: &str,
        ctx: &mut Interpreter,
        data: FuncData,
    ) -> NativeFuncResult {
        match name {
            "from" => {
                check_args(arg_range!(1), data.args.len())?;
                get_params!(
                    Value::Ref(key) = Type::Ref
                ;data,ctx);
                if let Value::Struct(obj) = &ctx.heap[*key] {
                    *self = obj.env.vars.clone();
                    return Ok(Value::Null);
                }
                let list = get_list!(key, ctx);
                let mut buffer = Vec::new();
                for item in list.iter() {
                    let Value::Ref(tuple_key) = item else {
                        let err =
                            create_err(format!("Unexpected type {item} in rows"), &mut ctx.heap);
                        return Ok(err);
                    };
                    let tuple = get_list!(tuple_key, ctx);
                    if tuple.len() != 2 {
                        let err = create_err(
                            format!("Each row must have exactly two columns."),
                            &mut ctx.heap,
                        );
                        return Ok(err);
                    }

                    buffer.push((tuple[0].to_string(), tuple[1].clone()))
                }

                *self = HashMap::from_iter(buffer);
                Ok(Value::Null)
            }
            "get" => {
                check_args(arg_range!(1), data.args.len())?;
                get_params!(
                    Value::Str(key) = Type::Str
                ;data,ctx);
                let val = self.get(key).unwrap_or(&Value::Null).clone();
                Ok(val)
            }
            "set" => {
                check_args(arg_range!(2), data.args.len())?;
                get_params!(
                    Value::Str(key) = Type::Str
                ;data,ctx);
                self.insert(key.to_owned(), data.args[1].to_owned());
                return Ok(Value::Null);
            }
            "values" => {
                check_args(arg_range!(0), data.args.len())?;
                let values: Vec<Value> = self.values().cloned().collect();
                let key = ctx.heap.insert(Value::List(values));
                return Ok(Value::Ref(key));
            }
            "keys" => {
                check_args(arg_range!(0), data.args.len())?;
                let values: Vec<Value> = self.keys().cloned().map(|key| Value::Str(key)).collect();
                let key = ctx.heap.insert(Value::List(values));
                return Ok(Value::Ref(key));
            }
            "len" => {
                check_args(arg_range!(0), data.args.len())?;
                return Ok(Value::Num(self.len() as f64));
            }
            "remove" => {
                check_args(arg_range!(1), data.args.len())?;
                get_params!(
                    Value::Str(key) = Type::Str
                ;data,ctx);
                if self.remove(key).is_none() {
                    return Ok(create_err("Key not found", &mut ctx.heap));
                }

                return Ok(Value::Null);
            }
            "has_key" => {
                check_args(arg_range!(1), data.args.len())?;
                get_params!(
                    Value::Str(key) = Type::Str
                ;data,ctx);
                return Ok(Value::Bool(self.contains_key(key)));
            }
            _ => return Err(Ce::MethodNotFound),
        }
    }
    fn lang_repr(&self) -> String {
        return "hi".to_owned();
    }
}
