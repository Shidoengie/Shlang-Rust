use crate::{
    Interpreter,
    backend::{
        defaults::create_err,
        values::*,
    },
    check_args, get_list, get_native_params,
};
use std::collections::HashMap;

pub fn make_hashmap(data: NativeConstructorData, ctx: &mut Interpreter) -> NativeConstructorResult {
    if !data.arguments.is_empty() {
        return Err("This struct doesnt accept any arguments".to_string());
    }
    Ok(HashMap::<String, Value>::new().into())
}
type Ce = NativeCallError;
impl NativeTraitID for HashMap<String, Value> {
    fn get_obj_id() -> &'static str {
        "Map"
    }
}
impl NativeTrait for HashMap<String, Value> {
    fn call_native_method(
        &mut self,
        name: &str,
        ctx: &mut Interpreter,
        data: FuncData,
    ) -> NativeFuncResult {
        match name {
            "from" => {
                get_native_params!(
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
                        let err = create_err(
                            format!("Unexpected type {v} in rows", v = item.get_type()),
                            &mut ctx.heap,
                        );
                        return Ok(err);
                    };
                    let tuple = get_list!(tuple_key, ctx);
                    if tuple.len() != 2 {
                        let err = create_err(
                            "Each row must have exactly two columns.".to_string(),
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
                get_native_params!(
                    Value::Str(key) = Type::Str
                ;data,ctx);
                let val = self.get(key).unwrap_or(&Value::Null).clone();
                Ok(val)
            }
            "set" => {
                get_native_params!(
                    Value::Str(key) = Type::Str
                ;data,ctx);
                self.insert(key.to_owned(), data.args[1].to_owned());
                Ok(Value::Null)
            }
            "values" => {
                check_args!(data.args.len())?;
                let values: Vec<Value> = self.values().cloned().collect();
                let key = ctx.heap.insert(Value::List(values));
                Ok(Value::Ref(key))
            }
            "keys" => {
                check_args!(data.args.len())?;
                let values: Vec<Value> = self.keys().cloned().map(Value::Str).collect();
                let key = ctx.heap.insert(Value::List(values));
                Ok(Value::Ref(key))
            }
            "len" => {
                check_args!(data.args.len())?;
                Ok(Value::Num(self.len() as f64))
            }
            "remove" => {
                get_native_params!(
                    Value::Str(key) = Type::Str
                ;data,ctx);
                if self.remove(key).is_none() {
                    return Ok(create_err("Key not found", &mut ctx.heap));
                }

                Ok(Value::Null)
            }
            "has_key" => {
                get_native_params!(
                    Value::Str(key) = Type::Str
                ;data,ctx);
                Ok(Value::Bool(self.contains_key(key)))
            }
            _ => Err(Ce::MethodNotFound),
        }
    }
    fn lang_repr(&self) -> String {
        let mut buffer = String::new();
        buffer += "{";
        for (index, (k, v)) in self.iter().enumerate() {
            buffer += &format!("\"{k}\"");
            buffer += ":";
            buffer += &v.to_string();
            if index < self.len() - 1 {
                buffer += ", "
            }
        }
        buffer += "}";
        buffer
    }
}
