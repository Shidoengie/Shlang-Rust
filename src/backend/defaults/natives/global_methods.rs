use super::*;
use crate::backend::defaults::functions::to_str;
use crate::backend::defaults::{create_err, get_error_obj};
use crate::backend::values::*;
use crate::frontend::tokens::map_keyword;
use crate::lang_errors::LangError;
use crate::{Interpreter, get_params};
use crate::{catch, check_args};
type Ce = NativeCallError;
#[derive(Debug, Clone)]
pub struct GlobalMethods(pub Value);
impl NativeTrait for GlobalMethods {
    fn get_id(&self) -> &str {
        return "GlobalMethods";
    }
    fn call_native_method(
        &mut self,
        name: &str,
        ctx: &mut Interpreter,
        data: FuncData,
    ) -> NativeFuncResult {
        let arg_len = data.args.len();

        match name {
            "try" => {
                check_args!(0, arg_len)?;

                let Some(obj) = get_error_obj(&self.0, &mut ctx.heap) else {
                    return Ok(self.0.clone());
                };
                let msg = obj.get_prop("msg").unwrap();
                return Err(Ce::Panic(msg));
            }
            "catch" => {
                check_args!(1, arg_len)?;
                get_params!(
                    Value::Closure(func) = Type::Function
                ;data,ctx);
                if let Value::Ref(id) = self.0 {
                    let Value::Struct(obj) = &ctx.heap[id] else {
                        return Ok(self.0.clone());
                    };
                    if obj.id.as_ref().is_some_and(|id| id != "Error") {
                        return Ok(self.0.clone());
                    }
                } else {
                    return Ok(self.0.clone());
                };

                let res = catch!(
                    err {
                        return Err(Ce::Major(err.item));
                    } in ctx.call_closure(func.to_owned(), &[self.0.clone()], data.span)
                );
                return Ok(res);
            }
            "to_str" => {
                check_args!(0, arg_len)?;
                to_str(
                    FuncData {
                        args: vec![self.0.clone()],
                        span: data.span,
                        parent: data.parent,
                    },
                    ctx,
                )
                .map_err(|err| err.into())
            }
            _ => return Err(Ce::MethodNotFound),
        }
    }
}
