use std::process::{Command, Output};
#[derive(Debug, Clone)]
pub struct CommandWrapper {
    process: String,
    arg_stream: Vec<String>,
}
impl CommandWrapper {
    pub fn new(process: String) -> Self {
        Self {
            process,
            arg_stream: vec![],
        }
    }
    pub fn arg(&mut self, name: String, value: String) {
        self.arg_stream.push(name);
        self.arg_stream.push(value);
    }
    pub fn args(&mut self, args: &[String]) {
        self.arg_stream.clone_from_slice(args);
    }

    pub fn finish(&self) -> std::io::Result<Output> {
        if cfg!(windows) {
            return Command::new("powershell")
                .arg("/C")
                .arg(self.process.clone())
                .args(&self.arg_stream)
                .output();
        }

        Command::new("sh")
            .arg("/C")
            .arg(self.process.clone())
            .args(&self.arg_stream)
            .output()
    }
}
impl NativeTraitID for Output {
    fn get_obj_id() -> &'static str {
        "Output"
    }
}
impl NativeTrait for Output {
    fn call_native_method(
        &mut self,
        name: &str,
        ctx: &mut crate::Interpreter,
        data: FuncData,
    ) -> NativeFuncResult {
        let len = data.args.len();
        match name {
            "stderr" => {
                check_args!(len)?;
                let Ok(out) = String::from_utf8(self.stderr.clone()) else {
                    return Ok(create_err("stdout was not utf8", &mut ctx.heap));
                };
                Ok(Value::Str(out))
            }
            "stdout" => {
                check_args!(len)?;
                let Ok(out) = String::from_utf8(self.stdout.clone()) else {
                    return Ok(create_err("stderr was not utf8", &mut ctx.heap));
                };
                Ok(Value::Str(out))
            }
            "status" => {
                check_args!(len)?;
                let Some(code) = self.status.code() else {
                    return Ok(Value::Null);
                };
                Ok(Value::Num(code as f64))
            }
            "sucess" => {
                check_args!(len)?;
                Ok(Value::Bool(self.status.success()))
            }
            _ => Err(Ce::MethodNotFound),
        }
    }
}
use crate::{
    Interpreter,
    backend::{
        defaults::create_err,
        values::{NativeCallError as Ce, *},
    },
    check_args, get_list, get_native_params,
};
pub fn make(data: NativeConstructorData, ctx: &mut Interpreter) -> NativeConstructorResult {
    let Some(name) = data.arguments.get("cmd") else {
        return Err("Name argument is required".to_string());
    };

    let obj = CommandWrapper::new(name.to_string());
    Ok(obj.into())
}
impl NativeTraitID for CommandWrapper {
    fn get_obj_id() -> &'static str {
        "Cmd"
    }
}
impl NativeTrait for CommandWrapper {
    fn call_native_method(
        &mut self,
        name: &str,
        ctx: &mut crate::Interpreter,
        data: FuncData,
    ) -> NativeFuncResult {
        let len = data.args.len();
        match name {
            "arg" => {
                get_native_params!(
                    2;
                    Value::Str(name) = Type::Str,
                    Value::Str(value) = Type::Str
                ;data,ctx);
                self.arg(name.clone(), value.clone());
                Ok(Value::Null)
            }
            "args" => {
                get_native_params!(
                    1;
                    Value::Ref(key) = Type::Ref
                ;data,ctx);
                let list = get_list!(key, ctx);
                let mut buf = vec![];
                for i in list {
                    buf.push(i.to_string());
                }
                self.args(&buf);
                Ok(Value::Null)
            }
            "finish" => {
                check_args!(len)?;
                match self.finish() {
                    Ok(ok) => {
                        let key = ctx.heap.insert(ok.into());
                        Ok(Value::Ref(key))
                    }
                    Err(err) => Ok(create_err(err, &mut ctx.heap)),
                }
            }
            _ => Err(Ce::MethodNotFound),
        }
    }
}
