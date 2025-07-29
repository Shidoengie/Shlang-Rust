use std::process::{Command, Output};

use crate::{
    Interpreter,
    backend::{
        defaults::create_err,
        values::{NativeCallError as Ce, *},
    },
    check_args, get_list, get_native_params,
};
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
    pub fn arg(&mut self, name: String) {
        self.arg_stream.push(name);
    }
    pub fn args(&mut self, args: &[String]) {
        self.arg_stream.clone_from_slice(args);
    }

    pub fn finish(&self) -> std::io::Result<OutputWrapper> {
        let out = if cfg!(windows) {
            Command::new("powershell")
                .arg("/C")
                .arg(self.process.clone())
                .args(&self.arg_stream)
                .output()?
        } else {
            Command::new("sh")
                .arg("/C")
                .arg(self.process.clone())
                .args(&self.arg_stream)
                .output()?
        };
        Ok(OutputWrapper::new(out))
    }
}

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
        let key = data.key.expect("Expected refkey to exist");
        match name {
            "arg" => {
                get_native_params!(
                    Value::Str(name) = Type::Str
                ;data,ctx);
                self.arg(name.clone());

                Ok(Value::Ref(*key))
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
                Ok(Value::Ref(*key))
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
#[derive(Debug, Clone)]
pub struct OutputWrapper {
    inner: Output,
}
impl OutputWrapper {
    pub fn new(inner: Output) -> Self {
        Self { inner }
    }
}

impl NativeTraitID for OutputWrapper {
    fn get_obj_id() -> &'static str {
        "Output"
    }
}

impl NativeTrait for OutputWrapper {
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
                let Ok(out) = String::from_utf8(self.inner.stdout.clone()) else {
                    return Ok(create_err("stderr was not utf8", &mut ctx.heap));
                };
                Ok(Value::Str(out))
            }
            "stdout" => {
                check_args!(len)?;
                let Ok(out) = String::from_utf8(self.inner.stdout.clone()) else {
                    return Ok(create_err("stdout was not utf8", &mut ctx.heap));
                };
                Ok(Value::Str(out))
            }
            "status" => {
                check_args!(len)?;
                let Some(code) = self.inner.status.code() else {
                    return Ok(Value::Null);
                };
                Ok(Value::Num(code as f64))
            }
            "success" => {
                check_args!(len)?;
                Ok(Value::Bool(self.inner.status.success()))
            }
            _ => Err(Ce::MethodNotFound),
        }
    }
}
