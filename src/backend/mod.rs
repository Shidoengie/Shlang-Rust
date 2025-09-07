use crate::{
    backend::vm::StackVM, filestore::FileStore, frontend::Compiler, lang_errors::LangError,
};

pub mod vm;

#[derive(Debug, Default)]
pub struct Runtime {
    compiler: Compiler,
}
impl Runtime {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn execute(&mut self, input: &str) -> Result<(), Box<dyn LangError>> {
        match self.compiler.compile(input) {
            Ok((ir, spanmap)) => {
                let res = StackVM::new(ir).exec();
                res.map_err(|err| {
                    let code = err.to_spanned_code(&spanmap);
                    self.compiler.print_langerr(&code);
                    Box::new(code) as Box<_>
                })
            }
            Err(err) => {
                self.compiler.print_langerr(&*err);
                return Err(err);
            }
        }
    }
    }
