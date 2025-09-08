use std::ops::Deref;

use crate::{
    backend::vm::StackVM, filestore::FileStore, frontend::Compiler, lang_errors::LangError, spanmap,
};

pub mod vm;

#[derive(Debug, Default)]
pub struct Runtime {
    compiler: Compiler,
    silent: bool,
}
impl Runtime {
    pub fn new() -> Self {
        Self {
            compiler: Compiler::new(),
            silent: false,
        }
    }
    pub fn make(file_store: FileStore, silent: bool) -> Self {
        Self {
            compiler: Compiler::make(file_store, silent),
            silent,
        }
    }
    pub fn execute(&mut self, input: &str) -> Result<(), Box<dyn LangError>> {
        let (ir, spanmap) = self.compiler.compile(input)?;
        let res = StackVM::new(ir).exec();
        res.map_err(|err| {
            let code = err.to_spanned_code(&spanmap);
            Box::new(code) as Box<dyn LangError>
        })
        .inspect_err(|err| {
            if !self.silent {
                self.compiler
                    .print_langerr(err.deref())
                    .expect("Could not print error.");
            }
        })
    }
    pub fn execute_expr(&mut self, input: &str) -> Result<(), Box<dyn LangError>> {
        let (ir, spanmap) = self.compiler.compile_expr(input)?;
        let res = StackVM::new(ir).exec();
        res.map_err(|err| {
            let code = err.to_spanned_code(&spanmap);

            Box::new(code) as Box<dyn LangError>
        })
        .inspect_err(|err| {
            if !self.silent {
                self.compiler
                    .print_langerr(err.deref())
                    .expect("Could not print error.");
            }
        })
    }
}
