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
        let bytecode = self.compiler.compile(input)?;
        let res = StackVM::new(bytecode.ops, bytecode.global_count, bytecode.local_count).exec();
        res.map_err(|err| {
            let code = err.to_spanned_code(&bytecode.span_map);
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
        let input = format!("do {{ {input} }}");
        let bytecode = self.compiler.compile_expr(&input)?;
        let mut vm = StackVM::new(bytecode.ops, bytecode.global_count, bytecode.local_count);
        let res = vm.exec();
        res.map_err(|err| {
            let code = err.to_spanned_code(&bytecode.span_map);

            Box::new(code) as Box<dyn LangError>
        })
        .inspect_err(|err| {
            if !self.silent {
                self.compiler
                    .print_langerr(err.deref())
                    .expect("Could not print error.");
            }
        })
        .inspect(|_| println!("{:?}", vm.values))
    }
}
