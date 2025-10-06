use std::ops::Deref;

use crate::{
    backend::{assembler::Assembler, instructions::ByteCode, vm::StackVM},
    filestore::FileStore,
    frontend::Compiler,
    lang_errors::LangError,
};

pub mod assembler;
pub mod instructions;
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
    pub fn get_filestore(self) -> FileStore {
        self.compiler.file_store
    }
    pub fn make(file_store: FileStore, silent: bool) -> Self {
        Self {
            compiler: Compiler::make(file_store, silent),
            silent,
        }
    }
    pub fn assemble(&mut self, input: &str) -> Result<ByteCode, Box<dyn LangError>> {
        let ir = self.compiler.compile(input)?;
        Ok(Assembler::assemble(ir))
    }
    pub fn assemble_expr(&mut self, input: &str) -> Result<ByteCode, Box<dyn LangError>> {
        let ir = self.compiler.compile_expr(input)?;
        Ok(Assembler::assemble(ir))
    }
    pub fn execute(&mut self, input: &str) -> Result<(), Box<dyn LangError>> {
        let ir = self.compiler.compile(input)?;
        let bytecode = Assembler::assemble(ir);
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
        let ir = self.compiler.compile_expr(&input)?;
        let bytecode = Assembler::assemble(ir);
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
