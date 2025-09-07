use crate::{
    backend::vm::{self, StackVM},
    frontend::{Compiler, ir::instructions::Value},
    test_func,
};

pub fn run_expr(input: &str) -> vm::Result<Vec<(Value, usize)>> {
    let ops = Compiler::default().compile_expr(input).unwrap();
    let mut vm = StackVM::new(ops.0);
    vm.exec()?;
    return Ok(vm.values);
}
test_func!(
    vm_expr, run_expr , {
        "Simple addition" => "1+2+3"

    }
);
