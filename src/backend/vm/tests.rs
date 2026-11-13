use crate::backend::{
	Runtime,
	vm::{self, values::Value},
};

use test_file_macro::test_file;
pub fn run_expr(input: &str) -> vm::Result<Vec<Value>> {
	let bytecode = Runtime::default().assemble_expr(input).unwrap();
	let mut vm = bytecode.new_vm();
	vm.exec()?;
	Ok(vm.values)
}
test_file!(vm_expr, run_expr, "src/backend/vm/tests.shlang");
