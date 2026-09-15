use crate::{frontend::*, test_file};

fn test_ir<'a>(input: &'a str) -> Result<Ir<'a>, Box<dyn LangError>> {
	let mut compiler = Compiler::new();
	compiler.compile_expr(input)
}

test_file!(ir_expr, test_ir, "src/frontend/ir/tests.shlang");
