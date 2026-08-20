use crate::{frontend::*, test_func};

fn test_ir<'a>(input: &'a str) -> Result<Ir<'a>, Box<dyn LangError>> {
	let mut compiler = Compiler::new();
	compiler.compile_expr(input)
}

test_func!(
	ir_expr,test_ir, {
		"Basic arithmetic" => {1+(2-(3*(4/(5 % 6))))}
		"Comparisions" => "1 != (2 > (3 >= ( 4 < ( 5 <= (6 == (8 ?? 9) ) ) ) ) )"
		"Boolean operators" => "true || (false && true)"
		"Unary operators" => "-1 != !true"
		"Branches" => {if true {1+2;3;} else {2+4;}}
		"Functions" => "func hello(a) {println(a,2,3);}"
		"Loops" => {
			loop {
				continue;
				break;
			}
		},
		"While Loops" => {
		while true {
			break
		}
		},
	}
);
