use crate::frontend::ir::codegen::{self, IRgen};
use crate::frontend::ir::instructions::OpCode;
use crate::frontend::*;

use crate::spans::Spanned;
use crate::test_func;

fn test_ir(input: &str) -> Result<Vec<OpCode>, Box<dyn LangError>> {
    Compiler::compile_expr(input)
}

test_func!(
    ir_expr,test_ir, {
        "Basic arithmetic" => "1+(2-(3*(4/(5 % 6))))",
        "Comparisions" => "1 != (2 > (3 >= ( 4 < ( 5 <= (6 == (8 ?? 9) ) ) ) ) )",
        "Boolean operators" => "true || (false && true)"
        "Unary operators" => "-1 != !true"
        "Branches" => "if true {1+2;3+4;} else {2+4;}"
    }
);
