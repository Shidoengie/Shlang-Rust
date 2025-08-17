use crate::frontend::ir::codegen::{self, IRgen};
use crate::frontend::ir::instructions::OpCode;
use crate::frontend::*;

use crate::spans::Spanned;
use crate::test_func;
use ast::parser::Parser;
use nameres::resolver;
use nameres::resolver::NameErr;
use std::{usize, *};

fn test_ir(input: &str) -> codegen::Result<Vec<OpCode>> {
    let expr = Parser::parse_expr(input, usize::MAX).unwrap();
    let expr = resolver::NameRes::resolve_expr(expr).unwrap();
    IRgen::generate_expr(expr)
}

test_func!(
    ir_expr,test_ir, {
        "Basic arithmetic" => "1+(2-(3*(4/(5 % 6))))",
        "Comparisions" => "1 != (2 > (3 >= ( 4 < ( 5 <= (6 == (8 ?? 9) ) ) ) ) )",
        "Boolean operators" => "true || (false && true)"
        "Unary operators" => "-1 != !true"
    }
);
