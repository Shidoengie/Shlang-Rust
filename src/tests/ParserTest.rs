use crate::AstNodes;
use crate::Parser;
use insta::*;
use std::io;
use std::io::Write;
use std::*;
use AstNodes::*;
fn parse_full(source: &str) -> Block {
    let mut parser = Parser::new(source);
    return parser.batch_parse();
}
fn parse_expr(source: &str) -> Node {
    let mut parser = Parser::new(source);
    return parser.parse_expr();
}
macro_rules! test_func {
    ($func:expr,($($name:ident : $input:expr)*)) => {
        $(
        #[test]
        fn $name () {
            // Perform your desired operation using the input parameter
            assert_debug_snapshot!($func($input));
        }
        )*
    };
}
test_func!(parse_full,(
declare_and_assign_expression:"var a = 1+2+b;"
single_var_decl:"var a;"
multi_var_decl:"var a; var a;"
var_declare_and_assign_value:"var a = 2;"
));
test_func!(parse_expr,(
parse_nested_paren:"((1+2)+(1+2));"
parse_paren:"(1+2+b);"
expression_operators:"1*2+2-2/2%5 and 10 or 10!=1==2<4<=3>0>=3;"
unary_operator:"!true;"
multiple_unary:"!true + !true;"
unary_operator_paren:"!(0);"
unary_operators:"!0 +(+0)+(-0);"
empty_call:"a();"
call:"a((1+2),true,\"1\");"
call_as_val:"a(1,1,1)+1;"
call_with_paren:"a((1),1,1);"
assignement:"b = 1;"
));
