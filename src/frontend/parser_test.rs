use super::nodes::*;
use super::parser::*;

use insta::*;
use std::*;

fn parse_expr(source: &str) -> ParseRes<NodeSpan> {
    let mut parser = Parser::new(source);
    parser.parse_expr()
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

test_func!(parse_expr,(
parse_nested_paren:"((1+2)+(1+2));"
parse_paren:"(1+2+b);"
expression_operators:"1*2+2-2/2%5 and 10 or 10!=1==2<4<=3>0>=3;"
unary_operator:"!true;"
multiple_unary:"!true + !true;"
unary_operator_paren:"!(0);"
unary_operators:"!0+(-0);"
empty_call:"a();"
call:"a((1+2),true,'1');"
call_as_val:"a(1,1,1)+1;"
call_with_paren:"a((1),1,1);"

assignement:"b = 1;"
single_var_expr_decl:"var a;"
var_expr_decl_with_expr_:"var a = 1+2+b;"
do_block:"do{var a = 1;1+a}"
func_expr:"func(b){1}"
func_decl_expr:"func a (b){1}"
nested_block:"func(b){do{1};}"
nested_block_with_result:"func(b){do{1}1}"
struct_access:"b.a*2"
struct_access_call:"a.b().c"
struct_body:"struct abc{};"
nested_struct:"struct abc{struct dfg{};};"
index:"a[1]"
index_with_call:"a[1]()[2]"
list_literal:"[1,2,3]"
));
