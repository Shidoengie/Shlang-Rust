use crate::AstNodes;
use crate::Parser;
use std::io;
use std::io::Write;
use std::*;
use AstNodes::*;
use insta::*;
fn parse_text(source: &str) -> Node {
    let mut parser = Parser::new(source);
    return parser.batch_parse();
}
macro_rules! create_function {
    ($($name:ident : $input:expr)*) => {
        $(
        #[test]
        fn $name () {
            // Perform your desired operation using the input parameter
            assert_debug_snapshot!(parse_text($input));
        }
        )*
    };
}
create_function!(
declare_and_assign_expression:"var a = 1+2+b;" 
single_var_decl:"var a;"
multi_var_decl:"var a; var a;"
var_declare_and_assign_value:"var a = 2;"
parse_nested_paren:"var a = ((1+2)+(1+2));"
parse_paren:"var a = (1+2+b);"
expression_operators:"var a = 1*2+2-2/2%5 and 10 or 10!=1==2<4<=3>0>=3;"
unary_operator:"var a = !true;"
multiple_unary:"var a = !true + !true;"
unary_operator_paren:"var a = !(0);"
unary_operators:"var a = !0 +(+0)+(-0);"
);
