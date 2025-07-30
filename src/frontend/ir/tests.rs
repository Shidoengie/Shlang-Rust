use crate::frontend::ir::IRgen;

use insta::*;
use std::*;

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
