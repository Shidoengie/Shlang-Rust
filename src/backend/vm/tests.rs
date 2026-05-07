use crate::{
    backend::{
        Runtime,
        instructions::*,
        vm::{self, StackVM},
    },
    test_func,
};

pub fn run_expr(input: &str) -> vm::Result<Vec<Value>> {
    let bytecode = Runtime::default().assemble_expr(input).unwrap();
    let mut vm = StackVM::new(
        bytecode.ops,
        bytecode.global_count,
        bytecode.local_count,
        bytecode.globals,
    );
    vm.exec()?;
    Ok(vm.values)
}
test_func!(
    vm_expr, run_expr , {
        "Simple addition" => "1+2+3",
        "Div" => "1/2",
        "Eq" => "1.0/2.0 == 0.5",
        "NotEq" => "1/2 != 0.5",

        // --- Basic Arithmetic ---
        "Subtraction int" => "10 - 4",
        "Subtraction float" => "5.5 - 1.25",
        "Multiplication int" => "5 * 3",
        "Multiplication float" => "2.5 * 2.0",
        "Division int" => "10 / 3", // Should be integer division
        "Division float" => "5.0 / 2.0",
        "Modulo" => "10 % 3",

        // --- Unary Operations ---
        "Negation int" => "-10",
        "Negation float" => "-3.14",
        "Logical not true" => "not true",
        "Logical not false" => "not false",

        // --- Operator Precedence and Grouping ---
        "Precedence add mult" => "2 + 3 * 4",      // Should be 14
        "Precedence with parens" => "(2 + 3) * 4", // Should be 20
        "Precedence with sub and div" => "10 - 6 / 2", // Should be 7
        "Complex precedence" => "-(5 + 2) * 3",     // Should be -21

        // --- Comparison Operations ---
        "Greater than true" => "5 > 3",
        "Greater than false" => "3 > 5",
        "Lesser than true" => "3 < 5",
        "Lesser than false" => "5 < 3",
        "Greater or equal true (equal)" => "5 >= 5",
        "Greater or equal true (greater)" => "6 >= 5",
        "Greater or equal false" => "4 >= 5",
        "Lesser or equal true (equal)" => "5 <= 5",
        "Lesser or equal true (lesser)" => "4 <= 5",
        "Lesser or equal false" => "6 <= 5",

        // --- Equality and Inequality ---
        "Equality int" => "42 == 42",
        "Equality float" => "3.14 == 3.14",
        "Equality string" => "\"hello\" == \"hello\"",
        "Equality bool" => "true == true",
        "Equality null" => "null == null",
        "Inequality different types" => "10 == null", // Should be false
        "Inequality int" => "10 != 5",
        "Inequality string" => "\"hello\" != \"world\"",
        "Inequality bool" => "true != false",
        "Logical and true" => "true and true",
        "Logical and false left" => "false and true",
        "Logical and false right" => "true and false",
        "Logical or true left" => "true or false",
        "Logical or true right" => "false or true",
        "Logical or false" => "false or false",
        "Complex logical expr" => "(true or false) and (not false)",

        "NullCoalescing with null" => "null ?? 10",
        "NullCoalescing with number" => "5 ?? 10",
        "NullCoalescing with string" => "\"hello\" ?? \"world\"",
        "NullCoalescing with false" => "false ?? true", // false is not null
        "Chained NullCoalescing" => "null ?? null ?? 20"
    }
);
