use super::nodes::*;
use super::parser::{self, *};
use crate::collections::FileId;
use crate::test_file;
use std::*;

fn parse_expr(source: &str) -> parser::Result<Program<'_>> {
	Parser::parse(source, FileId::ANON)
}

test_file!(
	test_parser_expressions,
	parse_expr,
	"src/frontend/ast/tests/expressions.shlang"
);
test_file!(
	test_parser_calls,
	parse_expr,
	"src/frontend/ast/tests/calls.shlang"
);
test_file!(
	test_parser_variables,
	parse_expr,
	"src/frontend/ast/tests/variables.shlang"
);
test_file!(
	test_parser_functions,
	parse_expr,
	"src/frontend/ast/tests/functions.shlang"
);
test_file!(
	test_parser_access,
	parse_expr,
	"src/frontend/ast/tests/access.shlang"
);
test_file!(
	test_parser_structs,
	parse_expr,
	"src/frontend/ast/tests/structs.shlang"
);
test_file!(
	test_parser_classes,
	parse_expr,
	"src/frontend/ast/tests/classes.shlang"
);

test_file!(
	test_parser_block_expressions,
	parse_expr,
	"src/frontend/ast/tests/block-expressions.shlang"
);
