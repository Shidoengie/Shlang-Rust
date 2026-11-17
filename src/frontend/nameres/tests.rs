use crate::{
	frontend::{
		Compiler,
		nameres::resolved_nodes::{Literal, ResolvedAst, ResolvedAstNode, ResolvedNode},
	},
	lang_errors::LangResult,
};

use test_file_macro::test_file;
fn test_nameres(body: &str) -> LangResult<ResolvedAstNode<'_>> {
	let string = format!("do {{ {body} }}").leak();
	Compiler::new().resolve_expr(string)
}
fn test_global_nameres(body: &str) -> LangResult<ResolvedAst> {
	Compiler::new().resolve(body)
}

test_file!(
	test_locals,
	test_nameres,
	"src/frontend/nameres/locals.shlang",
);

test_file!(
	test_globals,
	test_global_nameres,
	"src/frontend/nameres/tests.shlang",
);

#[test]
fn anonymous_class_self_type_resolves_to_its_local_slot() {
	let mut compiler = Compiler::new();
	let resolved = compiler
		.resolve_expr("do { class { static func get() { return Self; } } }")
		.unwrap();
	let ResolvedNode::DoBlock(block) = &resolved.node.item else {
		panic!("expected a do block");
	};
	let ResolvedNode::Result(class_node) = &block[0].item else {
		panic!("expected the block result");
	};
	let ResolvedNode::Literal(Literal::ClassLit(class)) = class_node.item.as_ref() else {
		panic!("expected a class literal");
	};
	assert_eq!(class.id, 0);
	assert!(!class.is_global);
	assert_eq!(resolved.local_count, 1);

	let method = class.methods.values().next().unwrap();
	let ResolvedNode::Return(value) = &method.block[0].item else {
		panic!("expected a return");
	};
	assert!(matches!(
		value.item.as_ref(),
		ResolvedNode::Variable {
			id: 0,
			is_global: false
		}
	));
}

#[test]
fn named_global_class_self_type_resolves_to_its_global_slot() {
	let mut compiler = Compiler::new();
	let resolved = compiler
		.resolve("class Example { static func get() { return Self; } }")
		.unwrap();
	let ResolvedNode::Literal(Literal::ClassLit(class)) = &resolved.proc[0].item else {
		panic!("expected a class literal");
	};
	assert_eq!(class.id, 4);
	assert!(class.is_global);
	assert_eq!(resolved.global_count, 5);

	let method = class.methods.values().next().unwrap();
	let ResolvedNode::Return(value) = &method.block[0].item else {
		panic!("expected a return");
	};
	assert!(matches!(
		value.item.as_ref(),
		ResolvedNode::Variable {
			id: 4,
			is_global: true
		}
	));
}

#[test]
fn self_type_outside_class_is_rejected() {
	let mut compiler = Compiler::new();
	assert!(compiler.resolve_expr("Self").is_err());
}
