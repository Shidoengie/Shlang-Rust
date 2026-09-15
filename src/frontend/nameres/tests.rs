use crate::{
	frontend::{
		Compiler,
		nameres::resolved_nodes::{ResolvedAst, ResolvedAstNode},
	},
	lang_errors::LangResult,
	test_file,
};

fn test_nameres(body: &str) -> LangResult<ResolvedAstNode> {
	Compiler::new().resolve_expr(body)
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
