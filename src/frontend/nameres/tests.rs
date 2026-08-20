use crate::{
	frontend::{
		Compiler, FileStore,
		ast::parser::Parser,
		nameres::{
			resolved_nodes::{ResolvedAst, ResolvedAstNode},
			resolver::{self, NameRes},
		},
	},
	lang_errors::LangResult,
	test_func,
};

fn test_nameres(body: &str) -> LangResult<ResolvedAstNode> {
	Compiler::new().resolve_expr(body)
}
fn test_global_nameres(body: &str) -> LangResult<ResolvedAst> {
	Compiler::new().resolve(body)
}

test_func!(
	test_locals , test_nameres, {
		"Variable overloading"=> {
			var a = 1;
			var a = 1;
			var a = 1;
			var b = a;
		},
		"Variable usage" => {
			var a = 1;
			var b = a;
		},
		"Undefined variable usage"=> {var a = c;},
		"Scopes"=> {
			var a = 10;
			do {
				var a = a;
			}
		},
		"Variable used outside scope"=>{
			do {
				var a = 10;
			};
			var b = a;
		},
		"Loops"=>{
			for i in [1,2,3] {
				var b = i
			}
		},
		"Loop var out of scope"=>{
			for i in [1,2,3] {
				var b = i
			};
			var d = i;
		},
		"Functions" => {
			var b = 10;
			func a(c) {
				var d = b;
			}
		},
		"ids" => {
			var a = 10;
			do {
				var b = 20;
				var c = 30;
				var d = 123;
			}
			var c = 10;
			do {
				var b = 20;
				var c = 30;
				var d = 123;
			}
		}

	},
	test_globals,test_global_nameres, {
		"global_fns"=> {
	var b = a();
	func a() {
		return 10;
	}
	},
	"with_scopes" => r#"
        func main(){
            var a = 10;
            do {
                var a = a;
            }
        }
    
    "#,
	}
);
