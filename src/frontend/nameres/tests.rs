

use crate::{
    frontend::{
        FileStore,
        ast::parser::Parser,
        nameres::{
            resolved_nodes::{ResolvedAst, ResolvedAstNode},
            resolver::{self, NameRes},
        },
    },
    test_func,
};

fn test_nameres(body: &str) -> resolver::Result<ResolvedAstNode> {
    let input = format!("do {{ {body} }}");
    let mut file_store = FileStore::new();
    let file_id = file_store.add(input.clone());
    let ast = Parser::parse_expr(&input, file_id).unwrap();
    NameRes::new(file_store).resolve_expr(ast)
}
fn test_global_nameres(body: &str) -> resolver::Result<ResolvedAst> {
    let mut file_store = FileStore::new();
    let file_id = file_store.add(body.to_owned());
    let ast = Parser::parse(body, file_id).unwrap();
    NameRes::new(file_store).resolve(ast)
}

test_func!(
    test_locals , test_nameres, {
        "Variable overloading"=> "var a = 1; var a = 1; var a = 1; var b = a;",
        "Variable usage" => "var a = 1; var b = a;",
        "Undefined variable usage"=> "var a = c;",
        "Scopes"=>"var a = 10; do {var a = a;}",
        "Variable used outside scope"=>"do {var a = 10;}; var b = a;",
        "Loops"=>"for i in [1,2,3] {var b = i}",
        "Loop var out of scope"=>"for i in [1,2,3] {var b = i}; var d = i;",
        "Functions" => r#"
            var b = 10; 
            func a(c) {
                var d = b;
            }
        "#
    },
    test_globals,test_global_nameres, {
        "global_fns"=> r#"
    var b = a();
    func a() {
        return 10;
    } 
    "#,
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
