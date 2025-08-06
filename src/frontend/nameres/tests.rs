use std::{any::Any, collections::HashMap, fmt::Debug};

use crate::{
    frontend::{
        Parser,
        ast::{DeclType, Node, NodeSpan, NodeStream},
        nameres::{error::NameErr, resolver::NameRes, scope::Scope},
    },
    hashmap,
    spans::{IntoSpanned, Span, Spanned},
    test_func,
};

fn test_nameres(body: &str) -> Result<NodeSpan, Spanned<NameErr>> {
    let input = format!("do {{ {body} }}");
    let mut parser = Parser::new(&input);
    let ast = parser.parse_expr(false).unwrap();
    NameRes::default().resolve_node(ast, &mut Scope::default())
}
fn test_global_nameres(body: &str) -> Result<Vec<Spanned<DeclType>>, Spanned<NameErr>> {
    let mut parser = Parser::new(body);
    let ast = parser.parse().unwrap();
    NameRes::resolve(ast)
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
    "#
    }
);
