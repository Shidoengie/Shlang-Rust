use crate::{
    frontend::{
        Parser,
        nameres::{error::NameErr, resolver::NameRes},
        nodes::{Node, NodeStream},
    },
    spans::{IntoSpanned, Span, Spanned},
    test_func,
};

fn test_nameres(body: &str) -> Result<NodeStream, Spanned<NameErr>> {
    let mut parser = Parser::new(body);
    let ast = parser.parse().unwrap();
    NameRes::resolve(ast)
}

test_func!(test_nameres,(
    simple_vars: "var a = 1; var a = 1; var a = 1;"
    var_usage:"var a = 1; var b = a;"
    undefined_var:"var a = c;"

    scoping:"var a = 10; do {var a = a;}"
    var_used_outside_scope:"do {var a = 10;}; var b = a;"
));
