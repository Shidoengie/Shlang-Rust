use crate::AstNodes;
use crate::Parser;
use std::io;
use std::io::Write;
use std::*;
use AstNodes::*;
use insta::*;
fn num(val: f64) -> Node {
    Node::from(Value::Num(val))
}
fn boxed_num(val: f64) -> Box<Node> {
    Box::new(Node::from(Value::Num(val)))
}
fn none() -> Node {
    Node::from(Value::NoneType)
}
fn boxed_none() -> Box<Node> {
    Box::new(Node::from(Value::NoneType))
}
#[test]
fn single_var_decl() {
    let test_block = vec![Node::declaration("a", none())];
    assert_eq!(parse_text("var a;"), Node::block(test_block));
}
#[test]
pub fn multi_var_decl() {
    let test_block = vec![
        Node::declaration("a", none()),
        Node::declaration("a", none()),
    ];
    assert_eq!(parse_text("var a; var a;"), Node::block(test_block));
}
#[test]
pub fn var_declare_and_assign_value() {
    let test_block = vec![Node::declaration("a", num(2.0))];

    assert_eq!(parse_text("var a = 2;"), Node::block(test_block));
}
#[test]
pub fn insta_declare_and_assign_expression() {
    assert_debug_snapshot!(parse_text("var a = 1+2+b;"));
}
#[test]
pub fn expression_operators(){
    assert_debug_snapshot!(parse_text("var a = 1*2+2-2/2%5 and 10 or 10!=1==2<4<=3>0>=3;"));
}
#[test]
pub fn parse_paren(){
    assert_debug_snapshot!(parse_text("var a = (1+2+b);"));
}
fn parse_file(path: &str) -> Node {
    let test_file_contents =
        fs::read_to_string(path).expect("LogRocket: Should have been able to read the file");
    let mut parser = Parser::new(test_file_contents.as_str());
    return parser.batch_parse();
}
fn parse_text(source: &str) -> Node {
    let mut parser = Parser::new(source);
    return parser.batch_parse();
}
