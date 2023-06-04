use crate::Parser;
use crate::AstNodes;
use AstNodes::*;
use std::io;
use std::io::Write;
use std::*;
#[test]
fn single_var_decl(){

    let test_block = vec![Node::declaration("a".to_string(), Value::NoneType.as_node())];
    assert_eq!(parse_text("var a;"),Node::block(test_block));
}
#[test]
pub fn multi_var_decl() {
    let test_block = vec![Node::declaration("a".to_string(), Value::NoneType.as_node()),Node::declaration("a".to_string(), Value::NoneType.as_node())];
    assert_eq!(parse_text("var a; var a;"),Node::block(test_block));
}
fn parse_file(path:&str)->Node{
    let test_file_contents = fs::read_to_string(path)
        .expect("LogRocket: Should have been able to read the file");
    let mut parser = Parser::new(test_file_contents.as_str());
    return parser.batch_parse();
}
fn parse_text(source:&str)->Node{
    let mut parser = Parser::new(source);
    return parser.batch_parse();

}
