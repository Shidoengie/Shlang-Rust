use crate::frontend::nodes::*;
use crate::frontend::parser::*;
use crate::frontend::stacknodes::StackOp as Op;
use crate::frontend::stacknodes::*;
use crate::spans::Spanned;
use crate::spans::*;
pub struct IRgen {
    pub stack: Vec<Spanned<Op>>,
    idents: Vec<String>,
}
impl IRgen {
    pub fn new() -> Self {
        Self {
            stack: vec![],
            idents: vec![],
        }
    }
    pub fn generate(&mut self, input: &str) {
        let mut parser = Parser::new(input.clone());

        let (prog, _) = parser.parse().unwrap();

        println!("{prog:?}");
        for node in prog {
            self.node_gen(node);
        }
    }
    fn push_val(&mut self, val: Value, span: impl SpanUtil) {
        self.stack
            .push(StackOp::Push(val).to_spanned(span.take_span()));
    }
    fn node_gen(&mut self, node: NodeSpan) {
        let span = node.span;
        match node.item {
            Node::Float(num) => self.push_val(Value::Float(num), span),
            Node::Int(num) => self.push_val(Value::Int(num), span),
            Node::Bool(cond) => self.push_val(Value::Bool(cond), span),
            Node::Str(txt) => self.push_val(Value::Str(txt), span),
            Node::BinaryNode(expr) => {
                self.node_gen(expr.left.deref_item());
                self.node_gen(expr.right.deref_item());
                let op = StackOp::BinaryOp(expr.kind).to_spanned(span);
                self.stack.push(op);
            }
            Node::UnaryNode(expr) => {
                self.node_gen(expr.target.deref_item());

                self.stack
                    .push(StackOp::UnaryOp(expr.kind).to_spanned(span));
            }
            Node::Declaration(name, expr) => {
                self.node_gen(expr.deref_item());
                self.idents.push(name);
            }
            Node::Assignment(request) => {
                let expr = request.value.deref_item();
                match request.target.deref_item().item {
                    Node::Variable(name) => {
                        let index = self
                            .idents
                            .binary_search(&name)
                            .expect("Non Existent variable dumbasss");
                        self.node_gen(expr);
                        self.stack.push(StackOp::Store(index).to_spanned(span));
                    }
                    _ => todo!(),
                };
            }
            Node::Variable(name) => {
                let index = self
                    .idents
                    .binary_search(&name)
                    .expect("Non Existent variable dumbasss");
                self.stack.push(Op::Load(index).to_spanned(span))
            }
            _ => {
                todo!()
            }
        }
    }
}
