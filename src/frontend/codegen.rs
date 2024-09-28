use crate::frontend::nodes::*;
use crate::frontend::parser::*;
use crate::frontend::stacknodes::StackOp as Op;
use crate::frontend::stacknodes::*;
pub struct IRgen {
    stack: Stack,
    idents: Vec<String>,
}
impl IRgen {
    pub fn new() -> Self {
        Self {
            stack: vec![],
            idents: vec![],
        }
    }
    pub fn binary_to_op(&self, kind: BinaryOp) -> MathOp {
        match kind {
            BinaryOp::Add => MathOp::Add,
            BinaryOp::Subtract => MathOp::Subtract,
            BinaryOp::Divide => MathOp::Divide,
            BinaryOp::Multiply => MathOp::Multiply,
            BinaryOp::Modulo => MathOp::Modulo,
            BinaryOp::And => MathOp::And,
            BinaryOp::OR => MathOp::Or,
            BinaryOp::IsEqual => MathOp::IsEqual,
            BinaryOp::IsDifferent => MathOp::IsDifferent,
            BinaryOp::Greater => MathOp::Greater,
            BinaryOp::Lesser => MathOp::Lesser,
            BinaryOp::GreaterOrEqual => MathOp::GreaterOrEqual,
            BinaryOp::LesserOrEqual => MathOp::LesserOrEqual,
            BinaryOp::NullCoalescing => MathOp::NullCoalescing,
        }
    }
    pub fn unary_to_op(&self, kind: UnaryOp) -> MathOp {
        match kind {
            UnaryOp::Negate => MathOp::Subtract,
            UnaryOp::Not => MathOp::Not,
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
    fn node_gen(&mut self, node: NodeSpan) {
        match node.item {
            Node::Literal(val) => self.stack.push(Op::Push(val)),
            Node::BinaryNode(node) => {
                self.node_gen(node.left.deref_item());
                self.node_gen(node.right.deref_item());
                self.stack
                    .push(StackOp::MathOp(self.binary_to_op(node.kind)));
            }
            Node::UnaryNode(node) => {
                self.node_gen(node.target.deref_item());

                self.stack
                    .push(StackOp::MathOp(self.unary_to_op(node.kind)));
            }
            Node::Declaration(name, expr) => {
                self.node_gen(expr.deref_item());
                self.idents.push(name);
            }
            Node::Assignment(name, expr) => {
                let index = self
                    .idents
                    .binary_search(&name)
                    .expect("Non Existent variable dumbasss");
                self.node_gen(*expr);
                self.stack.push(StackOp::Store(index));
            }
            Node::Variable(name) => {
                let index = self
                    .idents
                    .binary_search(&name)
                    .expect("Non Existent variable dumbasss");
                self.stack.push(Op::Load(index))
            }
            _ => {
                todo!()
            }
        }
    }
}
