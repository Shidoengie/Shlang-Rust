use crate::{
    frontend::{
        nameres::scope::{Scope, VarInfo},
        nodes::*,
    },
    lang_errors::LangError,
    spans::IntoSpanned,
};
type Res<T = NodeSpan> = Result<T, NameErr>;
pub enum NameErr {
    Unspecified(String),
}
impl LangError for NameErr {
    fn msg(&self) -> String {
        match self {
            Self::Unspecified(msg) => msg.to_string(),
        }
    }
}

#[derive(Debug, Default)]
pub struct NameRes {}
impl NameRes {
    fn resolve_node(&mut self, node: &NodeSpan, parent: &mut Scope) -> Res {
        let span = node.span;
        match &node.item {
            Node::Declaration(name, _) => {
                parent.define(name, VarInfo::new(name));
            }
            Node::BinaryNode(bin) => {
                let left = self.resolve_node(&bin.left.clone().deref_item(), parent)?;
                let right = self.resolve_node(&bin.right.clone().deref_item(), parent)?;
                return Ok(BinaryNode {
                    left: left.box_item(),
                    right: right.box_item(),
                    kind: bin.kind.clone(),
                }
                .to_nodespan(span));
            }
            node => return Ok(node.clone().to_spanned(span)),
        }
        todo!()
    }
    fn resolve_block(&mut self, ast: NodeStream, parent: &mut Scope) -> Res<NodeStream> {
        let mut new_scope = Scope::new_child_in(parent.clone());
        for node in ast.iter() {
            self.resolve_node(&node, &mut new_scope)?;
        }
        let Some(mod_parent) = new_scope.parent else {
            unimplemented!("Parent should always exist");
        };
        *parent = *mod_parent;
        todo!()
    }
}
