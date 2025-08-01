use std::{
    collections::HashMap,
    fmt::Display,
    num::{NonZeroU32, NonZeroU64},
};

use crate::{
    frontend::{
        ast::{self, *},
        nameres::{
            error::NameErr,
            scope::{Scope, VarInfo},
        },
    },
    hashmap,
    spans::{IntoSpanned, Spanned},
};
type Res<T = NodeSpan> = Result<T, Spanned<NameErr>>;

#[derive(Debug, Default)]
pub struct NameRes {
    name_count: HashMap<String, NonZeroU32>,
}
impl NameRes {
    pub fn resolve(ast: NodeStream) -> Res<NodeStream> {
        let mut resolver = Self::default();
        resolver.resolve_block(ast, &mut Scope::default())
    }
    fn gen_name(&mut self, name: impl AsRef<str>) -> String {
        let name = name.as_ref();
        let Some(count) = self.name_count.get_mut(name) else {
            self.name_count
                .insert(name.to_owned(), NonZeroU32::new(1).unwrap());
            return format!("{name}@0");
        };
        let old_count = *count;
        let new_count = count.saturating_add(1);
        *count = new_count;
        return format!("{name}@{old_count}");
    }
    fn resolve_node(&mut self, node: NodeSpan, parent: &mut Scope) -> Res {
        let span = node.span;
        match node.item {
            Node::Declaration(name, value) => {
                let new_name = self.gen_name(&name);
                let value = self.resolve_node(value.deref_item(), parent)?;
                parent.define(name.clone(), VarInfo::new(&new_name));
                return Ok(Node::Declaration(new_name, value.box_item()).to_spanned(span));
            }
            Node::While(node) => {
                let condition = self
                    .resolve_node(node.condition.deref_item(), parent)?
                    .box_item();
                let proc = self.resolve_block(node.proc, parent)?;
                return Ok(ast::While { condition, proc }.to_nodespan(span));
            }
            Node::Loop(block) => {
                let block = self.resolve_block(block, parent)?;
                return Ok(Node::Loop(block).to_spanned(span));
            }
            Node::Branch(branch) => {
                let condition = self
                    .resolve_node(branch.condition.deref_item(), parent)?
                    .box_item();
                let if_block = self.resolve_block(branch.if_block, parent)?;
                let else_block = if let Some(else_block) = branch.else_block {
                    Some(self.resolve_block(else_block, parent)?)
                } else {
                    None
                };
                return Ok(ast::Branch {
                    condition,
                    else_block,
                    if_block,
                }
                .to_nodespan(span));
            }
            Node::Variable(name) => {
                let Some(info) = parent.get_var(&name) else {
                    return Err(NameErr::UndefinedVar(name).to_spanned(span));
                };
                return Ok(Node::Variable(info.name).to_spanned(span));
            }

            Node::DoBlock(block) => {
                let block = self.resolve_block(block, parent)?;
                Ok(Node::DoBlock(block).to_spanned(span))
            }
            Node::ResultNode(node) => {
                return Ok(Node::ResultNode(
                    self.resolve_node(node.deref_item(), parent)?.box_item(),
                )
                .to_spanned(span));
            }
            Node::BinaryNode(bin) => {
                let left = self.resolve_node(bin.left.deref_item(), parent)?;
                let right = self.resolve_node(bin.right.deref_item(), parent)?;
                return Ok(BinaryNode {
                    left: left.box_item(),
                    right: right.box_item(),
                    kind: bin.kind,
                }
                .to_nodespan(span));
            }
            Node::StructDef(obj) => {
                let mut buf: HashMap<String, NodeSpan> = hashmap!();
                for (name, node) in obj {
                    let node = self.resolve_node(node, parent)?;
                    buf.insert(name, node);
                }
                Ok(Node::StructDef(buf).to_spanned(span))
            }
            Node::RecordLit(obj) => {
                let mut buf: HashMap<String, NodeSpan> = hashmap!();
                for (name, node) in obj {
                    let node = self.resolve_node(node, parent)?;
                    buf.insert(name, node);
                }
                Ok(Node::RecordLit(buf).to_spanned(span))
            }
            node => return Ok(node.to_spanned(span)),
        }
    }
    fn resolve_block(&mut self, ast: NodeStream, parent: &mut Scope) -> Res<NodeStream> {
        let mut new_scope = Scope::new_child_in(parent.clone());
        let mut buffer: NodeStream = vec![];
        for node in ast {
            let resolved = self.resolve_node(node, &mut new_scope)?;
            buffer.push(resolved);
        }
        let Some(mod_parent) = new_scope.parent else {
            unimplemented!("Parent should always exist");
        };
        *parent = *mod_parent;
        return Ok(buffer);
    }
}
