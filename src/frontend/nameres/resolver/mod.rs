mod error;
use crate::{
    frontend::{
        ast::nodes::{self, *},
        nameres::scope::{Scope, VarInfo},
    },
    hashmap,
    spans::{IntoSpanned, Span, Spanned},
};
pub use error::NameErr;
use std::{collections::HashMap, num::NonZeroU32};
pub type Result<T = NodeSpan> = std::result::Result<T, Spanned<NameErr>>;
type DeclStream = Vec<Spanned<DeclType>>;
#[derive(Debug, Default)]
pub struct NameRes {
    name_count: HashMap<String, NonZeroU32>,
    globals: HashMap<String, usize>,
}
impl NameRes {
    pub fn resolve(ast: DeclStream) -> Result<DeclStream> {
        let mut globals = HashMap::<String, usize>::new();
        for (index, val) in ast.iter().enumerate() {
            match &val.item {
                DeclType::VarDecl(decl) => globals.insert(decl.name.to_string(), index),
            };
        }
        let mut resolver = Self {
            globals,
            ..Default::default()
        };
        resolver.resolve_toplevel(ast)
    }
    pub fn resolve_expr(expr: NodeSpan) -> Result<NodeSpan> {
        Self::default().resolve_node(expr, &mut Scope::default())
    }
    fn resolve_toplevel(&mut self, decls: DeclStream) -> Result<DeclStream> {
        let mut root = Scope::default();
        let mut new_decls = vec![];
        for i in decls {
            match i.item {
                DeclType::VarDecl(mut decl) => {
                    decl.expr = self
                        .resolve_node(decl.expr.deref_item(), &mut root)?
                        .box_item();
                    new_decls.push(DeclType::VarDecl(decl).to_spanned(i.span));
                }
            };
        }
        Ok(new_decls)
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
    fn get_var(
        &mut self,
        name: impl AsRef<str>,
        parent: &mut Scope,
        span: Span,
    ) -> Result<VarInfo> {
        let name = name.as_ref();

        if let Some(info) = parent.get_var(&name) {
            return Ok(info);
        }
        let Some(info) = parent.get_var(&name).or_else(|| {
            self.globals
                .get(name)
                .map(|_| return VarInfo::new(&name, true))
        }) else {
            return Err(NameErr::UndefinedVar(name.to_string()).to_spanned(span));
        };
        Ok(info)
    }
    fn resolve_var_decl(&mut self, mut decl: VarDecl, parent: &mut Scope) -> Result<VarDecl> {
        let new_name = self.gen_name(&decl.name);
        decl.expr = self
            .resolve_node(decl.expr.deref_item(), parent)?
            .box_item();
        parent.define(decl.name, VarInfo::new(&new_name, false));
        decl.name = new_name;
        return Ok(decl);
    }
    pub fn resolve_node(&mut self, node: NodeSpan, parent: &mut Scope) -> Result {
        let span = node.span;
        match node.item {
            Node::VarDecl(decl) => {
                return Ok(self.resolve_var_decl(decl, parent)?.to_nodespan(span));
            }
            Node::Assignment { target, value } => {
                let target = self.resolve_node(target.deref_item(), parent)?.box_item();
                let value = self.resolve_node(value.deref_item(), parent)?.box_item();
                return Ok(Node::Assignment { target, value }.to_spanned(span));
            }
            Node::Variable(name) => {
                let info = self.get_var(name, parent, span)?;
                return Ok(Node::Variable(info.name).to_spanned(span));
            }

            Node::FuncDef(func) => {
                let mut func_scope = Scope::default();
                let mut args = vec![];
                for arg in func.args {
                    let new_name = self.gen_name(arg.clone());
                    func_scope.define(arg, VarInfo::new(&new_name, false));
                    args.push(new_name);
                }
                let block = self.resolve_block_with(func.block, parent, func_scope)?;
                return Ok(Node::FuncDef(FuncDef {
                    block,
                    args,
                    captures: func.captures,
                })
                .to_spanned(span));
            }
            Node::While(node) => {
                let condition = self
                    .resolve_node(node.condition.deref_item(), parent)?
                    .box_item();
                let proc = self.resolve_block(node.proc, parent)?;
                return Ok(While { condition, proc }.to_nodespan(span));
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
                return Ok(nodes::Branch {
                    condition,
                    else_block,
                    if_block,
                }
                .to_nodespan(span));
            }

            Node::DoBlock(block) => {
                let block = self.resolve_block(block, parent)?;
                Ok(Node::DoBlock(block).to_spanned(span))
            }
            Node::ReturnNode(node) => {
                return Ok(Node::ReturnNode(
                    self.resolve_node(node.deref_item(), parent)?.box_item(),
                )
                .to_spanned(span));
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
            Node::ForLoop(forloop) => {
                let ident = self.gen_name(&forloop.ident);
                let mut base = Scope::default();
                base.define(
                    forloop.ident,
                    VarInfo {
                        name: ident.clone(),
                        global: false,
                    },
                );
                let list = self
                    .resolve_node(forloop.list.deref_item(), parent)?
                    .box_item();
                let proc = self.resolve_block_with(forloop.proc, parent, base)?;
                Ok(ForLoop { ident, list, proc }.to_nodespan(span))
            }
            Node::Index { target, index } => {
                let target = self.resolve_node(target.deref_item(), parent)?.box_item();
                let index = self.resolve_node(index.deref_item(), parent)?.box_item();
                return Ok(Node::Index { target, index }.to_spanned(span));
            }
            Node::StructDef(obj) => {
                let mut buf: HashMap<String, NodeSpan> = hashmap!();
                for (name, node) in obj {
                    let node = self.resolve_node(node, parent)?;
                    buf.insert(name, node);
                }
                Ok(Node::StructDef(buf).to_spanned(span))
            }
            Node::Call(call) => {
                let callee = self
                    .resolve_node(call.callee.deref_item(), parent)?
                    .box_item();
                let mut args = vec![];
                for node in call.args {
                    args.push(self.resolve_node(node, parent)?);
                }
                return Ok(Call { args, callee }.to_nodespan(span));
            }
            Node::FieldAccess(mut field) => {
                field.target = self
                    .resolve_node(field.target.deref_item(), parent)?
                    .box_item();
                return Ok(field.to_nodespan(span));
            }

            Node::RecordLit(obj) => {
                let mut buf: HashMap<String, NodeSpan> = hashmap!();
                for (name, node) in obj {
                    let node = self.resolve_node(node, parent)?;
                    buf.insert(name, node);
                }
                Ok(Node::RecordLit(buf).to_spanned(span))
            }
            Node::ListLit(list) => {
                let mut buf = vec![];
                for node in list {
                    buf.push(self.resolve_node(node, parent)?);
                }
                return Ok(Node::ListLit(buf).to_spanned(span));
            }
            Node::UnaryNode(mut un) => {
                un.target = self
                    .resolve_node(un.target.deref_item(), parent)?
                    .box_item();
                return Ok(un.to_nodespan(span));
            }
            Node::Constructor(mut con) => {
                con.name = self.get_var(&con.name, parent, span)?.name;
                let mut buf = HashMap::<String, NodeSpan>::new();
                for (name, value) in con.params {
                    let value = self.resolve_node(value, parent)?;
                    buf.insert(name, value);
                }
                con.params = buf;
                return Ok(con.to_nodespan(span));
            }
            Node::ContinueNode
            | Node::BreakNode
            | Node::Bool(_)
            | Node::Float(_)
            | Node::Null
            | Node::Str(_)
            | Node::Int(_)
            | Node::DontResult => return Ok(node), //node => return Ok(node.to_spanned(span)),
        }
    }
    fn resolve_block_with(
        &mut self,
        ast: NodeStream,
        parent: &mut Scope,
        mut base: Scope,
    ) -> Result<NodeStream> {
        base.parent = Some(Box::new(parent.clone()));
        let mut buffer: NodeStream = vec![];
        for node in ast {
            let resolved = self.resolve_node(node, &mut base)?;
            buffer.push(resolved);
        }
        let Some(mod_parent) = base.parent else {
            unimplemented!("Parent should always exist");
        };
        *parent = *mod_parent;
        return Ok(buffer);
    }
    fn resolve_block(&mut self, ast: NodeStream, parent: &mut Scope) -> Result<NodeStream> {
        return self.resolve_block_with(ast, parent, Scope::default());
    }
}
