mod error;
use crate::{
    frontend::{
        FileStore,
        ast::nodes::*,
        nameres::{
            resolved_nodes::*,
            scope::{Scope, VarInfo},
        },
    },
    hashmap,
    spans::{IntoSpanned, Span, Spanned},
};
pub use error::NameErr;

use std::collections::HashMap;
pub type Result<T = RNodeRef> = std::result::Result<T, Spanned<NameErr>>;
type DeclStream = Vec<Spanned<Item>>;
#[derive(Default)]
pub struct NameRes {
    ident_counter: usize,
    globals: HashMap<String, usize>,
    pub(crate) file_store: FileStore,
    node_pool: NodePool,
    max_locals: usize,
}
impl NameRes {
    pub fn new(file_store: FileStore) -> Self {
        Self {
            file_store,
            ..Default::default()
        }
    }
    pub fn resolve(&mut self, ast: DeclStream) -> Result<ResolvedAst> {
        self.add_global("print".to_owned());
        let decls = self.resolve_toplevel(ast)?;
        Ok(ResolvedAst::new(
            decls,
            self.node_pool.clone(),
            self.globals.len(),
            self.max_locals,
        ))
    }
    pub fn resolve_expr(&mut self, expr: NodeSpan) -> Result<ResolvedAstNode> {
        self.add_global("print".to_owned());
        let node = self.resolve_node(expr, &mut Scope::default())?;
        Ok(ResolvedAstNode::new(
            node,
            self.node_pool.clone(),
            self.globals.len(),
            self.max_locals,
        ))
    }
    fn add_global(&mut self, name: String) {
        if self.globals.contains_key(&name) {
            return;
        }
        let id = self.globals.len();
        self.globals.insert(name.to_string(), id);
    }
    fn add_node(&mut self, node: ResolvedNode, span: Span) -> Result {
        let idx = RNodeRef(self.node_pool.len());
        self.node_pool.push(node.to_spanned(span));
        Ok(idx)
    }
    pub fn resolve_toplevel(&mut self, decls: DeclStream) -> Result<Vec<Spanned<ResItem>>> {
        self.add_global("print".to_owned());
        for val in decls.iter() {
            match &val.item {
                Item::Decl(decl) => {
                    self.add_global(decl.name.to_owned());
                }
            };
        }
        let mut root = Scope::default();
        let mut new_decls = vec![];
        for i in decls {
            match i.item {
                Item::Decl(decl) => {
                    let expr = self.resolve_node(decl.expr.deref_item(), &mut root)?;
                    let id = &self.globals[&decl.name];

                    new_decls.push(
                        ResItem::Decl(ResolvedDecl {
                            id: *id,
                            expr,
                            is_global: true,
                        })
                        .to_spanned(i.span),
                    );
                    self.ident_counter = 0;
                }
            };
        }
        Ok(new_decls)
    }
    fn gen_name(&mut self) -> usize {
        let old_count = self.ident_counter;
        self.ident_counter += 1;
        old_count
    }
    fn get_var(
        &mut self,
        name: impl AsRef<str>,
        parent: &mut Scope,
        span: Span,
    ) -> Result<VarInfo> {
        let name = name.as_ref();

        if let Some(info) = parent.get_var(name) {
            return Ok(info);
        }
        let Some(info) = parent.get_var(name).or_else(|| {
            self.globals
                .get(name)
                .map(|id| VarInfo::new(name.to_string(), true, *id))
        }) else {
            return Err(NameErr::UndefinedVar(name.to_string()).to_spanned(span));
        };
        Ok(info)
    }
    fn resolve_var_decl(&mut self, decl: VarDecl, parent: &mut Scope) -> Result<ResolvedDecl> {
        let expr = self.resolve_node(decl.expr.deref_item(), parent)?;
        let id = self.def_local(decl.name, parent);

        Ok(ResolvedDecl {
            expr,
            id,
            is_global: false,
        })
    }
    fn resolve_list(&mut self, list: NodeStream, parent: &mut Scope) -> Result<Vec<RNodeRef>> {
        let mut new = vec![];
        for node in list {
            new.push(self.resolve_node(node, parent)?)
        }
        Ok(new)
    }
    fn def_local(&mut self, name: String, parent: &mut Scope) -> usize {
        let id = self.gen_name();
        parent.define(name.clone(), VarInfo::new(name, false, id));
        id
    }
    fn resolve_node(&mut self, node: NodeSpan, parent: &mut Scope) -> Result {
        let span = node.span;
        match node.item {
            Node::VarDecl(decl) => {
                let decl = self.resolve_var_decl(decl, parent)?;
                self.add_node(ResolvedNode::Decl(decl), span)
            }
            Node::Assignment { target, value } => {
                let target = self.resolve_node(target.deref_item(), parent)?;
                let value = self.resolve_node(value.deref_item(), parent)?;
                self.add_node(ResolvedNode::Assignment { target, value }, span)
            }
            Node::Variable(name) => {
                let info = self.get_var(name, parent, span)?;

                self.add_node(
                    ResolvedNode::Variable {
                        id: info.id,
                        is_global: info.global,
                    },
                    span,
                )
            }

            Node::FuncDef(func) => {
                let mut func_scope = Scope::default();
                let mut args = vec![];
                for arg in func.args {
                    let new_name = self.def_local(arg, &mut func_scope);
                    args.push(new_name);
                }
                let old_max = self.max_locals;
                self.max_locals = 0;
                let block = self.resolve_block_with(func.block, parent, func_scope)?;
                let max_locals = self.max_locals;
                self.max_locals = old_max;
                let resolved = ResolvedNode::FuncLit(FuncLit {
                    idents: args,
                    block,
                    captures: func.captures,
                    local_count: max_locals,
                });
                self.add_node(resolved, span)
            }
            Node::While(node) => {
                let condition = self.resolve_node(node.condition.deref_item(), parent)?;
                let block = self.resolve_block(node.proc, parent)?;
                self.add_node(ResolvedNode::While { block, condition }, span)
            }
            Node::Loop(block) => {
                let block = self.resolve_block(block, parent)?;
                self.add_node(ResolvedNode::Loop(block), span)
            }
            Node::Branch(branch) => {
                let condition = self.resolve_node(branch.condition.deref_item(), parent)?;

                let if_block = self.resolve_block(branch.if_block, parent)?;
                let else_block = if let Some(else_block) = branch.else_block {
                    Some(self.resolve_block(else_block, parent)?)
                } else {
                    None
                };
                self.add_node(
                    ResolvedNode::Branch {
                        condition,
                        if_block,
                        else_block,
                    },
                    span,
                )
            }

            Node::DoBlock(block) => {
                let block = self.resolve_block(block, parent)?;
                self.add_node(ResolvedNode::DoBlock(block), span)
            }
            Node::ReturnNode(node) => {
                let node = self.resolve_node(node.deref_item(), parent)?;
                self.add_node(ResolvedNode::ReturnNode(node), span)
            }
            Node::ResultNode(node) => {
                let node = self.resolve_node(node.deref_item(), parent)?;
                self.add_node(ResolvedNode::ResultNode(node), span)
            }

            Node::BinaryNode(bin) => {
                let left = self.resolve_node(bin.left.deref_item(), parent)?;
                let right = self.resolve_node(bin.right.deref_item(), parent)?;
                self.add_node(
                    ResolvedNode::BinaryNode {
                        left,
                        right,
                        kind: bin.kind,
                    },
                    span,
                )
            }
            Node::ForLoop(forloop) => {
                let mut base = Scope::default();
                let loop_var = self.def_local(forloop.ident, &mut base);

                let list = self.resolve_node(forloop.list.deref_item(), parent)?;

                let block = self.resolve_block_with(forloop.proc, parent, base)?;
                self.add_node(
                    ResolvedNode::ForLoop {
                        loop_var,
                        list,
                        block,
                    },
                    span,
                )
            }
            Node::Index { target, index } => {
                let target = self.resolve_node(target.deref_item(), parent)?;
                let index = self.resolve_node(index.deref_item(), parent)?;
                self.add_node(ResolvedNode::Index { target, index }, span)
            }
            Node::StructDef(obj) => {
                let mut buf = hashmap!();
                for (name, node) in obj {
                    let node = self.resolve_node(node, parent)?;
                    buf.insert(name, node);
                }
                self.add_node(ResolvedNode::StructDef(buf), span)
            }
            Node::Call(call) => {
                let callee = self.resolve_node(call.callee.deref_item(), parent)?;
                let mut args = vec![];
                for node in call.args {
                    args.push(self.resolve_node(node, parent)?);
                }
                self.add_node(ResolvedNode::Call { args, callee }, span)
            }
            Node::FieldAccess(field) => {
                let target = self.resolve_node(field.target.deref_item(), parent)?;
                let requested = match field.requested.item {
                    AccessType::Method {
                        args,
                        callee,
                        callee_span,
                    } => ResAccessType::Method {
                        callee,
                        callee_span,
                        args: self.resolve_list(args, parent)?,
                    },
                    AccessType::Property(prop) => ResAccessType::Property(prop),
                }
                .to_spanned(field.requested.span);
                self.add_node(ResolvedNode::FieldAccess(target, requested), span)
            }

            Node::RecordLit(obj) => {
                let mut buf = hashmap!();
                for (name, node) in obj {
                    let node = self.resolve_node(node, parent)?;
                    buf.insert(name, node);
                }
                self.add_node(ResolvedNode::RecordLit(buf), span)
            }
            Node::ListLit(list) => {
                let list = self.resolve_list(list, parent)?;
                self.add_node(ResolvedNode::ListLit(list), span)
            }
            Node::UnaryNode(un) => {
                let target = self.resolve_node(un.target.deref_item(), parent)?;
                self.add_node(ResolvedNode::UnaryNode(un.kind, target), span)
            }
            Node::Constructor(con) => {
                let target = self.resolve_node(con.target.deref_item(), parent)?;
                let mut params = HashMap::new();
                for (name, value) in con.params {
                    let value = self.resolve_node(value, parent)?;
                    params.insert(name, value);
                }
                self.add_node(ResolvedNode::Constructor { target, params }, span)
            }

            Node::ContinueNode => self.add_node(ResolvedNode::ContinueNode, span),
            Node::BreakNode => self.add_node(ResolvedNode::BreakNode, span),
            Node::Bool(v) => self.add_node(ResolvedNode::Bool(v), span),
            Node::Float(v) => self.add_node(ResolvedNode::Float(v), span),
            Node::Null => self.add_node(ResolvedNode::Null, span),
            Node::Str(v) => self.add_node(ResolvedNode::Str(v), span),
            Node::Int(v) => self.add_node(ResolvedNode::Int(v), span),
            Node::DontResult => Err(NameErr::UnexpectedSemi.to_spanned(span)),
        }
    }
    fn resolve_block_with(
        &mut self,
        ast: NodeStream,
        parent: &mut Scope,
        mut base: Scope,
    ) -> Result<Vec<RNodeRef>> {
        base.parent = Some(Box::new(parent.clone()));
        let mut buffer: Vec<RNodeRef> = vec![];
        let base_locals = self.ident_counter;
        for node in ast {
            let resolved = self.resolve_node(node, &mut base)?;
            buffer.push(resolved);
        }
        self.max_locals = self.max_locals.max(self.ident_counter);
        self.ident_counter = base_locals;
        let Some(mod_parent) = base.parent else {
            unimplemented!("Parent should always exist");
        };
        *parent = *mod_parent;
        Ok(buffer)
    }
    fn resolve_block(&mut self, ast: NodeStream, parent: &mut Scope) -> Result<Vec<RNodeRef>> {
        self.resolve_block_with(ast, parent, Scope::default())
    }
}
