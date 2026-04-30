mod error;
use crate::{
    frontend::{
        FileStore,
        ast::nodes::{self as ast, Node as AstNode, NodeSpan},
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
type DeclStream = Vec<Spanned<ast::Item>>;
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
            globals: hashmap!(
                println => 0,
                input => 1
            ),
            ..Default::default()
        }
    }
    pub fn resolve(&mut self, ast: Vec<Spanned<ast::Node>>) -> Result<ResolvedAst> {
        let decls = self.resolve_toplevel(ast)?;
        Ok(ResolvedAst {
            proc: decls,
            pool: self.node_pool.clone(),
            global_count: self.globals.len(),
            local_count: self.max_locals,
        })
    }
    pub fn resolve_expr(&mut self, expr: ast::NodeSpan) -> Result<ResolvedAstNode> {
        let node = self.resolve_noderef(expr, &mut Scope::default())?;
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
    pub fn resolve_toplevel(&mut self, exprs: Vec<ast::NodeSpan>) -> Result<Vec<RNodeSpan>> {
        for val in exprs.iter() {
            let AstNode::Decl(decl) = &val.item else {
                continue;
            };
            if !decl.hoisted {
                continue;
            }
            let id = self.globals.len();
            self.add_global(decl.name.to_owned());
        }
        let mut root = Scope::default();
        let mut resolved = vec![];
        let mut localcount = 0usize;
        for val in exprs {
            let AstNode::Decl(decl) = &val.item else {
                let node = self.resolve_node(val, &mut root)?;
                resolved.push(node);
                continue;
            };
            if !decl.hoisted {
                localcount += 1;
            }
            let expr = self.resolve_noderef(decl.expr.deref_item(), &mut root)?;
            let id = &self.globals[&decl.name];

            resolved.push(
                Decl {
                    id: *id,
                    expr,
                    is_global: true,
                }
                .to_rnodespan(val.span),
            );
            self.ident_counter = 0;
        }
        Ok(resolved)
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
    fn resolve_var_decl(&mut self, decl: ast::Decl, parent: &mut Scope) -> Result<(Decl, usize)> {
        let (expr, localcount) = self.resolve_noderef(decl.expr.deref_item(), parent)?;
        let id = self.def_local(decl.name, parent);

        Ok((
            Decl {
                expr,
                id,
                is_global: false,
            },
            localcount + 1,
        ))
    }
    fn resolve_list(
        &mut self,
        list: Vec<Spanned<AstNode>>,
        parent: &mut Scope,
    ) -> Result<Vec<RNodeRef>> {
        let mut new = vec![];
        for node in list {
            new.push(self.resolve_noderef(node, parent)?)
        }
        Ok(new)
    }
    fn def_local(&mut self, name: String, parent: &mut Scope) -> usize {
        let id = self.gen_name();
        parent.define(name.clone(), VarInfo::new(name, false, id));
        id
    }
    fn resolve_node(
        &mut self,
        node: ast::NodeSpan,
        parent: &mut Scope,
    ) -> Result<(RNodeSpan, usize)> {
        use ResolvedNode as RNode;
        let span = node.span;
        match node.item {
            AstNode::Decl(decl) => {
                let decl = self.resolve_var_decl(decl, parent)?;
                Ok((RNode::Decl(decl.0).to_spanned(span), decl.1))
            }
            AstNode::Assignment { target, value } => {
                let target = self.resolve_noderef(target.deref_item(), parent)?;
                let value = self.resolve_noderef(value.deref_item(), parent)?;
                Ok((RNode::Assignment { target, value }.to_spanned(span), 0))
            }
            AstNode::Variable(name) => {
                let info = self.get_var(name, parent, span)?;
                Ok((
                    RNode::Variable {
                        id: info.id,
                        is_global: info.global,
                    }
                    .to_spanned(span),
                    0,
                ))
            }

            AstNode::FunctionLit(func) => {
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
                let resolved = RNode::FunctionLit(FunctionLit {
                    idents: args,
                    block,
                    captures: func.captures,
                    local_count: max_locals,
                });
                Ok((resolved.to_spanned(span), 0))
            }
            AstNode::While(node) => {
                let condition = self.resolve_noderef(node.condition.deref_item(), parent)?;
                let block = self.resolve_block(node.proc, parent)?;
                Ok((RNode::While { block, condition }.to_spanned(span), 0))
            }
            AstNode::Loop(block) => {
                let block = self.resolve_block(block, parent)?;
                Ok((RNode::Loop(block).to_spanned(span), 0))
            }
            AstNode::Branch(branch) => {
                let condition = self.resolve_noderef(branch.condition.deref_item(), parent)?;

                let if_block = self.resolve_block(branch.if_block, parent)?;
                let else_block = if let Some(else_block) = branch.else_block {
                    Some(self.resolve_block(else_block, parent)?)
                } else {
                    None
                };
                Ok((
                    Branch {
                        condition,
                        if_block,
                        else_block,
                    }
                    .to_rnodespan(span),
                    0,
                ))
            }

            AstNode::DoBlock(block) => {
                let block = self.resolve_block(block, parent)?;
                Ok((RNode::DoBlock(block).to_spanned(span), 0))
            }
            AstNode::Return(node) => {
                let node = self.resolve_noderef(node.deref_item(), parent)?;
                Ok((RNode::Return(node).to_spanned(span), 0))
            }
            AstNode::Result(node) => {
                let node = self.resolve_noderef(node.deref_item(), parent)?;
                Ok((RNode::Result(node).to_spanned(span), 0))
            }

            AstNode::BinaryNode(bin) => {
                let left = self.resolve_noderef(bin.left.deref_item(), parent)?;
                let right = self.resolve_noderef(bin.right.deref_item(), parent)?;
                Ok((
                    RNode::BinaryNode {
                        left,
                        right,
                        kind: bin.kind,
                    }
                    .to_spanned(span),
                    0,
                ))
            }
            AstNode::ForLoop(forloop) => {
                let mut base = Scope::default();
                let loop_var = self.def_local(forloop.ident, &mut base);

                let list = self.resolve_noderef(forloop.list.deref_item(), parent)?;

                let block = self.resolve_block_with(forloop.proc, parent, base)?;
                Ok((
                    RNode::ForLoop {
                        loop_var,
                        list,
                        block,
                    }
                    .to_spanned(span),
                    0,
                ))
            }
            AstNode::Index { target, index } => {
                let target = self.resolve_noderef(target.deref_item(), parent)?;
                let index = self.resolve_noderef(index.deref_item(), parent)?;
                Ok((RNode::Index { target, index }.to_spanned(span), 0))
            }
            AstNode::StructLit(obj) => {
                let mut buf = hashmap!();
                for (name, node) in obj {
                    let node = self.resolve_noderef(node, parent)?;
                    buf.insert(name, node);
                }
                Ok((RNode::StructDef(buf).to_spanned(span), 0))
            }
            AstNode::Call(call) => {
                let callee = self.resolve_noderef(call.callee.deref_item(), parent)?;
                let mut args = vec![];
                for node in call.args {
                    args.push(self.resolve_noderef(node, parent)?);
                }
                Ok((RNode::Call { args, callee }.to_spanned(span), 0))
            }
            AstNode::FieldAccess(field) => {
                let target = self.resolve_noderef(field.target.deref_item(), parent)?;
                let requested = match field.requested.item {
                    ast::AccessType::Method {
                        args,
                        arg_span,
                        callee,
                        callee_span,
                    } => AccessType::Method {
                        callee,
                        callee_span,
                        arg_span,
                        args: self.resolve_list(args, parent)?,
                    },
                    ast::AccessType::Property(prop) => AccessType::Property(prop),
                }
                .to_spanned(field.requested.span);
                Ok((RNode::FieldAccess(target, requested).to_spanned(span), 0))
            }

            AstNode::RecordLit(obj) => {
                let mut buf = hashmap!();
                for (name, node) in obj {
                    let node = self.resolve_noderef(node, parent)?;
                    buf.insert(name, node);
                }
                Ok((RNode::RecordLit(buf).to_spanned(span), 0))
            }
            AstNode::ListLit(list) => {
                let list = self.resolve_list(list, parent)?;
                Ok((RNode::ListLit(list).to_spanned(span), 0))
            }
            AstNode::UnaryNode(un) => {
                let (target, localcount) = self.resolve_noderef(un.target.deref_item(), parent)?;
                Ok((
                    RNode::UnaryNode(un.kind, target).to_spanned(span),
                    localcount,
                ))
            }
            AstNode::Constructor(con) => {
                let (target, parent_localcount) =
                    self.resolve_noderef(con.target.deref_item(), parent)?;
                let mut max_locals = parent_localcount;
                let mut params = HashMap::new();
                for (name, value) in con.params {
                    let (value, localcount) = self.resolve_noderef(value, parent)?;
                    if localcount > max_locals {
                        max_locals = localcount
                    }
                    params.insert(name, value);
                }
                Ok((
                    RNode::Constructor { target, params }.to_spanned(span),
                    max_locals,
                ))
            }

            AstNode::ContinueNode => Ok((RNode::Continue.to_spanned(span), 0)),
            AstNode::BreakNode => Ok((RNode::Break.to_spanned(span), 0)),
            AstNode::Bool(v) => Ok((RNode::Bool(v).to_spanned(span), 0)),
            AstNode::Float(v) => Ok((RNode::Float(v).to_spanned(span), 0)),
            AstNode::Null => Ok((RNode::Null.to_spanned(span), 0)),
            AstNode::Str(v) => Ok((RNode::String(v).to_spanned(span), 0)),
            AstNode::Int(v) => Ok((RNode::Int(v).to_spanned(span), 0)),
            AstNode::DontResult => Err(NameErr::UnexpectedSemi.to_spanned(span)),
        }
    }
    fn resolve_noderef(
        &mut self,
        node: ast::NodeSpan,
        parent: &mut Scope,
    ) -> Result<(RNodeRef, usize)> {
        let idx = RNodeRef(self.node_pool.len());
        let (node, localcount) = self.resolve_node(node, parent)?;
        self.node_pool.push(node);
        Ok((idx, localcount))
    }
    fn resolve_block_with(
        &mut self,
        ast: ast::Block,
        parent: &mut Scope,
        mut base: Scope,
    ) -> Result<(Block,usize)> {
        let ast_span = ast.span;
        base.parent = Some(Box::new(parent.clone()));
        let mut buffer = vec![];
        let base_locals = ;
        for node in ast {
            let resolved = self.resolve_noderef(node, &mut base)?;
            buffer.push(resolved);
        }
        self.max_locals = self.max_locals.max(self.ident_counter);
        self.ident_counter = base_locals;
        let Some(mod_parent) = base.parent else {
            unimplemented!("Parent should always exist");
        };
        *parent = *mod_parent;
        Ok(buffer.to_spanned(ast_span))
    }
    fn resolve_block(&mut self, ast: ast::Block, parent: &mut Scope) -> Result<(Block,usize)> {
        self.resolve_block_with(ast, parent, Scope::default())
    }
}
