mod error;
use crate::{
    frontend::{
        FileStore,
        ast::nodes::{self as ast, Node as AstNode},
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
pub type Result<T = RNodeSpan> = std::result::Result<T, Spanned<NameErr>>;
type DeclStream = Vec<Spanned<ast::Item>>;
use ResolvedNode as RNode;
#[derive(Default)]
pub struct NameRes {
    ident_counter: usize,
    globals: HashMap<String, usize>,
    pub(crate) file_store: FileStore,
    scope_locals_stack: Vec<usize>,
}
impl NameRes {
    pub fn new(file_store: FileStore) -> Self {
        Self {
            file_store,
            globals: hashmap!(
                println => 0,
                input => 1,
                str_len => 2
            ),
            ..Default::default()
        }
    }
    pub fn resolve(&mut self, ast: Vec<Spanned<ast::Node>>) -> Result<ResolvedAst> {
        self.push_scope();
        let decls = self.resolve_toplevel(ast)?;
        let local_count = self.pop_scope();
        Ok(ResolvedAst {
            proc: decls,

            global_count: self.globals.len(),
            local_count,
        })
    }
    pub fn resolve_expr(&mut self, expr: ast::NodeSpan) -> Result<ResolvedAstNode> {
        let start_ident = self.ident_counter;
        self.push_scope();
        let node = self.resolve_node(expr, &mut Scope::default())?;
        let local_count = self.pop_scope();
        self.ident_counter = start_ident;
        Ok(ResolvedAstNode::new(node, self.globals.len(), local_count))
    }
    fn add_global(&mut self, name: String) {
        if self.globals.contains_key(&name) {
            return;
        }
        let id = self.globals.len();
        self.globals.insert(name.to_string(), id);
    }

    pub fn resolve_toplevel(&mut self, exprs: Vec<ast::NodeSpan>) -> Result<Vec<RNodeSpan>> {
        for val in exprs.iter() {
            let AstNode::Decl(decl) = &val.item else {
                continue;
            };
            if !decl.hoisted {
                continue;
            }

            self.add_global(decl.name.to_owned());
        }

        let mut root = Scope::default();
        let mut local_decls = vec![];
        let mut hoisted_decls = vec![];
        let base_locals = 0usize;
        self.push_scope();
        for node in exprs {
            let AstNode::Decl(decl) = &node.item else {
                let node = self.resolve_node(node, &mut root)?;
                local_decls.push(node);
                continue;
            };
            if !decl.hoisted {
                let node = self.resolve_node(node, &mut root)?;
                local_decls.push(node);
                continue;
            }
            let expr = self
                .resolve_node(decl.expr.clone().deref_item(), &mut root)?
                .box_item();

            let id = &self.globals[&decl.name];

            hoisted_decls.push(
                Decl {
                    id: *id,
                    expr,
                    is_global: true,
                }
                .to_rnodespan(node.span),
            );
        }
        let inner_max = self.pop_scope();
        self.ident_counter = base_locals;
        if let Some(max) = self.scope_locals_stack.last_mut() {
            *max = (*max).max(inner_max);
        }
        let mut decls = hoisted_decls;
        decls.append(&mut local_decls);
        Ok(decls)
    }
    fn push_scope(&mut self) {
        self.scope_locals_stack.push(0);
    }
    fn pop_scope(&mut self) -> usize {
        self.scope_locals_stack.pop().unwrap_or(0)
    }
    fn update_scope_locals(&mut self) {
        if let Some(max) = self.scope_locals_stack.last_mut() {
            *max = self.ident_counter;
        }
    }
    fn gen_name(&mut self) -> usize {
        let old_count = self.ident_counter;
        self.ident_counter += 1;
        self.update_scope_locals();
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
                .map(|id| VarInfo::new(name.to_string(), true, true, *id))
        }) else {
            return Err(NameErr::UndefinedVar(name.to_string()).to_spanned(span));
        };
        Ok(info)
    }
    fn resolve_var_decl(&mut self, decl: ast::Decl, parent: &mut Scope) -> Result<Decl> {
        let expr = self
            .resolve_node(decl.expr.deref_item(), parent)?
            .box_item();
        let id = if decl.readonly {
            self.def_readonly(decl.name, parent)
        } else {
            self.def_local(decl.name, parent)
        };

        Ok(Decl {
            expr,
            id,
            is_global: false,
        })
    }
    fn resolve_list(
        &mut self,
        list: Vec<Spanned<AstNode>>,
        parent: &mut Scope,
    ) -> Result<Vec<RNodeSpan>> {
        let mut new = vec![];
        for node in list {
            new.push(self.resolve_node(node, parent)?)
        }
        Ok(new)
    }
    fn def_local(&mut self, name: String, parent: &mut Scope) -> usize {
        let id = self.gen_name();
        parent.define(name.clone(), VarInfo::new(name, false, false, id));
        id
    }
    fn def_readonly(&mut self, name: String, parent: &mut Scope) -> usize {
        let id = self.gen_name();
        parent.define(name.clone(), VarInfo::new(name, false, true, id));
        id
    }
    fn resolve_assignment(
        &mut self,
        target: Spanned<Box<AstNode>>,
        value: Spanned<Box<AstNode>>,
        parent: &mut Scope,
        span: Span,
    ) -> Result {
        let target = target.deref_item();
        let Spanned {
            item: AstNode::Variable(ref name),
            span,
        } = target
        else {
            let target = self.resolve_node(target, parent)?.box_item();
            let value = self.resolve_node(value.deref_item(), parent)?.box_item();
            return Ok(RNode::Assignment { target, value }.to_spanned(span));
        };
        let info = self.get_var(name, parent, span)?;
        if info.readonly {
            return Err(
                NameErr::Unspecified(format!("Cannot assign to a readonly variable"))
                    .to_spanned(span),
            );
        }
        return Ok(RNode::Variable {
            id: info.id,
            is_global: info.global,
        }
        .to_spanned(span));
    }
    fn resolve_node(&mut self, node: ast::NodeSpan, parent: &mut Scope) -> Result<RNodeSpan> {
        use ResolvedNode as RNode;
        let span = node.span;
        match node.item {
            AstNode::Decl(decl) => {
                let decl = self.resolve_var_decl(decl, parent)?;
                Ok(RNode::Decl(decl).to_spanned(span))
            }
            AstNode::Assignment { target, value } => {
                self.resolve_assignment(target, value, parent, span)
            }
            AstNode::Variable(name) => {
                let info = self.get_var(name, parent, span)?;
                Ok(RNode::Variable {
                    id: info.id,
                    is_global: info.global,
                }
                .to_spanned(span))
            }

            AstNode::FunctionLit(func) => {
                let mut func_scope = Scope::default();
                let mut args = vec![];
                for arg in func.args {
                    let new_name = self.def_local(arg, &mut func_scope);
                    args.push(new_name);
                }
                let old_ident_counter = self.ident_counter;
                self.ident_counter = 0;
                self.push_scope();
                let block = self.resolve_block_with(func.block, parent, func_scope)?;
                let local_count = self.pop_scope();
                self.ident_counter = old_ident_counter;
                let resolved = RNode::FunctionLit(FunctionLit {
                    idents: args,
                    block,
                    captures: func.captures,
                    local_count,
                });
                Ok(resolved.to_spanned(span))
            }
            AstNode::While(node) => {
                let condition = self
                    .resolve_node(node.condition.deref_item(), parent)?
                    .box_item();
                let block = self.resolve_block(node.proc, parent)?;
                Ok(RNode::While { block, condition }.to_spanned(span))
            }
            AstNode::Loop(block) => {
                let block = self.resolve_block(block, parent)?;
                Ok(RNode::Loop(block).to_spanned(span))
            }
            AstNode::Branch(branch) => {
                let condition = self
                    .resolve_node(branch.condition.deref_item(), parent)?
                    .box_item();

                let if_block = self.resolve_block(branch.if_block, parent)?;
                let else_block = if let Some(else_block) = branch.else_block {
                    Some(self.resolve_block(else_block, parent)?)
                } else {
                    None
                };
                Ok(Branch {
                    condition,
                    if_block,
                    else_block,
                }
                .to_rnodespan(span))
            }

            AstNode::DoBlock(block) => {
                let block = self.resolve_block(block, parent)?;
                Ok(RNode::DoBlock(block).to_spanned(span))
            }
            AstNode::Return(node) => {
                let node = self.resolve_node(node.deref_item(), parent)?.box_item();
                Ok(RNode::Return(node).to_spanned(span))
            }
            AstNode::Result(node) => {
                let node = self.resolve_node(node.deref_item(), parent)?.box_item();
                Ok(RNode::Result(node).to_spanned(span))
            }

            AstNode::BinaryNode(bin) => {
                let left = self.resolve_node(bin.left.deref_item(), parent)?.box_item();
                let right = self
                    .resolve_node(bin.right.deref_item(), parent)?
                    .box_item();
                Ok(RNode::BinaryNode {
                    left,
                    right,
                    kind: bin.kind,
                }
                .to_spanned(span))
            }
            AstNode::ForLoop(forloop) => {
                let mut base = Scope::default();
                let loop_var = self.def_local(forloop.ident, &mut base);

                let list = self
                    .resolve_node(forloop.list.deref_item(), parent)?
                    .box_item();

                let block = self.resolve_block_with(forloop.proc, parent, base)?;
                Ok(RNode::ForLoop {
                    loop_var,
                    list,
                    block,
                }
                .to_spanned(span))
            }
            AstNode::Index { target, index } => {
                let target = self.resolve_node(target.deref_item(), parent)?.box_item();
                let index = self.resolve_node(index.deref_item(), parent)?.box_item();
                Ok(RNode::Index { target, index }.to_spanned(span))
            }
            AstNode::StructLit(obj) => {
                let mut buf = hashmap!();
                for (name, node) in obj {
                    let node = self.resolve_node(node, parent)?.box_item();
                    buf.insert(name, node);
                }
                Ok(RNode::StructDef(buf).to_spanned(span))
            }
            AstNode::Call(call) => {
                let callee = self
                    .resolve_node(call.callee.deref_item(), parent)?
                    .box_item();
                let mut args = vec![];
                for node in call.args {
                    args.push(self.resolve_node(node, parent)?);
                }
                Ok(RNode::Call { args, callee }.to_spanned(span))
            }
            AstNode::FieldAccess(field) => {
                let target = self
                    .resolve_node(field.target.deref_item(), parent)?
                    .box_item();
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
                Ok(RNode::FieldAccess(target, requested).to_spanned(span))
            }

            AstNode::RecordLit(obj) => {
                let mut buf = hashmap!();
                for (name, node) in obj {
                    let node = self.resolve_node(node, parent)?.box_item();
                    buf.insert(name, node);
                }
                Ok(RNode::RecordLit(buf).to_spanned(span))
            }
            AstNode::ListLit(list) => {
                let list = self.resolve_list(list, parent)?;
                Ok(RNode::ListLit(list).to_spanned(span))
            }
            AstNode::UnaryNode(un) => {
                let target = self
                    .resolve_node(un.target.deref_item(), parent)?
                    .box_item();
                Ok(RNode::UnaryNode(un.kind, target).to_spanned(span))
            }
            AstNode::Constructor(con) => {
                let target = self
                    .resolve_node(con.target.deref_item(), parent)?
                    .box_item();
                let mut params = HashMap::new();
                for (name, value) in con.params {
                    let value = self.resolve_node(value, parent)?;
                    params.insert(name, value);
                }
                Ok(RNode::Constructor { target, params }.to_spanned(span))
            }

            AstNode::ContinueNode => Ok(RNode::Continue.to_spanned(span)),
            AstNode::BreakNode => Ok(RNode::Break.to_spanned(span)),
            AstNode::Bool(v) => Ok(RNode::Bool(v).to_spanned(span)),
            AstNode::Float(v) => Ok(RNode::Float(v).to_spanned(span)),
            AstNode::Null => Ok(RNode::Null.to_spanned(span)),
            AstNode::Str(v) => Ok(RNode::String(v).to_spanned(span)),
            AstNode::Int(v) => Ok(RNode::Int(v).to_spanned(span)),
            AstNode::DontResult => Err(NameErr::UnexpectedSemi.to_spanned(span)),
        }
    }
    fn resolve_block_with(
        &mut self,
        ast: ast::Block,
        parent: &mut Scope,
        mut base: Scope,
    ) -> Result<Block> {
        let ast_span = ast.span;
        base.parent = Some(Box::new(parent.clone()));
        let mut buffer = vec![];
        let base_locals = self.ident_counter;
        self.push_scope();
        for node in ast {
            let resolved = self.resolve_node(node, &mut base)?;
            buffer.push(resolved);
        }
        let inner_max = self.pop_scope();
        self.ident_counter = base_locals;
        // Propagate the inner scope's max to the parent scope
        if let Some(max) = self.scope_locals_stack.last_mut() {
            *max = (*max).max(inner_max);
        }
        let Some(mod_parent) = base.parent else {
            unimplemented!("Parent should always exist");
        };
        *parent = *mod_parent;
        Ok(buffer.to_spanned(ast_span))
    }
    fn resolve_block(&mut self, ast: ast::Block, parent: &mut Scope) -> Result<Block> {
        self.resolve_block_with(ast, parent, Scope::default())
    }
}
