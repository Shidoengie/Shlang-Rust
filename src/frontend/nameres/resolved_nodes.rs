use core::fmt;
use std::{
    collections::HashMap,
    fmt::Debug,
    ops::{Deref, DerefMut, Index},
};

use crate::{
    frontend::ast::nodes::{BinaryOp, UnaryOp},
    spans::{Span, Spanned},
};
#[derive(Clone, Copy)]
pub struct RNodeRef(pub usize);
#[derive(Clone, Default)]
pub struct NodePool(Vec<Spanned<ResolvedNode>>);

impl NodePool {
    pub fn stringify_node(&self, node_ref: RNodeRef) -> String {
        let node = ResolvedAstNode::new(node_ref, self.clone(), 0, 0);
        format!("{node:?}")
    }
}
impl Deref for NodePool {
    type Target = Vec<Spanned<ResolvedNode>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for NodePool {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Index<RNodeRef> for NodePool {
    type Output = Spanned<ResolvedNode>;
    fn index(&self, index: RNodeRef) -> &Self::Output {
        &self.0[index.0]
    }
}
#[derive(Clone)]
pub enum ResolvedNode {
    Null,
    Bool(bool),
    Str(String),
    Float(f64),
    Int(i64),
    BinaryNode {
        kind: BinaryOp,
        left: RNodeRef,
        right: RNodeRef,
    },
    UnaryNode(UnaryOp, RNodeRef),
    Result(RNodeRef),
    Return(RNodeRef),
    Break,
    Continue,

    Assignment {
        target: RNodeRef,
        value: RNodeRef,
    },
    Variable {
        id: usize,
        is_global: bool,
    },

    Declaration(Declaration),

    Index {
        target: RNodeRef,
        index: RNodeRef,
    },
    FunctionLit(FunctionLit),
    ListLit(Vec<RNodeRef>),
    Call {
        callee: RNodeRef,
        args: Vec<RNodeRef>,
    },

    Branch(Branch),

    Loop(Block),
    While {
        condition: RNodeRef,
        block: Block,
    },
    Constructor {
        target: RNodeRef,
        params: HashMap<String, RNodeRef>,
    },
    ForLoop {
        loop_var: usize,
        list: RNodeRef,
        block: Block,
    },

    DoBlock(Block),
    StructDef(HashMap<String, RNodeRef>),
    RecordLit(HashMap<String, RNodeRef>),
    FieldAccess(RNodeRef, Spanned<AccessType>),
}
pub type Block = Spanned<Vec<RNodeRef>>;

#[derive(Clone)]
pub struct Branch {
    pub condition: RNodeRef,
    pub if_block: Block,
    pub else_block: Option<Block>,
}
impl From<Branch> for ResolvedNode {
    fn from(value: Branch) -> Self {
        Self::Branch(value)
    }
}
#[derive(Clone)]
pub struct Declaration {
    pub id: usize,
    pub expr: RNodeRef,
    pub is_global: bool,
}
#[derive(Clone)]
pub struct FunctionLit {
    pub captures: bool,
    pub idents: Vec<usize>,
    pub block: Block,
    pub local_count: usize,
}
#[derive(Clone)]
pub enum AccessType {
    Property(String),
    Method {
        callee: String,
        callee_span: Span,
        args: Vec<RNodeRef>,
        arg_span: Span,
    },
}
#[derive(Clone)]
pub enum Item {
    Decl(Declaration),
}
fn display_block(
    f: &mut fmt::Formatter<'_>,
    block: &[RNodeRef],
    node_pool: &NodePool,
    depth: usize,
) -> fmt::Result {
    let indent = "  ".repeat(depth);
    write!(f, "{{")?;
    if block.is_empty() {
        return write!(f, "}}");
    } else {
        writeln!(f)?;
    }
    for &item in block {
        node_debug_display(f, item, node_pool, depth + 1, true)?;
        writeln!(f, ";")?;
    }
    write!(f, "{indent}}}")?;
    Ok(())
}
fn node_debug_display(
    f: &mut fmt::Formatter<'_>,
    node_ref: RNodeRef,
    node_pool: &NodePool,
    depth: usize,
    start_of_line: bool,
) -> fmt::Result {
    use ResolvedNode as RNode;

    // The base indentation for the current node.
    let indent = if depth > 0 {
        "  ".repeat(depth)
    } else {
        String::new()
    };

    // The indentation for child elements (fields, list items, etc.).
    let child_indent = "  ".repeat(depth + 1);

    // Your closure is the correct pattern. It prints the title, handling the
    // start_of_line flag to decide if indentation is needed.
    let write_title = |f: &mut fmt::Formatter, title: &str| -> fmt::Result {
        if start_of_line {
            f.write_str(&indent)?;
        }
        f.write_str(title)?;
        Ok(())
    };
    let expr = &node_pool[node_ref];

    match &expr.item {
        // --- Simple Nodes ---
        RNode::Null => write_title(f, "Null")?,
        RNode::Bool(b) => write_title(f, &format!("Bool({b})"))?,
        RNode::Str(s) => write_title(f, &format!("Str({s:?})"))?,
        RNode::Float(fl) => write_title(f, &format!("Float({fl})"))?,
        RNode::Int(i) => write_title(f, &format!("Int({i})"))?,
        RNode::Variable { id, is_global } => {
            write_title(f, &format!("Variable(id: {id}, global: {is_global})"))?
        }
        RNode::Break => write_title(f, "BreakNode")?,
        RNode::Continue => write_title(f, "ContinueNode")?,

        // --- Nodes with a single inline-able expression ---
        RNode::Result(expr) | RNode::Return(expr) => {
            let name = if matches!(node_pool[node_ref].item, RNode::Result(_)) {
                "ResultNode"
            } else {
                "ReturnNode"
            };
            write_title(f, &format!("{name}(\n"))?;

            node_debug_display(f, *expr, node_pool, depth + 1, true)?; // Call inline
            write!(f, "\n{indent})")?;
        }

        // --- Complex, Multi-line Nodes ---
        RNode::Assignment { target, value } => {
            write_title(f, "Assignment(")?;
            writeln!(f)?;
            write!(f, "{child_indent}target:")?;
            node_debug_display(f, *target, node_pool, depth + 2, false)?;
            writeln!(f, ",")?;
            write!(f, "{child_indent}value:")?;
            node_debug_display(f, *value, node_pool, depth + 2, false)?;
            writeln!(f)?;
            write!(f, "{indent})")?;
        }
        RNode::BinaryNode { kind, left, right } => {
            write_title(f, "BinaryNode(")?;
            writeln!(f)?;
            writeln!(f, "{child_indent}kind: {kind:?},")?;
            write!(f, "{child_indent}left:")?;
            node_debug_display(f, *left, node_pool, depth + 2, false)?;
            writeln!(f, ",")?;
            write!(f, "{child_indent}right:")?;
            node_debug_display(f, *right, node_pool, depth + 2, false)?;
            write!(f, "\n{indent})")?;
        }
        RNode::UnaryNode(op, operand) => {
            write_title(f, "UnaryNode(")?;
            writeln!(f)?;
            writeln!(f, "{child_indent}op: {op:?},")?;
            write!(f, "{child_indent}operand:")?;
            node_debug_display(f, *operand, node_pool, depth + 2, false)?;
            write!(f, "\n{indent})")?;
        }
        RNode::Declaration(Declaration {
            id,
            expr,
            is_global,
        }) => {
            write_title(f, "Decl(")?;
            writeln!(f)?;
            writeln!(f, "{child_indent}id: {id},")?;
            writeln!(f, "{child_indent}global: {is_global}")?;
            write!(f, "{child_indent}expr: ")?;
            node_debug_display(f, *expr, node_pool, depth + 1, false)?; // The key inline call
            writeln!(f)?;
            write!(f, "{indent})")?;
        }
        RNode::Index { target, index } => {
            write_title(f, "Index(")?;
            writeln!(f)?;
            write!(f, "{child_indent}target:")?;
            node_debug_display(f, *target, node_pool, depth + 1, false)?;
            writeln!(f, ",")?;
            write!(f, "{child_indent}index:")?;
            node_debug_display(f, *index, node_pool, depth + 1, false)?;
            write!(f, "\n{indent})")?;
        }
        RNode::FunctionLit(func) => {
            write_title(f, "FuncDef(")?;
            writeln!(f)?;
            writeln!(f, "{child_indent}captures: {},", func.captures)?;
            writeln!(f, "{child_indent}idents: {:?},", func.idents)?;
            writeln!(f, "{child_indent}local_count: {:?},", func.local_count)?;
            write!(f, "{child_indent}block:")?;
            display_block(f, &func.block, node_pool, depth + 1)?;
            writeln!(f)?;
            write!(f, "{indent})")?;
        }
        RNode::ListLit(items) => {
            write_title(f, "ListLit(")?;
            if !items.is_empty() {
                writeln!(f)?;
                for &item in items {
                    node_debug_display(f, item, node_pool, depth + 1, true)?;
                    writeln!(f, ",")?;
                }
            }
            write!(f, "{indent})")?;
        }
        RNode::Call { callee, args } => {
            write_title(f, "Call(")?;
            writeln!(f)?;
            write!(f, "{child_indent}callee:")?;
            node_debug_display(f, *callee, node_pool, depth + 2, false)?;
            writeln!(f, ",")?;
            write!(f, "{child_indent}args:[")?;
            if !args.is_empty() {
                writeln!(f)?;
                for &arg in args {
                    node_debug_display(f, arg, node_pool, depth + 2, true)?;
                    writeln!(f, ",")?;
                }
                writeln!(f, "{child_indent}]")?;
            } else {
                writeln!(f, "]")?;
            }
            write!(f, "{indent})")?;
        }
        RNode::Branch(branch) => {
            write_title(f, "Branch(")?;
            writeln!(f)?;
            write!(f, "{child_indent}condition:")?;
            node_debug_display(f, branch.condition, node_pool, depth + 1, false)?;
            writeln!(f, ",")?;
            write!(f, "{child_indent}if_block:")?;
            display_block(f, &branch.if_block.item, node_pool, depth + 1)?;
            writeln!(f)?;
            if let Some(else_b) = &branch.else_block {
                write!(f, "{child_indent}else_block:")?;
                display_block(f, &else_b.item, node_pool, depth + 1)?;
                writeln!(f)?;
            }
            write!(f, "{indent})")?;
        }
        RNode::Loop(block) | RNode::DoBlock(block) => {
            let name = if matches!(node_pool[node_ref].item, RNode::Loop(_)) {
                "Loop"
            } else {
                "DoBlock"
            };
            write_title(f, name)?;
            display_block(f, block, node_pool, depth)?;
        }
        RNode::While { condition, block } => {
            write_title(f, "While(")?;
            writeln!(f)?;
            write!(f, "{child_indent}condition:")?;
            node_debug_display(f, *condition, node_pool, depth + 1, false)?;
            writeln!(f, ",")?;
            write!(f, "{child_indent}block:")?;
            display_block(f, block, node_pool, depth + 1)?;
            writeln!(f)?;
            write!(f, "{indent})")?;
        }
        RNode::Constructor { target, params } => {
            write_title(f, "Constructor(")?;
            writeln!(f)?;
            write!(f, "{child_indent}target:")?;
            node_debug_display(f, *target, node_pool, depth + 1, false)?;
            writeln!(f, ",")?;
            if params.is_empty() {
                writeln!(f, "{child_indent}params:()")?;
            } else {
                writeln!(f, "{child_indent}params:(")?;
                for (key, &value) in params {
                    write!(f, "{}{key:?}: ", "  ".repeat(depth + 2))?;
                    node_debug_display(f, value, node_pool, depth + 2, false)?;
                    writeln!(f)?;
                }
                writeln!(f, "{child_indent})")?;
            }
            write!(f, "{indent})")?;
        }
        RNode::ForLoop {
            loop_var,
            list,
            block,
        } => {
            write_title(f, "ForLoop(")?;
            writeln!(f)?;
            writeln!(f, "{child_indent}loop_var: {loop_var},")?;
            write!(f, "{child_indent}list:")?;
            node_debug_display(f, *list, node_pool, depth + 1, false)?;
            writeln!(f)?;
            write!(f, "{child_indent}block:")?;
            display_block(f, block, node_pool, depth + 1)?;
            writeln!(f)?;
            write!(f, "{indent})")?;
        }
        RNode::StructDef(fields) | RNode::RecordLit(fields) => {
            let name = if matches!(node_pool[node_ref].item, RNode::StructDef(_)) {
                "StructDef"
            } else {
                "RecordLit"
            };
            write_title(f, &format!("{name}("))?;
            if fields.is_empty() {
                return f.write_str(")");
            }
            writeln!(f)?;
            for (key, &value) in fields {
                write!(f, "{child_indent}{key:?}: ")?;
                node_debug_display(f, value, node_pool, depth + 1, false)?;
                writeln!(f)?;
            }
            write!(f, "{indent})")?;
        }
        RNode::FieldAccess(target, access) => {
            write_title(f, "FieldAccess(")?;
            writeln!(f)?;
            write!(f, "{child_indent}target:")?;
            node_debug_display(f, *target, node_pool, depth + 1, false)?;
            writeln!(f, ",")?;
            write!(f, "{child_indent}requested:")?;
            match &access.item {
                AccessType::Property(prop) => {
                    writeln!(f, "Property({prop:?})")?;
                }
                AccessType::Method { callee, args, .. } => {
                    writeln!(f, "Method(")?;
                    let method_child_indent = "  ".repeat(depth + 2);
                    writeln!(f, "{method_child_indent}callee: {callee:?},")?;
                    write!(f, "{method_child_indent}args: [")?;
                    if !args.is_empty() {
                        writeln!(f,)?;
                        for &arg in args {
                            node_debug_display(f, arg, node_pool, depth + 3, true)?;
                            writeln!(f, ",")?;
                        }
                        writeln!(f, "{method_child_indent}]")?;
                    } else {
                        writeln!(f, "]")?;
                    }
                    writeln!(f, "{child_indent})")?;
                }
            }
            write!(f, "{indent})")?;
        }
    }
    write!(f, "[{:?},{:?}]", expr.span.start, expr.span.end)?;

    Ok(())
}
pub struct ResolvedAstNode {
    pub node: RNodeRef,
    pub pool: NodePool,
    pub global_count: usize,
    pub local_count: usize,
}
impl Debug for ResolvedAstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "ResolvedNode(")?;
        writeln!(f, "  global_count: {},", self.global_count)?;
        writeln!(f, "  local_count: {},", self.local_count)?;
        writeln!(f, "){{")?;
        node_debug_display(f, self.node, &self.pool, 1, true)?;
        write!(f, "\n}}")
    }
}

impl ResolvedAstNode {
    pub fn new(node: RNodeRef, pool: NodePool, global_count: usize, local_count: usize) -> Self {
        Self {
            node,
            pool,
            global_count,
            local_count,
        }
    }
}
pub struct ResolvedAst {
    pub proc: Vec<Spanned<Item>>,
    pub pool: NodePool,
    pub global_count: usize,
    pub local_count: usize,
    pub entry_point: Option<usize>,
}

impl Debug for ResolvedAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Ast(")?;
        writeln!(f, "  global_count: {},", self.global_count)?;
        writeln!(f, "  local_count: {},", self.local_count)?;
        writeln!(f, ") = {{")?;
        for Spanned { item, span } in self.proc.iter() {
            match item {
                Item::Decl(Declaration {
                    id,
                    expr,
                    is_global,
                }) => {
                    writeln!(f, "  Decl(")?;
                    writeln!(f, "    id: {id}")?;
                    writeln!(f, "    global: {is_global}")?;

                    write!(f, "    expr: ",)?;
                    node_debug_display(f, *expr, &self.pool, 2, false)?;
                    writeln!(f)?;
                    write!(f, "  )")?;
                }
            };
            writeln!(f, "[{:?},{:?}];", span.start, span.end)?
        }
        write!(f, "}}")?;
        Ok(())
    }
}
