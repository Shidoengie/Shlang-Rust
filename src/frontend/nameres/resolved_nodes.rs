use core::fmt;
use std::{
    collections::HashMap,
    fmt::{Debug, Display, write},
};

use crate::{
    frontend::ast::nodes::{BinaryOp, UnaryOp},
    spans::{Span, Spanned},
};
pub type NodePool = Vec<Spanned<ResolvedNode>>;
#[derive(Clone, Copy)]
pub struct RNodeRef(pub usize);
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
    ResultNode(RNodeRef),
    ReturnNode(RNodeRef),
    BreakNode,
    ContinueNode,

    Assignment {
        target: RNodeRef,
        value: RNodeRef,
    },
    Variable(usize),
    Decl(ResolvedDecl),
    Index {
        target: RNodeRef,
        index: RNodeRef,
    },
    FuncDef {
        captures: bool,
        idents: Vec<usize>,
        block: Vec<RNodeRef>,
    },
    ListLit(Vec<RNodeRef>),
    Call {
        callee: RNodeRef,
        args: Vec<RNodeRef>,
    },

    Branch {
        condition: RNodeRef,
        if_block: Vec<RNodeRef>,
        else_block: Option<Vec<RNodeRef>>,
    },

    Loop(Vec<RNodeRef>),
    While {
        condition: RNodeRef,
        block: Vec<RNodeRef>,
    },
    Constructor {
        target: RNodeRef,
        params: HashMap<String, RNodeRef>,
    },
    ForLoop {
        loop_var: usize,
        list: RNodeRef,
        block: Vec<RNodeRef>,
    },
    DoBlock(Vec<RNodeRef>),
    StructDef(HashMap<String, RNodeRef>),
    RecordLit(HashMap<String, RNodeRef>),
    FieldAccess(RNodeRef, Spanned<ResAccessType>),
}

#[derive(Clone)]
pub struct ResolvedDecl {
    pub id: usize,

    pub expr: RNodeRef,
}

#[derive(Clone)]
pub enum ResAccessType {
    Property(String),
    Method {
        callee: String,
        callee_span: Span,
        args: Vec<RNodeRef>,
    },
}
#[derive(Clone)]
pub enum ResDeclType {
    Decl(ResolvedDecl),
}
fn display_block<'a>(
    f: &mut fmt::Formatter<'a>,
    block: &[RNodeRef],
    node_pool: &[Spanned<ResolvedNode>],
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
fn node_debug_display<'a>(
    f: &mut fmt::Formatter<'a>,
    node_ref: RNodeRef,
    node_pool: &[Spanned<ResolvedNode>],
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
    let expr = &node_pool[node_ref.0];

    match &expr.item {
        // --- Simple Nodes ---
        RNode::Null => write_title(f, "Null")?,
        RNode::Bool(b) => write_title(f, &format!("Bool({b})"))?,
        RNode::Str(s) => write_title(f, &format!("Str({s:?})"))?,
        RNode::Float(fl) => write_title(f, &format!("Float({fl})"))?,
        RNode::Int(i) => write_title(f, &format!("Int({i})"))?,
        RNode::Variable(id) => write_title(f, &format!("Variable(id: {id})"))?,
        RNode::BreakNode => write_title(f, "BreakNode")?,
        RNode::ContinueNode => write_title(f, "ContinueNode")?,

        // --- Nodes with a single inline-able expression ---
        RNode::ResultNode(expr) | RNode::ReturnNode(expr) => {
            let name = if matches!(node_pool[node_ref.0].item, RNode::ResultNode(_)) {
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
        RNode::Decl(ResolvedDecl { id, expr }) => {
            write_title(f, "Decl(")?;
            writeln!(f)?;
            writeln!(f, "{child_indent}id: {id},")?;
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
        RNode::FuncDef {
            captures,
            idents,
            block,
        } => {
            write_title(f, "FuncDef(")?;
            writeln!(f)?;
            writeln!(f, "{child_indent}captures: {captures},")?;
            writeln!(f, "{child_indent}idents: {idents:?},")?;
            write!(f, "{child_indent}block:")?;
            display_block(f, block, node_pool, depth + 1)?;
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
        RNode::Branch {
            condition,
            if_block,
            else_block,
        } => {
            write_title(f, "Branch(")?;
            writeln!(f)?;
            write!(f, "{child_indent}condition:")?;
            node_debug_display(f, *condition, node_pool, depth + 1, false)?;
            writeln!(f, ",")?;
            write!(f, "{child_indent}if_block:")?;
            display_block(f, &if_block, node_pool, depth + 1)?;
            writeln!(f)?;
            if let Some(else_b) = else_block {
                write!(f, "{child_indent}else_block:")?;
                display_block(f, &else_b, node_pool, depth + 1)?;
                writeln!(f)?;
            }
            write!(f, "{indent})")?;
        }
        RNode::Loop(block) | RNode::DoBlock(block) => {
            let name = if matches!(node_pool[node_ref.0].item, RNode::Loop(_)) {
                "Loop"
            } else {
                "DoBlock"
            };
            write_title(f, &format!("{name}",))?;
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
            writeln!(f, "")?;
            write!(f, "{indent})")?;
        }
        RNode::StructDef(fields) | RNode::RecordLit(fields) => {
            let name = if matches!(node_pool[node_ref.0].item, RNode::StructDef(_)) {
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
                ResAccessType::Property(prop) => {
                    writeln!(f, "Property({prop:?})")?;
                }
                ResAccessType::Method { callee, args, .. } => {
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
pub struct ResolvedAstNode(pub RNodeRef, pub NodePool);
impl Debug for ResolvedAstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        node_debug_display(f, self.0, &self.1, 0, true)
    }
}

pub struct ResolvedAst(pub Vec<Spanned<ResDeclType>>, pub NodePool);
impl Debug for ResolvedAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for Spanned { item, span } in self.0.iter() {
            match item {
                ResDeclType::Decl(ResolvedDecl { id, expr }) => f
                    .debug_struct("Decl")
                    .field("id", &id)
                    .field("expr", &ResolvedAstNode(*expr, self.1.clone()))
                    .finish()?,
            };
            writeln!(f, "[{:?},{:?}]", span.start, span.end)?
        }
        Ok(())
    }
}
