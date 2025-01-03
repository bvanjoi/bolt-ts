mod expr;
mod node;
mod node_flags;
mod stmt;
mod ty;

pub mod visitor;

pub use expr::*;
pub use stmt::*;
pub use ty::*;
pub use visitor::Visitor;

use bolt_ts_span::Span;
pub use node::{Node, NodeID};
pub use node_flags::NodeFlags;

#[derive(Debug, Clone, Copy)]
pub struct Program<'cx> {
    pub id: NodeID,
    pub stmts: Stmts<'cx>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct EntityName<'cx> {
    pub kind: EntityNameKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum EntityNameKind<'cx> {
    Ident(&'cx Ident),
    Qualified(&'cx QualifiedName<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct QualifiedName<'cx> {
    pub id: NodeID,
    pub left: &'cx EntityName<'cx>,
    pub right: &'cx Ident,
}
