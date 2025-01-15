mod expr;
mod node;
mod node_flags;
mod pprint;
mod stmt;
mod ty;

pub mod visitor;

pub use expr::*;
pub use pprint::*;
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

impl EntityName<'_> {
    pub fn span(&self) -> Span {
        use EntityNameKind::*;
        match self.kind {
            Ident(ident) => ident.span,
            Qualified(name) => name.span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EntityNameKind<'cx> {
    Ident(&'cx Ident),
    Qualified(&'cx QualifiedName<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct QualifiedName<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub left: &'cx EntityName<'cx>,
    pub right: &'cx Ident,
}
