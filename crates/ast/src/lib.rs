mod expr;
pub mod keyword;
mod node;
mod node_flags;
mod pprint;
mod stmt;
mod token;
mod ty;

pub mod visitor;

pub use expr::*;
pub use pprint::*;
pub use stmt::*;
pub use token::*;
pub use ty::*;
pub use visitor::Visitor;

use bolt_ts_span::Span;
pub use node::{FnFlags, Node, NodeID};
pub use node_flags::NodeFlags;

#[derive(Debug, Clone, Copy)]
pub struct Program<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub stmts: Stmts<'cx>,
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

    pub fn id(&self) -> NodeID {
        use EntityNameKind::*;
        match self.kind {
            Ident(ident) => ident.id,
            Qualified(name) => name.id,
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
