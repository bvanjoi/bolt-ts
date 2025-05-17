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

#[derive(Debug, Clone, Copy)]
pub enum DeclarationName<'cx> {
    Ident(&'cx Ident),
    NumLit(&'cx NumLit),
    StringLit {
        raw: &'cx StringLit,
        key: bolt_ts_atom::AtomId,
    },
    Computed(&'cx ComputedPropName<'cx>),
}

impl<'cx> DeclarationName<'cx> {
    pub fn from_prop_name(n: &'cx PropName<'cx>) -> Self {
        use PropNameKind::*;
        match n.kind {
            Ident(n) => DeclarationName::Ident(n),
            StringLit { raw, key } => DeclarationName::StringLit { raw, key },
            NumLit(n) => DeclarationName::NumLit(n),
            Computed(n) => DeclarationName::Computed(n),
        }
    }

    pub fn from_object_binding_name(n: &'cx ObjectBindingName<'cx>) -> Option<Self> {
        use ObjectBindingName::*;
        match n {
            Shorthand(n) => Some(DeclarationName::Ident(n)),
            Prop { name, .. } => DeclarationName::from_binding(name),
        }
    }

    pub fn from_binding(n: &'cx Binding<'cx>) -> Option<Self> {
        use BindingKind::*;
        match n.kind {
            Ident(n) => Some(DeclarationName::Ident(n)),
            ObjectPat(_) => None,
            ArrayPat(_) => None,
        }
    }

    pub fn is_dynamic_name(&self) -> bool {
        use DeclarationName::*;
        match self {
            Computed(_) => true,
            // TODO: element access
            _ => false,
        }
    }

    pub fn is_late_bindable_ast(&self) -> bool {
        use DeclarationName::*;
        let expr = match self {
            Computed(n) => n.expr,
            // TODO: element access
            _ => return false,
        };
        expr.is_entity_name_expr()
    }

    pub fn span(&self) -> Span {
        use DeclarationName::*;
        match self {
            Ident(n) => n.span,
            NumLit(n) => n.span,
            StringLit { raw, .. } => raw.span,
            Computed(n) => n.span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SingleLineComment {
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct MultiLineComment {
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum Comment {
    SingleLine(SingleLineComment),
    MultiLine(MultiLineComment),
}

impl Comment {
    pub fn span(&self) -> Span {
        match self {
            Comment::SingleLine(c) => c.span,
            Comment::MultiLine(c) => c.span,
        }
    }
}
