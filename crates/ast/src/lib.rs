mod expr;
mod jsx;
pub mod keyword;
mod node;
mod node_flags;
mod pprint;
mod stmt;
mod token;
pub mod r#trait;
mod ty;

pub mod visitor;

pub use self::expr::*;
pub use self::jsx::*;
pub use self::node::{FnFlags, Node, NodeID};
pub use self::node_flags::NodeFlags;
pub use self::pprint::*;
pub use self::stmt::*;
pub use self::token::*;
pub use self::ty::*;
pub use self::visitor::Visitor;

use bolt_ts_span::Span;

#[derive(Debug, Clone)]
pub struct Program<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub stmts: Stmts<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum DeclarationName<'cx> {
    Ident(&'cx Ident),
    PrivateIdent(&'cx PrivateIdent),
    NumLit(&'cx NumLit),
    StringLit {
        raw: &'cx StringLit,
        key: bolt_ts_atom::Atom,
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
            PrivateIdent(n) => DeclarationName::PrivateIdent(n),
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
            Computed(n) => {
                !n.expr.is_string_or_number_lit_like() && !n.expr.is_signed_numeric_lit()
            }
            // TODO: element access
            _ => false,
        }
    }

    pub fn is_late_bindable_ast(&self) -> bool {
        use DeclarationName::*;
        match self {
            Computed(n) => n.expr.is_entity_name_expr(),
            // TODO: element access
            _ => false,
        }
    }

    pub fn span(&self) -> Span {
        use DeclarationName::*;
        match self {
            Ident(n) => n.span,
            NumLit(n) => n.span,
            StringLit { raw, .. } => raw.span,
            Computed(n) => n.span,
            PrivateIdent(n) => n.span,
        }
    }

    pub fn to_string(&self, atoms: &bolt_ts_atom::AtomIntern) -> String {
        use DeclarationName::*;
        match self {
            Ident(n) => atoms.get(n.name).to_string(),
            NumLit(n) => n.val.to_string(),
            StringLit { raw, .. } => atoms.get(raw.val).to_string(),
            Computed(n) => todo!(),
            PrivateIdent(n) => todo!(),
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
