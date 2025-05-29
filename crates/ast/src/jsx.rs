use bolt_ts_atom::AtomId;
use bolt_ts_span::Span;

use crate::NodeID;

#[derive(Debug, Clone, Copy)]
pub struct JsxText {
    pub id: NodeID,
    pub span: Span,
    pub text: AtomId,
    pub contains_only_trivia_whitespace: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct JsxClosingFrag {
    pub id: NodeID,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct JsxSpreadAttr<'cx> {
    pub id: NodeID,
    pub expr: &'cx super::Expr<'cx>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum JsxAttrName<'cx> {
    Ident(&'cx super::Ident),
    Ns(&'cx JsxNsName<'cx>),
}

impl JsxAttrName<'_> {
    pub fn id(&self) -> NodeID {
        match self {
            JsxAttrName::Ident(n) => n.id,
            JsxAttrName::Ns(n) => n.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct JsxNsName<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ns: &'cx super::Ident,
    pub name: &'cx super::Ident,
}

#[derive(Debug, Clone, Copy)]
pub enum JsxAttrValue<'cx> {
    StringLit(&'cx super::StringLit),
    JsxExpr(&'cx JsxExpr<'cx>),
}

impl JsxAttrValue<'_> {
    pub fn id(&self) -> NodeID {
        match self {
            JsxAttrValue::StringLit(n) => n.id,
            JsxAttrValue::JsxExpr(n) => n.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum JsxAttr<'cx> {
    Spread(&'cx JsxSpreadAttr<'cx>),
    Named(&'cx JsxNamedAttr<'cx>),
}

impl JsxAttr<'_> {
    pub fn id(&self) -> NodeID {
        match self {
            JsxAttr::Spread(n) => n.id,
            JsxAttr::Named(n) => n.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct JsxNamedAttr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: JsxAttrName<'cx>,
    pub init: Option<JsxAttrValue<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub enum JsxTagName<'cx> {
    Ident(&'cx super::Ident),
    This(&'cx super::ThisExpr),
    Ns(&'cx JsxNsName<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct JsxExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub dotdotdot_token: Option<Span>,
    pub expr: Option<&'cx super::Expr<'cx>>,
}

pub type JsxAttrs<'cx> = &'cx [JsxAttr<'cx>];
