use bolt_ts_atom::Atom;
use bolt_ts_span::Span;

use crate::NodeID;

#[derive(Debug, Clone)]
pub struct JsxText {
    pub id: NodeID,
    pub span: Span,
    pub text: Atom,
    pub contains_only_trivia_whitespace: bool,
}

#[derive(Debug, Clone)]
pub struct JsxClosingFrag {
    pub id: NodeID,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct JsxSpreadAttr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx super::Expr<'cx>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct JsxNsName<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ns: &'cx super::Ident,
    pub name: &'cx super::Ident,
}

#[derive(Debug, Clone, Copy)]
pub enum JsxAttrValue<'cx> {
    StringLit(&'cx super::StringLit),
    Expr(&'cx JsxExpr<'cx>),
    Ele(&'cx JsxElem<'cx>),
    SelfClosingEle(&'cx JsxSelfClosingElem<'cx>),
    Frag(&'cx JsxFrag<'cx>),
}

impl JsxAttrValue<'_> {
    pub fn id(&self) -> NodeID {
        match self {
            JsxAttrValue::StringLit(n) => n.id,
            JsxAttrValue::Expr(n) => n.id,
            JsxAttrValue::Ele(n) => n.id,
            JsxAttrValue::SelfClosingEle(n) => n.id,
            JsxAttrValue::Frag(n) => n.id,
        }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
    /// only contains `Ident`/`This`/`Ns`/`PropAccess`
    PropAccess(&'cx super::PropAccessExpr<'cx>),
}

impl JsxTagName<'_> {
    pub fn id(&self) -> NodeID {
        match self {
            JsxTagName::Ident(n) => n.id,
            JsxTagName::This(n) => n.id,
            JsxTagName::Ns(n) => n.id,
            JsxTagName::PropAccess(n) => n.id,
        }
    }
    pub fn span(&self) -> Span {
        match self {
            JsxTagName::Ident(n) => n.span,
            JsxTagName::This(n) => n.span,
            JsxTagName::Ns(n) => n.span,
            JsxTagName::PropAccess(n) => n.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct JsxExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub dotdotdot_token: Option<Span>,
    pub expr: Option<&'cx super::Expr<'cx>>,
}

pub type JsxAttrs<'cx> = &'cx [JsxAttr<'cx>];

#[derive(Debug, Clone)]
pub struct JsxOpeningElem<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tag_name: JsxTagName<'cx>,
    pub ty_args: Option<&'cx super::Tys<'cx>>,
    pub attrs: JsxAttrs<'cx>,
}

#[derive(Debug, Clone)]
pub struct JsxClosingElem<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tag_name: JsxTagName<'cx>,
}

#[derive(Debug, Clone)]
pub struct JsxOpeningFrag {
    pub id: NodeID,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct JsxElem<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub opening_elem: &'cx JsxOpeningElem<'cx>,
    pub children: &'cx [JsxChild<'cx>],
    pub closing_elem: &'cx JsxClosingElem<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum JsxChild<'cx> {
    Text(&'cx JsxText),
    Expr(&'cx JsxExpr<'cx>),
    Elem(&'cx JsxElem<'cx>),
    SelfClosingEle(&'cx JsxSelfClosingElem<'cx>),
    Frag(&'cx JsxFrag<'cx>),
}

impl<'cx> JsxChild<'cx> {
    pub fn span(&self) -> Span {
        match self {
            JsxChild::Text(n) => n.span,
            JsxChild::Expr(n) => n.span,
            JsxChild::Elem(n) => n.span,
            JsxChild::SelfClosingEle(n) => n.span,
            JsxChild::Frag(n) => n.span,
        }
    }

    pub fn id(&self) -> NodeID {
        match self {
            JsxChild::Text(n) => n.id,
            JsxChild::Expr(n) => n.id,
            JsxChild::Elem(n) => n.id,
            JsxChild::SelfClosingEle(n) => n.id,
            JsxChild::Frag(n) => n.id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct JsxFrag<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub opening_frag: &'cx JsxOpeningFrag,
    pub children: &'cx [JsxChild<'cx>],
    pub closing_frag: &'cx JsxClosingFrag,
}

#[derive(Debug, Clone)]
pub struct JsxSelfClosingElem<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tag_name: JsxTagName<'cx>,
    pub ty_args: Option<&'cx super::Tys<'cx>>,
    pub attrs: JsxAttrs<'cx>,
}
