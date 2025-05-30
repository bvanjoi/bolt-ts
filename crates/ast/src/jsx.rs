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
    Expr(&'cx JsxExpr<'cx>),
    Ele(&'cx JsxEle<'cx>),
    SelfClosingEle(&'cx JsxSelfClosingEle<'cx>),
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
pub enum JsxTagNameExpr<'cx> {
    Ident(&'cx super::Ident),
    This(&'cx super::ThisExpr),
    Ns(&'cx JsxNsName<'cx>),
    /// only contains `Ident`/`This`/`Ns`/`PropAccess`
    PropAccess(&'cx super::PropAccessExpr<'cx>),
}

impl JsxTagNameExpr<'_> {
    pub fn id(&self) -> NodeID {
        match self {
            JsxTagNameExpr::Ident(n) => n.id,
            JsxTagNameExpr::This(n) => n.id,
            JsxTagNameExpr::Ns(n) => n.id,
            JsxTagNameExpr::PropAccess(n) => n.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct JsxExpr<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub dotdotdot_token: Option<Span>,
    pub expr: Option<&'cx super::Expr<'cx>>,
}

pub type JsxAttrs<'cx> = &'cx [JsxAttr<'cx>];

#[derive(Debug, Clone, Copy)]
pub struct JsxOpeningEle<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tag_name: JsxTagNameExpr<'cx>,
    pub ty_args: Option<&'cx super::Tys<'cx>>,
    pub attrs: JsxAttrs<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct JsxClosingEle<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tag_name: JsxTagNameExpr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct JsxOpeningFrag {
    pub id: NodeID,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct JsxEle<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub opening_ele: &'cx JsxOpeningEle<'cx>,
    pub children: &'cx [JsxChild<'cx>],
    pub closing_ele: &'cx JsxClosingEle<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum JsxChild<'cx> {
    Text(&'cx JsxText),
    Expr(&'cx JsxExpr<'cx>),
    Ele(&'cx JsxEle<'cx>),
    SelfClosingEle(&'cx JsxSelfClosingEle<'cx>),
    Frag(&'cx JsxFrag<'cx>),
}

impl<'cx> JsxChild<'cx> {
    pub fn span(&self) -> Span {
        match self {
            JsxChild::Text(n) => n.span,
            JsxChild::Expr(n) => n.span,
            JsxChild::Ele(n) => n.span,
            JsxChild::SelfClosingEle(n) => n.span,
            JsxChild::Frag(n) => n.span,
        }
    }

    pub fn id(&self) -> NodeID {
        match self {
            JsxChild::Text(n) => n.id,
            JsxChild::Expr(n) => n.id,
            JsxChild::Ele(n) => n.id,
            JsxChild::SelfClosingEle(n) => n.id,
            JsxChild::Frag(n) => n.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct JsxFrag<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub opening_ele: &'cx JsxOpeningFrag,
    pub children: &'cx [JsxChild<'cx>],
    pub closing_ele: &'cx JsxClosingFrag,
}

#[derive(Debug, Clone, Copy)]
pub struct JsxSelfClosingEle<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tag_name: JsxTagNameExpr<'cx>,
    pub ty_args: Option<&'cx super::Tys<'cx>>,
    pub attrs: JsxAttrs<'cx>,
}
