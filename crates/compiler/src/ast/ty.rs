use super::*;
use bolt_ts_atom::AtomId;

#[derive(Debug, Clone, Copy)]
pub struct Ty<'cx> {
    pub kind: TyKind<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct Tys<'cx> {
    pub span: Span,
    pub list: &'cx [&'cx Ty<'cx>],
}

impl Ty<'_> {
    pub fn span(&self) -> Span {
        match self.kind {
            TyKind::Array(array) => array.span,
            TyKind::Fn(f) => f.span,
            TyKind::ObjectLit(lit) => lit.span,
            TyKind::Tuple(tuple) => tuple.span,
            TyKind::Rest(rest) => rest.span,
            TyKind::IndexedAccess(n) => n.span,
            TyKind::Cond(n) => n.span,
            TyKind::Refer(n) => n.span,
            TyKind::Union(n) => n.span,
            TyKind::Intersection(n) => n.span,
            TyKind::Typeof(n) => n.span,
            TyKind::Mapped(n) => n.span,
            TyKind::TyOp(n) => n.span,
            TyKind::Ctor(n) => n.span,
            TyKind::Pred(n) => n.span,
            TyKind::Lit(n) => n.span,
        }
    }

    pub fn id(&self) -> NodeID {
        match self.kind {
            TyKind::Array(node) => node.id,
            TyKind::Fn(node) => node.id,
            TyKind::ObjectLit(node) => node.id,
            TyKind::Tuple(tuple) => tuple.id,
            TyKind::Rest(rest) => rest.id,
            TyKind::IndexedAccess(n) => n.id,
            TyKind::Cond(n) => n.id,
            TyKind::Refer(n) => n.id,
            TyKind::Union(n) => n.id,
            TyKind::Intersection(n) => n.id,
            TyKind::Typeof(n) => n.id,
            TyKind::Mapped(n) => n.id,
            TyKind::TyOp(n) => n.id,
            TyKind::Ctor(n) => n.id,
            TyKind::Pred(n) => n.id,
            TyKind::Lit(n) => n.id,
        }
    }

    pub fn skip_ty_parens(&self) -> &Ty {
        match self.kind {
            _ => self,
        }
    }

    pub fn is_simple_tuple_ty(&self) -> bool {
        let TyKind::Tuple(tuple) = self.kind else {
            return false;
        };
        !tuple.tys.is_empty() && !tuple.tys.iter().any(|e| matches!(e.kind, TyKind::Rest(..)))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TyKind<'cx> {
    Refer(&'cx ReferTy<'cx>),
    Array(&'cx ArrayTy<'cx>),
    IndexedAccess(&'cx IndexedAccessTy<'cx>),
    Fn(&'cx FnTy<'cx>),
    Ctor(&'cx CtorTy<'cx>),
    ObjectLit(&'cx ObjectLitTy<'cx>),
    Lit(&'cx LitTy),
    Tuple(&'cx TupleTy<'cx>),
    Rest(&'cx RestTy<'cx>),
    Cond(&'cx CondTy<'cx>),
    Union(&'cx UnionTy<'cx>),
    Intersection(&'cx IntersectionTy<'cx>),
    Typeof(&'cx TypeofTy<'cx>),
    Mapped(&'cx MappedTy<'cx>),
    TyOp(&'cx TyOp<'cx>),
    Pred(&'cx PredTy<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct PredTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Ident,
    pub ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeofTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx EntityName<'cx>,
    pub args: Option<&'cx self::Tys<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct LitTy {
    pub id: NodeID,
    pub span: Span,
    pub kind: LitTyKind,
}

#[derive(Debug, Clone, Copy)]
pub enum LitTyKind {
    Null,
    True,
    False,
    Undefined,
    Void,
    Num(f64),
    String(AtomId),
}

#[derive(Debug, Clone, Copy)]
pub struct UnionTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tys: &'cx [&'cx Ty<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct IntersectionTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tys: &'cx [&'cx Ty<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct ReferTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx EntityName<'cx>,
    pub ty_args: Option<&'cx Tys<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CondTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub check_ty: &'cx self::Ty<'cx>,
    pub extends_ty: &'cx self::Ty<'cx>,
    pub true_ty: &'cx self::Ty<'cx>,
    pub false_ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct RestTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct TupleTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tys: &'cx [&'cx Ty<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct CtorSigDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CallSigDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectLitTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub members: ObjectTyMembers<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct FnTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct CtorTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct IndexedAccessTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx self::Ty<'cx>,
    pub index_ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ArrayTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ele: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct PropName<'cx> {
    pub kind: PropNameKind<'cx>,
}

impl PropName<'_> {
    pub fn span(&self) -> Span {
        match self.kind {
            PropNameKind::Ident(ident) => ident.span,
            PropNameKind::NumLit(num) => num.span,
            PropNameKind::StringLit { raw, .. } => raw.span,
        }
    }

    pub fn id(&self) -> NodeID {
        match self.kind {
            PropNameKind::Ident(ident) => ident.id,
            PropNameKind::NumLit(num) => num.id,
            PropNameKind::StringLit { raw, .. } => raw.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PropNameKind<'cx> {
    Ident(&'cx Ident),
    StringLit { raw: &'cx StringLit, key: AtomId },
    NumLit(&'cx NumLit),
}

impl PropNameKind<'_> {
    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            PropNameKind::Ident(ident) => Some(ident),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectMember<'cx> {
    pub kind: ObjectMemberKind<'cx>,
}

impl ObjectMember<'_> {
    pub fn span(&self) -> Span {
        match self.kind {
            ObjectMemberKind::Shorthand(n) => n.span,
            ObjectMemberKind::Prop(n) => n.span,
            ObjectMemberKind::Method(n) => n.span,
        }
    }

    pub fn id(&self) -> NodeID {
        match self.kind {
            ObjectMemberKind::Shorthand(n) => n.id,
            ObjectMemberKind::Prop(n) => n.id,
            ObjectMemberKind::Method(n) => n.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ObjectMemberKind<'cx> {
    Shorthand(&'cx ObjectShorthandMember<'cx>),
    Prop(&'cx ObjectPropMember<'cx>),
    Method(&'cx ObjectMethodMember<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectMethodMember<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx PropName<'cx>,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx Ty<'cx>>,
    pub body: &'cx BlockStmt<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectShorthandMember<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Ident,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectPropMember<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx PropName<'cx>,
    pub value: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectLit<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub members: &'cx [&'cx ObjectMember<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct MappedTyParam<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Ident,
    pub constraint: &'cx Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct MappedTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_param: &'cx MappedTyParam<'cx>,
    pub readonly_token: Option<Span>,
    pub name_ty: Option<&'cx Ty<'cx>>,
    pub question_token: Option<Span>,
    pub ty: Option<&'cx Ty<'cx>>,
    pub members: &'cx [&'cx ObjectTyMember<'cx>],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TyOpKind {
    Keyof,
    Readonly,
    Unique,
}

#[derive(Debug, Clone, Copy)]
pub struct TyOp<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub op: TyOpKind,
    pub ty: &'cx Ty<'cx>,
}
