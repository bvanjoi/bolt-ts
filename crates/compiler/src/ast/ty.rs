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
            TyKind::NumLit(num) => num.span,
            TyKind::StringLit(s) => s.span,
            TyKind::Tuple(tuple) => tuple.span,
            TyKind::Rest(rest) => rest.span,
            TyKind::IndexedAccess(n) => n.span,
            TyKind::Cond(n) => n.span,
            TyKind::Refer(n) => n.span,
            TyKind::Union(n) => n.span,
            TyKind::Intersection(n) => n.span,
            TyKind::BooleanLit(n) => n.span,
            TyKind::NullLit(n) => n.span,
            TyKind::Typeof(n) => n.span,
        }
    }

    pub fn id(&self) -> NodeID {
        match self.kind {
            TyKind::Array(node) => node.id,
            TyKind::Fn(node) => node.id,
            TyKind::ObjectLit(node) => node.id,
            TyKind::NumLit(num) => num.id,
            TyKind::StringLit(s) => s.id,
            TyKind::Tuple(tuple) => tuple.id,
            TyKind::Rest(rest) => rest.id,
            TyKind::IndexedAccess(n) => n.id,
            TyKind::Cond(n) => n.id,
            TyKind::Refer(n) => n.id,
            TyKind::Union(n) => n.id,
            TyKind::Intersection(n) => n.id,
            TyKind::BooleanLit(n) => n.id,
            TyKind::NullLit(n) => n.id,
            TyKind::Typeof(n) => n.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TyKind<'cx> {
    Refer(&'cx ReferTy<'cx>),
    Array(&'cx ArrayTy<'cx>),
    IndexedAccess(&'cx IndexedAccessTy<'cx>),
    Fn(&'cx FnTy<'cx>),
    ObjectLit(&'cx ObjectLitTy<'cx>),
    NumLit(&'cx NumLitTy),
    BooleanLit(&'cx BoolLitTy),
    NullLit(&'cx NullLitTy),
    StringLit(&'cx StringLitTy),
    Tuple(&'cx TupleTy<'cx>),
    Rest(&'cx RestTy<'cx>),
    Cond(&'cx CondTy<'cx>),
    Union(&'cx UnionTy<'cx>),
    Intersection(&'cx IntersectionTy<'cx>),
    Typeof(&'cx TypeofTy<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct TypeofTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx EntityName<'cx>,
    pub args: Option<&'cx self::Tys<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct LitTy<T> {
    pub id: NodeID,
    pub span: Span,
    pub val: T,
}

pub type NumLitTy = LitTy<f64>;
pub type BoolLitTy = LitTy<bool>;
pub type NullLitTy = LitTy<()>;
pub type StringLitTy = LitTy<AtomId>;

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
            PropNameKind::StringLit(lit) => lit.span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PropNameKind<'cx> {
    Ident(&'cx Ident),
    StringLit(&'cx StringLit),
    NumLit(&'cx NumLit),
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectMemberField<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx PropName<'cx>,
    pub value: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectLit<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub members: &'cx [&'cx ObjectMemberField<'cx>],
}
