use super::*;
use crate::keyword;

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

impl<'cx> Ty<'cx> {
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
            TyKind::Paren(n) => n.span,
            TyKind::Infer(n) => n.span,
            TyKind::Intrinsic(n) => n.span,
            TyKind::Nullable(n) => n.span,
            TyKind::NamedTuple(n) => n.span,
            TyKind::TemplateLit(n) => n.span,
            TyKind::This(n) => n.span,
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
            TyKind::Paren(n) => n.id,
            TyKind::Infer(n) => n.id,
            TyKind::Nullable(n) => n.id,
            TyKind::NamedTuple(n) => n.id,
            TyKind::TemplateLit(n) => n.id,
            TyKind::Intrinsic(n) => n.id,
            TyKind::This(n) => n.id,
        }
    }

    pub fn skip_ty_parens(&self) -> &Ty {
        self
    }

    pub fn is_simple_tuple_ty(&self) -> bool {
        let TyKind::Tuple(tuple) = self.kind else {
            return false;
        };
        !tuple.tys.is_empty() && !tuple.tys.iter().any(|e| matches!(e.kind, TyKind::Rest(..)))
    }

    pub fn is_const_ty_refer(&self) -> bool {
        let TyKind::Refer(refer) = self.kind else {
            return false;
        };
        let EntityNameKind::Ident(name) = refer.name.kind else {
            return false;
        };
        name.name == keyword::KW_CONST && refer.ty_args.is_none()
    }

    pub fn as_unary_tuple_ty(&'cx self) -> Option<&'cx Ty<'cx>> {
        let TyKind::Tuple(tuple) = self.kind else {
            return None;
        };
        (tuple.tys.len() == 1).then(|| tuple.tys[0])
    }

    pub fn is_this_less(&self) -> bool {
        use TyKind::*;
        match self.kind {
            Lit(_) => true,
            Array(arr) => arr.ele.is_this_less(),
            Refer(refer) => {
                if let ty::EntityNameKind::Ident(n) = refer.name.kind
                    && matches!(
                        n.name,
                        keyword::IDENT_ANY
                            | keyword::IDENT_UNKNOWN
                            | keyword::IDENT_STRING
                            | keyword::IDENT_NUMBER
                            | keyword::IDENT_BIGINT
                            | keyword::IDENT_BOOLEAN
                            | keyword::IDENT_SYMBOL
                            | keyword::IDENT_OBJECT
                            | keyword::IDENT_NEVER
                    )
                {
                    return true;
                }
                refer
                    .ty_args
                    .is_none_or(|ty_args| !ty_args.list.iter().all(|ty| ty.is_this_less()))
            }
            _ => false,
        }
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
    NamedTuple(&'cx NamedTupleTy<'cx>),
    Tuple(&'cx TupleTy<'cx>),
    Rest(&'cx RestTy<'cx>),
    Cond(&'cx CondTy<'cx>),
    Union(&'cx UnionTy<'cx>),
    Intersection(&'cx IntersectionTy<'cx>),
    Typeof(&'cx TypeofTy<'cx>),
    Mapped(&'cx MappedTy<'cx>),
    TyOp(&'cx TyOp<'cx>),
    Pred(&'cx PredTy<'cx>),
    Paren(&'cx ParenTy<'cx>),
    Infer(&'cx InferTy<'cx>),
    Intrinsic(&'cx IntrinsicTy),
    Nullable(&'cx NullableTy<'cx>),
    TemplateLit(&'cx TemplateLitTy<'cx>),
    This(&'cx ThisTy),
}

impl TyKind<'_> {
    pub fn is_lit(&self) -> bool {
        matches!(self, TyKind::Lit(_))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ThisTy {
    pub id: NodeID,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct TemplateLitTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub head: &'cx TemplateHead,
    pub spans: &'cx [&'cx TemplateSpanTy<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct TemplateSpanTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx Ty<'cx>,
    pub text: AtomId,
    pub is_tail: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct NullableTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct IntrinsicTy {
    pub id: NodeID,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct ParenTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx Ty<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub enum PredTyName<'cx> {
    Ident(&'cx Ident),
    This(&'cx ThisTy),
}
#[derive(Debug, Clone, Copy)]
pub struct PredTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub asserts: Option<Span>,
    pub name: PredTyName<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeofTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx EntityName<'cx>,
    pub ty_args: Option<&'cx self::Tys<'cx>>,
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
    BigInt { neg: bool, val: AtomId },
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
pub struct NamedTupleTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub dotdotdot: Option<Span>,
    pub name: &'cx Ident,
    pub question: Option<Span>,
    pub ty: &'cx Ty<'cx>,
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
            PropNameKind::Computed(n) => n.span,
        }
    }

    pub fn id(&self) -> NodeID {
        match self.kind {
            PropNameKind::Ident(ident) => ident.id,
            PropNameKind::NumLit(num) => num.id,
            PropNameKind::StringLit { raw, .. } => raw.id,
            PropNameKind::Computed(n) => n.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PropNameKind<'cx> {
    Ident(&'cx Ident),
    StringLit { raw: &'cx StringLit, key: AtomId },
    NumLit(&'cx NumLit),
    Computed(&'cx ComputedPropName<'cx>),
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
pub struct ComputedPropName<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
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
            ObjectMemberKind::SpreadAssignment(n) => n.span,
        }
    }

    pub fn id(&self) -> NodeID {
        match self.kind {
            ObjectMemberKind::Shorthand(n) => n.id,
            ObjectMemberKind::Prop(n) => n.id,
            ObjectMemberKind::Method(n) => n.id,
            ObjectMemberKind::SpreadAssignment(n) => n.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ObjectMemberKind<'cx> {
    Shorthand(&'cx ObjectShorthandMember<'cx>),
    Prop(&'cx ObjectPropMember<'cx>),
    Method(&'cx ObjectMethodMember<'cx>),
    SpreadAssignment(&'cx SpreadAssignment<'cx>),
}

#[derive(Debug, Clone, Copy)]
pub struct SpreadAssignment<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
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
    pub init: &'cx Expr<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectLit<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub members: &'cx [&'cx ObjectMember<'cx>],
}

#[derive(Debug, Clone, Copy)]
pub struct MappedTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_param: &'cx self::TyParam<'cx>,
    pub readonly_token: Option<Token>,
    pub name_ty: Option<&'cx Ty<'cx>>,
    pub question_token: Option<Token>,
    pub ty: Option<&'cx Ty<'cx>>,
    pub members: &'cx [&'cx ObjectTyMember<'cx>],
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct MappedTyModifiers: u8 {
        const INCLUDE_READONLY  = 1 << 0;
        const EXCLUDE_READONLY  = 1 << 1;
        const INCLUDE_OPTIONAL  = 1 << 2;
        const EXCLUDE_OPTIONAL  = 1 << 3;
    }
}

impl MappedTy<'_> {
    pub fn get_modifiers(&self) -> MappedTyModifiers {
        (if let Some(r) = self.readonly_token {
            if r.kind == TokenKind::Minus {
                MappedTyModifiers::EXCLUDE_READONLY
            } else {
                MappedTyModifiers::INCLUDE_READONLY
            }
        } else {
            MappedTyModifiers::empty()
        }) | (if let Some(q) = self.question_token {
            if q.kind == TokenKind::Minus {
                MappedTyModifiers::EXCLUDE_OPTIONAL
            } else {
                MappedTyModifiers::INCLUDE_OPTIONAL
            }
        } else {
            MappedTyModifiers::empty()
        })
    }

    pub fn get_modified_readonly_state(state: bool, modifiers: MappedTyModifiers) -> bool {
        if modifiers.contains(MappedTyModifiers::INCLUDE_READONLY) {
            true
        } else if modifiers.contains(MappedTyModifiers::EXCLUDE_READONLY) {
            false
        } else {
            state
        }
    }
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

#[derive(Debug, Clone, Copy)]
pub struct InferTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_param: &'cx TyParam<'cx>,
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

/// ```txt
/// type A = B.C;
///          ~~~
/// ```
#[derive(Debug, Clone, Copy)]
pub struct QualifiedName<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub left: &'cx EntityName<'cx>,
    pub right: &'cx Ident,
}
