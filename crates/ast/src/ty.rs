use super::keyword;
use super::*;

use bolt_ts_atom::{Atom, AtomIntern};

#[derive(Debug, Clone)]
pub struct Ty<'cx> {
    pub kind: TyKind<'cx>,
}

#[derive(Debug, Clone)]
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

    pub fn skip_ty_parens(&'cx self) -> &'cx Ty<'cx> {
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct ThisTy {
    pub id: NodeID,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TemplateLitTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub head: &'cx TemplateHead,
    pub spans: &'cx [&'cx TemplateSpanTy<'cx>],
}

#[derive(Debug, Clone)]
pub struct TemplateSpanTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx Ty<'cx>,
    pub text: Atom,
    pub is_tail: bool,
}

#[derive(Debug, Clone)]
pub struct NullableTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx Ty<'cx>,
}

#[derive(Debug, Clone)]
pub struct IntrinsicTy {
    pub id: NodeID,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ParenTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx Ty<'cx>,
}

#[derive(Debug, Clone)]
pub enum PredTyName<'cx> {
    Ident(&'cx Ident),
    This(&'cx ThisTy),
}
#[derive(Debug, Clone)]
pub struct PredTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub asserts: Option<Span>,
    pub name: PredTyName<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
}

#[derive(Debug, Clone)]
pub struct TypeofTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx EntityName<'cx>,
    pub ty_args: Option<&'cx self::Tys<'cx>>,
}

#[derive(Debug, Clone)]
pub struct LitTy {
    pub id: NodeID,
    pub span: Span,
    pub kind: LitTyKind,
}

#[derive(Debug, Clone)]
pub enum LitTyKind {
    Null,
    True,
    False,
    Undefined,
    Void,
    Num(f64),
    String(Atom),
    BigInt { neg: bool, val: Atom },
}

#[derive(Debug, Clone)]
pub struct UnionTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tys: &'cx [&'cx Ty<'cx>],
}

#[derive(Debug, Clone)]
pub struct IntersectionTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tys: &'cx [&'cx Ty<'cx>],
}

#[derive(Debug, Clone)]
pub struct ReferTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx EntityName<'cx>,
    pub ty_args: Option<&'cx Tys<'cx>>,
}

#[derive(Debug, Clone)]
pub struct CondTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub check_ty: &'cx self::Ty<'cx>,
    pub extends_ty: &'cx self::Ty<'cx>,
    pub true_ty: &'cx self::Ty<'cx>,
    pub false_ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone)]
pub struct RestTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone)]
pub struct NamedTupleTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub dotdotdot: Option<Span>,
    pub name: &'cx Ident,
    pub question: Option<Span>,
    pub ty: &'cx Ty<'cx>,
}

#[derive(Debug, Clone)]
pub struct TupleTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub tys: &'cx [&'cx Ty<'cx>],
}

#[derive(Debug, Clone)]
pub struct CtorSigDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
}

#[derive(Debug, Clone)]
pub struct CallSigDecl<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx self::Ty<'cx>>,
}

#[derive(Debug, Clone)]
pub struct ObjectLitTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub members: ObjectTyMembers<'cx>,
}

/// ```txt
/// type A = (value: number) => string;
///          ~~~~~~~~~~~~~~~~~~~~~~~~~
/// ```
#[derive(Debug, Clone)]
pub struct FnTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: &'cx self::Ty<'cx>,
}

/// ```txt
/// type A = new (value: number) => string;
///          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// ```
#[derive(Debug, Clone)]
pub struct CtorTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub modifiers: Option<&'cx Modifiers<'cx>>,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone)]
pub struct IndexedAccessTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty: &'cx self::Ty<'cx>,
    pub index_ty: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone)]
pub struct ArrayTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ele: &'cx self::Ty<'cx>,
}

#[derive(Debug, Clone)]
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
            PropNameKind::PrivateIdent(ident) => ident.span,
        }
    }

    pub fn id(&self) -> NodeID {
        match self.kind {
            PropNameKind::Ident(ident) => ident.id,
            PropNameKind::NumLit(num) => num.id,
            PropNameKind::StringLit { raw, .. } => raw.id,
            PropNameKind::Computed(n) => n.id,
            PropNameKind::PrivateIdent(ident) => ident.id,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PropNameKind<'cx> {
    Ident(&'cx Ident),
    PrivateIdent(&'cx PrivateIdent),
    StringLit { raw: &'cx StringLit, key: Atom },
    NumLit(&'cx NumLit),
    Computed(&'cx ComputedPropName<'cx>),
}

impl<'cx> PropNameKind<'cx> {
    pub fn as_ident(&self) -> Option<&'cx Ident> {
        match self {
            PropNameKind::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn as_computed(&self) -> Option<&'cx ComputedPropName<'cx>> {
        match self {
            PropNameKind::Computed(n) => Some(n),
            _ => None,
        }
    }

    pub fn get_name(&self, atoms: &mut AtomIntern) -> Option<Atom> {
        match self {
            PropNameKind::Ident(ident) => Some(ident.name),
            PropNameKind::StringLit { raw, .. } => Some(raw.val),
            PropNameKind::NumLit(lit) => Some(atoms.atom(&lit.val.to_string())),
            PropNameKind::Computed(n) => match n.expr.kind {
                super::ExprKind::StringLit(s) => Some(s.val),
                super::ExprKind::NumLit(n) => Some(atoms.atom(&n.val.to_string())),
                _ => None,
            },
            PropNameKind::PrivateIdent(private_ident) => Some(private_ident.name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ComputedPropName<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
}

#[derive(Debug, Clone)]
pub struct ObjectMember<'cx> {
    pub kind: ObjectMemberKind<'cx>,
}

impl ObjectMember<'_> {
    pub fn span(&self) -> Span {
        match self.kind {
            ObjectMemberKind::Shorthand(n) => n.span,
            ObjectMemberKind::PropAssignment(n) => n.span,
            ObjectMemberKind::Method(n) => n.span,
            ObjectMemberKind::SpreadAssignment(n) => n.span,
            ObjectMemberKind::Getter(n) => n.span,
            ObjectMemberKind::Setter(n) => n.span,
        }
    }

    pub fn id(&self) -> NodeID {
        match self.kind {
            ObjectMemberKind::Shorthand(n) => n.id,
            ObjectMemberKind::PropAssignment(n) => n.id,
            ObjectMemberKind::Method(n) => n.id,
            ObjectMemberKind::SpreadAssignment(n) => n.id,
            ObjectMemberKind::Getter(n) => n.id,
            ObjectMemberKind::Setter(n) => n.id,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ObjectMemberKind<'cx> {
    Shorthand(&'cx ObjectShorthandMember<'cx>),
    PropAssignment(&'cx ObjectPropAssignment<'cx>),
    Method(&'cx ObjectMethodMember<'cx>),
    SpreadAssignment(&'cx SpreadAssignment<'cx>),
    Getter(&'cx GetterDecl<'cx>),
    Setter(&'cx SetterDecl<'cx>),
}

#[derive(Debug, Clone)]
pub struct SpreadAssignment<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub expr: &'cx Expr<'cx>,
}

#[derive(Debug, Clone)]
pub struct ObjectMethodMember<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx PropName<'cx>,
    pub ty_params: Option<TyParams<'cx>>,
    pub params: ParamsDecl<'cx>,
    pub ty: Option<&'cx Ty<'cx>>,
    pub body: &'cx BlockStmt<'cx>,
}

#[derive(Debug, Clone)]
pub struct ObjectShorthandMember<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx Ident,
}

/// ```txt
/// let a = {
///     v: 42
/// //  ~~~~~
/// }
/// ```
#[derive(Debug, Clone)]
pub struct ObjectPropAssignment<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub name: &'cx PropName<'cx>,
    pub init: &'cx Expr<'cx>,
}

#[derive(Debug, Clone)]
pub struct ObjectLit<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub members: &'cx [&'cx ObjectMember<'cx>],
}

#[derive(Debug, Clone)]
pub struct MappedTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_param: &'cx self::TyParam<'cx>,
    /// - Plus token means `+readonly`,
    /// - Minus token means `-readonly`,
    /// - Readonly token means `readonly`.
    /// - Other token is unreachable.
    /// - No token means no modifier.
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
        let readonly = if let Some(r) = self.readonly_token {
            if r.kind == TokenKind::Minus {
                MappedTyModifiers::EXCLUDE_READONLY
            } else {
                MappedTyModifiers::INCLUDE_READONLY
            }
        } else {
            MappedTyModifiers::empty()
        };
        let question = if let Some(q) = self.question_token {
            if q.kind == TokenKind::Minus {
                MappedTyModifiers::EXCLUDE_OPTIONAL
            } else {
                MappedTyModifiers::INCLUDE_OPTIONAL
            }
        } else {
            MappedTyModifiers::empty()
        };
        readonly.union(question)
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

#[derive(Debug, Clone)]
pub struct TyOp<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub op: TyOpKind,
    pub ty: &'cx Ty<'cx>,
}

#[derive(Debug, Clone)]
pub struct InferTy<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub ty_param: &'cx TyParam<'cx>,
}

#[derive(Debug, Clone)]
pub struct EntityName<'cx> {
    pub kind: EntityNameKind<'cx>,
}

impl<'cx> EntityName<'cx> {
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

    pub fn get_first_identifier(&self) -> &'cx Ident {
        match self.kind {
            EntityNameKind::Ident(ident) => ident,
            EntityNameKind::Qualified(q) => q.left.get_first_identifier(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EntityNameKind<'cx> {
    Ident(&'cx Ident),
    Qualified(&'cx QualifiedName<'cx>),
}

/// ```txt
/// type A = B.C;
/// //       ~~~
/// ```
#[derive(Debug, Clone)]
pub struct QualifiedName<'cx> {
    pub id: NodeID,
    pub span: Span,
    pub left: &'cx EntityName<'cx>,
    pub right: &'cx Ident,
}
