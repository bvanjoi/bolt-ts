mod facts;
mod mapper;
mod object_shape;
mod object_ty;
mod sig;

use crate::atoms::{AtomId, AtomMap};
use crate::bind::{Binder, SymbolID};
use crate::{ast, keyword};

pub use self::facts::{has_type_facts, TypeFacts};
pub use self::mapper::{ArrayTyMapper, CompositeTyMapper, TyMapper};
pub use self::mapper::{DeferredTyMapper, FnTyMapper, MergedTyMapper, SimpleTyMapper};
pub use self::object_shape::ObjectShape;
pub use self::object_ty::ElementFlags;
pub use self::object_ty::TyReference;
pub use self::object_ty::{ArrayTy, IndexInfo, ObjectTy, TupleShape, TupleTy};
pub use self::object_ty::{ClassTy, FnTy, InterfaceTy, ObjectLitTy, ObjectTyKind};
pub use self::sig::{Sig, SigDecl, SigFlags};

bolt_ts_span::new_index!(TyID);
bolt_ts_span::new_index!(TyVarID);

#[derive(Debug, Clone, Copy)]
pub struct Ty<'cx> {
    pub kind: TyKind<'cx>,
    pub id: TyID,
}

impl<'cx> PartialEq for Ty<'cx> {
    fn eq(&self, other: &Self) -> bool {
        if self.id == other.id {
            assert!(std::ptr::eq(&self.kind, &other.kind), "extra allocation");
            true
        } else {
            false
        }
    }
}

impl<'cx> Ty<'cx> {
    pub fn new(id: TyID, kind: TyKind<'cx>) -> Self {
        Self { kind, id }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TyKind<'cx> {
    Any,
    Unknown,
    String,
    Number,
    Boolean,
    StringLit(&'cx StringLitTy),
    NumberLit(&'cx NumberLitTy),
    TrueLit,
    FalseLit,
    Void,
    Undefined,
    Null,
    Union(&'cx UnionTy<'cx>),
    Object(&'cx ObjectTy<'cx>),
    Param(&'cx ParamTy),
    Var(TyVarID),
    IndexedAccess(&'cx IndexedAccessTy<'cx>),
    Cond(&'cx CondTy<'cx>),
}

macro_rules! as_ty_kind {
    ($kind: ident, $ty:ty, $as_kind: ident, $expect_kind: ident, $is_kind: ident) => {
        impl<'cx> TyKind<'cx> {
            #[inline(always)]
            pub fn $as_kind(&self) -> Option<$ty> {
                match self {
                    TyKind::$kind(ty) => Some(ty),
                    _ => None,
                }
            }
            #[inline(always)]
            pub fn $is_kind(&self) -> bool {
                self.$as_kind().is_some()
            }
            #[inline(always)]
            pub fn $expect_kind(&self) -> $ty {
                self.$as_kind().unwrap()
            }
        }
    };
    ($kind: ident, $is_kind: ident) => {
        impl<'cx> TyKind<'cx> {
            #[inline(always)]
            pub fn $is_kind(&self) -> bool {
                match self {
                    TyKind::$kind => true,
                    _ => false,
                }
            }
        }
    };
}

as_ty_kind!(
    StringLit,
    &'cx StringLitTy,
    as_string_lit,
    expect_string_lit,
    is_string_lit
);
as_ty_kind!(
    NumberLit,
    &'cx NumberLitTy,
    as_number_lit,
    expect_number_lit,
    is_number_lit
);
as_ty_kind!(Any, is_any);
as_ty_kind!(Number, is_number);
as_ty_kind!(String, is_string);
as_ty_kind!(Boolean, is_boolean);
as_ty_kind!(TrueLit, is_true_lit);
as_ty_kind!(FalseLit, is_false_lit);
as_ty_kind!(Null, is_null);
as_ty_kind!(Undefined, is_undefined);
as_ty_kind!(Union, &'cx UnionTy<'cx>, as_union, expect_union, is_union);
as_ty_kind!(
    Object,
    &'cx ObjectTy<'cx>,
    as_object,
    expect_object,
    is_object
);
as_ty_kind!(Param, &'cx ParamTy, as_param, expect_param, is_param);
as_ty_kind!(Var, &TyVarID, as_ty_var, expect_ty_var, is_ty_var);
as_ty_kind!(Cond, &CondTy<'cx>, as_cond_ty, expect_cond_ty, is_cond_ty);

impl<'cx> TyKind<'cx> {
    pub fn to_string(&self, binder: &'cx Binder, atoms: &'cx AtomMap) -> String {
        match self {
            TyKind::NumberLit(_) => "number".to_string(),
            TyKind::Union(union) => union
                .tys
                .iter()
                .map(|ty| ty.kind.to_string(binder, atoms))
                .collect::<Vec<_>>()
                .join(" | "),
            TyKind::StringLit(_) => todo!(),
            TyKind::Object(object) => object.kind.to_string(&binder, atoms),
            TyKind::Var(id) => {
                // todo: delay bug
                format!("#{id:#?}")
            }
            TyKind::Param(_) => todo!(),
            TyKind::IndexedAccess(_) => "indexedAccess".to_string(),
            TyKind::Cond(_) => "cond".to_string(),
            TyKind::Any => keyword::IDENT_ANY_STR.to_string(),
            TyKind::Unknown => keyword::IDENT_UNKNOWN_STR.to_string(),
            TyKind::String => keyword::IDENT_STRING_STR.to_string(),
            TyKind::Number => keyword::IDENT_NUMBER_STR.to_string(),
            TyKind::Boolean => keyword::IDENT_BOOLEAN_STR.to_string(),
            TyKind::TrueLit => keyword::KW_TRUE_STR.to_string(),
            TyKind::FalseLit => keyword::KW_FALSE_STR.to_string(),
            TyKind::Void => keyword::IDENT_VOID_STR.to_string(),
            TyKind::Undefined => keyword::IDENT_UNDEFINED_STR.to_string(),
            TyKind::Null => keyword::KW_NULL_STR.to_string(),
        }
    }

    pub fn is_primitive(&self) -> bool {
        use TyKind::*;
        if self.is_string_like() || self.is_number_like() || self.is_boolean_like() {
            true
        } else {
            matches!(self, Null)
        }
    }

    pub fn is_union_or_intersection(&self) -> bool {
        self.is_union()
    }

    pub fn is_object_or_intersection(&self) -> bool {
        self.is_object()
    }

    pub fn is_lit(&self) -> bool {
        use TyKind::*;
        if matches!(self, StringLit(_) | NumberLit(_)) {
            true
        } else {
            self.is_true_lit() | self.is_false_lit()
        }
    }

    pub fn is_number_like(&self) -> bool {
        use TyKind::*;
        if matches!(self, NumberLit(_)) {
            true
        } else {
            self.is_number()
        }
    }

    pub fn is_string_like(&self) -> bool {
        use TyKind::*;
        if matches!(self, StringLit(_)) {
            true
        } else {
            self.is_string()
        }
    }

    pub fn is_boolean_like(&self) -> bool {
        self.is_true_lit() | self.is_false_lit()
    }

    pub fn is_structured(&self) -> bool {
        self.is_union() | self.is_object()
    }

    pub fn is_structured_or_instantiable(&self) -> bool {
        self.is_structured()
    }

    pub fn definitely_non_nullable(&self) -> bool {
        self.is_number_like() || self.is_string_like()
    }

    pub fn is_nullable(&self) -> bool {
        self.is_null() || self.is_undefined()
    }

    pub fn is_fresh(&self) -> bool {
        self.is_lit()
    }

    pub fn is_type_variable(&self) -> bool {
        self.is_ty_var() || self.is_param()
    }

    pub fn is_instantiable_non_primitive(&self) -> bool {
        self.is_type_variable() || self.is_cond_ty()
    }

    pub fn is_generic(&self) -> bool {
        self.is_instantiable_non_primitive()
    }

    pub fn is_generic_tuple_type(&self) -> bool {
        if let Some(tup) = self.as_object_tuple() {
            tup.combined_flags.intersects(ElementFlags::VARIADIC)
        } else {
            false
        }
    }

    pub fn is_generic_object(&self) -> bool {
        self.is_instantiable_non_primitive() || self.is_generic_tuple_type()
    }

    pub fn is_tuple(&self) -> bool {
        if self.is_object_flags_type() {
            // TODO: object flags
            true
        } else {
            false
        }
    }

    pub fn is_object_flags_type(&self) -> bool {
        self.is_any() || self.is_nullable() | self.is_object() || self.is_union()
    }

    pub fn is_any_or_unknown(&self) -> bool {
        self.is_any()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CondTyRoot<'cx> {
    pub node: &'cx ast::CondTy<'cx>,
    pub check_ty: &'cx Ty<'cx>,
    pub extends_ty: &'cx Ty<'cx>,
    pub outer_ty_params: Option<Tys<'cx>>,
    pub is_distributive: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct CondTy<'cx> {
    pub root: &'cx CondTyRoot<'cx>,
    pub check_ty: &'cx Ty<'cx>,
    pub extends_ty: &'cx Ty<'cx>,
    pub mapper: Option<&'cx TyMapper<'cx>>,
}

pub type Tys<'cx> = &'cx [&'cx Ty<'cx>];

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct AccessFlags: u16 {
        const INCLUDE_UNDEFINED              = 1 << 0;
        const NO_INDEX_SIGNATURES            = 1 << 1;
        const WRITING                        = 1 << 2;
        const CACHE_SYMBOL                   = 1 << 3;
        const ALLOWING_MISSING               = 1 << 4;
        const EXPRESSION_POSITION            = 1 << 5;
        const REPORT_DEPRECATED              = 1 << 6;
        const SUPPRESS_NO_IMPLICIT_ANY_ERROR = 1 << 7;
        const Contextual                     = 1 << 8;
        const PERSISTENT                     = Self::INCLUDE_UNDEFINED.bits();
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IndexedAccessTy<'cx> {
    pub object_ty: &'cx self::Ty<'cx>,
    pub index_ty: &'cx self::Ty<'cx>,
    pub access_flags: AccessFlags,
    // alias_symbol: Option<SymbolID>,
    // alias_ty_arguments: Option<&'cx Tys<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct UnionTy<'cx> {
    pub tys: Tys<'cx>,
}

#[derive(Debug, Clone, Copy)]
pub struct NumberLitTy {
    pub val: f64,
}

#[derive(Debug, Clone, Copy)]
pub struct StringLitTy {
    pub val: AtomId,
}

#[derive(Debug, Clone, Copy)]
pub struct ParamTy {
    pub symbol: SymbolID,
}
