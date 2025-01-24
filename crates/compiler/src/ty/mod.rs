mod check_flags;
mod facts;
mod flags;
mod mapper;
mod object_shape;
mod object_ty;
mod pprint;
mod sig;

use bolt_ts_atom::AtomId;

use crate::bind::{Symbol, SymbolID};
use crate::check::TyChecker;
use crate::{ast, keyword};

pub use self::check_flags::CheckFlags;
pub use self::facts::{has_type_facts, TypeFacts, TYPEOF_NE_FACTS};
pub use self::flags::{ObjectFlags, TypeFlags};
pub use self::mapper::{ArrayTyMapper, SimpleTyMapper, TyMapper};
pub use self::object_shape::ObjectShape;
pub use self::object_ty::ElementFlags;
pub use self::object_ty::SingleSigTy;
pub use self::object_ty::{AnonymousTy, InterfaceTy, ObjectTyKind};
pub use self::object_ty::{DeclaredMembers, ReferenceTy, StructuredMembers};
pub use self::object_ty::{IndexInfo, IndexInfos, ObjectTy, TupleTy};
pub use self::pprint::*;
pub use self::sig::{Sig, SigFlags, SigID, SigKind, Sigs};

bolt_ts_utils::index!(TyID);

impl TyID {
    pub(crate) fn new(id: u32) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Ty<'cx> {
    pub id: TyID,
    pub kind: TyKind<'cx>,
    pub flags: TypeFlags,
}

impl PartialEq for Ty<'_> {
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
        Self {
            kind,
            id,
            flags: TypeFlags::empty(),
        }
    }

    pub fn get_object_flags(&self) -> ObjectFlags {
        match self.kind {
            TyKind::Object(object) => object.flags,
            _ => ObjectFlags::empty(),
        }
    }

    pub fn is_object_literal(&self) -> bool {
        self.get_object_flags()
            .contains(ObjectFlags::OBJECT_LITERAL)
    }

    pub fn is_object_or_array_literal(&self) -> bool {
        self.get_object_flags()
            .intersects(ObjectFlags::OBJECT_LITERAL | ObjectFlags::ARRAY_LITERAL)
    }

    pub fn get_propagating_flags_of_tys(
        tys: Tys<'cx>,
        _exclude_kinds: Option<TypeFlags>,
    ) -> ObjectFlags {
        tys.iter().fold(ObjectFlags::empty(), |flags, ty| {
            flags | ty.get_object_flags()
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnionReduction {
    None,
    Lit,
    Subtype,
}

#[derive(Debug, Clone, Copy)]
pub enum TyKind<'cx> {
    Any,
    Unknown,
    String,
    Number,
    Boolean,
    Never,
    StringLit(&'cx StringLitTy),
    NumberLit(&'cx NumberLitTy),
    TrueLit,
    FalseLit,
    Void,
    Undefined,
    Null,
    NonPrimitive,
    Union(&'cx UnionTy<'cx>),
    Object(&'cx ObjectTy<'cx>),
    Param(&'cx ParamTy<'cx>),
    IndexedAccess(&'cx IndexedAccessTy<'cx>),
    Cond(&'cx CondTy<'cx>),
    Index(&'cx IndexTy<'cx>),
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
as_ty_kind!(
    IndexedAccess,
    &'cx IndexedAccessTy<'cx>,
    as_indexed_access,
    expect_indexed_access,
    is_indexed_access
);
as_ty_kind!(Void, is_void);
as_ty_kind!(Any, is_any);
as_ty_kind!(Never, is_never);
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
as_ty_kind!(Cond, &CondTy<'cx>, as_cond_ty, expect_cond_ty, is_cond_ty);
as_ty_kind!(
    Index,
    &IndexTy<'cx>,
    as_index_ty,
    expect_index_ty,
    is_index_ty
);

impl<'cx> Ty<'cx> {
    pub fn to_string(&self, checker: &mut TyChecker<'cx>) -> String {
        if self.kind.is_array(checker) {
            let ele = self.kind.expect_object_reference().resolved_ty_args[0].to_string(checker);
            return format!("{ele}[]");
        }
        match self.kind {
            TyKind::Object(object) => object.kind.to_string(self, checker),
            TyKind::NumberLit(lit) => format!("{}", lit.val),
            TyKind::StringLit(lit) => format!("\"{}\"", checker.atoms.get(lit.val)),
            TyKind::Union(_) if self == checker.boolean_ty() => {
                keyword::IDENT_BOOLEAN_STR.to_string()
            }
            TyKind::Union(union) => union
                .tys
                .iter()
                .map(|ty| ty.to_string(checker))
                .collect::<Vec<_>>()
                .join(" | "),
            TyKind::Param(param) => {
                if param.symbol == Symbol::ERR {
                    "error".to_string()
                } else {
                    let name = checker.binder.symbol(param.symbol).name;
                    checker.atoms.get(name.expect_atom()).to_string()
                }
            }
            TyKind::IndexedAccess(_) => "indexedAccess".to_string(),
            TyKind::Cond(_) => "cond".to_string(),
            TyKind::Any => keyword::IDENT_ANY_STR.to_string(),
            TyKind::Unknown => keyword::IDENT_UNKNOWN_STR.to_string(),
            TyKind::String => keyword::IDENT_STRING_STR.to_string(),
            TyKind::Number => keyword::IDENT_NUMBER_STR.to_string(),
            TyKind::Boolean => keyword::IDENT_BOOLEAN_STR.to_string(),
            TyKind::TrueLit => keyword::KW_TRUE_STR.to_string(),
            TyKind::FalseLit => keyword::KW_FALSE_STR.to_string(),
            TyKind::Void => keyword::KW_VOID_STR.to_string(),
            TyKind::Undefined => keyword::IDENT_UNDEFINED_STR.to_string(),
            TyKind::Null => keyword::KW_NULL_STR.to_string(),
            TyKind::NonPrimitive => keyword::IDENT_OBJECT_STR.to_string(),
            TyKind::Never => keyword::IDENT_NEVER_STR.to_string(),
            TyKind::Index(n) => n.ty.to_string(checker),
        }
    }

    pub fn symbol(&self) -> Option<SymbolID> {
        match self.kind {
            TyKind::Object(ty) => match ty.kind {
                ObjectTyKind::Interface(ty) => Some(ty.symbol),
                ObjectTyKind::Reference(ty) => ty.target.symbol(),
                ObjectTyKind::Anonymous(ty) => Some(ty.symbol),
                _ => None,
            },
            TyKind::Param(ty) => Some(ty.symbol),
            TyKind::Union(_) => todo!(),
            TyKind::IndexedAccess(_) => todo!(),
            TyKind::Cond(_) => todo!(),
            _ => None,
        }
    }
}

impl TyKind<'_> {
    pub fn maybe_type_of_kind(&self, f: impl Fn(&Self) -> bool + Copy) -> bool {
        if f(self) {
            true
        } else if let Some(union) = self.as_union() {
            union.tys.iter().any(|ty| ty.kind.maybe_type_of_kind(f))
        } else {
            // TODO: support intersection
            false
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

    pub fn is_intersection(&self) -> bool {
        false
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
        self.is_boolean() || self.is_true_lit() || self.is_false_lit()
    }

    pub fn is_structured(&self) -> bool {
        self.is_union() || self.is_object()
    }

    pub fn is_structured_or_instantiable(&self) -> bool {
        self.is_structured() || self.is_instantiable()
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
        self.is_param() || self.is_indexed_access()
    }

    pub fn is_instantiable_non_primitive(&self) -> bool {
        self.is_index_ty() || self.is_type_variable() || self.is_cond_ty()
    }

    pub fn is_instantiable_primitive(&self) -> bool {
        // todo: `index`, `template literal`, `string mapping`
        false
    }

    pub fn is_instantiable(&self) -> bool {
        self.is_instantiable_non_primitive() || self.is_instantiable_primitive()
    }

    pub fn is_generic(&self) -> bool {
        self.is_instantiable_non_primitive()
    }

    pub fn is_generic_tuple_type(&self) -> bool {
        self.as_object_reference()
            .and_then(|refer| refer.target.kind.as_object_tuple())
            .map(|tup| tup.combined_flags.intersects(ElementFlags::VARIADIC))
            .unwrap_or_default()
    }

    pub fn is_this_ty_param(&self) -> bool {
        self.as_param()
            .map(|param| param.is_this_ty)
            .unwrap_or_default()
    }

    pub fn is_generic_object(&self) -> bool {
        self.is_instantiable_non_primitive() || self.is_generic_tuple_type()
    }

    pub fn is_generic_index_ty(&self) -> bool {
        self.is_instantiable_non_primitive()
    }

    pub fn is_tuple(&self) -> bool {
        self.as_object_reference()
            .map(|refer| refer.target.kind.is_object_tuple())
            .unwrap_or_default()
    }

    pub fn is_array(&self, checker: &TyChecker) -> bool {
        if let Some(a) = self.as_object_reference() {
            let b = checker.global_array_ty().kind.expect_object_reference();
            a.target == b.target
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
    pub object_flags: ObjectFlags,
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
pub struct ParamTy<'cx> {
    pub symbol: SymbolID,
    pub offset: usize,
    pub target: Option<&'cx self::Ty<'cx>>,
    pub is_this_ty: bool,
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct IndexFlags: u8 {
        const STRINGS_ONLY          = 1 << 0;
        const NO_INDEX_SIGNATURES   = 1 << 1;
        const NO_REDUCIBLE_CHECK    = 1 << 2;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IndexTy<'cx> {
    pub ty: &'cx self::Ty<'cx>,
    pub index_flags: IndexFlags,
}
