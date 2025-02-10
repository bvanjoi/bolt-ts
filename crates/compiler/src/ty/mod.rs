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
pub use self::mapper::{ArrayTyMapper, TyMap, TyMapper};
pub use self::mapper::{CompositeTyMapper, MergedTyMapper};
pub use self::object_shape::ObjectShape;
pub use self::object_ty::ElementFlags;
pub use self::object_ty::SingleSigTy;
pub use self::object_ty::{AnonymousTy, InterfaceTy, ObjectTyKind};
pub use self::object_ty::{DeclaredMembers, ReferenceTy, StructuredMembers};
pub use self::object_ty::{IndexInfo, IndexInfos, ObjectTy, TupleTy};
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
    pub fn new(id: TyID, kind: TyKind<'cx>, flags: TypeFlags) -> Self {
        Self { kind, id, flags }
    }

    pub fn get_object_flags(&self) -> ObjectFlags {
        match self.kind {
            TyKind::Object(object) => object.flags,
            TyKind::Intrinsic(i) => i.object_flags,
            TyKind::Union(u) => u.object_flags,
            TyKind::Intersection(i) => i.object_flags,
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
        tys: &[&'cx Ty<'cx>],
        _exclude_kinds: Option<TypeFlags>,
    ) -> ObjectFlags {
        tys.iter().fold(ObjectFlags::empty(), |flags, ty| {
            flags | ty.get_object_flags()
        })
    }

    pub fn is_no_infer_ty(&self) -> bool {
        self.kind
            .as_substitution_ty()
            .map(|sub| sub.constraint.flags.intersects(TypeFlags::UNKNOWN))
            .unwrap_or_default()
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
    Intrinsic(&'cx IntrinsicTy),
    StringLit(&'cx StringLitTy),
    NumberLit(&'cx NumberLitTy),
    Union(&'cx UnionTy<'cx>),
    Intersection(&'cx IntersectionTy<'cx>),
    Object(&'cx ObjectTy<'cx>),
    Param(&'cx ParamTy<'cx>),
    IndexedAccess(&'cx IndexedAccessTy<'cx>),
    Cond(&'cx CondTy<'cx>),
    Index(&'cx IndexTy<'cx>),
    Substitution(&'cx SubstitutionTy<'cx>),
}

macro_rules! as_ty_kind {
    ($kind: ident, $ty:ty, $name: ident) => {
        paste::paste! {
            impl<'cx> TyKind<'cx> {
                #[inline(always)]
                pub fn [<as_ $name>](&self) -> Option<$ty> {
                    match self {
                        TyKind::$kind(ty) => Some(ty),
                        _ => None,
                    }
                }
                #[inline(always)]
                pub fn [<is_ $name>](&self) -> bool {
                    self.[<as_ $name>]().is_some()
                }
                #[inline(always)]
                pub fn [<expect_ $name>](&self) -> $ty {
                    self.[<as_ $name>]().unwrap()
                }
            }
        }
    };
}

as_ty_kind!(StringLit, &'cx StringLitTy, string_lit);
as_ty_kind!(NumberLit, &'cx NumberLitTy, number_lit);
as_ty_kind!(IndexedAccess, &'cx IndexedAccessTy<'cx>, indexed_access);
as_ty_kind!(Union, &'cx UnionTy<'cx>, union);
as_ty_kind!(Intersection, &'cx IntersectionTy<'cx>, intersection);
as_ty_kind!(Object, &'cx ObjectTy<'cx>, object);
as_ty_kind!(Param, &'cx ParamTy<'cx>, param);
as_ty_kind!(Cond, &'cx CondTy<'cx>, cond_ty);
as_ty_kind!(Index, &IndexTy<'cx>, index_ty);
as_ty_kind!(Substitution, &SubstitutionTy<'cx>, substitution_ty);

impl<'cx> Ty<'cx> {
    pub fn to_string(&'cx self, checker: &mut TyChecker<'cx>) -> String {
        if self.kind.is_array(checker) {
            let ele = checker.get_ty_arguments(self)[0];
            let ele = ele.to_string(checker);
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
            TyKind::Intersection(i) => i
                .tys
                .iter()
                .map(|ty| ty.to_string(checker))
                .collect::<Vec<_>>()
                .join(" & "),
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
            TyKind::Index(n) => n.ty.to_string(checker),
            TyKind::Intrinsic(i) => checker.atoms.get(i.name).to_string(),
            TyKind::Substitution(_) => "substitution".to_string(),
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
            TyKind::Union(_) => None,
            TyKind::IndexedAccess(_) => todo!(),
            TyKind::Cond(_) => todo!(),
            _ => None,
        }
    }

    pub fn maybe_type_of_kind(&self, flags: TypeFlags) -> bool {
        if self.flags.intersects(flags) {
            true
        } else if let Some(union) = self.kind.as_union() {
            union.tys.iter().any(|ty| ty.maybe_type_of_kind(flags))
        } else {
            // TODO: support intersection
            false
        }
    }
}

impl<'cx> TyKind<'cx> {
    pub fn is_union_or_intersection(&self) -> bool {
        self.is_union() || self.is_intersection()
    }

    pub fn tys_of_union_or_intersection(&self) -> Option<Tys<'cx>> {
        if let Some(union) = self.as_union() {
            Some(union.tys)
        } else if let Some(intersection) = self.as_intersection() {
            Some(intersection.tys)
        } else {
            None
        }
    }

    pub fn is_object_or_intersection(&self) -> bool {
        self.is_object() || self.is_intersection()
    }

    pub fn is_structured(&self) -> bool {
        self.is_union() || self.is_object()
    }

    pub fn is_structured_or_instantiable(&self) -> bool {
        self.is_structured() || self.is_instantiable()
    }

    pub fn is_type_variable(&self) -> bool {
        self.is_param() || self.is_indexed_access()
    }

    pub fn is_instantiable_non_primitive(&self) -> bool {
        self.is_type_variable() || self.is_cond_ty() || self.is_substitution_ty()
    }

    pub fn is_instantiable_primitive(&self) -> bool {
        // todo: `template literal`, `string mapping`
        self.is_index_ty()
    }

    pub fn is_instantiable(&self) -> bool {
        self.is_instantiable_non_primitive() || self.is_instantiable_primitive()
    }

    pub fn is_generic(&self) -> bool {
        !self.get_generic_object_flags().is_empty()
    }

    pub fn is_single_element_generic_tuple_type(&self) -> bool {
        self.is_generic_tuple_type() && {
            let r = self.expect_object_reference();
            r.target.kind.expect_object_tuple().element_flags.len() == 1
        }
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
        self.get_generic_object_flags()
            .intersects(ObjectFlags::IS_GENERIC_OBJECT_TYPE)
    }

    pub fn is_generic_index_ty(&self) -> bool {
        self.get_generic_object_flags()
            .intersects(ObjectFlags::IS_GENERIC_INDEX_TYPE)
    }

    fn get_generic_object_flags(&self) -> ObjectFlags {
        if self.is_union_or_intersection() {
            // TODO:
            ObjectFlags::empty()
        } else if let Some(ty) = self.as_substitution_ty() {
            // TODO: cache?
            (ty.base_ty.kind.get_generic_object_flags()
                | ty.constraint.kind.get_generic_object_flags())
                & ObjectFlags::IS_GENERIC_TYPE
        } else {
            (if self.is_instantiable_non_primitive() || self.is_generic_tuple_type() {
                ObjectFlags::IS_GENERIC_OBJECT_TYPE
            } else {
                ObjectFlags::empty()
            }) | (if self.is_instantiable() || self.is_index_ty() {
                ObjectFlags::IS_GENERIC_INDEX_TYPE
            } else {
                ObjectFlags::empty()
            })
        }
    }

    pub fn is_tuple(&self) -> bool {
        self.as_object_reference()
            .map(|refer| refer.target.kind.is_object_tuple())
            .unwrap_or_default()
    }

    pub fn is_array(&self, checker: &TyChecker<'cx>) -> bool {
        self.as_object_reference().is_some_and(|ty| {
            ty.target == checker.global_array_ty()
                || ty.target == checker.global_readonly_array_ty()
        })
    }

    pub fn is_readonly_array(&self, checker: &TyChecker<'cx>) -> bool {
        self.as_object_reference()
            .is_some_and(|ty| ty.target == checker.global_readonly_array_ty())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CondTyRoot<'cx> {
    pub node: &'cx ast::CondTy<'cx>,
    pub check_ty: &'cx Ty<'cx>,
    pub extends_ty: &'cx Ty<'cx>,
    pub outer_ty_params: Option<Tys<'cx>>,
    pub is_distributive: bool,
    pub infer_ty_params: Option<Tys<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CondTy<'cx> {
    pub root: &'cx CondTyRoot<'cx>,
    pub check_ty: &'cx Ty<'cx>,
    pub extends_ty: &'cx Ty<'cx>,
    pub mapper: Option<&'cx dyn TyMap<'cx>>,
    pub combined_mapper: Option<&'cx dyn TyMap<'cx>>,
    pub alias_symbol: Option<SymbolID>,
    pub alias_ty_args: Option<Tys<'cx>>,
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
pub struct IntersectionTy<'cx> {
    pub tys: Tys<'cx>,
    pub object_flags: ObjectFlags,
    pub alias_symbol: Option<SymbolID>,
    pub alias_ty_arguments: Option<Tys<'cx>>,
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
    pub offset: Option<usize>,
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

#[derive(Debug, Clone, Copy)]
pub struct IntrinsicTy {
    pub object_flags: ObjectFlags,
    pub name: AtomId,
}

#[derive(Debug, Clone, Copy)]
pub struct SubstitutionTy<'cx> {
    pub object_flags: ObjectFlags,
    pub base_ty: &'cx self::Ty<'cx>,
    pub constraint: &'cx self::Ty<'cx>,
}
