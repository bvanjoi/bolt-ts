mod facts;
mod links;
mod mapper;
mod num_lit;
mod object_ty;
mod pprint;
mod sig;

use bolt_ts_ast::{self as ast};
use bolt_ts_atom::Atom;
use bolt_ts_binder::{Symbol, SymbolID, SymbolName};

pub use bolt_ts_ty::CheckFlags;
pub use bolt_ts_ty::IndexFlags;
pub use bolt_ts_ty::ObjectFlags;
pub use bolt_ts_ty::TypeFacts;
pub use bolt_ts_ty::TypeFlags;

pub use self::facts::{TYPEOF_NE_FACTS, typeof_ne_facts};
pub use self::links::ConditionalLinks;
pub use self::links::ConditionalLinksArena;
pub use self::links::ConditionalLinksID;
pub use self::links::InterfaceTyLinksArena;
pub use self::links::PromiseOrAwaitableTyLinks;
pub use self::links::{CommonTyLinks, CommonTyLinksArena, CommonTyLinksID};
pub use self::links::{FreshTyLinksArena, FreshTyLinksID};
pub use self::links::{ObjectMappedTyLinks, ObjectMappedTyLinksArena};
pub use self::links::{PromiseOrAwaitableTyLinksArena, PromiseOrAwaitableTyLinksID};
pub use self::links::{UnionTyLinks, UnionTyLinksArena, UnionTyLinksID};
pub use self::mapper::{ArrayTyMapper, TyMap, TyMapper};
pub use self::mapper::{CompositeTyMapper, MergedTyMapper};
pub use self::num_lit::NumberLitTy;
pub use self::object_ty::ElementFlags;
pub use self::object_ty::SingleSigTy;
pub use self::object_ty::{AnonymousTy, InterfaceTy, ObjectTyKind, ReverseMappedTy};
pub use self::object_ty::{DeclaredMembers, ReferenceTy, StructuredMembers};
pub use self::object_ty::{IndexInfo, IndexInfos, ObjectTy, TupleTy};
pub use self::object_ty::{MappedTy, MappedTyNameTyKind};
pub use self::sig::{Sig, SigFlags, SigID, SigKind, Sigs};
use super::check::TyChecker;

bolt_ts_utils::index!(TyID);

impl nohash_hasher::IsEnabled for TyID {}

impl TyID {
    pub(crate) fn new(id: u32) -> Self {
        Self(id)
    }
}

#[derive(Debug)]
pub struct Ty<'cx> {
    pub id: TyID,
    pub kind: TyKind<'cx>,
    pub flags: TypeFlags,
    pub links: CommonTyLinksID<'cx>,
}

impl std::hash::Hash for Ty<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for Ty<'_> {
    fn eq(&self, other: &Self) -> bool {
        debug_assert!(self.id != other.id || std::ptr::eq(&self.kind, &other.kind));
        self.id == other.id
    }
}

impl Eq for Ty<'_> {}

impl<'cx> Ty<'cx> {
    pub fn new(
        id: TyID,
        kind: TyKind<'cx>,
        flags: TypeFlags,
        links: links::CommonTyLinksID<'cx>,
    ) -> Self {
        Self {
            kind,
            id,
            flags,
            links,
        }
    }

    pub fn is_neither_unit_ty_nor_never(&self) -> bool {
        !self
            .flags
            .intersects(TypeFlags::UNIT.union(TypeFlags::NEVER))
    }

    pub fn contains_undefined_ty(&self) -> bool {
        let ty = if let Some(u) = self.kind.as_union() {
            u.tys[0]
        } else {
            self
        };
        ty.flags.contains(TypeFlags::UNDEFINED)
    }

    pub fn is_fresh_object_literal(&self) -> bool {
        self.get_object_flags()
            .contains(ObjectFlags::FRESH_LITERAL.union(ObjectFlags::OBJECT_LITERAL))
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
            .intersects(ObjectFlags::OBJECT_LITERAL.union(ObjectFlags::ARRAY_LITERAL))
    }

    pub fn get_propagating_flags_of_tys(
        tys: &[&'cx Ty<'cx>],
        _exclude_kinds: Option<TypeFlags>,
    ) -> ObjectFlags {
        tys.iter().fold(ObjectFlags::empty(), |flags, ty| {
            flags | ty.get_object_flags()
        }) & ObjectFlags::PROPAGATING_FLAGS
    }

    pub fn is_no_infer_ty(&self) -> bool {
        self.kind
            .as_substitution_ty()
            .map(|sub| sub.constraint.flags.intersects(TypeFlags::UNKNOWN))
            .unwrap_or_default()
    }

    pub fn is_generic_string_like(&self) -> bool {
        self.flags
            .intersects(TypeFlags::TEMPLATE_LITERAL.union(TypeFlags::STRING_MAPPING))
            && !self.is_pattern_lit_ty()
    }

    pub fn intrinsic_name(&'cx self) -> Option<Atom> {
        let i = self.kind.as_intrinsic();
        i.map(|i| i.name)
    }

    pub fn count(&self) -> usize {
        match self.kind {
            TyKind::Union(u) => u.tys.len(),
            _ => 1,
        }
    }

    pub fn as_class_or_interface_ty(&self) -> Option<&'cx InterfaceTy<'cx>> {
        if let Some(i) = self.kind.as_object_interface() {
            Some(i)
        } else if let Some(r) = self.kind.as_object_reference()
            && let Some(i) = r.target.kind.as_object_interface()
        {
            Some(i)
        } else {
            None
        }
    }

    pub fn promise_or_awaitable_ty_links(&self) -> Option<PromiseOrAwaitableTyLinksID<'cx>> {
        match self.kind {
            TyKind::Union(ty) => Some(ty.promise_or_awaitable_links),
            TyKind::Object(ty) => ty.kind.promise_or_awaitable_ty_links(),
            _ => None,
        }
    }

    pub fn is_non_deferred_ty_reference(&self) -> bool {
        if !self.get_object_flags().contains(ObjectFlags::REFERENCE) {
            return false;
        }
        let object_ty = self.kind.expect_object();
        match object_ty.kind {
            ObjectTyKind::Reference(n) => n.node.is_none(),
            ObjectTyKind::Tuple(_) => true,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnionReduction {
    None,
    Lit,
    Subtype,
}

#[derive(Debug, Clone, Copy)]
pub enum TyKind<'cx> {
    Intrinsic(&'cx IntrinsicTy),
    StringLit(&'cx StringLitTy<'cx>),
    NumberLit(&'cx NumberLitTy<'cx>),
    BigIntLit(&'cx BigIntLitTy<'cx>),
    UniqueESSymbol(&'cx UniqueESSymbolTy),
    Union(&'cx UnionTy<'cx>),
    Intersection(&'cx IntersectionTy<'cx>),
    Object(&'cx ObjectTy<'cx>),
    Param(&'cx ParamTy<'cx>),
    IndexedAccess(&'cx IndexedAccessTy<'cx>),
    Cond(&'cx CondTy<'cx>),
    Index(&'cx IndexTy<'cx>),
    Substitution(&'cx SubstitutionTy<'cx>),
    StringMapping(&'cx StringMappingTy<'cx>),
    TemplateLit(&'cx TemplateLitTy<'cx>),
    Enum(&'cx EnumTy<'cx>),
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

as_ty_kind!(Intrinsic, &'cx IntrinsicTy, intrinsic);
as_ty_kind!(Enum, &'cx EnumTy<'cx>, enum);
as_ty_kind!(StringLit, &'cx StringLitTy<'cx>, string_lit);
as_ty_kind!(NumberLit, &'cx NumberLitTy<'cx>, number_lit);
as_ty_kind!(BigIntLit, &'cx BigIntLitTy<'cx>, bigint_lit);
as_ty_kind!(UniqueESSymbol, &'cx UniqueESSymbolTy, unique_es_symbol);
as_ty_kind!(IndexedAccess, &'cx IndexedAccessTy<'cx>, indexed_access);
as_ty_kind!(Union, &'cx UnionTy<'cx>, union);
as_ty_kind!(Intersection, &'cx IntersectionTy<'cx>, intersection);
as_ty_kind!(Object, &'cx ObjectTy<'cx>, object);
as_ty_kind!(Param, &'cx ParamTy<'cx>, param);
as_ty_kind!(Cond, &'cx CondTy<'cx>, cond_ty);
as_ty_kind!(Index, &IndexTy<'cx>, index_ty);
as_ty_kind!(Substitution, &SubstitutionTy<'cx>, substitution_ty);
as_ty_kind!(StringMapping, &StringMappingTy<'cx>, string_mapping_ty);
as_ty_kind!(TemplateLit, &TemplateLitTy<'cx>, template_lit_ty);

impl<'cx> Ty<'cx> {
    fn print_enum_symbol(&'cx self, checker: &mut TyChecker<'cx>, symbol: SymbolID) -> String {
        let name = checker.binder.symbol(symbol).name;
        checker.atoms.get(name.expect_atom()).to_string()
    }

    fn print_enum_lit_symbol(&'cx self, checker: &TyChecker<'cx>, symbol: SymbolID) -> String {
        let s = checker.binder.symbol(symbol);
        let value_decl = s.value_decl.unwrap();
        let prop = checker.atoms.get(s.name.expect_atom());
        let p = s.parent.unwrap();
        let p = checker.binder.symbol(p);
        let object = checker.atoms.get(p.name.expect_atom());
        let enum_member = checker.p.node(value_decl).expect_enum_member();
        match enum_member.name {
            ast::EnumMemberNameKind::Ident(_) => {
                format!("{object}.{prop}")
            }
            ast::EnumMemberNameKind::StringLit { .. } => {
                format!("{object}[\"{prop}\"]")
            }
        }
    }

    pub fn to_string(&'cx self, checker: &mut TyChecker<'cx>) -> String {
        if let Some(alias_symbol) = self.alias_symbol() {
            let s = checker.binder.symbol(alias_symbol);
            return s.name.to_string(&checker.atoms);
        } else if self.kind.is_array(checker) {
            let ele = checker.get_ty_arguments(self)[0];
            let ele = ele.to_string(checker);
            return format!("{ele}[]");
        } else if self == checker.boolean_ty() {
            return "boolean".to_string();
        }
        match self.kind {
            TyKind::Object(object) => object.kind.to_string(self, checker),
            TyKind::NumberLit(lit) => {
                if self.flags.intersects(TypeFlags::ENUM_LITERAL) {
                    let symbol = lit.symbol.unwrap();
                    self.print_enum_lit_symbol(checker, symbol)
                } else if lit.is(f64::INFINITY) {
                    "Infinity".to_string()
                } else if lit.is(f64::NEG_INFINITY) {
                    "-Infinity".to_string()
                } else {
                    format!("{}", lit.val.val())
                }
            }
            TyKind::BigIntLit(lit) => format!("{}n", checker.atoms.get(lit.val)),
            TyKind::StringLit(lit) => {
                if self.flags.intersects(TypeFlags::ENUM_LITERAL) {
                    let symbol = lit.symbol.unwrap();
                    self.print_enum_lit_symbol(checker, symbol)
                } else {
                    format!("\"{}\"", checker.atoms.get(lit.val))
                }
            }
            TyKind::Union(union) => union.tys.iter().fold(String::new(), |mut s, ty| {
                if !s.is_empty() {
                    s.push_str(" | ");
                }
                if ty.kind.is_object_anonymous()
                    && (!checker.get_signatures_of_type(ty, SigKind::Call).is_empty()
                        || !checker
                            .get_signatures_of_type(ty, SigKind::Constructor)
                            .is_empty())
                {
                    s.push_str(&format!("({})", checker.print_ty(ty)))
                } else {
                    s.push_str(checker.print_ty(ty))
                }
                s
            }),
            TyKind::Intersection(i) => i.tys.iter().fold(String::new(), |mut s, ty| {
                if !s.is_empty() {
                    s.push_str(" & ");
                }
                if ty.kind.is_object_anonymous()
                    && (!checker.get_signatures_of_type(ty, SigKind::Call).is_empty()
                        || !checker
                            .get_signatures_of_type(ty, SigKind::Constructor)
                            .is_empty())
                {
                    s.push_str(&format!("({})", checker.print_ty(ty)))
                } else {
                    s.push_str(checker.print_ty(ty))
                }
                s
            }),

            TyKind::Param(param) => {
                if param.symbol == Symbol::ERR {
                    "error_param".to_string()
                } else {
                    let name = checker.binder.symbol(param.symbol).name;
                    checker.atoms.get(name.expect_atom()).to_string()
                }
            }
            TyKind::IndexedAccess(_) => "indexedAccess".to_string(),
            TyKind::Cond(n) => {
                if let Some(symbol) = n.root.alias_symbol {
                    let name = checker.binder.symbol(symbol).name;
                    checker.atoms.get(name.expect_atom()).to_string()
                } else {
                    "cond".to_string()
                }
            }
            TyKind::Index(n) => n.ty.to_string(checker),
            TyKind::Intrinsic(i) => checker.atoms.get(i.name).to_string(),
            TyKind::Substitution(_) => "substitution".to_string(),
            TyKind::StringMapping(s) => {
                let name = checker.binder.symbol(s.symbol).name;
                checker.atoms.get(name.expect_atom()).to_string()
            }
            TyKind::TemplateLit(n) => {
                let mut s = String::with_capacity(32);
                s.push('`');
                for i in 0..n.texts.len() {
                    let text = n.texts[i];
                    s.push_str(checker.atoms.get(text));
                    if let Some(ty) = n.tys.get(i) {
                        s.push_str(&format!("${{{}}}", ty.to_string(checker)));
                    }
                }
                s.push('`');
                s
            }
            TyKind::UniqueESSymbol(_) => "unique es symbol".to_string(),
            TyKind::Enum(n) => self.print_enum_symbol(checker, n.symbol),
        }
    }

    pub fn symbol(&self) -> Option<SymbolID> {
        match self.kind {
            TyKind::Object(ty) => match ty.kind {
                ObjectTyKind::Interface(ty) => Some(ty.symbol),
                ObjectTyKind::Reference(ty) => ty.target.symbol(),
                ObjectTyKind::Anonymous(ty) => ty.symbol,
                ObjectTyKind::Mapped(ty) => Some(ty.symbol),
                _ => None,
            },
            TyKind::Param(ty) => Some(ty.symbol),
            TyKind::Union(_) => None,
            TyKind::IndexedAccess(_) => todo!(),
            TyKind::Cond(_) => None,
            TyKind::TemplateLit(_) => todo!(),
            _ => None,
        }
    }

    pub fn maybe_type_of_kind(&self, flags: TypeFlags) -> bool {
        if self.flags.intersects(flags) {
            true
        } else if let Some(tys) = self.kind.tys_of_union_or_intersection() {
            tys.iter().any(|ty| ty.maybe_type_of_kind(flags))
        } else {
            false
        }
    }

    pub fn is_tuple(&self) -> bool {
        self.as_tuple().is_some()
    }

    pub fn as_tuple(&self) -> Option<&'cx TupleTy<'cx>> {
        if self.get_object_flags().contains(ObjectFlags::REFERENCE) {
            let object_ty = self.kind.expect_object();
            return match object_ty.kind {
                ObjectTyKind::Tuple(ty) => Some(ty),
                ObjectTyKind::Reference(ty) => ty.target.kind.as_object_tuple(),
                _ => unreachable!(),
            };
        };
        None
    }

    pub fn useable_as_prop_name(&self) -> bool {
        self.flags
            .intersects(TypeFlags::STRING_OR_NUMBER_LITERAL_OR_UNIQUE)
    }

    pub fn is_pattern_lit_placeholder_ty(&self) -> bool {
        if let Some(i) = self.kind.as_intersection() {
            let mut seen_placeholder = false;
            for t in i.tys {
                if t.flags
                    .intersects(TypeFlags::LITERAL.union(TypeFlags::NULLABLE))
                    || t.is_pattern_lit_placeholder_ty()
                {
                    seen_placeholder = true;
                } else if !t.flags.intersects(TypeFlags::OBJECT) {
                    return false;
                }
            }
            seen_placeholder
        } else {
            const PLACEHOLDER: TypeFlags = TypeFlags::ANY
                .union(TypeFlags::STRING)
                .union(TypeFlags::NUMBER)
                .union(TypeFlags::BIG_INT);
            self.flags.intersects(PLACEHOLDER) || self.is_pattern_lit_ty()
        }
    }

    pub fn is_literal_ty(&self) -> bool {
        self.flags.intersects(TypeFlags::BOOLEAN)
            || if let Some(u) = self.kind.as_union() {
                if self.flags.contains(TypeFlags::ENUM_LITERAL) {
                    true
                } else {
                    u.tys.iter().all(|ty| ty.is_unit())
                }
            } else {
                self.is_unit()
            }
    }

    pub fn is_pattern_lit_ty(&self) -> bool {
        match self.kind {
            TyKind::TemplateLit(n) => n.tys.iter().all(|ty| ty.is_pattern_lit_placeholder_ty()),
            TyKind::StringMapping(n) => n.ty.is_pattern_lit_placeholder_ty(),
            _ => false,
        }
    }

    pub fn fresh_ty_links_id(&self) -> Option<FreshTyLinksID<'cx>> {
        let links = match self.kind {
            TyKind::StringLit(n) => n.links,
            TyKind::NumberLit(n) => n.links,
            TyKind::BigIntLit(n) => n.links,
            TyKind::Union(n) => n.fresh_ty_links,
            TyKind::Enum(n) => n.fresh_ty_links,
            TyKind::Object(n) => return n.kind.as_anonymous().map(|a| a.fresh_ty_links),
            _ => return None,
        };
        Some(links)
    }

    pub const fn is_unit(&self) -> bool {
        self.flags.intersects(TypeFlags::UNIT)
    }

    pub fn is_readonly_array(&self, checker: &TyChecker<'cx>) -> bool {
        self.kind.as_object_reference().is_some_and(|ty| {
            let t = checker.global_readonly_array_ty();
            ty.target == t || self == t
        })
    }

    pub fn alias_symbol(&self) -> Option<SymbolID> {
        match self.kind {
            TyKind::Union(ty) => ty.alias_symbol,
            TyKind::Intersection(ty) => ty.alias_symbol,
            TyKind::Cond(ty) => ty.alias_symbol,
            TyKind::Object(ty) => ty.kind.alias_symbol(),
            _ => None,
        }
    }

    pub fn alias_ty_arguments(&self) -> Option<Tys<'cx>> {
        match self.kind {
            TyKind::Union(ty) => ty.alias_ty_arguments,
            TyKind::Intersection(ty) => ty.alias_ty_arguments,
            TyKind::Cond(ty) => ty.alias_ty_arguments,
            TyKind::Object(ty) => ty.kind.alias_ty_arguments(),
            _ => None,
        }
    }

    pub fn is_coercible_under_double_equals(source: &Self, target: &Self) -> bool {
        source.flags.intersects(
            TypeFlags::NUMBER
                .union(TypeFlags::STRING)
                .union(TypeFlags::BOOLEAN_LITERAL),
        ) || target.flags.intersects(
            TypeFlags::NUMBER
                .union(TypeFlags::STRING)
                .union(TypeFlags::BOOLEAN),
        )
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
        self.is_object() || self.is_union() || self.is_intersection()
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
        self.is_index_ty() || self.is_string_mapping_ty() | self.is_template_lit_ty()
    }

    pub fn is_instantiable(&self) -> bool {
        self.is_instantiable_non_primitive() || self.is_instantiable_primitive()
    }

    pub fn is_single_element_generic_tuple_type(&self) -> bool {
        self.is_generic_tuple_type() && {
            let r = self.expect_object_reference();
            r.target.kind.expect_object_tuple().element_flags.len() == 1
        }
    }

    pub fn is_generic_tuple_type(&self) -> bool {
        self.as_object()
            .is_some_and(|o| o.kind.as_generic_tuple_type().is_some())
    }

    pub fn is_this_ty_param(&self) -> bool {
        self.as_param()
            .map(|param| param.is_this_ty)
            .unwrap_or_default()
    }

    pub fn is_array(&self, checker: &TyChecker<'cx>) -> bool {
        self.as_object_reference().is_some_and(|ty| {
            ty.target == checker.global_array_ty()
                || ty.target == checker.global_readonly_array_ty()
        })
    }
}

#[derive(Debug)]
pub struct UniqueESSymbolTy {
    pub symbol: SymbolID,
    pub escape_name: SymbolName,
}

#[derive(Debug)]
pub struct TemplateLitTy<'cx> {
    pub texts: &'cx [Atom],
    pub tys: Tys<'cx>,
}

#[derive(Debug)]
pub struct StringMappingTy<'cx> {
    pub symbol: SymbolID,
    pub ty: &'cx Ty<'cx>,
}

#[derive(Debug)]
pub struct CondTyRoot<'cx> {
    pub node: &'cx ast::CondTy<'cx>,
    pub check_ty: &'cx Ty<'cx>,
    pub extends_ty: &'cx Ty<'cx>,
    pub is_distributive: bool,
    pub infer_ty_params: Option<Tys<'cx>>,
    pub outer_ty_params: Option<Tys<'cx>>,
    pub alias_symbol: Option<SymbolID>,
    pub alias_ty_args: Option<Tys<'cx>>,
}

#[derive(Debug)]
pub struct CondTy<'cx> {
    pub root: &'cx CondTyRoot<'cx>,
    pub check_ty: &'cx Ty<'cx>,
    pub extends_ty: &'cx Ty<'cx>,
    pub mapper: Option<&'cx dyn TyMap<'cx>>,
    pub combined_mapper: Option<&'cx dyn TyMap<'cx>>,
    pub alias_symbol: Option<SymbolID>,
    pub alias_ty_arguments: Option<Tys<'cx>>,
    pub conditional_links: ConditionalLinksID<'cx>,
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

#[derive(Debug)]
pub struct IndexedAccessTy<'cx> {
    pub object_ty: &'cx self::Ty<'cx>,
    pub index_ty: &'cx self::Ty<'cx>,
    pub access_flags: AccessFlags,
    // alias_symbol: Option<SymbolID>,
    // alias_ty_arguments: Option<&'cx Tys<'cx>>,
}

#[derive(Debug)]
pub struct UnionTy<'cx> {
    pub tys: Tys<'cx>,
    pub origin: Option<&'cx self::Ty<'cx>>,
    pub object_flags: ObjectFlags,
    pub fresh_ty_links: FreshTyLinksID<'cx>,
    pub union_ty_links: UnionTyLinksID<'cx>,
    pub promise_or_awaitable_links: PromiseOrAwaitableTyLinksID<'cx>,
    pub alias_symbol: Option<SymbolID>,
    pub alias_ty_arguments: Option<Tys<'cx>>,
}

#[derive(Debug)]
pub struct IntersectionTy<'cx> {
    pub tys: Tys<'cx>,
    pub object_flags: ObjectFlags,
    pub alias_symbol: Option<SymbolID>,
    pub alias_ty_arguments: Option<Tys<'cx>>,
}

#[derive(Debug)]
pub struct StringLitTy<'cx> {
    pub val: Atom,
    pub symbol: Option<SymbolID>,
    pub links: FreshTyLinksID<'cx>,
}

#[derive(Debug)]
pub struct BigIntLitTy<'cx> {
    pub neg: bool,
    pub val: Atom,
    pub links: FreshTyLinksID<'cx>,
}

#[derive(Debug)]
pub struct ParamTy<'cx> {
    pub symbol: SymbolID,
    pub offset: Option<usize>,
    pub target: Option<&'cx self::Ty<'cx>>,
    pub is_this_ty: bool,
}

///```ts
/// type A<B> = {
/// [key in keyof B]: string
///       //~~~~~~~ index type
/// }
/// ```
#[derive(Debug)]
pub struct IndexTy<'cx> {
    pub ty: &'cx self::Ty<'cx>,
    pub index_flags: IndexFlags,
}

#[derive(Debug)]
pub struct IntrinsicTy {
    pub object_flags: ObjectFlags,
    pub name: Atom,
}

/// For example:
///
/// ```ts
/// type A<T> = T extends number ? T : string;
///                              //~ this is a substitution type:
///                              // - `base_type` is `T`,
///                              // - `constraint` is `number`.
/// ```
#[derive(Debug)]
pub struct SubstitutionTy<'cx> {
    pub object_flags: ObjectFlags,
    pub base_ty: &'cx self::Ty<'cx>,
    pub constraint: &'cx self::Ty<'cx>,
}

#[derive(Debug)]
pub struct IterationTys<'cx> {
    pub yield_ty: &'cx self::Ty<'cx>,
    pub return_ty: &'cx self::Ty<'cx>,
    pub next_ty: &'cx self::Ty<'cx>,
}

#[derive(Debug)]
pub struct EnumTy<'cx> {
    pub symbol: SymbolID,
    pub fresh_ty_links: FreshTyLinksID<'cx>,
}
