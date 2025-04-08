mod assign;
mod check_bin_like;
mod check_call_like;
mod check_class_decl_like;
mod check_deferred;
mod check_expr;
mod check_expr_with_ty_args;
mod check_fn_like_decl;
mod check_fn_like_expr;
mod check_fn_like_symbol;
mod check_interface;
mod check_stmt;
mod check_ty;
mod check_ty_refer_ty_or_import;
mod check_type_related_to;
mod check_var_like;
mod create_ty;
mod cycle_check;
mod elaborate_error;
pub(crate) mod errors;
mod expect;
mod flow;
mod fn_mapper;
mod get_base_ty;
mod get_context;
mod get_contextual;
mod get_declared_ty;
mod get_effective_node;
mod get_mapped_ty_info;
mod get_sig;
mod get_simplified_ty;
mod get_symbol;
mod get_this_ty;
mod get_ty;
mod get_type_from_ty_refer_like;
mod get_type_from_var_like;
mod get_variances;
mod get_widened_ty;
mod get_write_ty;
mod index_info;
mod infer;
mod instantiate;
mod instantiation_ty_map;
mod is_context_sensitive;
mod is_deeply_nested_type;
mod is_valid;
mod links;
mod merge;
mod node_check_flags;
mod relation;
mod resolve;
mod resolve_structured_member;
mod symbol_info;
mod transient_symbol;
mod type_assignable;
mod type_predicate;
pub mod utils;

use std::borrow::Cow;

use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_config::NormalizedCompilerOptions;
use bolt_ts_utils::{fx_hashmap_with_capacity, no_hashmap_with_capacity, no_hashset_with_capacity};
use enumflags2::BitFlag;
use rustc_hash::{FxBuildHasher, FxHashMap};

use self::check_expr::IterationUse;
use self::create_ty::IntersectionFlags;
use self::cycle_check::ResolutionKey;
use self::flow::FlowTy;
use self::fn_mapper::{PermissiveMapper, RestrictiveMapper};
use self::get_context::{InferenceContextual, TyContextual};
use self::get_contextual::ContextFlags;
use self::get_simplified_ty::SimplifiedKind;
use self::get_variances::VarianceFlags;
use self::infer::InferenceContext;
use self::infer::{InferenceFlags, InferencePriority};
use self::instantiation_ty_map::InstantiationTyMap;
use self::instantiation_ty_map::{
    IndexedAccessTyMap, IntersectionMap, StringMappingTyMap, TyAliasInstantiationMap, TyCacheTrait,
    TyKey, UnionMap,
};
use self::links::NodeLinks;
pub use self::links::SymbolLinks;
use self::links::{SigLinks, TyLinks};
pub(crate) use self::merge::merge_module_augmentation_list_for_global;
use self::node_check_flags::NodeCheckFlags;
pub use self::resolve::ExpectedArgsCount;
use self::symbol_info::SymbolInfo;
use self::transient_symbol::TransientSymbol;
pub(crate) use self::transient_symbol::TransientSymbols;
use self::type_predicate::TyPred;
use self::utils::contains_ty;

use crate::bind::{
    self, FlowID, FlowNodes, GlobalSymbols, MergedSymbols, Symbol, SymbolFlags, SymbolID,
    SymbolName, SymbolTable,
};
use crate::graph::ModuleGraph;
use crate::parser::{AccessKind, AssignmentKind, Parser};
use crate::ty::{CheckFlags, IndexFlags, IterationTys, TYPEOF_NE_FACTS};
use crate::ty::{ElementFlags, ObjectFlags, Sig, SigFlags, SigID, TyID, TypeFacts, TypeFlags};
use crate::ty::{TyMapper, has_type_facts};
use crate::{ecma_rules, ir, keyword, ty};
use bolt_ts_ast::{self as ast};
use bolt_ts_ast::{BinOp, pprint_ident};

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(super) struct Ternary: u8 {
        const FALSE = 0x0;
        const UNKNOWN = 0x1;
        const MAYBE = 0x3;
        const TRUE = u8::MAX;
    }

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub(super) struct CheckMode: u8 {
        const CONTEXTUAL                = 1 << 0;
        const INFERENTIAL               = 1 << 1;
        const SKIP_CONTEXT_SENSITIVE    = 1 << 2;
        const SKIP_GENERIC_FUNCTIONS    = 1 << 3;
        const IS_FOR_SIGNATURE_HELP     = 1 << 4;
        const REST_BINDING_ELEMENT      = 1 << 5;
        const TYPE_ONLY                 = 1 << 6;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct F64Represent {
    inner: u64,
}

impl F64Represent {
    fn new(val: f64) -> Self {
        Self {
            inner: val.to_bits(),
        }
    }
    pub fn val(&self) -> f64 {
        f64::from_bits(self.inner)
    }
}

impl From<f64> for F64Represent {
    fn from(val: f64) -> Self {
        F64Represent::new(val)
    }
}

impl From<usize> for F64Represent {
    fn from(val: usize) -> Self {
        F64Represent::new(val as f64)
    }
}

impl From<F64Represent> for f64 {
    fn from(val: F64Represent) -> Self {
        f64::from_bits(val.inner)
    }
}

impl nohash_hasher::IsEnabled for F64Represent {}

bolt_ts_utils::index!(InferenceContextId);

pub struct TyChecker<'cx> {
    pub atoms: &'cx mut AtomMap<'cx>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    module_arena: &'cx bolt_ts_span::ModuleArena,
    config: &'cx NormalizedCompilerOptions,
    arena: &'cx bumpalo::Bump,
    pub(super) tys: Vec<&'cx ty::Ty<'cx>>,
    sigs: Vec<&'cx Sig<'cx>>,
    flow_nodes: Vec<FlowNodes<'cx>>,
    num_lit_tys: nohash_hasher::IntMap<F64Represent, &'cx ty::Ty<'cx>>,
    string_lit_tys: nohash_hasher::IntMap<AtomId, &'cx ty::Ty<'cx>>,
    bigint_lit_tys: FxHashMap<(bool, AtomId), &'cx ty::Ty<'cx>>,
    union_tys: UnionMap<'cx>,
    intersection_tys: IntersectionMap<'cx>,
    indexed_access_tys: IndexedAccessTyMap<'cx>,
    string_mapping_tys: StringMappingTyMap<'cx>,
    type_name: nohash_hasher::IntMap<TyID, String>,
    tuple_tys: nohash_hasher::IntMap<u64, &'cx ty::Ty<'cx>>,
    transient_symbols: TransientSymbols<'cx>,

    check_mode: Option<CheckMode>,
    inferences: Vec<InferenceContext<'cx>>,
    inference_contextual: Vec<InferenceContextual>,
    type_contextual: Vec<TyContextual<'cx>>,
    deferred_nodes: Vec<indexmap::IndexSet<ast::NodeID, FxBuildHasher>>,
    // === links ===
    symbol_links: FxHashMap<SymbolID, SymbolLinks<'cx>>,
    node_links: FxHashMap<ast::NodeID, NodeLinks<'cx>>,
    sig_links: nohash_hasher::IntMap<SigID, SigLinks<'cx>>,
    ty_links: nohash_hasher::IntMap<TyID, TyLinks<'cx>>,
    instantiation_ty_map: InstantiationTyMap<'cx>,
    ty_alias_instantiation_map: TyAliasInstantiationMap<'cx>,
    iteration_tys_map: nohash_hasher::IntMap<TyKey, ty::IterationTys<'cx>>,
    mark_tys: nohash_hasher::IntSet<TyID>,
    shared_flow_info: Vec<(FlowID, FlowTy<'cx>)>,
    common_ty_links_arena: ty::CommonTyLinksArena<'cx>,
    fresh_ty_links_arena: ty::FreshTyLinksArena<'cx>,
    interface_ty_links_arena: ty::InterfaceTyLinksArena<'cx>,
    // === ast ===
    pub p: &'cx Parser<'cx>,
    pub mg: &'cx ModuleGraph,
    // === global ===
    // === intrinsic types ===
    pub any_ty: &'cx ty::Ty<'cx>,
    pub auto_ty: &'cx ty::Ty<'cx>,
    pub wildcard_ty: &'cx ty::Ty<'cx>,
    pub error_ty: &'cx ty::Ty<'cx>,
    pub unknown_ty: &'cx ty::Ty<'cx>,
    pub undefined_ty: &'cx ty::Ty<'cx>,
    pub missing_ty: &'cx ty::Ty<'cx>,
    pub undefined_or_missing_ty: &'cx ty::Ty<'cx>,
    pub undefined_widening_ty: &'cx ty::Ty<'cx>,
    pub never_ty: &'cx ty::Ty<'cx>,
    pub silent_never_ty: &'cx ty::Ty<'cx>,
    pub implicit_never_ty: &'cx ty::Ty<'cx>,
    pub void_ty: &'cx ty::Ty<'cx>,
    pub null_ty: &'cx ty::Ty<'cx>,
    pub false_ty: &'cx ty::Ty<'cx>,
    pub regular_false_ty: &'cx ty::Ty<'cx>,
    pub true_ty: &'cx ty::Ty<'cx>,
    pub regular_true_ty: &'cx ty::Ty<'cx>,
    pub number_ty: &'cx ty::Ty<'cx>,
    pub string_ty: &'cx ty::Ty<'cx>,
    pub bigint_ty: &'cx ty::Ty<'cx>,
    pub non_primitive_ty: &'cx ty::Ty<'cx>,
    pub es_symbol_ty: &'cx ty::Ty<'cx>,
    pub non_inferrable_any_ty: &'cx ty::Ty<'cx>,
    pub intrinsic_marker_ty: &'cx ty::Ty<'cx>,

    permissive_mapper: &'cx PermissiveMapper,
    restrictive_mapper: &'cx RestrictiveMapper,
    // =======================
    error_symbol: SymbolID,
    global_this_symbol: SymbolID,
    arguments_symbol: SymbolID,
    resolving_symbol: SymbolID,
    empty_ty_literal_symbol: SymbolID,
    empty_symbols: &'cx SymbolTable,

    boolean_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    string_or_number_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    string_number_symbol_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    any_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    auto_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    typeof_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    unknown_sig: std::cell::OnceCell<&'cx Sig<'cx>>,
    any_fn_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    circular_constraint_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    resolving_default_type: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    no_constraint_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    empty_generic_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    empty_object_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    empty_ty_literal_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_object_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_fn_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_callable_fn_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_newable_fn_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_readonly_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    any_readonly_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_number_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_string_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_boolean_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_regexp_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_symbol_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    mark_super_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    mark_sub_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    mark_other_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    array_variances: std::cell::OnceCell<&'cx [VarianceFlags]>,
    no_ty_pred: std::cell::OnceCell<&'cx TyPred<'cx>>,
    template_constraint_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    any_iteration_tys: std::cell::OnceCell<IterationTys<'cx>>,
    empty_array: &'cx [u8; 0],
    // === resolver ===
    pub binder: &'cx mut bind::Binder,
    global_symbols: &'cx mut GlobalSymbols,
    merged_symbols: &'cx mut MergedSymbols,

    // === cycle check ===
    resolution_start: i32,
    resolution_tys: thin_vec::ThinVec<ResolutionKey>,
    resolution_res: thin_vec::ThinVec<bool>,
    current_node: Option<ast::NodeID>,
}

#[derive(Clone, Copy, Debug)]
enum PropName {
    String(AtomId),
    Num(f64),
}

fn cast_empty_array<'cx, T>(empty_array: &[u8; 0]) -> &'cx [T] {
    unsafe { &*(empty_array as *const [u8] as *const [T]) }
}

impl<'cx> TyChecker<'cx> {
    pub(crate) fn new(
        ty_arena: &'cx bumpalo::Bump,
        p: &'cx Parser<'cx>,
        mg: &'cx ModuleGraph,
        atoms: &'cx mut AtomMap<'cx>,
        config: &'cx NormalizedCompilerOptions,
        flow_nodes: Vec<FlowNodes<'cx>>,
        module_arena: &'cx bolt_ts_span::ModuleArena,
        binder: &'cx mut bind::Binder,
        merged_symbols: &'cx mut MergedSymbols,
        global_symbols: &'cx mut GlobalSymbols,
    ) -> Self {
        let mut symbol_links = fx_hashmap_with_capacity(p.module_count() * 1024);
        let mut transient_symbols = TransientSymbols::new(p.module_count() * 1024 * 64);
        let diags = Vec::with_capacity(p.module_count() * 32);
        let empty_array: &'cx [u8; 0] = ty_arena.alloc([]);
        let empty_symbols = ty_arena.alloc(bind::SymbolTable::new(0));

        let cap = p.module_count() * 1024 * 64;
        let mut tys = Vec::with_capacity(cap);
        let mut common_ty_links_arena = ty::CommonTyLinksArena::with_capacity(cap);

        macro_rules! make_intrinsic_type {
            ( { $( ($name: ident, $atom_id: expr, $ty_flags: expr, $object_flags: expr) ),* $(,)? } ) => {
                $(
                    let $name = {
                        let ty = ty::IntrinsicTy {
                            object_flags: $object_flags,
                            name: $atom_id,
                        };
                        let kind = ty::TyKind::Intrinsic(ty_arena.alloc(ty));
                        TyChecker::make_ty(kind, $ty_flags, &mut tys, &mut common_ty_links_arena, ty_arena)
                    };
                )*
            };
        }
        make_intrinsic_type!({
            (any_ty,                keyword::IDENT_ANY,     TypeFlags::ANY,             ObjectFlags::empty()),
            (auto_ty,               keyword::IDENT_ANY,     TypeFlags::ANY,             ObjectFlags::NON_INFERRABLE_TYPE),
            (wildcard_ty,           keyword::IDENT_ANY,     TypeFlags::ANY,             ObjectFlags::empty()),
            (error_ty,              keyword::IDENT_ERROR,   TypeFlags::ANY,             ObjectFlags::empty()),
            (non_inferrable_any_ty, keyword::IDENT_ANY,     TypeFlags::ANY,             ObjectFlags::CONTAINS_WIDENING_TYPE),
            (intrinsic_marker_ty,   keyword::KW_INTRINSIC,  TypeFlags::ANY,             ObjectFlags::empty()),
            (unknown_ty,            keyword::IDENT_UNKNOWN, TypeFlags::UNKNOWN,         ObjectFlags::empty()),
            (undefined_ty,          keyword::KW_UNDEFINED,  TypeFlags::UNDEFINED,       ObjectFlags::empty()),
            (missing_ty,            keyword::KW_UNDEFINED,  TypeFlags::UNDEFINED,       ObjectFlags::empty()),
            (es_symbol_ty,          keyword::IDENT_SYMBOL,  TypeFlags::ES_SYMBOL,       ObjectFlags::empty()),
            (void_ty,               keyword::KW_VOID,       TypeFlags::VOID,            ObjectFlags::empty()),
            (never_ty,              keyword::IDENT_NEVER,   TypeFlags::NEVER,           ObjectFlags::empty()),
            (null_ty,               keyword::KW_NULL,       TypeFlags::NULL,            ObjectFlags::empty()),
            (false_ty,              keyword::KW_FALSE,      TypeFlags::BOOLEAN_LITERAL, ObjectFlags::empty()),
            (regular_false_ty,      keyword::KW_FALSE,      TypeFlags::BOOLEAN_LITERAL, ObjectFlags::empty()),
            (true_ty,               keyword::KW_TRUE,       TypeFlags::BOOLEAN_LITERAL, ObjectFlags::empty()),
            (regular_true_ty,       keyword::KW_TRUE,       TypeFlags::BOOLEAN_LITERAL, ObjectFlags::empty()),
            (number_ty,             keyword::IDENT_NUMBER,  TypeFlags::NUMBER,          ObjectFlags::empty()),
            (string_ty,             keyword::IDENT_STRING,  TypeFlags::STRING,          ObjectFlags::empty()),
            (bigint_ty,             keyword::IDENT_BIGINT,  TypeFlags::BIG_INT,         ObjectFlags::empty()),
            (non_primitive_ty,      keyword::IDENT_OBJECT,  TypeFlags::NON_PRIMITIVE,   ObjectFlags::empty()),
            (silent_never_ty,       keyword::IDENT_NEVER,   TypeFlags::NEVER,           ObjectFlags::NON_INFERRABLE_TYPE),
            (implicit_never_ty,     keyword::IDENT_NEVER,   TypeFlags::NEVER,           ObjectFlags::empty()),
        });

        let undefined_or_missing_ty = if *config.exact_optional_property_types() {
            missing_ty
        } else {
            undefined_ty
        };
        let undefined_widening_ty = if *config.strict_null_checks() {
            undefined_ty
        } else {
            let ty = ty::IntrinsicTy {
                object_flags: ObjectFlags::CONTAINS_WIDENING_TYPE,
                name: keyword::KW_UNDEFINED,
            };
            let kind = ty::TyKind::Intrinsic(ty_arena.alloc(ty));
            TyChecker::make_ty(
                kind,
                TypeFlags::UNDEFINED,
                &mut tys,
                &mut common_ty_links_arena,
                ty_arena,
            )
        };

        macro_rules! make_builtin_symbol {
            ( { $( ($symbol_name: ident, $name: expr, $flags: expr, $links: expr, $builtin_id: ident, $declarations: expr) ),* $(,)? } ) => {
                $(
                    let $symbol_name = {
                        let symbol = TransientSymbol {
                            name: $name,
                            flags: $flags,
                            links: Some($links),
                            decls: $declarations,
                            value_declaration: None,
                            merged_id: None,
                        };
                        let s = transient_symbols.create_transient_symbol(symbol);
                        assert_eq!(s, Symbol::$builtin_id);
                        symbol_links.insert(s, $links);
                        s
                    };
                )*
            };
        }
        let global_this_symbol_name = SymbolName::Atom(keyword::IDENT_GLOBAL_THIS);
        make_builtin_symbol!({
            (error_symbol,              SymbolName::Atom(keyword::IDENT_EMPTY),         SymbolFlags::empty(),       SymbolLinks::default().with_ty(error_ty),           ERR,                Default::default()),
            (global_this_symbol,        global_this_symbol_name,                        SymbolFlags::empty(),       SymbolLinks::default()
                                                                                                                            .with_check_flags(CheckFlags::READONLY),    GLOBAL_THIS,        Default::default()),
            (arguments_symbol,          SymbolName::Atom(keyword::IDENT_ARGUMENTS),     SymbolFlags::PROPERTY,      SymbolLinks::default(),                             ARGUMENTS,          Default::default()),
            (resolving_symbol,          SymbolName::Resolving,                          SymbolFlags::empty(),       SymbolLinks::default(),                             RESOLVING,          Default::default()),
            (empty_ty_literal_symbol,   SymbolName::Type,                               SymbolFlags::TYPE_LITERAL,  SymbolLinks::default(),                             EMPTY_TYPE_LITERAL, Default::default()),
        });

        let prev = global_symbols
            .0
            .insert(global_this_symbol_name, global_this_symbol);
        assert!(prev.is_none());

        let restrictive_mapper = ty_arena.alloc(RestrictiveMapper);
        let permissive_mapper = ty_arena.alloc(PermissiveMapper);

        let mut this = Self {
            atoms,
            p,
            mg,
            config,

            tys,
            sigs: Vec::with_capacity(p.module_count() * 256),
            arena: ty_arena,
            diags,
            module_arena,

            num_lit_tys: no_hashmap_with_capacity(1024 * 8),
            string_lit_tys: no_hashmap_with_capacity(1024 * 8),
            bigint_lit_tys: fx_hashmap_with_capacity(512),
            union_tys: UnionMap::new(1024 * 8),
            intersection_tys: IntersectionMap::new(1024 * 8),
            indexed_access_tys: IndexedAccessTyMap::new(1024 * 8),
            string_mapping_tys: StringMappingTyMap::new(1024 * 8),
            instantiation_ty_map: InstantiationTyMap::new(1024 * 16),
            ty_alias_instantiation_map: TyAliasInstantiationMap::new(1024 * 16),
            iteration_tys_map: no_hashmap_with_capacity(1024 * 4),
            mark_tys: no_hashset_with_capacity(1024 * 4),
            transient_symbols,

            shared_flow_info: Vec::with_capacity(1024),
            flow_nodes,

            error_symbol,
            global_this_symbol,
            arguments_symbol,
            resolving_symbol,
            empty_ty_literal_symbol,
            empty_symbols,

            empty_array,
            any_ty,
            auto_ty,
            wildcard_ty,
            error_ty,
            unknown_ty,
            undefined_ty,
            missing_ty,
            undefined_or_missing_ty,
            undefined_widening_ty,
            never_ty,
            silent_never_ty,
            implicit_never_ty,
            void_ty,
            null_ty,
            false_ty,
            regular_false_ty,
            true_ty,
            regular_true_ty,
            number_ty,
            string_ty,
            bigint_ty,
            non_primitive_ty,
            es_symbol_ty,
            non_inferrable_any_ty,
            intrinsic_marker_ty,

            restrictive_mapper,
            permissive_mapper,

            any_fn_ty: Default::default(),
            circular_constraint_ty: Default::default(),
            no_constraint_ty: Default::default(),
            resolving_default_type: Default::default(),
            empty_generic_ty: Default::default(),
            empty_object_ty: Default::default(),
            boolean_ty: Default::default(),
            typeof_ty: Default::default(),
            string_or_number_ty: Default::default(),
            string_number_symbol_ty: Default::default(),
            any_array_ty: Default::default(),
            auto_array_ty: Default::default(),
            empty_ty_literal_ty: Default::default(),
            global_number_ty: Default::default(),
            global_string_ty: Default::default(),
            global_symbol_ty: Default::default(),
            global_boolean_ty: Default::default(),
            global_array_ty: Default::default(),
            global_readonly_array_ty: Default::default(),
            global_regexp_ty: Default::default(),
            any_readonly_array_ty: Default::default(),
            global_fn_ty: Default::default(),
            global_callable_fn_ty: Default::default(),
            global_newable_fn_ty: Default::default(),
            global_object_ty: Default::default(),
            array_variances: Default::default(),
            mark_super_ty: Default::default(),
            mark_sub_ty: Default::default(),
            mark_other_ty: Default::default(),
            template_constraint_ty: Default::default(),
            any_iteration_tys: Default::default(),

            no_ty_pred: Default::default(),

            unknown_sig: Default::default(),

            type_name: no_hashmap_with_capacity(1024 * 8),

            symbol_links,
            node_links: fx_hashmap_with_capacity(cap),
            sig_links: no_hashmap_with_capacity(cap),
            ty_links: no_hashmap_with_capacity(cap),
            tuple_tys: no_hashmap_with_capacity(cap),
            common_ty_links_arena,
            fresh_ty_links_arena: ty::FreshTyLinksArena::with_capacity(cap),
            interface_ty_links_arena: ty::InterfaceTyLinksArena::with_capacity(cap),

            resolution_tys: thin_vec::ThinVec::with_capacity(128),
            resolution_res: thin_vec::ThinVec::with_capacity(128),
            resolution_start: 0,

            binder,
            merged_symbols,
            global_symbols,
            inferences: Vec::with_capacity(cap),
            inference_contextual: Vec::with_capacity(256),
            type_contextual: Vec::with_capacity(256),
            check_mode: None,
            deferred_nodes: vec![
                indexmap::IndexSet::with_capacity_and_hasher(64, FxBuildHasher);
                p.module_count()
            ],
            current_node: None,
        };

        macro_rules! make_global {
            ( { $( ($name: ident, $make_ty: expr) ),* $(,)? } ) => {
                $(
                    let $name = $make_ty;
                    this.$name.set($name).unwrap();
                )*
            };
        }
        make_global!({
            (boolean_ty,                this.get_union_ty(&[regular_false_ty, regular_true_ty], ty::UnionReduction::Lit)),
            (string_or_number_ty,       this.get_union_ty(&[this.string_ty, this.number_ty], ty::UnionReduction::Lit)),
            (string_number_symbol_ty,   this.get_union_ty(&[this.string_ty, this.number_ty, this.es_symbol_ty], ty::UnionReduction::Lit)),
            (global_number_ty,          this.get_global_type(SymbolName::Atom(keyword::IDENT_NUMBER_CLASS))),
            (global_boolean_ty,         this.get_global_type(SymbolName::Atom(keyword::IDENT_BOOLEAN_CLASS))),
            (global_symbol_ty,          this.get_global_type(SymbolName::Atom(keyword::IDENT_SYMBOL_CLASS))),
            (global_string_ty,          this.get_global_type(SymbolName::Atom(keyword::IDENT_STRING_CLASS))),
            (global_array_ty,           this.get_global_type(SymbolName::Atom(keyword::IDENT_ARRAY_CLASS))),
            (global_regexp_ty,          this.get_global_type(SymbolName::Atom(keyword::IDENT_REGEXP_CLASS))),
            (any_array_ty,              this.create_array_ty(this.any_ty, false)),
            (global_readonly_array_ty,  this.get_global_type(SymbolName::Atom(keyword::IDENT_READONLY_ARRAY_CLASS))),
            (any_readonly_array_ty,     this.any_array_ty()),
            (typeof_ty,                 {
                                            let tys = TYPEOF_NE_FACTS.iter().map(|(key, _)| this.get_string_literal_type(*key)).collect::<Vec<_>>();
                                            this.get_union_ty(&tys, ty::UnionReduction::Lit)
                                        }),
            (any_fn_ty,                 this.create_anonymous_ty_with_resolved(None, ObjectFlags::NON_INFERRABLE_TYPE, this.alloc(Default::default()), Default::default(), Default::default(), Default::default())),
            (no_constraint_ty,          this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default())),
            (circular_constraint_ty,    this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default())),
            (resolving_default_type,    this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default())),
            (empty_generic_ty,          this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default())),
            (empty_object_ty,           this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default())),
            (empty_ty_literal_ty,       this.create_anonymous_ty_with_resolved(Some(empty_ty_literal_symbol), Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default())),
            (global_object_ty,          this.get_global_type(SymbolName::Atom(keyword::IDENT_OBJECT_CLASS))),
            (global_fn_ty,              this.get_global_type(SymbolName::Atom(keyword::IDENT_FUNCTION_CLASS))),
            (global_callable_fn_ty,     this.get_global_type(SymbolName::Atom(keyword::IDENT_CALLABLE_FUNCTION_CLASS))),
            (global_newable_fn_ty,      this.get_global_type(SymbolName::Atom(keyword::IDENT_NEWABLE_FUNCTION_CLASS))),
            (mark_sub_ty,               this.create_param_ty(Symbol::ERR, None, false)),
            (mark_other_ty,             this.create_param_ty(Symbol::ERR, None, false)),
            (mark_super_ty,             this.create_param_ty(Symbol::ERR, None, false)),
            (template_constraint_ty,    this.get_union_ty(&[string_ty, number_ty, boolean_ty, bigint_ty, null_ty, undefined_ty], ty::UnionReduction::Lit)),
            (any_iteration_tys,         this.create_iteration_tys(any_ty, any_ty, any_ty)),
            (unknown_sig,               this.new_sig(Sig { flags: SigFlags::empty(), ty_params: None, params: &[], min_args_count: 0, ret: None, node_id: None, target: None, mapper: None, id: SigID::dummy(), class_decl: None })),
            (array_variances,           this.alloc([VarianceFlags::COVARIANT])),
            (no_ty_pred,                this.create_ident_ty_pred(keyword::IDENT_EMPTY, 0, any_ty))
        });

        this.type_name.insert(boolean_ty.id, "boolean".to_string());

        let iarguments = this.get_global_type(SymbolName::Atom(keyword::IDENT_IARGUMENTS_CLASS));
        this.get_mut_symbol_links(arguments_symbol)
            .set_ty(iarguments);

        let mut auto_array_ty = this.create_array_ty(this.auto_ty, false);
        if auto_array_ty == empty_object_ty {
            auto_array_ty = this.create_anonymous_ty_with_resolved(
                None,
                Default::default(),
                this.alloc(Default::default()),
                Default::default(),
                Default::default(),
                Default::default(),
            );
        }
        this.auto_array_ty.set(auto_array_ty).unwrap();

        this.merge_module_augmentation_list_for_non_global();

        this
    }

    pub fn print_ty<'a>(&'a mut self, ty: &'cx ty::Ty<'cx>) -> &'a str {
        let type_name = (!self.type_name.contains_key(&ty.id)).then(|| ty.to_string(self));
        self.type_name
            .entry(ty.id)
            .or_insert_with(|| type_name.unwrap())
    }

    pub fn unknown_sig(&self) -> &'cx Sig<'cx> {
        self.unknown_sig.get().unwrap()
    }

    pub fn any_iteration_tys(&self) -> ty::IterationTys<'cx> {
        self.any_iteration_tys.get().copied().unwrap()
    }

    pub fn array_variances(&self) -> &'cx [VarianceFlags] {
        self.array_variances.get().unwrap()
    }

    pub fn check_program(&mut self, program: &'cx ast::Program<'cx>) {
        for stmt in program.stmts {
            self.check_stmt(stmt);
        }
    }

    fn is_applicable_index_ty(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        self.is_type_assignable_to(source, target)
            || (target == self.string_ty && self.is_type_assignable_to(source, self.number_ty))
    }

    fn get_base_constraint_or_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        self.get_base_constraint_of_ty(ty).unwrap_or(ty)
    }

    fn get_base_constraint_of_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        if ty.flags.intersects(
            TypeFlags::INSTANTIABLE_NON_PRIMITIVE
                .union(TypeFlags::UNION_OR_INTERSECTION)
                .union(TypeFlags::TEMPLATE_LITERAL)
                .union(TypeFlags::STRING_MAPPING),
        ) || ty.kind.is_generic_tuple_type()
        {
            let constraint = self.get_resolved_base_constraint(ty);
            if constraint != self.no_constraint_ty() && constraint != self.circular_constraint_ty()
            {
                Some(constraint)
            } else {
                None
            }
        } else if ty.kind.is_index_ty() {
            Some(self.string_number_symbol_ty())
        } else {
            None
        }
    }

    fn get_apparent_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let ty = if ty.kind.is_instantiable() {
            self.get_base_constraint_of_ty(ty)
                .unwrap_or(self.undefined_ty)
        } else {
            ty
        };
        let flags = ty.flags;
        if flags.intersects(TypeFlags::NUMBER_LIKE) {
            self.global_number_ty()
        } else if flags.intersects(TypeFlags::STRING_LIKE) {
            self.global_string_ty()
        } else if flags.intersects(TypeFlags::BOOLEAN_LIKE) {
            self.global_boolean_ty()
        } else if flags.intersects(TypeFlags::ES_SYMBOL_LIKE) {
            self.global_symbol_ty()
        } else if flags.intersects(TypeFlags::NON_PRIMITIVE) {
            self.empty_object_ty()
        } else if flags.intersects(TypeFlags::INDEX) {
            self.string_number_symbol_ty()
        } else if flags.intersects(TypeFlags::UNKNOWN) && !*self.config.strict_null_checks() {
            self.empty_object_ty()
        } else {
            ty
        }
    }

    fn get_reduced_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.kind.as_union().is_some_and(|union| {
            union
                .object_flags
                .intersects(ObjectFlags::CONTAINS_INTERSECTIONS)
        }) {
            // TODO:
        } else if let Some(intersection) = ty.kind.as_intersection() {
            // intersection
            //     .object_flags
            //     .intersects(ObjectFlags::IS_NEVER_INTERSECTION_COMPUTED);

            if intersection
                .object_flags
                .intersects(ObjectFlags::IS_NEVER_INTERSECTION)
            {
                return self.never_ty;
            } else {
                return ty;
            }
        }
        ty
    }

    fn get_reduced_apparent_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let ty = self.get_reduced_ty(ty);
        let ty = self.get_apparent_ty(ty);
        self.get_reduced_ty(ty)
    }

    fn get_applicable_index_infos(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        prop_name_ty: &'cx ty::Ty<'cx>,
    ) -> Vec<&'cx ty::IndexInfo<'cx>> {
        self.index_infos_of_ty(ty)
            .iter()
            .filter_map(|info| {
                if self.is_applicable_index_ty(prop_name_ty, info.key_ty) {
                    Some(*info)
                } else {
                    None
                }
            })
            .collect()
    }

    fn check_index_constraint_for_prop(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        prop: SymbolID,
        prop_name_ty: &'cx ty::Ty<'cx>,
        prop_ty: &'cx ty::Ty<'cx>,
    ) {
        for index_info in self.get_applicable_index_infos(ty, prop_name_ty) {
            if !self.is_type_assignable_to(prop_ty, index_info.val_ty) {
                let prop_decl = self.get_symbol_decl(prop).unwrap();
                let prop_node = self.p.node(prop_decl);
                let prop_name = match prop_node {
                    ast::Node::ClassPropElem(prop) => prop.name,
                    ast::Node::PropSignature(prop) => prop.name,
                    _ => unreachable!(),
                };
                let prop_name = match prop_name.kind {
                    ast::PropNameKind::Ident(ident) => self.atoms.get(ident.name).to_string(),
                    ast::PropNameKind::NumLit(num) => num.val.to_string(),
                    ast::PropNameKind::StringLit { raw, .. } => {
                        format!("\"{}\"", self.atoms.get(raw.val))
                    }
                    bolt_ts_ast::PropNameKind::Computed(_) => "[...]".to_string(),
                };
                let error = errors::PropertyAOfTypeBIsNotAssignableToCIndexTypeD {
                    span: prop_node.span(),
                    prop: prop_name,
                    ty_b: self.print_ty(prop_ty).to_string(),
                    ty_c: self.print_ty(index_info.key_ty).to_string(),
                    index_ty_d: self.print_ty(index_info.val_ty).to_string(),
                };
                self.push_error(Box::new(error));
                return;
            }
        }

        if ty.kind.as_object_interface().is_some() {
            let base_tys = self
                .get_ty_links(ty.id)
                .expect_structured_members()
                .base_tys;
            for base_ty in base_tys {
                self.check_index_constraint_for_prop(base_ty, prop, prop_name_ty, prop_ty);
            }
        } else {
            // unreachable!("{:#?}", ty)
        }
    }

    fn get_lit_ty_from_prop_name(&mut self, prop_name: &ast::PropName<'cx>) -> &'cx ty::Ty<'cx> {
        match prop_name.kind {
            ast::PropNameKind::Ident(ident) => self.get_string_literal_type(ident.name),
            ast::PropNameKind::NumLit(num) => {
                let ty = self.get_number_literal_type(num.val);
                self.get_regular_ty_of_literal_ty(ty)
            }
            ast::PropNameKind::StringLit { key, .. } => self.get_string_literal_type(key),
            bolt_ts_ast::PropNameKind::Computed(_) => todo!(),
        }
    }

    fn get_declaration_modifier_flags_from_symbol(
        &self,
        symbol: SymbolID,
        is_write: Option<bool>,
    ) -> enumflags2::BitFlags<bolt_ts_ast::ModifierKind> {
        let is_write = is_write.unwrap_or(true);
        let s = self.symbol(symbol);
        fn find_decls<'cx>(
            this: &TyChecker<'cx>,
            s: transient_symbol::CheckSymbol,
            f: impl Fn(ast::Node<'cx>) -> bool,
        ) -> Option<ast::NodeID> {
            s.declarations()
                .iter()
                .find(|id| f(this.p.node(**id)))
                .copied()
        }
        if let Some(value_declaration) = s.value_declaration() {
            let decl = is_write
                .then(|| find_decls(self, s, |n| n.is_setter_decl()))
                .flatten()
                .or_else(|| {
                    if s.flags().intersects(SymbolFlags::GET_ACCESSOR) {
                        find_decls(self, s, |n| n.is_getter_decl())
                    } else {
                        None
                    }
                })
                .unwrap_or(value_declaration);
            let flags = self.p.get_combined_modifier_flags(decl);
            // TODO: if s.parent.flags & Class
            return flags & ast::ModifierKind::ACCESSIBILITY;
        }
        let check_flags = self.get_check_flags(symbol);
        if check_flags.intersects(CheckFlags::SYNTHETIC) {
            let access_modifier: enumflags2::BitFlags<_> =
                if check_flags.intersects(CheckFlags::CONTAINS_PRIVATE) {
                    ast::ModifierKind::Private
                } else if check_flags.intersects(CheckFlags::CONTAINS_PUBLIC) {
                    ast::ModifierKind::Public
                } else {
                    ast::ModifierKind::Protected
                }
                .into();
            let static_modifier = if check_flags.intersects(CheckFlags::CONTAINS_STATIC) {
                ast::ModifierKind::Static.into()
            } else {
                ast::ModifierKind::empty()
            };
            static_modifier | access_modifier
        } else if s.flags().intersects(SymbolFlags::PROPERTY) {
            use ast::ModifierKind;
            enumflags2::make_bitflags!(ModifierKind::{Public | Static})
        } else {
            Default::default()
        }
    }

    fn get_lit_ty_from_prop(
        &mut self,
        prop: SymbolID,
        include: TypeFlags,
        include_non_public: bool,
    ) -> &'cx ty::Ty<'cx> {
        if include_non_public
            || !self
                .get_declaration_modifier_flags_from_symbol(prop, None)
                .intersects(ast::ModifierKind::NON_PUBLIC_ACCESSIBILITY_MODIFIER)
        {
            // TODO: late bound
            let ty = match self.get_symbol_links(prop).get_name_ty() {
                Some(named_ty) => Some(named_ty),
                None => {
                    let symbol_name = self.symbol(prop).name();
                    if symbol_name == SymbolName::ExportDefault {
                        Some(self.get_string_literal_type(keyword::KW_DEFAULT))
                    } else {
                        self.symbol(prop)
                            .value_declaration()
                            .and_then(|v_decl| {
                                self.p.get_name_of_decl(v_decl).map(|name| {
                                    let kind = match name {
                                        ast::DeclarationName::Ident(ident) => {
                                            ast::PropNameKind::Ident(ident)
                                        }
                                        ast::DeclarationName::NumLit(lit) => {
                                            ast::PropNameKind::NumLit(lit)
                                        }
                                        ast::DeclarationName::StringLit { raw, key } => {
                                            ast::PropNameKind::StringLit { raw, key }
                                        }
                                        ast::DeclarationName::Computed(n) => {
                                            ast::PropNameKind::Computed(n)
                                        }
                                    };
                                    ast::PropName { kind }
                                })
                            })
                            .map(|name| self.get_lit_ty_from_prop_name(&name))
                            .or_else(|| {
                                if let Some(num) = symbol_name.as_numeric() {
                                    let val = num.to_string();
                                    let atom = self.atoms.insert_by_str(Cow::Owned(val));
                                    Some(self.get_string_literal_type(atom))
                                } else if let Some(atom) = symbol_name.as_atom() {
                                    Some(self.get_string_literal_type(atom))
                                } else {
                                    None
                                }
                            })
                    }
                }
            };
            if let Some(ty) = ty {
                if ty.flags.intersects(include) {
                    return ty;
                }
            }
        }
        self.never_ty
    }

    fn check_index_constraints(&mut self, ty: &'cx ty::Ty<'cx>, is_static_index: bool) {
        let index_infos = self.get_index_infos_of_ty(ty);
        if index_infos.is_empty() {
            return;
        }
        for prop in self.properties_of_object_type(ty) {
            if !(is_static_index
                && self
                    .symbol(*prop)
                    .flags()
                    .intersects(SymbolFlags::PROTOTYPE))
            {
                let prop_ty = self.get_type_of_symbol(*prop);
                let prop_name_ty = self.get_lit_ty_from_prop(
                    *prop,
                    TypeFlags::STRING_OR_NUMBER_LITERAL_OR_UNIQUE,
                    true,
                );
                self.check_index_constraint_for_prop(ty, *prop, prop_name_ty, prop_ty);
            }
        }
    }

    fn get_containing_fn_or_class_static_block(&self, node: ast::NodeID) -> Option<ast::NodeID> {
        self.p.find_ancestor(node, |node| {
            node.is_fn_like_or_class_static_block_decl().then_some(true)
        })
    }

    fn instantiate_ty_with_single_generic_call_sig(
        &mut self,
        id: ast::NodeID,
        ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(check_mode) = self.check_mode else {
            return ty;
        };
        if !check_mode.intersects(CheckMode::INFERENTIAL | CheckMode::SKIP_GENERIC_FUNCTIONS) {
            return ty;
        }
        let Some((sig, kind)) = self
            .get_single_sig(ty, ty::SigKind::Call, true)
            .map(|sig| (sig, ty::SigKind::Call))
            .or_else(|| {
                self.get_single_sig(ty, ty::SigKind::Constructor, true)
                    .map(|sig| (sig, ty::SigKind::Constructor))
            })
        else {
            return ty;
        };
        let Some(ty_params) = sig.ty_params else {
            return ty;
        };
        let Some(contextual_ty) =
            self.get_apparent_ty_of_contextual_ty(id, Some(ContextFlags::NO_CONSTRAINTS))
        else {
            return ty;
        };
        let Some(contextual_sig) = self.get_single_sig(contextual_ty, kind, false) else {
            return ty;
        };
        if contextual_sig.ty_params.is_some() {
            return ty;
        }
        if check_mode.intersects(CheckMode::SKIP_GENERIC_FUNCTIONS) {
            self.skip_generic_fn(id, check_mode);
            return self.any_fn_ty();
        }

        let context = self.get_inference_context(id).unwrap();
        let inference = context.inference.unwrap();
        let ret_sig = self
            .get_inference_sig(inference)
            .map(|sig| self.get_ret_ty_of_sig(sig))
            .and_then(|ret_ty| self.get_single_call_or_ctor_sig(ret_ty));
        if let Some(ret_sig) = ret_sig {
            if ret_sig.ty_params.is_none()
                && !self
                    .inference_infos(inference)
                    .iter()
                    .all(|info| info.has_inference_candidates())
            {
                todo!()
            }
        }

        let sig = self.instantiate_sig_in_context_of(sig, contextual_sig, Some(inference));
        let outer_ty_params = self
            .inference_contextual
            .iter()
            .filter_map(|inference| {
                inference.inference.map(|inference| {
                    self.inference_infos(inference)
                        .iter()
                        .map(|info| info.ty_param)
                        .collect::<Vec<_>>()
                })
            })
            .flatten()
            .collect::<Vec<_>>();
        self.get_or_create_ty_from_sig(sig, Some(self.alloc(outer_ty_params)))
    }

    fn get_or_create_ty_from_sig(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        outer_ty_params: Option<ty::Tys<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        //TODO: cache `isolated_sig_ty`
        let is_constructor = sig.node_id.is_none_or(|node_id| {
            use bolt_ts_ast::Node::*;
            matches!(self.p.node(node_id), ClassCtor(_) | CtorSigDecl(_))
        });
        if let Some(node_id) = sig.node_id {
            // decls in symbol
        }
        let symbol = self.create_transient_symbol(
            SymbolName::Fn,
            SymbolFlags::FUNCTION,
            SymbolLinks::default(),
            Default::default(), // TODO: use sig.decls
            None,               // TODO: use sig.value_decl
        );
        let outer_ty_params: Option<ty::Tys<'cx>> = if let Some(outer_ty_params) = outer_ty_params {
            Some(outer_ty_params)
        } else if let Some(id) = sig.node_id {
            if let Some(ty_params) = self.get_outer_ty_params(id, true) {
                Some(self.alloc(ty_params))
            } else {
                None
            }
        } else {
            None
        };
        let ty = self.create_single_sig_ty(ty::SingleSigTy {
            symbol,
            target: None,
            mapper: None,
            outer_ty_params,
        });
        let prev = self.ty_links.insert(
            ty.id,
            TyLinks::default().with_structured_members(self.alloc(ty::StructuredMembers {
                members: self.alloc(Default::default()),
                base_tys: &[],
                base_ctor_ty: None,
                call_sigs: if !is_constructor {
                    self.alloc([sig])
                } else {
                    &[]
                },
                ctor_sigs: if is_constructor {
                    self.alloc([sig])
                } else {
                    &[]
                },
                index_infos: &[],
                props: &[],
            })),
        );
        assert!(prev.is_none());
        ty
    }

    fn get_canonical_sig(&mut self, sig: &'cx ty::Sig<'cx>) -> &'cx ty::Sig<'cx> {
        let Some(ty_params) = sig.ty_params else {
            return sig;
        };
        if let Some(canonical_sig_cache) = self.get_sig_links(sig.id).get_canonical_sig() {
            return canonical_sig_cache;
        };
        let ty_args = ty_params
            .iter()
            .map(|ty| {
                let tp = ty.kind.expect_param();
                if let Some(target) = tp.target {
                    if self.get_constraint_of_ty_param(target).is_none() {
                        return target;
                    }
                }
                ty
            })
            .collect::<Vec<_>>();
        let ty_args = self.alloc(ty_args);
        let canonical_sig_cache = self.get_sig_instantiation(sig, Some(ty_args), false, None);
        self.get_mut_sig_links(sig.id)
            .set_canonical_sig(canonical_sig_cache);
        canonical_sig_cache
    }

    fn instantiate_sig_in_context_of(
        &mut self,
        sig: &'cx ty::Sig<'cx>,
        contextual_sig: &'cx ty::Sig<'cx>,
        inference_context: Option<InferenceContextId>,
    ) -> &'cx ty::Sig<'cx> {
        let context = {
            let ty_params = self.get_ty_params_for_mapper(sig);
            self.create_inference_context(ty_params, Some(sig), InferenceFlags::empty())
        };
        let rest_ty = contextual_sig.get_rest_ty(self);
        let mut mapper = None;
        if let Some(inference_context) = inference_context {
            if let Some(rest_ty) = rest_ty {
                if rest_ty.kind.is_param() {
                    mapper = Some(self.inference(inference_context).non_fixing_mapper);
                }
            } else {
                mapper = Some(self.inference(inference_context).mapper);
            }
        };
        let source_sig = if let Some(mapper) = mapper {
            self.instantiate_sig(contextual_sig, mapper, false)
        } else {
            contextual_sig
        };

        self.infer_state(context, None, false).apply_to_param_tys(
            source_sig,
            sig,
            |this, source, target| this.infer_from_tys(source, target),
        );
        if inference_context.is_none() {
            self.apply_to_ret_ty(contextual_sig, sig, |this, source, target| {
                this.infer_tys(
                    context,
                    source,
                    target,
                    Some(InferencePriority::RETURN_TYPE),
                    false,
                );
            });
        }

        let ty_args = self.get_inferred_tys(context);
        self.get_sig_instantiation(sig, Some(ty_args), false, None)
    }

    fn skip_generic_fn(&mut self, node: ast::NodeID, check_mode: CheckMode) {
        if check_mode.intersects(CheckMode::INFERENTIAL) {
            let context = self.get_inference_context(node).unwrap();
            let inference = context.inference.unwrap();
            self.config_inference_flags(inference, |flags| {
                *flags |= InferenceFlags::SKIPPED_GENERIC_FUNCTION;
            });
        }
    }

    fn try_get_this_ty_at(
        &mut self,
        expr: &'cx ast::ThisExpr,
        include_global_this: bool,
        container_id: Option<ast::NodeID>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let container_id =
            container_id.unwrap_or_else(|| self.p.get_this_container(expr.id, false, false));
        let container = self.p.node(container_id);
        if container.is_fn_like() {
            let this_ty = self
                .get_this_ty_of_decl(container_id)
                .or_else(|| self.get_contextual_this_param_ty(container_id));

            if let Some(this_ty) = this_ty {
                return Some(this_ty);
            }
        }

        if let Some(parent) = self.p.parent(container_id) {
            let p = self.p.node(parent);
            if p.is_class_like() {
                let symbol = self.get_symbol_of_decl(parent);
                let this_ty = if container.is_static() {
                    self.get_type_of_symbol(symbol)
                } else {
                    let ty = self
                        .get_declared_ty_of_symbol(symbol)
                        .kind
                        .expect_object_reference();
                    let ty = ty.target.kind.expect_object_interface();
                    ty.this_ty.unwrap()
                };
                return Some(this_ty);
            }
        }
        None
    }

    fn is_for_in_variable_for_numeric_prop_names(&self, expr: &'cx ast::Expr<'cx>) -> bool {
        let e = bolt_ts_ast::Expr::skip_parens(expr);
        if let ast::ExprKind::Ident(ident) = e.kind {
            let symbol = self.resolve_symbol_by_ident(ident);
            if self
                .symbol(symbol)
                .flags()
                .intersects(SymbolFlags::VARIABLE)
            {
                let mut child = ident.id;
                let mut node = self.p.parent(child);
                while let Some(id) = node {
                    let n = self.p.node(id);
                    if let Some(f) = n.as_for_in_stmt() {
                        todo!()
                    }
                    child = id;
                    node = self.p.parent(child);
                }
            }
        }
        false
    }

    fn type_has_static_prop(
        &mut self,
        prop_node: &'cx ast::Ident,
        containing_ty: &'cx ty::Ty<'cx>,
    ) -> bool {
        let Some(symbol) = containing_ty.symbol() else {
            return false;
        };
        let ty = self.get_type_of_symbol(symbol);
        let name = SymbolName::Atom(prop_node.name);
        let Some(prop) = self.get_prop_of_ty(ty, name) else {
            return false;
        };
        let decl = prop.decl(self.binder);
        self.p.node(decl).is_static()
    }

    fn report_non_existent_prop(
        &mut self,
        prop_node: &'cx ast::Ident,
        containing_ty: &'cx ty::Ty<'cx>,
    ) {
        if self
            .get_node_links(prop_node.id)
            .get_non_existent_prop_checked()
            .unwrap_or_default()
        {
            return;
        }
        self.get_mut_node_links(prop_node.id)
            .set_non_existent_prop_checked(true);

        let missing_prop = pprint_ident(prop_node, self.atoms);

        if self.type_has_static_prop(prop_node, containing_ty) {
            let mut error = errors::PropertyXDoesNotExistOnTypeY {
                span: prop_node.span,
                prop: missing_prop,
                ty: self.print_ty(containing_ty).to_string(),
                related: vec![],
            };
            error.related.push(errors::PropertyXDoesNotExistOnTypeYHelperKind::DidYourMeanToAccessTheStaticMemberInstead(
                errors::DidYourMeanToAccessTheStaticMemberInstead {
                    span: prop_node.span,
                    class_name: self.print_ty(containing_ty).to_string(),
                    prop_name: pprint_ident(prop_node, self.atoms),
                }),
            );
            self.push_error(Box::new(error));
        } else if containing_ty != self.undefined_ty {
            // FIXME: remove `containing_ty != self.undefined_ty`
            let error = errors::PropertyXDoesNotExistOnTypeY {
                span: prop_node.span,
                prop: missing_prop,
                ty: self.print_ty(containing_ty).to_string(),
                related: vec![],
            };
            self.push_error(Box::new(error));
        }
    }

    pub(super) fn _get_prop_of_ty(
        &mut self,
        node: ast::NodeID,
        apparent_left_ty: &'cx ty::Ty<'cx>,
        original_left_ty: &'cx ty::Ty<'cx>,
        prop: &'cx ast::Ident,
    ) -> &'cx ty::Ty<'cx> {
        let is_any_like = apparent_left_ty.flags.intersects(TypeFlags::ANY);

        if is_any_like {
            return self.error_ty;
        }

        let name = SymbolName::Atom(prop.name);

        let Some(prop) = self.get_prop_of_ty(apparent_left_ty, name) else {
            if name
                .as_atom()
                .is_some_and(|atom| atom != keyword::IDENT_EMPTY)
            {
                self.report_non_existent_prop(prop, original_left_ty);
            }
            return self.error_ty;
        };

        if self.p.access_kind(node) == AccessKind::Write {
            self.get_write_type_of_symbol(prop)
        } else {
            self.get_type_of_symbol(prop)
        }
    }

    fn check_prop_access_expr(&mut self, node: &'cx ast::PropAccessExpr<'cx>) -> &'cx ty::Ty<'cx> {
        let left = self.check_non_null_expr(node.expr);
        let apparent_ty = self.get_apparent_ty(left);
        self._get_prop_of_ty(node.id, apparent_ty, left, node.name)
    }

    /// `param.constraint`
    fn param_ty_constraint(&mut self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        let param = ty.kind.expect_param();
        if param.is_this_ty {
            // assert!(param.constraint.is_none());
            Some(self.tys[ty.id.as_usize() + 2])
        } else if ty == self.mark_sub_ty() {
            Some(self.mark_super_ty())
        } else {
            self.get_ty_links(ty.id).get_param_ty_constraint()
        }
    }

    fn param_ty_mapper(&self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx dyn ty::TyMap<'cx>> {
        let param = ty.kind.expect_param();
        param
            .target
            .is_some()
            .then(|| self.expect_ty_links(ty.id).expect_param_ty_mapper())
    }

    fn check_destructing_assign(
        &mut self,
        node: &'cx ast::AssignExpr<'cx>,
        right_ty: &'cx ty::Ty<'cx>,
        right_is_this: bool,
    ) -> &'cx ty::Ty<'cx> {
        if let ast::ExprKind::ArrayLit(array) = node.left.kind {
            if array.elems.is_empty() && right_ty.kind.is_array(self) {
                return right_ty;
            }
        }
        let left_ty = self.check_expr(node.left);
        self.check_binary_like_expr(node, left_ty, right_ty)
    }

    fn check_object_prop_member(
        &mut self,
        member: &'cx ast::ObjectPropMember<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        // TODO: computed member
        let ty = self.check_expr_for_mutable_location(member.init);
        ty
    }

    fn check_object_method_member(
        &mut self,
        member: &'cx ast::ObjectMethodMember<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.check_fn_like_expr_or_object_method_member(member.id);
        self.instantiate_ty_with_single_generic_call_sig(member.id, ty)
    }

    fn check_array_lit(&mut self, lit: &'cx ast::ArrayLit, force_tuple: bool) -> &'cx ty::Ty<'cx> {
        let mut element_types = Vec::with_capacity(lit.elems.len());
        let mut element_flags = Vec::with_capacity(lit.elems.len());
        self.push_cached_contextual_type(lit.id);
        let contextual_ty = self.get_apparent_ty_of_contextual_ty(lit.id, None);
        let is_const_context = self.p.is_const_context(lit.id);
        let is_tuple_context = if let Some(contextual_ty) = contextual_ty {
            self.is_tuple_like(contextual_ty)
        } else {
            false
        };

        let mut has_omitted_expr = false;
        for elem in lit.elems.iter() {
            if let ast::ExprKind::SpreadElement(e) = elem.kind {
                let spread_ty = self.check_expr(e.expr);
                if self.is_array_like_ty(spread_ty) {
                    element_types.push(spread_ty);
                    element_flags.push(ElementFlags::VARIADIC);
                } else {
                    // TODO: in_destructuring_pattern;
                    let t = self.check_iterated_ty_or_element_ty(
                        IterationUse::SPREAD,
                        spread_ty,
                        self.undefined_ty,
                        Some(e.expr.id()),
                    );
                    element_types.push(t);
                    element_flags.push(ElementFlags::REST);
                }
            } else if let ast::ExprKind::Omit(_) = elem.kind {
                has_omitted_expr = true;
                element_types.push(self.undefined_or_missing_ty);
                element_flags.push(ElementFlags::OPTIONAL);
            } else {
                let ty = self.check_expr_for_mutable_location(elem);
                element_types.push(self.add_optionality(ty, true, has_omitted_expr));
                element_flags.push(ElementFlags::REQUIRED);
            }
        }

        self.pop_type_context();

        if force_tuple || is_const_context || is_tuple_context {
            let element_types = self.alloc(element_types);
            let element_flags = self.alloc(element_flags);
            let tuple_ty = self.create_tuple_ty(element_types, Some(element_flags), false);
            self.create_array_literal_ty(tuple_ty)
        } else {
            let ty = if element_types.is_empty() {
                if *self.config.strict_null_checks() {
                    self.implicit_never_ty
                } else {
                    self.undefined_widening_ty
                }
            } else {
                let tys = self.alloc(element_types);
                let tys = self
                    .same_map_tys(Some(tys), |this, t, i| {
                        if element_flags[i].intersects(ElementFlags::VARIADIC) {
                            this.get_indexed_access_ty_or_undefined(t, this.number_ty, None, None)
                                .unwrap_or(this.any_ty)
                        } else {
                            t
                        }
                    })
                    .unwrap();
                self.get_union_ty(tys, ty::UnionReduction::Subtype)
            };
            let array_ty = self.create_array_ty(ty, false);
            self.create_array_literal_ty(array_ty)
        }
    }

    fn create_array_literal_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if !ty.get_object_flags().intersects(ObjectFlags::REFERENCE) {
            ty
        } else if let Some(literal_ty) = self.get_ty_links(ty.id).get_literal_ty() {
            literal_ty
        } else {
            let object_flags = ty.get_object_flags()
                | (ObjectFlags::ARRAY_LITERAL.union(ObjectFlags::CONTAINS_OBJECT_OR_ARRAY_LITERAL));
            let (target, resolved_ty_args) = if let Some(tuple) = ty.kind.as_object_tuple() {
                (ty, Some(tuple.resolved_ty_args))
            } else {
                let r = ty.kind.expect_object_reference();
                (r.target, self.get_ty_links(ty.id).get_resolved_ty_args())
            };
            let literal_ty = self.create_reference_ty(target, resolved_ty_args, object_flags);
            self.get_mut_ty_links(ty.id).set_literal_ty(literal_ty);
            literal_ty
        }
    }

    fn is_used_in_fn_or_instance_prop(
        &self,
        used: &'cx ast::Ident,
        decl: ast::NodeID,
        decl_container: ast::NodeID,
    ) -> bool {
        assert!(used.id.module() == decl.module());
        self.p
            .find_ancestor(used.id, |current| {
                let current_id = current.id();
                if current_id == decl_container {
                    return Some(false);
                } else if current.is_fn_like() {
                    return Some(true);
                } else if current.is_class_static_block_decl() {
                    return Some(self.p.node(decl).span().lo < used.span.lo);
                }

                let parent_id = self.p.parent(current_id)?;
                let parent_node = self.p.node(parent_id);
                let prop_decl = parent_node.as_class_prop_ele()?;

                let init_of_prop = prop_decl
                    .init
                    .map(|init| init.id() == current_id)
                    .unwrap_or_default();
                if init_of_prop {
                    if parent_node.is_static() {
                        let n = self.p.node(decl);
                        if n.is_class_method_ele() {
                            return Some(true);
                        } else if let Some(prop_decl) = n.as_class_prop_ele() {
                            if let Some(usage_class) = self.p.get_containing_class(used.id) {
                                if let Some(decl_class) = self.p.get_containing_class(decl) {
                                    if usage_class == decl_class {
                                        let prop_name = prop_decl.name;
                                        todo!()
                                    }
                                }
                            }
                        }
                    } else {
                        let n = self.p.node(decl);
                        let is_decl_instance_prop = n.is_class_prop_ele() && !n.is_static();
                        if !is_decl_instance_prop {
                            return Some(true);
                        } else if let Some(usage_class) = self.p.get_containing_class(used.id) {
                            if let Some(decl_class) = self.p.get_containing_class(decl) {
                                if usage_class == decl_class {
                                    return Some(true);
                                }
                            }
                        }
                    }
                }
                None
            })
            .is_some()
    }

    fn is_block_scoped_name_declared_before_use(
        &self,
        decl: ast::NodeID,
        used: &'cx ast::Ident,
    ) -> bool {
        let used_span = used.span;
        let decl_span = self.p.node(decl).span();
        let decl_pos = decl_span.lo;
        let decl_container = self.p.get_enclosing_blockscope_container(decl);

        if decl_pos < used_span.lo {
            let n = self.p.node(decl);
            if let Some(decl) = n.as_var_decl() {
                return !self.p.is_immediately_used_in_init_or_block_scoped_var(
                    decl,
                    used.id,
                    decl_container,
                );
            } else {
                return true;
            }
        }

        if self.is_used_in_fn_or_instance_prop(used, decl, decl_container) {
            return true;
        }

        false
    }

    fn check_resolved_block_scoped_var(&mut self, ident: &'cx ast::Ident, id: SymbolID) {
        let Some(decl) = self.binder.symbol(id).opt_decl() else {
            return;
        };

        if !self
            .p
            .node_flags(decl)
            .intersects(bolt_ts_ast::NodeFlags::AMBIENT)
            && !self.is_block_scoped_name_declared_before_use(decl, ident)
        {
            let (decl_span, kind) = match self.p.node(decl) {
                ast::Node::ClassDecl(class) => (class.name.unwrap().span, errors::DeclKind::Class),
                ast::Node::VarDecl(decl) => (decl.span, errors::DeclKind::BlockScopedVariable),
                _ => unreachable!(),
            };
            let name = self.atoms.get(ident.name).to_string();
            let error = errors::CannotUsedBeforeItsDeclaration {
                span: ident.span,
                kind,
                name: name.to_string(),
                related: [errors::DefinedHere {
                    span: decl_span,
                    kind,
                    name,
                }],
            };
            self.push_error(Box::new(error));
        }
    }

    fn check_ident(&mut self, ident: &'cx ast::Ident) -> &'cx ty::Ty<'cx> {
        match ident.name {
            keyword::KW_UNDEFINED => return self.undefined_ty,
            keyword::KW_NULL => return self.null_ty,
            _ => (),
        }

        let symbol = self.resolve_symbol_by_ident(ident);

        if symbol == Symbol::ERR {
            return self.error_ty;
        }

        if symbol == self.arguments_symbol {
            return self.get_type_of_symbol(symbol);
        }

        // TODO: move into name resolution.
        if self.symbol(symbol).flags().intersects(
            SymbolFlags::CLASS
                .union(SymbolFlags::BLOCK_SCOPED_VARIABLE)
                .union(SymbolFlags::ENUM),
        ) {
            self.check_resolved_block_scoped_var(ident, symbol);
        }

        let ty = self.get_type_of_symbol(symbol);
        let assignment_kind = self.p.get_assignment_kind(ident.id);
        if assignment_kind != AssignmentKind::None && symbol != Symbol::ERR {
            let symbol = self.binder.symbol(symbol);
            if !symbol.flags.intersects(SymbolFlags::VARIABLE) {
                let ty = if symbol.flags.intersects(SymbolFlags::CLASS) {
                    "class"
                } else if symbol.flags.intersects(SymbolFlags::FUNCTION) {
                    "function"
                } else if symbol.flags.intersects(SymbolFlags::ENUM) {
                    "enum"
                } else if symbol.flags.intersects(SymbolFlags::NAMESPACE) {
                    "namespace"
                } else {
                    unreachable!()
                };
                let error = errors::CannotAssignToNameBecauseItIsATy {
                    span: ident.span,
                    name: self.atoms.get(ident.name).to_string(),
                    ty: ty.to_string(),
                };
                self.push_error(Box::new(error));
                return self.error_ty;
            }
        }

        let decl = if symbol == self.global_this_symbol {
            return ty;
        } else if let Some(decl) = symbol.opt_decl(self.binder) {
            decl
        } else {
            return ty;
        };

        let is_alias = self
            .binder
            .symbol(symbol)
            .flags
            .intersects(SymbolFlags::ALIAS);

        let immediate_decl = decl;

        let decl_container = self.p.get_control_flow_container(decl);
        let flow_container = self.p.get_control_flow_container(ident.id);
        let is_outer_variable = flow_container != decl_container;

        let is_param = self.p.node(self.p.get_root_decl(decl)).is_param_decl();
        // let is_outer_variable = flow_container != decl_container;
        let type_is_auto = ty == self.auto_ty;
        let is_automatic_ty_is_non_null = type_is_auto;

        let is_never_initialized = if let Some(v) = self.p.node(immediate_decl).as_var_decl() {
            v.init.is_none()
        } else {
            false
        };
        let assume_initialized = is_param
            || is_alias
            || (is_outer_variable && !is_never_initialized)
            || self.p.node_flags(decl).intersects(ast::NodeFlags::AMBIENT);
        let init_ty = if is_automatic_ty_is_non_null {
            self.undefined_ty
        } else if assume_initialized {
            if is_param {
                // TODO: remove_optionality_from_decl_ty
                ty
            } else {
                ty
            }
        } else if type_is_auto {
            self.undefined_ty
        } else {
            ty
        };

        let flow_ty =
            self.get_flow_ty_of_reference(ident.id, ty, Some(init_ty), Some(flow_container), None);
        if is_automatic_ty_is_non_null {
            // TODO: get_non_nullable
        };
        flow_ty
    }

    fn check_bin_expr(&mut self, node: &'cx ast::BinExpr) -> &'cx ty::Ty<'cx> {
        let l = self.check_expr(node.left);
        let r = self.check_expr(node.right);
        self.check_bin_like_expr(node, node.op, node.left, l, node.right, r)
    }

    fn check_non_null_expr(&mut self, expr: &'cx ast::Expr<'cx>) -> &'cx ty::Ty<'cx> {
        let ty = self.check_expr(expr);
        self.check_non_null_type(ty, expr)
    }

    fn check_non_null_type(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if matches!(expr.kind, ast::ExprKind::NullLit(_)) {
            let error = errors::TheValueCannotBeUsedHere {
                span: expr.span(),
                value: "null".to_string(),
            };
            self.push_error(Box::new(error));
        } else if matches!(expr.kind, ast::ExprKind::Ident(ast::Ident { name, .. }) if *name == keyword::KW_UNDEFINED)
        {
            let error = errors::TheValueCannotBeUsedHere {
                span: expr.span(),
                value: "undefined".to_string(),
            };
            self.push_error(Box::new(error));
        }
        ty
    }

    fn check_binary_like_expr_for_add(
        &mut self,
        left_ty: &'cx ty::Ty<'cx>,
        right_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if self.is_type_assignable_to_kind(left_ty, TypeFlags::NUMBER_LIKE, true)
            && self.is_type_assignable_to_kind(right_ty, TypeFlags::NUMBER_LIKE, true)
        {
            Some(self.number_ty)
        } else if self.is_type_assignable_to_kind(left_ty, TypeFlags::STRING_LIKE, true)
            || self.is_type_assignable_to_kind(right_ty, TypeFlags::STRING_LIKE, true)
        {
            Some(self.string_ty)
        } else if self.is_type_any(Some(left_ty)) || self.is_type_any(Some(right_ty)) {
            Some(self.any_ty)
        } else {
            None
        }
    }

    fn check_bin_like_expr(
        &mut self,
        node: &'cx ast::BinExpr,
        op: BinOp,
        left: &'cx ast::Expr,
        left_ty: &'cx ty::Ty<'cx>,
        right: &'cx ast::Expr,
        right_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        use bolt_ts_ast::BinOpKind::*;
        match op.kind {
            Add => {
                let ty = match self.check_binary_like_expr_for_add(left_ty, right_ty) {
                    Some(ty) => ty,
                    None => {
                        let error = errors::OperatorCannotBeAppliedToTy1AndTy2 {
                            op: op.kind.to_string(),
                            ty1: left_ty.to_string(self),
                            ty2: right_ty.to_string(self),
                            span: node.span,
                        };
                        self.push_error(Box::new(error));
                        self.any_ty
                    }
                };
                ty
            }
            Sub => self.number_ty,
            Mul => self.undefined_ty,
            Div => self.number_ty,
            Pipe => {
                let left = self.check_non_null_type(left_ty, left);
                let right = self.check_non_null_type(right_ty, right);
                self.number_ty
            }
            LogicalAnd => {
                if has_type_facts(left_ty, TypeFacts::TRUTHY) {
                    left_ty
                } else {
                    right_ty
                }
            }
            PipePipe => {
                if has_type_facts(left_ty, TypeFacts::FALSE_FACTS) {
                    right_ty
                } else {
                    left_ty
                }
            }
            EqEq => self.boolean_ty(),
            EqEqEq => self.boolean_ty(),
            Less => self.boolean_ty(),
            LessEq => self.boolean_ty(),
            Shl => todo!(),
            Great => self.boolean_ty(),
            GreatEq => todo!(),
            Shr => todo!(),
            UShr => todo!(),
            BitAnd => todo!(),
            Instanceof => self.boolean_ty(),
            In => self.check_in_expr(left, left_ty, right, right_ty),
            Satisfies => todo!(),
            NEq => self.boolean_ty(),
            NEqEq => self.boolean_ty(),
        }
    }

    fn check_in_expr(
        &mut self,
        left: &'cx ast::Expr,
        left_ty: &'cx ty::Ty<'cx>,
        right: &'cx ast::Expr,
        right_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        self.check_type_assignable_to(left_ty, self.string_number_symbol_ty(), Some(left.id()));
        if self.check_type_assignable_to(right_ty, self.non_primitive_ty, Some(right.id()))
            == Ternary::FALSE
        {
            let right_ty = self.get_widened_literal_ty(right_ty);
            let error = ecma_rules::TheRightValueOfTheInOperatorMustBeAnObjectButGotTy {
                span: right.span(),
                ty: right_ty.to_string(self),
            };
            self.push_error(Box::new(error));
        }
        self.boolean_ty()
    }

    fn check_ty_param(&mut self, ty_param: &'cx ast::TyParam<'cx>) {
        if let Some(constraint) = ty_param.constraint {
            self.check_ty(constraint);
        }
        if let Some(default) = ty_param.default {
            self.check_ty(default);
        }

        let symbol = self.get_symbol_of_decl(ty_param.id);
        let ty_param = self.get_declared_ty_of_ty_param(symbol);
        self.get_base_constraint_of_ty(ty_param);
    }

    fn check_indexed_access_index_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        n: &'cx ast::IndexedAccessTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let Some(indexed_access_ty) = ty.kind.as_indexed_access() else {
            return ty;
        };
        let object_index_ty = if self.is_generic_mapped_ty(indexed_access_ty.object_ty)
            && self.get_mapped_ty_name_ty_kind(indexed_access_ty.object_ty)
                == ty::MappedTyNameTyKind::Remapping
        {
            self.get_index_ty_for_mapped_ty(indexed_access_ty.object_ty, ty::IndexFlags::empty())
        } else {
            self.get_index_ty(indexed_access_ty.object_ty, ty::IndexFlags::empty())
        };
        let has_number_index_info = self
            .get_index_info_of_ty(indexed_access_ty.object_ty, self.number_ty)
            .is_some();
        if self.every_type(indexed_access_ty.index_ty, |this, t| {
            this.is_type_assignable_to(t, object_index_ty)
                || has_number_index_info && this.is_applicable_index_ty(t, this.number_ty)
        }) {
            return ty;
        }

        if self.is_generic_object_ty(indexed_access_ty.object_ty) {
            // TODO:
        }

        let error = errors::TypeCannotBeUsedToIndexType {
            span: n.span,
            ty: self.print_ty(indexed_access_ty.index_ty).to_string(),
            index_ty: self.print_ty(indexed_access_ty.object_ty).to_string(),
        };
        self.push_error(Box::new(error));
        self.error_ty
    }

    fn check_ty_params(&mut self, ty_params: ast::TyParams<'cx>) {
        // let mut seen_default = false;
        for ty_param in ty_params {
            self.check_ty_param(ty_param)
        }
    }

    pub fn check_and_aggregate_ret_expr_tys(
        &mut self,
        f: ast::NodeID,
        body: &'cx ast::BlockStmt<'cx>,
    ) -> Option<Vec<&'cx ty::Ty<'cx>>> {
        let flags = self.p.node(f).fn_flags();
        let mut has_ret_with_no_expr = false;
        let mut has_ret_of_ty_never = false;

        fn for_each_ret_stmt<'cx, T>(
            id: ast::NodeID,
            checker: &mut TyChecker<'cx>,
            f: impl Fn(&mut TyChecker<'cx>, &'cx ast::RetStmt<'cx>) -> T + Copy,
            has_ret_with_no_expr: &mut bool,
            has_ret_of_ty_never: &mut bool,
        ) -> Vec<T> {
            fn t<'cx, T>(
                id: ast::NodeID,
                checker: &mut TyChecker<'cx>,
                f: impl Fn(&mut TyChecker<'cx>, &'cx ast::RetStmt<'cx>) -> T + Copy,
                v: &mut Vec<T>,
                has_ret_with_no_expr: &mut bool,
                has_ret_of_ty_never: &mut bool,
            ) {
                let node = checker.p.node(id);
                if let Some(ret) = node.as_ret_stmt() {
                    if let Some(ret_expr) = ret.expr {
                        // TODO: async function and await call;
                        // TODO: const reference
                    } else {
                        *has_ret_with_no_expr = true;
                    }
                    v.push(f(checker, ret))
                } else if let Some(b) = node.as_block_stmt() {
                    for stmt in b.stmts {
                        t(
                            stmt.id(),
                            checker,
                            f,
                            v,
                            has_ret_with_no_expr,
                            has_ret_of_ty_never,
                        );
                    }
                } else if let Some(node) = node.as_if_stmt() {
                    t(
                        node.then.id(),
                        checker,
                        f,
                        v,
                        has_ret_with_no_expr,
                        has_ret_of_ty_never,
                    );
                    if let Some(else_then) = node.else_then {
                        t(
                            else_then.id(),
                            checker,
                            f,
                            v,
                            has_ret_with_no_expr,
                            has_ret_of_ty_never,
                        );
                    }
                } else {
                    return;
                }
            }

            let mut v = Vec::with_capacity(8);
            t(
                id,
                checker,
                f,
                &mut v,
                has_ret_with_no_expr,
                has_ret_of_ty_never,
            );
            v
        }

        let tys = for_each_ret_stmt(
            body.id,
            self,
            |this, ret| {
                let Some(expr) = ret.expr else {
                    return this.undefined_ty;
                };
                let expr = bolt_ts_ast::Expr::skip_parens(expr);
                let old = if let Some(check_mode) = this.check_mode {
                    let old = this.check_mode;
                    this.check_mode = Some(check_mode & !CheckMode::SKIP_GENERIC_FUNCTIONS);
                    old
                } else {
                    None
                };
                let ty = this.check_expr_with_cache(expr);
                this.check_mode = old;
                ty
            },
            &mut has_ret_with_no_expr,
            &mut has_ret_of_ty_never,
        );

        if tys.is_empty() && !has_ret_with_no_expr && has_ret_of_ty_never {
            None
        } else {
            Some(tys)
        }
    }

    fn is_array_or_tuple(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.kind.is_array(self) || ty.is_tuple()
    }

    fn is_known_prop(&mut self, target: &'cx ty::Ty<'cx>, name: SymbolName) -> bool {
        if target.kind.as_object().is_some() {
            if self.get_prop_of_ty(target, name).is_some()
                || self.get_applicable_index_for_name(target, name).is_some()
            {
                return true;
            }
        } else if target.kind.is_union_or_intersection()
            && Self::is_excess_property_check_target(target)
        {
            let tys = target.kind.tys_of_union_or_intersection().unwrap();
            for t in tys {
                if self.is_known_prop(t, name) {
                    return true;
                }
            }
        };
        false
    }

    fn ty_has_call_or_ctor_sigs(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        !self
            .get_signatures_of_type(ty, ty::SigKind::Call)
            .is_empty()
            || !self
                .get_signatures_of_type(ty, ty::SigKind::Constructor)
                .is_empty()
    }

    fn is_object_ty_with_inferable_index(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if let Some(i) = ty.kind.as_intersection() {
            i.tys
                .iter()
                .all(|t| self.is_object_ty_with_inferable_index(t))
        } else if let Some(symbol) = ty.symbol() {
            let flags = self.binder.symbol(symbol).flags;
            flags.intersects(
                SymbolFlags::OBJECT_LITERAL
                    .union(SymbolFlags::TYPE_LITERAL)
                    .union(SymbolFlags::ENUM)
                    .union(SymbolFlags::VALUE_MODULE),
            ) && !flags.intersects(SymbolFlags::CLASS)
                && !self.ty_has_call_or_ctor_sigs(ty)
        } else {
            let object_flags = ty.get_object_flags();
            // TODO: reversed type
            object_flags.intersects(ObjectFlags::OBJECT_REST_TYPE)
        }
    }

    pub(crate) fn is_empty_anonymous_object_ty(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.kind.as_object_anonymous().is_some_and(|_| {
            if let Some(symbol) = ty.symbol() {
                let s = self.binder.symbol(symbol);
                s.flags.intersects(SymbolFlags::TYPE_LITERAL) && s.members().0.is_empty() // TODO: change `s.members()` to `self.get_members_of_symbol`
            } else if let Some(ty_link) = self.ty_links.get(&ty.id) {
                if let Some(t) = ty_link.get_structured_members() {
                    ty != self.any_fn_ty()
                        && t.props.is_empty()
                        && t.call_sigs.is_empty()
                        && t.ctor_sigs.is_empty()
                        && t.index_infos.is_empty()
                } else {
                    false
                }
            } else {
                false
            }
        })
    }

    fn is_empty_resolved_ty(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        let Some(m) = self
            .ty_links
            .get(&ty.id)
            .and_then(|l| l.get_structured_members())
        else {
            unreachable!()
        };
        ty != self.any_fn_ty()
            && m.props.is_empty()
            && m.call_sigs.is_empty()
            && m.ctor_sigs.is_empty()
            && m.index_infos.is_empty()
    }

    pub(crate) fn is_empty_object_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if ty.flags.intersects(TypeFlags::OBJECT) {
            !self.is_generic_mapped_ty(ty) && {
                self.resolve_structured_type_members(ty);
                self.is_empty_resolved_ty(ty)
            }
        } else if ty.flags.intersects(TypeFlags::NON_PRIMITIVE) {
            true
        } else if let Some(u) = ty.kind.as_union() {
            u.tys.iter().any(|t| self.is_empty_object_ty(t))
        } else if let Some(i) = ty.kind.as_intersection() {
            i.tys.iter().all(|t| self.is_empty_object_ty(t))
        } else {
            false
        }
    }

    fn get_key_prop_name(&self, union_ty: &'cx ty::UnionTy<'cx>) -> Option<SymbolName> {
        None
    }

    fn get_ty_of_prop_of_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
    ) -> Option<&'cx ty::Ty<'cx>> {
        self.get_prop_of_ty(ty, name)
            .map(|prop| self.get_type_of_symbol(prop))
    }

    fn get_matching_union_constituent_for_ty(
        &mut self,
        union_ty: &'cx ty::UnionTy<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let key_prop_name = self.get_key_prop_name(union_ty);
        let prop_ty = key_prop_name.and_then(|name| self.get_ty_of_prop_of_ty(ty, name));
        // TODO: prop_ty.and_then(|ty| self.)
        None
    }

    fn empty_array<T>(&self) -> &'cx [T] {
        cast_empty_array(self.empty_array)
    }

    fn create_ty_mapper(
        &self,
        sources: ty::Tys<'cx>,
        targets: ty::Tys<'cx>,
    ) -> &'cx ty::TyMapper<'cx> {
        let mapper = if sources.len() == 1 {
            let target = targets.first().copied().unwrap_or(self.any_ty);
            ty::TyMapper::make_unary(sources[0], target)
        } else {
            let mapper = self.create_array_ty_mapper(sources, Some(targets));
            ty::TyMapper::Array(mapper)
        };
        self.alloc(mapper)
    }

    fn create_array_ty_mapper(
        &self,
        sources: ty::Tys<'cx>,
        targets: Option<ty::Tys<'cx>>,
    ) -> ty::ArrayTyMapper<'cx> {
        assert!(sources.len() >= targets.map(|t| t.len()).unwrap_or_default());
        let mut mapper = sources
            .iter()
            .enumerate()
            .map(|(idx, &source)| {
                assert!(source.kind.is_param());
                let target = targets
                    .and_then(|tys| tys.get(idx))
                    .copied()
                    .unwrap_or(self.any_ty);
                (source, target)
            })
            .collect::<Vec<_>>();
        mapper.sort_unstable_by_key(|(source, _)| source.id.as_u32());
        let mapper = self.alloc(mapper);
        ty::ArrayTyMapper { mapper }
    }

    pub(super) fn is_type_any(&self, ty: Option<&'cx ty::Ty<'cx>>) -> bool {
        ty.is_some_and(|ty| ty.flags.intersects(TypeFlags::ANY))
    }

    fn get_non_nullable_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if *self.config.strict_null_checks() {
            // TODO:
            ty
        } else {
            ty
        }
    }

    fn is_ty_or_base_identical_to(&mut self, s: &'cx ty::Ty<'cx>, t: &'cx ty::Ty<'cx>) -> bool {
        (t.flags.intersects(TypeFlags::STRING) && s.flags.intersects(TypeFlags::STRING_LITERAL))
            || (t.flags.intersects(TypeFlags::NUMBER)
                && s.flags.intersects(TypeFlags::NUMBER_LITERAL))
            || self.is_type_identical_to(s, t)
    }

    fn is_type_identical_to(&mut self, s: &'cx ty::Ty<'cx>, t: &'cx ty::Ty<'cx>) -> bool {
        self.is_type_related_to(s, t, relation::RelationKind::Identity)
    }

    fn compare_types_identical(&mut self, s: &'cx ty::Ty<'cx>, t: &'cx ty::Ty<'cx>) -> Ternary {
        if self.is_type_identical_to(s, t) {
            Ternary::TRUE
        } else {
            Ternary::FALSE
        }
    }

    fn is_ty_closely_matched_by(s: &'cx ty::Ty<'cx>, t: &'cx ty::Ty<'cx>) -> bool {
        if s.kind.is_object() && t.kind.is_object() {
            let s_symbol = s.symbol();
            s_symbol.is_some() && s_symbol == t.symbol()
        } else {
            // TODO: alias_symbol
            false
        }
    }

    fn each_union_contains(&self, union_tys: &[&'cx ty::Ty<'cx>], ty: &'cx ty::Ty<'cx>) -> bool {
        for t in union_tys {
            let u = t.kind.expect_union();
            if !contains_ty(u.tys, ty) {
                if ty == self.missing_ty {
                    return contains_ty(u.tys, self.undefined_ty);
                } else if ty == self.undefined_ty {
                    return contains_ty(u.tys, self.missing_ty);
                }

                let primitive = if ty.flags.intersects(TypeFlags::STRING_LITERAL) {
                    self.string_ty
                } else if ty.flags.intersects(TypeFlags::NUMBER_LITERAL) {
                    self.number_ty
                } else {
                    return false;
                };
                if !contains_ty(u.tys, primitive) {
                    return false;
                }
            }
        }
        true
    }

    fn convert_auto_to_any(&self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty == self.auto_ty {
            self.any_ty
        } else if ty == self.auto_array_ty() {
            self.any_array_ty()
        } else {
            ty
        }
    }

    fn is_matching_reference(&self, source: ast::NodeID, target: ast::NodeID) -> bool {
        let t = self.p.node(target);
        let s = self.p.node(source);
        use bolt_ts_ast::Node::*;

        match s {
            Ident(s_ident) => {
                if self.p.is_this_in_type_query(source) {
                    t.is_this_expr()
                } else if let Some(t_ident) = t.as_ident() {
                    self.resolve_symbol_by_ident(s_ident) == self.resolve_symbol_by_ident(t_ident)
                } else if let Some(t_v) = t.as_var_decl() {
                    match t_v.binding.kind {
                        bolt_ts_ast::BindingKind::Ident(_) => {
                            self.resolve_symbol_by_ident(s_ident) == self.get_symbol_of_decl(t_v.id)
                        }
                        bolt_ts_ast::BindingKind::ObjectPat(_) => todo!(),
                        bolt_ts_ast::BindingKind::ArrayPat(_) => todo!(),
                    }
                } else if t.is_object_binding_elem() {
                    todo!()
                } else {
                    false
                }
            }
            ThisExpr(_) => t.is_this_expr(),
            _ => false,
        }
    }

    fn is_or_contain_matching_refer(&self, source: ast::NodeID, target: ast::NodeID) -> bool {
        self.is_matching_reference(source, target)
    }

    fn has_matching_arg(&mut self, expr: &'cx ast::Expr<'cx>, refer: ast::NodeID) -> bool {
        use bolt_ts_ast::ExprKind::*;
        let (args, expr) = match expr.kind {
            Call(call) => (call.args, call.expr),
            New(new) => (new.args.unwrap_or_default(), new.expr),
            _ => unreachable!(),
        };
        for arg in args {
            if self.is_or_contain_matching_refer(refer, arg.id()) {
                return true;
            }
        }

        if let ast::ExprKind::PropAccess(p) = expr.kind {
            if self.is_or_contain_matching_refer(refer, p.expr.id()) {
                return true;
            }
        }
        false
    }

    #[inline(always)]
    fn no_ty_pred(&self) -> &'cx TyPred<'cx> {
        self.no_ty_pred.get().unwrap()
    }

    fn is_ty_derived_from(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) -> bool {
        if let Some(u) = source.kind.as_union() {
            u.tys.iter().all(|t| self.is_ty_derived_from(t, target))
        } else if let Some(u) = target.kind.as_union() {
            u.tys.iter().any(|t| self.is_ty_derived_from(source, t))
        } else if let Some(i) = source.kind.as_intersection() {
            i.tys.iter().any(|t| self.is_ty_derived_from(t, target))
        } else if source.kind.is_instantiable_non_primitive() {
            let source = self
                .get_base_constraint_of_ty(source)
                .unwrap_or(self.unknown_ty);
            self.is_ty_derived_from(source, target)
        } else if self.is_empty_anonymous_object_ty(target) {
            source
                .flags
                .intersects(TypeFlags::OBJECT | TypeFlags::NON_PRIMITIVE)
        } else if target == self.global_object_ty() {
            source
                .flags
                .intersects(TypeFlags::OBJECT | TypeFlags::NON_PRIMITIVE)
                && !self.is_empty_anonymous_object_ty(source)
        } else if target == self.global_fn_ty() {
            source.flags.intersects(TypeFlags::OBJECT) && self.is_fn_object_ty(source)
        } else {
            self.has_base_ty(
                source,
                if let Some(r) = target.kind.as_object_reference() {
                    r.target
                } else {
                    target
                },
            ) || (target.kind.is_array(self)
                && !target.kind.is_readonly_array(self)
                && self.is_ty_derived_from(source, self.global_readonly_array_ty()))
        }
    }

    fn is_fn_object_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if ty
            .get_object_flags()
            .intersects(ObjectFlags::EVOLVING_ARRAY)
        {
            return false;
        };
        self.resolve_structured_type_members(ty);
        let members = self.ty_links[&ty.id].get_structured_members().unwrap();
        !members.call_sigs.is_empty() || !members.ctor_sigs.is_empty()
    }

    fn is_ty_sub_type_of(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) -> bool {
        self.is_type_related_to(source, target, relation::RelationKind::Subtype)
    }

    fn is_ty_strict_sub_type_of(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        self.is_type_related_to(source, target, relation::RelationKind::StrictSubtype)
    }

    fn get_constituent_ty_for_key_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        union_ty: &'cx ty::UnionTy<'cx>,
        key_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let key_prop_name = self.get_key_prop_name(union_ty);
        let prop_ty = key_prop_name.and_then(|name| self.get_ty_of_prop_of_ty(key_ty, name));
        prop_ty.and_then(|ty| self.get_matching_union_constituent_for_ty(union_ty, ty))
    }

    fn create_flow_ty(&self, ty: &'cx ty::Ty<'cx>, incomplete: bool) -> FlowTy<'cx> {
        if incomplete {
            let ty = if ty.flags.intersects(TypeFlags::NEVER) {
                self.silent_never_ty
            } else {
                ty
            };
            FlowTy::Incomplete {
                flags: TypeFlags::empty(),
                ty,
            }
        } else {
            FlowTy::Ty(ty)
        }
    }

    fn prepend_ty_mapping(
        &self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        mapper: Option<&'cx dyn ty::TyMap<'cx>>,
    ) -> &'cx dyn ty::TyMap<'cx> {
        if let Some(mapper) = mapper {
            let mapper1 = ty::TyMapper::make_unary(source, target);
            let mapper1 = self.alloc(mapper1);
            self.alloc(ty::MergedTyMapper {
                mapper1,
                mapper2: mapper,
            })
        } else {
            let mapper = ty::TyMapper::make_unary(source, target);
            self.alloc(mapper)
        }
    }

    fn append_ty_mapping(
        &mut self,
        mapper: Option<&'cx dyn ty::TyMap<'cx>>,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> &'cx dyn ty::TyMap<'cx> {
        if let Some(mapper) = mapper {
            let mapper2 = ty::TyMapper::make_unary(source, target);
            let mapper2 = self.alloc(mapper2);
            self.alloc(ty::MergedTyMapper {
                mapper1: mapper,
                mapper2,
            })
        } else {
            let mapper = ty::TyMapper::make_unary(source, target);
            self.alloc(mapper)
        }
    }

    fn is_tuple_like(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.is_tuple()
            || (ty.kind.is_array(self)
                && self
                    .get_ty_of_prop_of_ty(ty, SymbolName::Atom(keyword::IDENT_LENGTH))
                    .is_some_and(|length_ty| {
                        self.every_type(length_ty, |_, t| {
                            t.flags.intersects(TypeFlags::NUMBER_LITERAL)
                        })
                    }))
    }

    fn for_each_mapped_ty_prop_key_ty_and_index_sig_key_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        include: TypeFlags,
        string_only: bool,
        mut cb: impl FnMut(&mut Self, &'cx ty::Ty<'cx>),
    ) {
        let props = self.get_props_of_ty(ty);
        for prop in props {
            let prop = self.get_lit_ty_from_prop(*prop, include, false);
            cb(self, prop)
        }
        if ty.flags.intersects(TypeFlags::ANY) {
            cb(self, self.string_ty);
        } else {
            let infos = self.get_index_infos_of_ty(ty);
            for info in infos {
                if !string_only
                    || info
                        .key_ty
                        .flags
                        .intersects(TypeFlags::STRING | TypeFlags::TEMPLATE_LITERAL)
                {
                    cb(self, info.key_ty);
                }
            }
        }
    }

    fn get_known_keys_of_tuple_ty(&mut self, ty: &'cx ty::TupleTy<'cx>) -> &'cx ty::Ty<'cx> {
        let mut v = Vec::with_capacity(ty.element_flags.len() + 1);
        v.extend((0..ty.fixed_length).map(|i| {
            let val = i.to_string();
            let atom = self.atoms.insert_by_str(Cow::Owned(val));
            self.get_string_literal_type(atom)
        }));
        let t = if ty.readonly {
            self.global_readonly_array_ty()
        } else {
            self.global_array_ty()
        };
        let t = self.get_index_ty(t, IndexFlags::empty());
        v.push(t);
        self.get_union_ty(&v, ty::UnionReduction::Lit)
    }

    fn get_lower_bound_of_key_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if let Some(index_ty) = ty.kind.as_index_ty() {
            let t = self.get_apparent_ty(index_ty.ty);
            if t.kind.is_generic_tuple_type() {
                // TODO: get_known_keys_of_tuple_ty
                t
            } else {
                self.get_index_ty(t, ty::IndexFlags::empty())
            }
        } else if ty.kind.is_cond_ty() {
            // TODO: is_distributive
            return ty;
        } else if ty.kind.is_union() {
            return self
                .map_ty(ty, |this, t| Some(this.get_lower_bound_of_key_ty(t)), true)
                .unwrap();
        } else if let Some(i) = ty.kind.as_intersection() {
            let tys = i.tys;
            if tys.len() == 2
                && tys[0]
                    .flags
                    .intersects(TypeFlags::STRING | TypeFlags::NUMBER | TypeFlags::BIG_INT)
                && tys[1] == self.empty_ty_literal_ty()
            {
                return ty;
            } else {
                let tys = self
                    .same_map_tys(Some(tys), |this, t, _| this.get_lower_bound_of_key_ty(t))
                    .unwrap();
                self.get_intersection_ty(tys, IntersectionFlags::None, None, None)
            }
        } else {
            ty
        }
    }

    fn is_error(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty == self.error_ty
    }

    fn get_element_ty_of_array_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        ty.kind.is_array(self).then(|| self.get_ty_arguments(ty)[0])
    }

    fn get_element_ty_of_slice_of_tuple_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        index: usize,
        end_skip_count: Option<usize>,
        writing: Option<bool>,
        no_reductions: Option<bool>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let end_skip_count = end_skip_count.unwrap_or(0);
        let writing = writing.unwrap_or(false);
        let no_reductions = no_reductions.unwrap_or(false);
        let length = Self::get_ty_reference_arity(ty) - end_skip_count;
        let tup = ty.as_tuple().unwrap();
        if index < length {
            let ty_args = self.get_ty_arguments(ty);
            let mut element_tys = Vec::with_capacity(length);
            for (i, t) in ty_args.iter().enumerate().take(length).skip(index) {
                let t = if tup.element_flags[i].intersects(ElementFlags::VARIADIC) {
                    self.get_indexed_access_ty(t, self.number_ty, None, None)
                } else {
                    t
                };
                element_tys.push(t);
            }
            Some(if writing {
                self.get_intersection_ty(&element_tys, IntersectionFlags::None, None, None)
            } else {
                self.get_union_ty(
                    &element_tys,
                    if no_reductions {
                        ty::UnionReduction::None
                    } else {
                        ty::UnionReduction::Lit
                    },
                )
            })
        } else {
            None
        }
    }

    fn is_empty_literal_ty(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        if *self.config.strict_null_checks() {
            ty == self.implicit_never_ty
        } else {
            ty == self.undefined_widening_ty
        }
    }

    fn is_empty_array_lit_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        let Some(element_ty) = self.get_element_ty_of_array_ty(ty) else {
            return false;
        };
        self.is_empty_literal_ty(element_ty)
    }

    fn is_non_generic_object_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.kind.is_object() && !self.is_generic_mapped_ty(ty)
    }

    fn is_generic_mapped_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.kind.as_object_mapped().is_some_and(|_| {
            let constraint_ty = self.get_constraint_ty_from_mapped_ty(ty);
            if self.is_generic_index_ty(constraint_ty) {
                true
            } else {
                self.get_name_ty_from_mapped_ty(ty).is_some_and(|name_ty| {
                    let source = self.get_ty_param_from_mapped_ty(ty);
                    let mapper = TyMapper::make_unary(source, constraint_ty);
                    let mapper = self.alloc(mapper);
                    let ty = self.instantiate_ty(name_ty, Some(mapper));
                    self.is_generic_index_ty(ty)
                })
            }
        })
    }

    fn is_generic_object_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        self.get_generic_object_flags(ty)
            .intersects(ObjectFlags::IS_GENERIC_OBJECT_TYPE)
    }

    fn is_generic_index_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        self.get_generic_object_flags(ty)
            .intersects(ObjectFlags::IS_GENERIC_INDEX_TYPE)
    }

    fn get_generic_object_flags(&mut self, ty: &'cx ty::Ty<'cx>) -> ObjectFlags {
        if let Some(tys) = ty.kind.tys_of_union_or_intersection() {
            // TODO: cache?
            let object_flags = self
                .reduced_left(
                    tys,
                    |this, flags, t, _| flags | this.get_generic_object_flags(t),
                    Some(ObjectFlags::empty()),
                    None,
                    None,
                )
                .unwrap();
            object_flags.intersection(ObjectFlags::IS_GENERIC_TYPE)
        } else if let Some(ty) = ty.kind.as_substitution_ty() {
            // TODO: cache?
            (self.get_generic_object_flags(ty.base_ty)
                | self.get_generic_object_flags(ty.constraint))
                & ObjectFlags::IS_GENERIC_TYPE
        } else {
            (if ty.kind.is_instantiable_non_primitive()
                || self.is_generic_mapped_ty(ty)
                || ty.kind.is_generic_tuple_type()
            {
                ObjectFlags::IS_GENERIC_OBJECT_TYPE
            } else {
                ObjectFlags::empty()
            }) | (if ty.kind.is_instantiable_non_primitive()
                || ty.kind.is_index_ty()
                || ty.is_generic_string_like()
            {
                ObjectFlags::IS_GENERIC_INDEX_TYPE
            } else {
                ObjectFlags::empty()
            })
        }
    }

    fn is_generic(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        !self.get_generic_object_flags(ty).is_empty()
    }

    fn get_normalized_ty(
        &mut self,
        mut ty: &'cx ty::Ty<'cx>,
        kind: SimplifiedKind,
    ) -> &'cx ty::Ty<'cx> {
        loop {
            let t = if self.is_fresh_literal_ty(ty) {
                self.get_regular_ty(ty).unwrap()
            } else if let Some(s) = ty.kind.as_substitution_ty() {
                if kind == SimplifiedKind::Writing {
                    s.base_ty
                } else {
                    self.get_substitution_intersection(ty)
                }
            } else if ty.flags.intersects(TypeFlags::SIMPLIFIABLE) {
                self.get_simplified_ty(ty, kind)
            } else {
                ty
            };
            if t == ty {
                break t;
            };
            ty = t;
        }
    }

    fn get_ty_of_init(&mut self, init: &'cx ast::Expr<'cx>) -> &'cx ty::Ty<'cx> {
        if let Some(ty) = self.get_node_links(init.id()).get_resolved_ty() {
            ty
        } else {
            self.get_ty_of_expr(init)
        }
    }

    fn get_init_ty_of_var_decl(&mut self, var_decl: &'cx ast::VarDecl<'cx>) -> &'cx ty::Ty<'cx> {
        if let Some(init) = var_decl.init {
            self.get_ty_of_init(init)
        } else {
            self.error_ty
        }
    }

    fn ty_maybe_assignable_to(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        if let Some(u) = source.kind.as_union() {
            for t in u.tys {
                if self.is_type_assignable_to(t, target) {
                    return true;
                }
            }
        } else {
            return self.is_type_assignable_to(source, target);
        }
        false
    }

    fn get_assign_reduced_ty(
        &mut self,
        decl_ty: &'cx ty::Ty<'cx>,
        assigned_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if decl_ty == assigned_ty {
            decl_ty
        } else if assigned_ty.flags.intersects(TypeFlags::NEVER) {
            return assigned_ty;
        } else {
            // TODO: cache
            assert!(decl_ty.kind.is_union());
            let filtered_ty = self.filter_type(decl_ty, |this, t| {
                this.ty_maybe_assignable_to(assigned_ty, t)
            });
            let reduced_ty = if assigned_ty.flags.intersects(TypeFlags::BOOLEAN_LIKE)
                && self.is_fresh_literal_ty(assigned_ty)
            {
                self.map_ty(
                    filtered_ty,
                    |this, t| Some(this.get_fresh_ty_of_literal_ty(t)),
                    false,
                )
                .unwrap()
            } else {
                filtered_ty
            };
            if self.is_type_assignable_to(assigned_ty, reduced_ty) {
                reduced_ty
            } else {
                decl_ty
            }
        }
    }

    fn get_common_sub_ty(&mut self, tys: &[&'cx ty::Ty<'cx>]) -> &'cx ty::Ty<'cx> {
        self.reduced_left(
            tys,
            |this, s, t, _| {
                if this.is_ty_sub_type_of(t, s) { t } else { s }
            },
            None,
            None,
            None,
        )
        .unwrap()
    }

    fn reduced_left<T: Copy + Into<U>, U>(
        &mut self,
        array: &[T],
        f: impl Fn(&mut Self, U, T, usize) -> U,
        init: Option<U>,
        start: Option<usize>,
        count: Option<usize>,
    ) -> Option<U> {
        if !array.is_empty() {
            let size = array.len();
            if size > 0 {
                let mut pos = start.unwrap_or(0);
                let end = count.map_or(size - 1, |count| {
                    if pos + count > size - 1 {
                        size - 1
                    } else {
                        pos + count
                    }
                });
                let mut result;
                if init.is_none() && start.is_none() && count.is_none() {
                    result = array[pos].into();
                    pos += 2;
                } else {
                    result = init.unwrap();
                };
                while pos <= end {
                    result = f(self, result, array[pos], pos);
                    pos += 1;
                }
                return Some(result);
            }
        }
        init
    }

    fn is_array_like_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.kind.is_array(self)
            || !ty.flags.intersects(TypeFlags::NULLABLE)
                && self.is_type_assignable_to(ty, self.any_readonly_array_ty())
    }

    fn substitute_indexed_mapped_ty(
        &mut self,
        object_ty: &'cx ty::Ty<'cx>,
        index_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let mapped_ty = object_ty.kind.expect_object_mapped();
        let source = self.get_ty_param_from_mapped_ty(object_ty);
        let mapper = self.alloc(TyMapper::make_unary(source, index_ty));
        let template_mapper = self.combine_ty_mappers(mapped_ty.mapper, mapper);
        let instantiated_template_ty = {
            let template_ty =
                self.get_template_ty_from_mapped_ty(mapped_ty.target.unwrap_or(object_ty));
            self.instantiate_ty(template_ty, Some(template_mapper))
        };
        // TODO: is_optional
        let is_optional = false;
        self.add_optionality(instantiated_template_ty, true, is_optional)
    }

    fn is_circular_mapped_prop(&self, symbol: SymbolID) -> bool {
        self.get_check_flags(symbol).intersects(CheckFlags::MAPPED)
            && self.symbol_links[&symbol].get_ty().is_none()
            && self
                .find_resolution_cycle_start_index(ResolutionKey::Type(symbol))
                .is_some()
    }

    pub fn decl_modifier_flags_from_symbol(
        &self,
        symbol: SymbolID,
    ) -> enumflags2::BitFlags<ast::ModifierKind> {
        if let Some(decl) = self.get_symbol_decl(symbol) {
            let flags = self.p.get_combined_modifier_flags(decl);
            // TODO: handle symbol parent
            return flags & !ast::ModifierKind::ACCESSIBILITY;
        };

        if self
            .symbol(symbol)
            .flags()
            .intersects(SymbolFlags::PROPERTY)
        {
            return ast::ModifierKind::Public | ast::ModifierKind::Static;
        }
        enumflags2::BitFlags::empty()
    }

    fn get_mapped_ty_optionality(&self, ty: &'cx ty::MappedTy<'cx>) -> i32 {
        let ms = ty.decl.get_modifiers();
        if ms.intersects(ast::MappedTyModifiers::EXCLUDE_OPTIONAL) {
            -1
        } else if ms.intersects(ast::MappedTyModifiers::INCLUDE_OPTIONAL) {
            1
        } else {
            0
        }
    }

    fn get_combined_mapped_ty_optionality(&mut self, ty: &'cx ty::Ty<'cx>) -> i32 {
        if ty.get_object_flags().intersects(ObjectFlags::MAPPED) {
            let n = self.get_mapped_ty_optionality(ty.kind.expect_object_mapped());
            if n != 0 {
                n
            } else {
                let t = self.get_modifiers_ty_from_mapped_ty(ty);
                self.get_combined_mapped_ty_optionality(t)
            }
        } else if let Some(intersection) = ty.kind.as_intersection() {
            let optionality = self.get_combined_mapped_ty_optionality(intersection.tys[0]);
            if intersection.tys.iter().enumerate().all(|(i, t)| {
                if i == 0 {
                    true
                } else {
                    self.get_combined_mapped_ty_optionality(t) == optionality
                }
            }) {
                optionality
            } else {
                0
            }
        } else {
            0
        }
    }

    fn get_rest_ty_of_tuple_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx ty::Ty<'cx>> {
        let t = ty.as_tuple().unwrap();
        self.get_element_ty_of_slice_of_tuple_ty(ty, t.fixed_length, None, None, None)
    }

    fn get_string_like_ty_for_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.flags.intersects(TypeFlags::ANY | TypeFlags::STRING_LIKE) {
            ty
        } else {
            self.get_template_lit_ty(&[keyword::IDENT_EMPTY, keyword::IDENT_EMPTY], &[ty])
        }
    }

    fn is_generic_reducible_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if let Some(u) = ty.kind.as_union() {
            u.object_flags
                .intersects(ObjectFlags::CONTAINS_INTERSECTIONS)
                && u.tys.iter().any(|t| self.is_generic_reducible_ty(t))
        } else if ty.kind.is_intersection() {
            self.is_reducible_intersection(ty)
        } else {
            false
        }
    }

    fn is_reducible_intersection(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        let i = ty.kind.expect_intersection();
        // TODO: cache
        // let unique_literal_filled_instantiation = self.instantiate_ty(ty, mapper);
        false
    }

    fn is_nullable_ty(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        // TODO: has_ty_facts
        false
    }

    fn get_non_nullable_ty_if_needed(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if self.is_nullable_ty(ty) {
            self.get_non_nullable_ty(ty)
        } else {
            ty
        }
    }

    fn get_default_ty_argument_ty(&self, is_in_javascript_file: bool) -> &'cx ty::Ty<'cx> {
        if is_in_javascript_file {
            self.any_ty
        } else {
            self.undefined_ty
        }
    }

    fn is_distribution_dependent(&mut self, root: &ty::CondTyRoot<'cx>) -> bool {
        root.is_distributive
            && (self.is_ty_param_possibly_referenced(root.check_ty, root.node.check_ty.id())
                || self.is_ty_param_possibly_referenced(root.check_ty, root.node.false_ty.id()))
    }

    fn is_ty_param_possibly_referenced(&mut self, ty: &'cx ty::Ty<'cx>, node: ast::NodeID) -> bool {
        struct ContainReferenceVisitor<'cx, 'checker> {
            tp: &'cx ty::Ty<'cx>,
            checker: &'checker mut TyChecker<'cx>,
            contain_reference: bool,
        }

        impl<'cx, 'checker> ContainReferenceVisitor<'cx, 'checker> {
            fn new(ty: &'cx ty::Ty<'cx>, checker: &'checker mut TyChecker<'cx>) -> Self {
                Self {
                    tp: ty,
                    checker,
                    contain_reference: false,
                }
            }
        }
        impl<'cx> bolt_ts_ast::Visitor<'cx> for ContainReferenceVisitor<'cx, '_> {
            fn visit_ident(&mut self, n: &'cx bolt_ts_ast::Ident) {
                let t = self.tp.kind.expect_param();
                if !t.is_this_ty
                    && self.checker.p.is_part_of_ty_node(n.id)
                    && self.checker.p.maybe_ty_param_reference(n.id)
                    && self.checker.get_ty_from_ident(n) == self.tp
                {
                    self.contain_reference = true;
                }
            }
        }

        let tp = ty.kind.expect_param();
        let decl = {
            let s = self.symbol(tp.symbol);
            let decls = s.declarations();
            if decls.len() != 1 {
                return true;
            }
            decls[0]
        };
        let container = self.p.parent(decl);
        if let Some(container) = container {
            let mut n = node;
            while n != container {
                let node = self.p.node(n);
                if node.is_block_stmt()
                    || node.as_cond_ty().is_some_and(|c| {
                        let mut v = ContainReferenceVisitor::new(ty, self);
                        bolt_ts_ast::visitor::visit_ty(&mut v, c.extends_ty);
                        v.contain_reference
                    })
                {
                    return true;
                };
                let Some(next) = self.p.parent(n) else {
                    break;
                };
                n = next;
            }
            let n = self.p.node(n);
            let mut v = ContainReferenceVisitor::new(ty, self);
            bolt_ts_ast::visitor::visit_node(&mut v, &n);
            return v.contain_reference;
        }
        true
    }

    fn get_annotated_accessor_ty(&mut self, n: ast::NodeID) -> Option<&'cx ty::Ty<'cx>> {
        self.p
            .get_annotated_accessor_ty_node(n)
            .map(|n| self.get_ty_from_type_node(n))
    }

    fn remove_definitely_falsy_tys(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        self.filter_type(ty, |_, t| has_type_facts(t, TypeFacts::TRUTHY))
    }

    fn get_union_index_infos(&mut self, tys: &[&'cx ty::Ty<'cx>]) -> ty::IndexInfos<'cx> {
        let source_infos = self.get_index_infos_of_ty(tys[0]);
        let mut result = Vec::with_capacity(source_infos.len());
        for info in source_infos {
            if tys
                .iter()
                .all(|t| self.get_index_info_of_ty(t, info.key_ty).is_some())
            {
                let is_readonly = tys.iter().any(|t| {
                    self.get_index_info_of_ty(t, info.key_ty)
                        .unwrap()
                        .is_readonly
                });
                let tys = tys
                    .iter()
                    .map(|t| self.get_index_ty_of_ty(t, info.key_ty).unwrap())
                    .collect::<Vec<_>>();
                let val_ty = self.get_union_ty(&tys, ty::UnionReduction::Lit);
                let index_info = self.alloc(ty::IndexInfo {
                    key_ty: info.key_ty,
                    val_ty,
                    is_readonly,
                    symbol: Symbol::ERR,
                });
                result.push(index_info);
            }
        }

        if result.is_empty() {
            self.empty_array()
        } else {
            self.alloc(result)
        }
    }

    pub(super) fn get_spread_symbol(&mut self, prop: SymbolID, readonly: bool) -> SymbolID {
        let prop_flags = self.symbol(prop).flags();
        let is_setonly_accessor = prop_flags.intersects(SymbolFlags::SET_ACCESSOR)
            && !prop_flags.intersects(SymbolFlags::GET_ACCESSOR);
        if !is_setonly_accessor && readonly == self.is_readonly_symbol(prop) {
            prop
        } else {
            // TODO: is_late_check_flags
            let check_flags = if readonly {
                CheckFlags::READONLY
            } else {
                CheckFlags::empty()
            };
            let flags = SymbolFlags::PROPERTY | prop_flags.intersection(SymbolFlags::OPTIONAL);
            let name = self.symbol(prop).name();
            let name_ty = self.get_symbol_links(prop).get_name_ty();
            let ty = if is_setonly_accessor {
                self.undefined_ty
            } else {
                self.get_type_of_symbol(prop)
            };
            let links = SymbolLinks::default()
                .with_ty(ty)
                .with_check_flags(check_flags);
            let links = if let Some(name_ty) = name_ty {
                links.with_name_ty(name_ty)
            } else {
                links
            };
            let decls = self.symbol(prop).declarations().into();
            self.create_transient_symbol(name, flags, links, decls, None)
        }
    }

    fn check_decl_init(
        &mut self,
        decl: &impl ir::HasExprInit<'cx>,
        contextual_ty: Option<&'cx ty::Ty<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let init = decl.init().unwrap();
        // TODO: get_quick_ty_of_expr
        let ty = if let Some(contextual_ty) = contextual_ty {
            let check_mode = self.check_mode.unwrap_or(CheckMode::empty());
            let ty = self.check_expr_with_contextual_ty(init, contextual_ty, None, check_mode);
            ty
        } else {
            self.check_expr_with_cache(init)
        };
        ty
    }

    pub(super) fn check_external_module_exports(&mut self, node: &'cx ast::Program<'cx>) {
        let module_symbol = self.get_symbol_of_decl(node.id);
        if let Some(checked) = self.get_symbol_links(module_symbol).get_exports_checked() {
            if checked {
                return;
            }
        }
        // if let Some(exports) = self.get_exports_of_symbol(module_symbol) {}

        self.get_mut_symbol_links(module_symbol)
            .set_exports_checked(true);
    }

    pub(super) fn is_member_of_string_mapping(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        if target.flags.intersects(TypeFlags::ANY) {
            true
        } else if target
            .flags
            .intersects(TypeFlags::STRING.union(TypeFlags::TEMPLATE_LITERAL))
        {
            self.is_type_assignable_to(source, target)
        } else if target.flags.intersects(TypeFlags::STRING_MAPPING) {
            let mut target = target;
            let mut mapping_stack = Vec::with_capacity(8);
            while let Some(t_string_mapping_ty) = target.kind.as_string_mapping_ty() {
                mapping_stack.push(t_string_mapping_ty.symbol);
                target = t_string_mapping_ty.ty;
            }
            let mapped_source = mapping_stack.into_iter().fold(source, |memo, value| {
                self.get_string_mapping_ty(value, memo)
            });
            mapped_source == source && self.is_member_of_string_mapping(source, target)
        } else {
            false
        }
    }

    fn get_ty_with_facts(&mut self, ty: &'cx ty::Ty<'cx>, include: TypeFacts) -> &'cx ty::Ty<'cx> {
        self.filter_type(ty, |_, t| has_type_facts(t, include))
    }

    fn remove_missing_or_undefined_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if *self.config.exact_optional_property_types() {
            self.filter_type(ty, |this, t| t != this.missing_ty)
        } else {
            self.get_ty_with_facts(ty, TypeFacts::NE_UNDEFINED)
        }
    }
}

macro_rules! global_ty {
    ($($name: ident),* $(,)?) => {
        impl<'cx> TyChecker<'cx> {
            $(
                #[inline(always)]
                pub fn $name(&self) -> &'cx ty::Ty<'cx> {
                    self.$name.get().unwrap()
                }
            )*
        }
    };
}

global_ty!(
    boolean_ty,
    string_or_number_ty,
    string_number_symbol_ty,
    any_array_ty,
    auto_array_ty,
    any_fn_ty,
    circular_constraint_ty,
    resolving_default_type,
    empty_generic_ty,
    empty_object_ty,
    empty_ty_literal_ty,
    no_constraint_ty,
    typeof_ty,
    global_number_ty,
    global_string_ty,
    global_boolean_ty,
    global_symbol_ty,
    global_array_ty,
    global_readonly_array_ty,
    any_readonly_array_ty,
    global_fn_ty,
    global_callable_fn_ty,
    global_newable_fn_ty,
    global_object_ty,
    global_regexp_ty,
    template_constraint_ty,
    mark_super_ty,
    mark_sub_ty,
    mark_other_ty,
);
