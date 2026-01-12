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
pub mod errors;
mod eval;
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
mod is_this_less;
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

use std::fmt::Debug;

use bolt_ts_ast::{self as ast};
use bolt_ts_ast::{BinOp, pprint_ident};
use bolt_ts_atom::{Atom, AtomIntern};
use bolt_ts_config::NormalizedCompilerOptions;
use bolt_ts_span::{ModuleID, Span};
use bolt_ts_utils::{fx_hashmap_with_capacity, no_hashmap_with_capacity, no_hashset_with_capacity};
use check_type_related_to::RecursionFlags;
use enumflags2::BitFlag;
use rustc_hash::{FxBuildHasher, FxHashMap};

use self::check_expr::IterationUse;
use self::check_expr::get_suggestion_boolean_op;
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
use self::instantiation_ty_map::TyInstantiationMap;
use self::instantiation_ty_map::{
    IndexedAccessTyMap, IntersectionMap, StringMappingTyMap, TyAliasInstantiationMap, TyCacheTrait,
    TyKey, UnionMap,
};
use self::links::NodeLinks;
pub use self::links::SymbolLinks;
use self::links::{SigLinks, TyLinks};
pub use self::merge::merge_module_augmentation_list_for_global;
use self::node_check_flags::NodeCheckFlags;
pub use self::resolve::ExpectedArgsCount;
pub(crate) use self::symbol_info::SymbolInfo;
use self::transient_symbol::create_transient_symbol;
use self::type_predicate::TyPred;
use self::utils::contains_ty;

use super::ty::TyMapper;
use super::ty::{self, AccessFlags};
use super::ty::{CheckFlags, IndexFlags, IterationTys, TYPEOF_NE_FACTS};
use super::ty::{ElementFlags, ObjectFlags, Sig, SigFlags, SigID, TyID, TypeFacts, TypeFlags};

use bolt_ts_ast::keyword;
use bolt_ts_ast::r#trait::VarLike;
use bolt_ts_binder::{AccessKind, AssignmentKind, NodeQuery};
use bolt_ts_binder::{
    FlowID, FlowInNodes, FlowNodes, GlobalSymbols, MergedSymbols, ResolveResult, Symbol,
    SymbolFlags, SymbolID, SymbolName, SymbolTable, Symbols,
};
use bolt_ts_middle::F64Represent;
use bolt_ts_module_graph::{ModuleGraph, ModuleRes};
use bolt_ts_parser::ParsedMap;

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

bolt_ts_utils::index!(InferenceContextId);

pub struct TyChecker<'cx> {
    pub atoms: AtomIntern,
    pub diags: Vec<bolt_ts_errors::Diag>,
    pub module_arena: &'cx bolt_ts_span::ModuleArena,
    config: &'cx NormalizedCompilerOptions,
    arena: &'cx bolt_ts_arena::bumpalo::Bump,
    tys: Vec<&'cx ty::Ty<'cx>>,
    sigs: Vec<&'cx Sig<'cx>>,

    flow_nodes: Vec<FlowNodes<'cx>>,
    flow_in_nodes: Vec<FlowInNodes>,
    last_flow_node: Option<FlowID>,
    last_flow_reachable: bool,
    // TODO: use `Vec<Vec<bool>>`
    flow_node_reachable: FxHashMap<FlowID, bool>,

    num_lit_tys: nohash_hasher::IntMap<F64Represent, &'cx ty::Ty<'cx>>,
    string_lit_tys: FxHashMap<Atom, &'cx ty::Ty<'cx>>,
    bigint_lit_tys: FxHashMap<(bool, Atom), &'cx ty::Ty<'cx>>,
    union_tys: UnionMap<'cx>,
    intersection_tys: IntersectionMap<'cx>,
    indexed_access_tys: IndexedAccessTyMap<'cx>,
    string_mapping_tys: StringMappingTyMap<'cx>,
    type_name: nohash_hasher::IntMap<TyID, String>,
    tuple_tys: nohash_hasher::IntMap<u64, &'cx ty::Ty<'cx>>,

    check_mode: Option<CheckMode>,
    inferences: Vec<InferenceContext<'cx>>,
    inference_contextual: Vec<InferenceContextual>,
    activity_ty_mapper: Vec<&'cx dyn ty::TyMap<'cx>>,
    instantiation_count: u32,
    activity_ty_mapper_caches: Vec<nohash_hasher::IntMap<TyKey, &'cx ty::Ty<'cx>>>,
    type_contextual: Vec<TyContextual<'cx>>,
    deferred_nodes: Vec<indexmap::IndexSet<ast::NodeID, FxBuildHasher>>,
    // === links ===
    symbol_links: FxHashMap<SymbolID, SymbolLinks<'cx>>,
    transient_symbol_links: Vec<SymbolLinks<'cx>>,
    node_links: FxHashMap<ast::NodeID, NodeLinks<'cx>>,
    sig_links: nohash_hasher::IntMap<SigID, SigLinks<'cx>>,
    ty_links: nohash_hasher::IntMap<TyID, TyLinks<'cx>>,
    iteration_tys_map: nohash_hasher::IntMap<TyKey, ty::IterationTys<'cx>>,

    instantiation_ty_map: InstantiationTyMap<'cx>,
    ty_alias_instantiation_map: TyAliasInstantiationMap<'cx>,
    ty_instantiation_map: TyInstantiationMap<'cx>,

    mark_tys: nohash_hasher::IntSet<TyID>,
    shared_flow_info: Vec<(FlowID, FlowTy<'cx>)>,
    common_ty_links_arena: ty::CommonTyLinksArena<'cx>,
    fresh_ty_links_arena: ty::FreshTyLinksArena<'cx>,
    interface_ty_links_arena: ty::InterfaceTyLinksArena<'cx>,
    object_mapped_ty_links_arena: ty::ObjectMappedTyLinksArena<'cx>,
    // === ast ===
    pub p: &'cx ParsedMap<'cx>,
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
    pub unreachable_never_ty: &'cx ty::Ty<'cx>,
    pub void_ty: &'cx ty::Ty<'cx>,
    pub null_ty: &'cx ty::Ty<'cx>,
    pub null_widening_ty: &'cx ty::Ty<'cx>,
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
    undefined_symbol: SymbolID,
    empty_symbols: &'cx SymbolTable,

    enum_number_index_info: std::cell::OnceCell<&'cx ty::IndexInfo<'cx>>,
    unknown_union_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    boolean_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    string_or_number_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    string_number_symbol_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    number_or_bigint_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    any_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    auto_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    typeof_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    any_sig: std::cell::OnceCell<&'cx Sig<'cx>>,
    unknown_sig: std::cell::OnceCell<&'cx Sig<'cx>>,
    resolving_sig: std::cell::OnceCell<&'cx Sig<'cx>>,
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
    global_tpl_strings_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    mark_super_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    mark_sub_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    mark_other_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    array_variances: std::cell::OnceCell<&'cx [VarianceFlags]>,
    no_ty_pred: std::cell::OnceCell<&'cx TyPred<'cx>>,
    template_constraint_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    unknown_empty_object_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,

    deferred_global_non_nullable_type_alias: std::cell::OnceCell<Option<SymbolID>>,
    deferred_global_extract_symbol: std::cell::OnceCell<Option<SymbolID>>,

    any_iteration_tys: std::cell::OnceCell<IterationTys<'cx>>,
    empty_array: &'cx [u8; 0],
    never_intersection_tys: nohash_hasher::IntMap<ty::TyID, bool>,
    structure_members_placeholder: &'cx ty::StructuredMembers<'cx>,

    // === resolver ===
    pub binder: &'cx mut bolt_ts_binder::Binder,
    global_symbols: &'cx mut GlobalSymbols,
    merged_symbols: &'cx mut MergedSymbols,

    // === cycle check ===
    resolution_start: i32,
    resolution_tys: thin_vec::ThinVec<ResolutionKey>,
    resolution_res: thin_vec::ThinVec<bool>,
    current_node: Option<ast::NodeID>,
    reverse_mapped_source_stack: Vec<&'cx ty::Ty<'cx>>,
    reverse_mapped_target_stack: Vec<&'cx ty::Ty<'cx>>,
    reverse_expanding_flags: RecursionFlags,
}

fn cast_empty_array<'cx, T>(empty_array: &[u8; 0]) -> &'cx [T] {
    unsafe { &*(empty_array as *const [u8] as *const [T]) }
}

impl<'cx> TyChecker<'cx> {
    fn node_query(&self, module_id: ModuleID) -> NodeQuery<'cx, '_> {
        let p = self.p.get(module_id);
        let parent_map = &self.binder.get(module_id).parent_map;
        NodeQuery::new(parent_map, p)
    }

    fn parent(&self, id: ast::NodeID) -> Option<ast::NodeID> {
        self.binder.get(id.module()).parent_map.parent(id)
    }

    pub fn new(
        ty_arena: &'cx bolt_ts_arena::bumpalo::Bump,
        p: &'cx ParsedMap<'cx>,
        mg: &'cx ModuleGraph,
        atoms: AtomIntern,
        config: &'cx NormalizedCompilerOptions,
        flow_nodes: Vec<FlowNodes<'cx>>,
        flow_in_nodes: Vec<FlowInNodes>,
        module_arena: &'cx bolt_ts_span::ModuleArena,
        binder: &'cx mut bolt_ts_binder::Binder,
        merged_symbols: &'cx mut MergedSymbols,
        global_symbols: &'cx mut GlobalSymbols,
    ) -> Self {
        let cap = p.module_count() * 1024 * 64;
        let mut transient_symbols = Symbols::new_transient(p.module_count());
        let mut transient_symbol_links = Vec::with_capacity(cap);
        let diags = Vec::with_capacity(p.module_count() * 32);
        let empty_array: &'cx [u8; 0] = ty_arena.alloc([]);
        let empty_symbols = ty_arena.alloc(bolt_ts_binder::SymbolTable::new(0));

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
            (unreachable_never_ty,  keyword::IDENT_NEVER,   TypeFlags::NEVER,           ObjectFlags::NON_INFERRABLE_TYPE),
        });

        let undefined_or_missing_ty = if config.exact_optional_property_types() {
            missing_ty
        } else {
            undefined_ty
        };

        let undefined_widening_ty = if config.strict_null_checks() {
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

        let null_widening_ty = if config.strict_null_checks() {
            null_ty
        } else {
            let ty = ty::IntrinsicTy {
                object_flags: ObjectFlags::CONTAINS_WIDENING_TYPE,
                name: keyword::KW_NULL,
            };
            let kind = ty::TyKind::Intrinsic(ty_arena.alloc(ty));
            TyChecker::make_ty(
                kind,
                TypeFlags::NULL,
                &mut tys,
                &mut common_ty_links_arena,
                ty_arena,
            )
        };

        macro_rules! make_builtin_symbol {
            ( { $( ($symbol_name: ident, $name: expr, $flags: expr, $links: expr, $builtin_id: ident) ),* $(,)? } ) => {
                $(
                    let $symbol_name = {
                        let symbol = Symbol::new($name, $flags.union(SymbolFlags::TRANSIENT));
                        let s = create_transient_symbol(&mut transient_symbols, symbol);
                        assert_eq!(s.index_as_usize(), transient_symbol_links.len());
                        transient_symbol_links.push($links.unwrap_or_default());
                        assert_eq!(s, Symbol::$builtin_id);
                        s
                    };
                )*
            };
        }
        let global_this_symbol_name = SymbolName::Atom(keyword::IDENT_GLOBAL_THIS);
        make_builtin_symbol!({
            (error_symbol,              SymbolName::Atom(keyword::SPECIAL_IDENT_ERROR), SymbolFlags::empty(),       Some(SymbolLinks::default()
                                                                                                                            .with_ty(error_ty)),                        ERR               ),
            (global_this_symbol,        global_this_symbol_name,                        SymbolFlags::MODULE,        Some(SymbolLinks::default()
                                                                                                                            .with_check_flags(CheckFlags::READONLY)),   GLOBAL_THIS       ),
            (arguments_symbol,          SymbolName::Atom(keyword::IDENT_ARGUMENTS),     SymbolFlags::PROPERTY,      Some(SymbolLinks::default()),                       ARGUMENTS         ),
            (resolving_symbol,          SymbolName::Resolving,                          SymbolFlags::empty(),       None,                                               RESOLVING         ),
            (empty_ty_literal_symbol,   SymbolName::Type,                               SymbolFlags::TYPE_LITERAL,  None,                                               EMPTY_TYPE_LITERAL),
            (undefined_symbol,          SymbolName::Atom(keyword::KW_UNDEFINED),        SymbolFlags::PROPERTY,      None,                                               UNDEFINED),
        });

        let prev = global_symbols
            .0
            .insert(global_this_symbol_name, global_this_symbol);
        assert!(prev.is_none());

        let restrictive_mapper = ty_arena.alloc(RestrictiveMapper);
        let permissive_mapper = ty_arena.alloc(PermissiveMapper);

        assert_eq!(binder.bind_results.len(), module_arena.modules().len());
        binder.bind_results.push(ResolveResult {
            symbols: transient_symbols,
            final_res: Default::default(),
            diags: Default::default(),
            locals: Default::default(),
            parent_map: Default::default(),
        });

        let structure_members_placeholder = ty_arena.alloc(ty::StructuredMembers {
            members: &empty_symbols.0,
            call_sigs: cast_empty_array(empty_array),
            ctor_sigs: cast_empty_array(empty_array),
            index_infos: cast_empty_array(empty_array),
            props: cast_empty_array(empty_array),
        });

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
            string_lit_tys: fx_hashmap_with_capacity(1024 * 8),
            bigint_lit_tys: fx_hashmap_with_capacity(512),
            union_tys: UnionMap::new(1024 * 8),
            intersection_tys: IntersectionMap::new(1024 * 8),
            indexed_access_tys: IndexedAccessTyMap::new(1024 * 8),
            string_mapping_tys: StringMappingTyMap::new(1024 * 8),
            instantiation_ty_map: InstantiationTyMap::new(1024 * 16),
            ty_alias_instantiation_map: TyAliasInstantiationMap::new(1024 * 16),
            ty_instantiation_map: TyInstantiationMap::new(1024 * 16),
            iteration_tys_map: no_hashmap_with_capacity(1024 * 4),
            mark_tys: no_hashset_with_capacity(1024 * 4),

            shared_flow_info: Vec::with_capacity(1024),
            flow_node_reachable: fx_hashmap_with_capacity(flow_nodes.len() * 128),
            flow_nodes,
            flow_in_nodes,
            last_flow_node: None,
            last_flow_reachable: false,

            error_symbol,
            global_this_symbol,
            arguments_symbol,
            resolving_symbol,
            empty_ty_literal_symbol,
            undefined_symbol,

            empty_symbols,
            structure_members_placeholder,

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
            unreachable_never_ty,
            never_ty,
            silent_never_ty,
            implicit_never_ty,
            void_ty,
            null_ty,
            null_widening_ty,
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

            enum_number_index_info: Default::default(),

            number_or_bigint_ty: Default::default(),
            any_fn_ty: Default::default(),
            circular_constraint_ty: Default::default(),
            no_constraint_ty: Default::default(),
            resolving_default_type: Default::default(),
            empty_generic_ty: Default::default(),
            empty_object_ty: Default::default(),
            boolean_ty: Default::default(),
            unknown_union_ty: Default::default(),
            typeof_ty: Default::default(),
            string_or_number_ty: Default::default(),
            string_number_symbol_ty: Default::default(),
            any_array_ty: Default::default(),
            auto_array_ty: Default::default(),
            empty_ty_literal_ty: Default::default(),
            global_number_ty: Default::default(),
            global_string_ty: Default::default(),
            global_symbol_ty: Default::default(),
            global_tpl_strings_array_ty: Default::default(),
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
            unknown_empty_object_ty: Default::default(),

            deferred_global_extract_symbol: Default::default(),
            deferred_global_non_nullable_type_alias: Default::default(),

            no_ty_pred: Default::default(),

            any_sig: Default::default(),
            unknown_sig: Default::default(),
            resolving_sig: Default::default(),
            never_intersection_tys: no_hashmap_with_capacity(1024),

            type_name: no_hashmap_with_capacity(1024 * 8),

            symbol_links: fx_hashmap_with_capacity(cap),
            node_links: fx_hashmap_with_capacity(cap),
            sig_links: no_hashmap_with_capacity(cap),
            ty_links: no_hashmap_with_capacity(cap),
            tuple_tys: no_hashmap_with_capacity(cap),
            common_ty_links_arena,
            fresh_ty_links_arena: ty::FreshTyLinksArena::with_capacity(cap),
            interface_ty_links_arena: ty::InterfaceTyLinksArena::with_capacity(cap),
            object_mapped_ty_links_arena: ty::ObjectMappedTyLinksArena::with_capacity(cap),

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
            transient_symbol_links,
            reverse_mapped_source_stack: Vec::with_capacity(8),
            reverse_mapped_target_stack: Vec::with_capacity(8),
            reverse_expanding_flags: RecursionFlags::empty(),
            activity_ty_mapper: Vec::with_capacity(1024),
            activity_ty_mapper_caches: Vec::with_capacity(1024),
            instantiation_count: 0,
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
            (boolean_ty,                    this.get_union_ty(&[regular_false_ty, regular_true_ty],                 ty::UnionReduction::Lit, false, None, None)),
            (string_or_number_ty,           this.get_union_ty(&[this.string_ty, this.number_ty],                    ty::UnionReduction::Lit, false, None, None)),
            (string_number_symbol_ty,       this.get_union_ty(&[this.string_ty, this.number_ty, this.es_symbol_ty], ty::UnionReduction::Lit, false, None, None)),
            (number_or_bigint_ty,           this.get_union_ty(&[this.number_ty, this.bigint_ty],                    ty::UnionReduction::Lit, false, None, None)),
            (global_number_ty,              this.get_global_type(SymbolName::Atom(keyword::IDENT_NUMBER_CLASS))),
            (global_boolean_ty,             this.get_global_type(SymbolName::Atom(keyword::IDENT_BOOLEAN_CLASS))),
            (global_symbol_ty,              this.get_global_type(SymbolName::Atom(keyword::IDENT_SYMBOL_CLASS))),
            (global_string_ty,              this.get_global_type(SymbolName::Atom(keyword::IDENT_STRING_CLASS))),
            (global_array_ty,               this.get_global_type(SymbolName::Atom(keyword::IDENT_ARRAY_CLASS))),
            (global_regexp_ty,              this.get_global_type(SymbolName::Atom(keyword::IDENT_REGEXP_CLASS))),
            (global_tpl_strings_array_ty,   this.get_global_type(SymbolName::Atom(keyword::IDENT_TEMPLATE_STRINGS_ARRAY_CLASS))),
            (any_array_ty,                  this.create_array_ty(this.any_ty, false)),
            (global_readonly_array_ty,      this.get_global_type(SymbolName::Atom(keyword::IDENT_READONLY_ARRAY_CLASS))),
            (any_readonly_array_ty,         this.any_array_ty()),
            (typeof_ty,                 {
                                            let tys = TYPEOF_NE_FACTS.iter().map(|(key, _)| this.get_string_literal_type_from_string(*key)).collect::<Vec<_>>();
                                            this.get_union_ty(&tys, ty::UnionReduction::Lit, false, None, None)
                                        }),
            (any_fn_ty,                     this.create_anonymous_ty_with_resolved(None, ObjectFlags::NON_INFERRABLE_TYPE, this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (no_constraint_ty,              this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (circular_constraint_ty,        this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (resolving_default_type,        this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (empty_generic_ty,              this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (empty_object_ty,               this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (empty_ty_literal_ty,           this.create_anonymous_ty_with_resolved(Some(empty_ty_literal_symbol), Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (unknown_empty_object_ty,       this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (global_object_ty,              this.get_global_type(SymbolName::Atom(keyword::IDENT_OBJECT_CLASS))),
            (global_fn_ty,                  this.get_global_type(SymbolName::Atom(keyword::IDENT_FUNCTION_CLASS))),
            (global_callable_fn_ty,         this.get_global_type(SymbolName::Atom(keyword::IDENT_CALLABLE_FUNCTION_CLASS))),
            (global_newable_fn_ty,          this.get_global_type(SymbolName::Atom(keyword::IDENT_NEWABLE_FUNCTION_CLASS))),
            (mark_sub_ty,                   this.create_param_ty(Symbol::ERR, None, false)),
            (mark_other_ty,                 this.create_param_ty(Symbol::ERR, None, false)),
            (mark_super_ty,                 this.create_param_ty(Symbol::ERR, None, false)),
            (template_constraint_ty,        this.get_union_ty(&[string_ty, number_ty, boolean_ty, bigint_ty, null_ty, undefined_ty], ty::UnionReduction::Lit, false, None, None)),
            (any_iteration_tys,             this.create_iteration_tys(any_ty, any_ty, any_ty)),
            (any_sig,                       this.new_sig(Sig { flags: SigFlags::empty(), ty_params: None, this_param: None, params: cast_empty_array(empty_array), min_args_count: 0, ret: None, node_id: None, target: None, mapper: None, id: SigID::dummy(), class_decl: None })),
            (unknown_sig,                   this.new_sig(Sig { flags: SigFlags::empty(), ty_params: None, this_param: None, params: cast_empty_array(empty_array), min_args_count: 0, ret: None, node_id: None, target: None, mapper: None, id: SigID::dummy(), class_decl: None })),
            (resolving_sig,                 this.new_sig(Sig { flags: SigFlags::empty(), ty_params: None, this_param: None, params: cast_empty_array(empty_array), min_args_count: 0, ret: None, node_id: None, target: None, mapper: None, id: SigID::dummy(), class_decl: None })),
            (array_variances,               this.alloc([VarianceFlags::COVARIANT])),
            (no_ty_pred,                    this.create_ident_ty_pred(keyword::IDENT_EMPTY, 0, any_ty)),
            (enum_number_index_info,        this.alloc(ty::IndexInfo { symbol: Symbol::ERR, key_ty: number_ty, val_ty: string_ty, is_readonly: true }))
        });

        let unknown_union_ty = if this.config.strict_null_checks() {
            this.get_union_ty(
                &[this.undefined_ty, this.null_ty, unknown_empty_object_ty],
                ty::UnionReduction::Lit,
                false,
                None,
                None,
            )
        } else {
            unknown_ty
        };
        this.unknown_union_ty.set(unknown_union_ty).unwrap();

        debug_assert!(this.global_readonly_array_ty().is_readonly_array(&this));

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
                None,
            );
        }
        this.auto_array_ty.set(auto_array_ty).unwrap();

        // add_undefined_to_globals_or_error_on_redeclaration

        use indexmap::map::Entry;
        match this
            .global_symbols
            .0
            .entry(SymbolName::Atom(keyword::KW_UNDEFINED))
        {
            Entry::Occupied(occ) => {
                let symbol = *occ.get();
                if let Some(decls) = this.symbol(symbol).decls.clone() {
                    for decl in decls {
                        let n = this.p.node(decl);
                        if !this.node_query(decl.module()).is_type_decl(decl) {
                            let error =
                                errors::DeclarationNameConflictsWithBuiltInGlobalIdentifier {
                                    name: this.atoms().get(keyword::KW_UNDEFINED).to_string(),
                                    span: n.error_span(),
                                };
                            this.push_error(Box::new(error));
                        }
                    }
                }
            }
            Entry::Vacant(vac) => {
                vac.insert(undefined_symbol);
            }
        }

        this.merge_module_augmentation_list_for_non_global();

        this
    }

    pub fn print_ty<'a>(&'a mut self, ty: &'cx ty::Ty<'cx>) -> &'a str {
        let type_name = (!self.type_name.contains_key(&ty.id)).then(|| ty.to_string(self));
        self.type_name
            .entry(ty.id)
            .or_insert_with(|| type_name.unwrap())
    }

    pub fn any_sig(&self) -> &'cx Sig<'cx> {
        self.any_sig.get().unwrap()
    }

    pub fn unknown_sig(&self) -> &'cx Sig<'cx> {
        self.unknown_sig.get().unwrap()
    }

    pub fn resolving_sig(&self) -> &'cx Sig<'cx> {
        self.resolving_sig.get().unwrap()
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
        let t = if ty.kind.is_instantiable() {
            self.get_base_constraint_of_ty(ty)
                .unwrap_or(self.unknown_ty)
        } else {
            ty
        };
        let flags = t.flags;
        let object_flags = t.get_object_flags();
        if object_flags.intersects(ObjectFlags::MAPPED) {
            self.get_apparent_ty_of_mapped_ty(t)
        } else if object_flags.intersects(ObjectFlags::REFERENCE) && t != ty {
            self.get_ty_with_this_arg(t, Some(ty), false)
        } else if flags.intersects(TypeFlags::INTERSECTION) {
            self.get_apparent_ty_of_intersection_ty(t, ty)
        } else if flags.intersects(TypeFlags::NUMBER_LIKE) {
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
        } else if flags.intersects(TypeFlags::UNKNOWN) && !self.config.strict_null_checks() {
            self.empty_object_ty()
        } else {
            t
        }
    }

    fn get_apparent_ty_of_mapped_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let m = ty.kind.expect_object_mapped();
        if let Some(resolved) =
            self.object_mapped_ty_links_arena[m.links].get_resolved_apparent_ty()
        {
            return resolved;
        }
        let t = self.get_resolved_apparent_ty_of_mapped_ty(ty);
        self.object_mapped_ty_links_arena[m.links].set_resolved_apparent_ty(t);
        t
    }

    fn get_resolved_apparent_ty_of_mapped_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        let m = ty.kind.expect_object_mapped();
        let target = m.target.unwrap_or(ty);
        let ty_var = self.get_homomorphic_ty_var(target);
        if let Some(ty_var) = ty_var
            && target.kind.expect_object_mapped().decl.name_ty.is_none()
            && let modifier_ty = self.get_modifiers_ty_from_mapped_ty(ty)
            && let Some(base_constraint) = if self.is_generic_mapped_ty(modifier_ty) {
                Some(self.get_apparent_ty_of_mapped_ty(modifier_ty))
            } else {
                self.get_base_constraint_of_ty(modifier_ty)
            }
            && self.every_type(base_constraint, |this, t| {
                this.is_array_or_tuple(t) || this.is_array_or_tuple_intersection(ty)
            })
        {
            let mapper = self.prepend_ty_mapping(ty_var, base_constraint, m.mapper);
            return self.instantiate_ty(target, Some(mapper));
        }
        ty
    }

    fn get_apparent_ty_of_intersection_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        this_arg: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty == this_arg {
            // TODO: `resolved_apparent_ty` cache
            self.get_ty_with_this_arg(ty, Some(this_arg), true)
        } else {
            // TODO: cache
            self.get_ty_with_this_arg(ty, Some(this_arg), true)
        }
    }

    fn get_reduced_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.kind.as_union().is_some_and(|union| {
            union
                .object_flags
                .intersects(ObjectFlags::CONTAINS_INTERSECTIONS)
        }) {
            // TODO:
        } else if ty.flags.intersects(TypeFlags::INTERSECTION) {
            if let Some(is_never_intersection_ty) = self.never_intersection_tys.get(&ty.id).copied()
            {
                return if is_never_intersection_ty {
                    self.never_ty
                } else {
                    ty
                };
            }
            let props = self.get_props_of_union_or_intersection(ty);
            let is_never_intersection_ty = props.iter().any(|p| self.is_never_reduced_prop(*p));
            self.never_intersection_tys
                .insert(ty.id, is_never_intersection_ty);
            // TODO: assert(prev.is_none());
            return if is_never_intersection_ty {
                self.never_ty
            } else {
                ty
            };
        }
        ty
    }

    fn is_never_reduced_prop(&mut self, symbol: SymbolID) -> bool {
        // TODO: || is_conflicting_private_prop
        self.is_discriminant_with_never_ty(symbol)
    }

    fn is_discriminant_with_never_ty(&mut self, symbol: SymbolID) -> bool {
        self.symbol(symbol).flags.intersects(SymbolFlags::OPTIONAL)
            && self
                .get_check_flags(symbol)
                .intersection(CheckFlags::DISCRIMINANT.union(CheckFlags::HAS_NEVER_TYPE))
                == CheckFlags::DISCRIMINANT
            && self
                .get_type_of_symbol(symbol)
                .flags
                .intersects(TypeFlags::NEVER)
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

        // if ty.kind.as_object_interface().is_some() {
        //     let base_tys = self
        //         .get_ty_links(ty.id)
        //         .expect_structured_members()
        //         .base_tys;
        //     for base_ty in base_tys {
        //         self.check_index_constraint_for_prop(base_ty, prop, prop_name_ty, prop_ty);
        //     }
        // } else {
        //     // unreachable!("{:#?}", ty)
        // }
    }

    fn get_lit_ty_from_prop_name(
        &mut self,
        prop_name: &ast::PropNameKind<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        match prop_name {
            ast::PropNameKind::Ident(ident) => self.get_string_literal_type_from_string(ident.name),
            ast::PropNameKind::NumLit(num) => {
                let ty = self.get_number_literal_type_from_number(num.val);
                self.get_regular_ty_of_literal_ty(ty)
            }
            ast::PropNameKind::StringLit { key, .. } => {
                self.get_string_literal_type_from_string(*key)
            }
            ast::PropNameKind::Computed(name) => {
                let ty = self.check_computed_prop_name(name);
                self.get_regular_ty_of_literal_ty(ty)
            }
        }
    }

    fn get_declaration_modifier_flags_from_symbol(
        &self,
        symbol: SymbolID,
        is_write: Option<bool>,
    ) -> enumflags2::BitFlags<bolt_ts_ast::ModifierKind> {
        let is_write = is_write.unwrap_or(false);
        let s = self.symbol(symbol);
        fn find_decls<'cx>(
            this: &TyChecker<'cx>,
            s: &Symbol,
            f: impl Fn(ast::Node<'cx>) -> bool,
        ) -> Option<ast::NodeID> {
            s.decls
                .as_ref()
                .and_then(|decls| decls.iter().find(|id| f(this.p.node(**id))).copied())
        }
        if let Some(value_declaration) = s.value_decl {
            let decl = is_write
                .then(|| find_decls(self, s, |n| n.is_setter_decl()))
                .flatten()
                .or_else(|| {
                    if s.flags.intersects(SymbolFlags::GET_ACCESSOR) {
                        find_decls(self, s, |n| n.is_getter_decl())
                    } else {
                        None
                    }
                })
                .unwrap_or(value_declaration);
            let flags = self
                .node_query(decl.module())
                .get_combined_modifier_flags(decl);
            return if let Some(p) = s.parent
                && let p = self.symbol(p)
                && p.flags.intersects(SymbolFlags::CLASS)
            {
                flags
            } else {
                flags & !ast::ModifierKind::ACCESSIBILITY
            };
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
        } else if s.flags.intersects(SymbolFlags::PROPERTY) {
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
                    let symbol_name = self.symbol(prop).name;
                    if symbol_name == SymbolName::ExportDefault {
                        Some(self.get_string_literal_type_from_string(keyword::KW_DEFAULT))
                    } else {
                        self.symbol(prop)
                            .value_decl
                            .and_then(|v_decl| {
                                self.node_query(v_decl.module())
                                    .get_name_of_decl(v_decl)
                                    .map(|name| {
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
                            .map(|name| self.get_lit_ty_from_prop_name(&name.kind))
                            .or_else(|| {
                                if let Some(num) = symbol_name.as_numeric() {
                                    let atom = self.atoms.atom(num.to_string().as_str());
                                    Some(self.get_string_literal_type_from_string(atom))
                                } else if let Some(atom) = symbol_name.as_atom() {
                                    Some(self.get_string_literal_type_from_string(atom))
                                } else {
                                    None
                                }
                            })
                    }
                }
            };
            if let Some(ty) = ty
                && ty.flags.intersects(include)
            {
                return ty;
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
            if !(is_static_index && self.symbol(*prop).flags.intersects(SymbolFlags::PROTOTYPE)) {
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
        self.node_query(node.module()).find_ancestor(node, |node| {
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
        if !check_mode.intersects(CheckMode::INFERENTIAL.union(CheckMode::SKIP_GENERIC_FUNCTIONS)) {
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
        if let Some(ret_sig) = ret_sig
            && ret_sig.ty_params.is_none()
            && !self
                .inference_infos(inference)
                .iter()
                .all(|info| info.has_inference_candidates())
        {
            todo!()
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
            SymbolFlags::FUNCTION.union(SymbolFlags::TRANSIENT),
            SymbolLinks::default(),
            Default::default(), // TODO: use sig.decls
            None,               // TODO: use sig.value_decl
        );
        let outer_ty_params: Option<ty::Tys<'cx>> = if let Some(outer_ty_params) = outer_ty_params {
            Some(outer_ty_params)
        } else if let Some(id) = sig.node_id
            && let Some(ty_params) = self.get_outer_ty_params::<true>(id)
        {
            Some(self.alloc(ty_params))
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
                call_sigs: if !is_constructor {
                    self.alloc([sig])
                } else {
                    self.empty_array()
                },
                ctor_sigs: if is_constructor {
                    self.alloc([sig])
                } else {
                    self.empty_array()
                },
                index_infos: self.empty_array(),
                props: self.empty_array(),
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
                if let Some(target) = tp.target
                    && self.get_constraint_of_ty_param(target).is_none()
                {
                    return target;
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

        self.apply_to_param_tys(source_sig, sig, |this, source, target| {
            let mut infer = this.infer_state(context, None, false, target);
            infer.infer_from_tys(source, target)
        });
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
        node: ast::NodeID,
        include_global_this: bool,
        container_id: Option<ast::NodeID>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let container_id = container_id.unwrap_or_else(|| {
            self.node_query(node.module())
                .get_this_container(node, false, false)
        });
        let container = self.p.node(container_id);
        if container.is_fn_like() {
            let this_ty = self
                .get_this_ty_of_decl(container_id)
                .or_else(|| self.get_contextual_this_param_ty(container_id));

            if let Some(this_ty) = this_ty {
                return Some(this_ty);
            }
        }

        if let Some(parent) = self.parent(container_id) {
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
                    // TODO: get_flow_type_reference
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
            if self.symbol(symbol).flags.intersects(SymbolFlags::VARIABLE) {
                let mut child = ident.id;
                let mut node = self.parent(child);
                while let Some(id) = node {
                    let n = self.p.node(id);
                    if let Some(f) = n.as_for_in_stmt() {
                        todo!()
                    }
                    child = id;
                    node = self.parent(child);
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

        let missing_prop = pprint_ident(prop_node, &self.atoms);

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
                    prop_name: pprint_ident(prop_node, &self.atoms),
                }),
            );
            self.push_error(Box::new(error));
        } else {
            let error = errors::PropertyXDoesNotExistOnTypeY {
                span: prop_node.span,
                prop: missing_prop,
                ty: self.print_ty(containing_ty).to_string(),
                related: vec![],
            };
            self.push_error(Box::new(error));
        }
    }

    pub(super) fn check_prop_access_expr_or_qualified_name(
        &mut self,
        node: ast::NodeID,
        left: ast::NodeID,
        left_ty: &'cx ty::Ty<'cx>,
        right: &'cx ast::Ident,
    ) -> &'cx ty::Ty<'cx> {
        let nq = self.node_query(node.module());
        let assignment_kind = nq.get_assignment_target_kind(node);
        let apparent_left_ty = {
            let t = if assignment_kind != AssignmentKind::None || nq.is_method_access_for_call(node)
            {
                self.get_widened_ty(left_ty)
            } else {
                left_ty
            };
            self.get_apparent_ty(t)
        };
        let is_any_like = self.is_type_any(apparent_left_ty); // TODO: apparent_left_ty == self.silent_never_ty;
        // TODO: is_private_identifier

        if is_any_like {
            // TODO: if self.p.node(left).is_ident() && parent_symbol
            return if self.is_error(apparent_left_ty) {
                self.error_ty
            } else {
                apparent_left_ty
            };
        }

        let name = SymbolName::Atom(right.name);
        let prop = self.get_prop_of_ty(apparent_left_ty, name);
        if let Some(prop) = prop {
            self.check_prop_not_used_before_declaration(prop, node, right);

            if self.get_node_links(node).get_resolved_symbol().is_none() {
                self.get_mut_node_links(node).set_resolved_symbol(prop);
            }
            self.check_prop_accessibility(
                node,
                self.p.node(left).is_super_expr(),
                false,
                apparent_left_ty,
                prop,
                true,
            );

            if self.is_assignment_to_readonly_entity(node, prop, assignment_kind) {
                let error = errors::CannotAssignToXBecauseItIsAReadOnlyProperty {
                    span: right.span,
                    prop: self.atoms.get(right.name).to_string(),
                };
                self.push_error(Box::new(error));
                return self.error_ty;
            }

            if self.node_query(node.module()).access_kind(node) == AccessKind::Write {
                self.get_write_type_of_symbol(prop)
            } else {
                self.get_type_of_symbol(prop)
            }
        } else {
            if name
                .as_atom()
                .is_some_and(|atom| atom != keyword::IDENT_EMPTY)
            {
                self.report_non_existent_prop(right, left_ty);
            }
            self.error_ty
        }
        // TODO: get_flow_type
    }

    fn check_prop_not_used_before_declaration(
        &mut self,
        prop: SymbolID,
        node: ast::NodeID,
        right: &'cx ast::Ident,
    ) {
        if self.p.get(node.module()).is_declaration {
            return;
        };
        let Some(value_decl) = self.symbol(prop).value_decl else {
            return;
        };
        let is_prop_declared_in_ancestor_class = |this: &mut Self| -> bool {
            let prop_symbol = this.symbol(prop);
            let Some(parent) = prop_symbol.parent else {
                return false;
            };
            let parent_symbol = this.binder.symbol(parent);
            let parent_name = parent_symbol.name;
            if !parent_symbol.flags.intersects(SymbolFlags::CLASS) {
                return false;
            }
            let mut class_ty = this.get_type_of_symbol(parent);
            loop {
                if class_ty.symbol().is_none() {
                    return false;
                };
                // get_super_class
                let x = this.get_base_tys(class_ty);
                if x.is_empty() {
                    return false;
                }
                class_ty = this.get_intersection_ty(x, IntersectionFlags::None, None, None);
                // ===
                if let Some(super_prop) = this.get_prop_of_ty(class_ty, parent_name) {
                    if this.symbol(super_prop).value_decl.is_some() {
                        return true;
                    }
                }
            }
        };

        if self
            .node_query(node.module())
            .is_in_prop_initializer_or_class_static_block(node, false)
            && let value_decl_node = self.p.node(value_decl)
            && !(value_decl_node.as_class_prop_elem().is_some_and(|n| {
                n.question.is_some()
                    && n.modifiers
                        .is_none_or(|ms| ms.flags.contains(ast::ModifierKind::Accessor))
            }))
            && let n = self.p.node(node)
            && !n
                .expr_of_access_expr()
                .is_some_and(|n| n.kind.is_access_expr())
            && !self.is_block_scoped_name_declared_before_use(value_decl, right)
            && !((value_decl_node.is_class_method_elem()
                || value_decl_node.is_object_method_member())
                && self
                    .node_query(value_decl.module())
                    .get_combined_modifier_flags(value_decl)
                    .contains(ast::ModifierKind::Static))
            && !is_prop_declared_in_ancestor_class(self)
        {
            let error = errors::Property0IsUsedBeforeItsInitialization {
                span: right.span,
                name: self.atoms.get(right.name).to_string(),
            };
            self.push_error(Box::new(error));
        }
    }

    fn check_prop_accessibility(
        &mut self,
        node: ast::NodeID,
        is_super: bool,
        writing: bool,
        ty: &'cx ty::Ty<'cx>,
        prop: SymbolID,
        report_error: bool,
    ) {
        let error_node = report_error.then_some(node);
        self.check_prop_accessibility_at_loc(node, is_super, writing, ty, prop, error_node);
    }

    fn symbol_hash_non_method_decl(&mut self, symbol: SymbolID) -> bool {
        self.for_each_prop(symbol, |this, s| {
            let s = this.symbol(s);
            !s.flags.contains(SymbolFlags::METHOD)
        })
        .unwrap()
    }

    fn for_each_prop<T>(
        &mut self,
        prop: SymbolID,
        f: impl FnOnce(&mut Self, SymbolID) -> T,
    ) -> Option<T> {
        if self.get_check_flags(prop).intersects(CheckFlags::SYNTHETIC) {
            todo!()
        } else {
            Some(f(self, prop))
        }
    }

    fn check_prop_accessibility_at_loc(
        &mut self,
        loc: ast::NodeID,
        is_super: bool,
        writing: bool,
        containing_ty: &'cx ty::Ty<'cx>,
        prop: SymbolID,
        error_node: Option<ast::NodeID>,
    ) -> bool {
        let flags = self.get_declaration_modifier_flags_from_symbol(prop, Some(writing));
        if is_super {
            if *self.config.target() < bolt_ts_config::Target::ES2015 {
                if self.symbol_hash_non_method_decl(prop) {
                    if let Some(error_node) = error_node {
                        let error = errors::OnlyPublicAndProtectedMethodsOfTheBaseClassAreAccessibleViaTheSuperKeyword {
                            span: self.p.node(error_node).span(),
                        };
                        self.push_error(Box::new(error));
                    }
                    return false;
                }
            }
        }

        if flags.intersects(ast::ModifierKind::Private) {
            // TODO: use parent symbol to find the class
            let p = self.parent(loc).unwrap();
            if self
                .node_query(p.module())
                .find_ancestor(p, |n| n.is_class_like().then_some(true))
                .is_none()
            {
                if let Some(error_node) = error_node {
                    let prop = self.symbol(prop).name.to_string(&self.atoms);
                    let error = errors::PropertyIsPrivateAndOnlyAccessibleWithinClass {
                        span: self.p.node(error_node).span(),
                        prop,
                    };
                    self.push_error(Box::new(error));
                }
                return false;
            }
        }

        true
    }

    fn check_prop_access_expr(&mut self, node: &'cx ast::PropAccessExpr<'cx>) -> &'cx ty::Ty<'cx> {
        let left_ty = self.check_non_null_expr(node.expr);
        self.check_prop_access_expr_or_qualified_name(node.id, node.expr.id(), left_ty, node.name)
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
        source_ty: &'cx ty::Ty<'cx>,
        right_is_this: bool,
    ) -> &'cx ty::Ty<'cx> {
        let target = node.left;
        if let ast::ExprKind::ArrayLit(array) = target.kind {
            return self.check_array_lit_assignment(array, source_ty);
        }
        let left_ty = self.check_expr(node.left);
        self.check_binary_like_expr(node, left_ty, source_ty)
    }

    fn check_array_lit_assignment(
        &mut self,
        array: &'cx ast::ArrayLit<'cx>,
        source_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let possibly_out_of_bounds_ty = self.check_iterated_ty_or_element_ty(
            IterationUse::DESTRUCTURING.union(IterationUse::POSSIBLY_OUT_OF_BOUNDS),
            source_ty,
            self.undefined_ty,
            Some(array.id),
        );
        for (index, elem) in array.elems.iter().enumerate() {
            let ty = possibly_out_of_bounds_ty;
            // TODO: spared
            self.check_array_lit_destructuring_elem_assignment(array, source_ty, index, ty)
        }
        source_ty
    }

    fn check_array_lit_destructuring_elem_assignment(
        &mut self,
        array: &'cx ast::ArrayLit<'cx>,
        source_ty: &'cx ty::Ty<'cx>,
        index: usize,
        ty: &'cx ty::Ty<'cx>,
    ) {
        let elem = &array.elems[index];
        if !matches!(elem.kind, ast::ExprKind::Omit(_)) {
            if !matches!(elem.kind, ast::ExprKind::SpreadElement(_)) {
                let index_ty = self.get_number_literal_type::<false>((index as f64).into(), None);
                if self.is_array_like_ty(source_ty) {
                    // TODO: `has_default_value`
                    let access_flags = AccessFlags::EXPRESSION_POSITION;
                    let element_ty = self.get_indexed_access_ty_or_undefined(
                        source_ty,
                        index_ty,
                        Some(access_flags),
                        Some(elem.id()),
                    );
                    // TODO: assigned_ty
                }
            }
        }
    }

    fn check_object_prop_member(
        &mut self,
        member: &'cx ast::ObjectPropAssignment<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        // TODO: computed member
        self.check_expr_for_mutable_location(member.init)
    }

    fn check_object_method_member(
        &mut self,
        member: &'cx ast::ObjectMethodMember<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        // TODO: computed member
        let ty = self.check_fn_like_expr_or_object_method_member(member.id);
        self.instantiate_ty_with_single_generic_call_sig(member.id, ty)
    }

    fn check_array_lit(&mut self, lit: &'cx ast::ArrayLit, force_tuple: bool) -> &'cx ty::Ty<'cx> {
        let mut element_types = Vec::with_capacity(lit.elems.len());
        let mut element_flags = Vec::with_capacity(lit.elems.len());
        self.push_cached_contextual_type(lit.id);
        let contextual_ty = self.get_apparent_ty_of_contextual_ty(lit.id, None);
        let is_const_context = self.is_const_context(lit.id);
        let is_tuple_context = if let Some(contextual_ty) = contextual_ty {
            self.some_type(contextual_ty, |this, ty| this.is_tuple_like(ty))
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
                if self.config.strict_null_checks() {
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
                self.get_union_ty(tys, ty::UnionReduction::Subtype, false, None, None)
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
        assert_eq!(used.id.module(), decl.module());
        self.node_query(used.id.module())
            .find_ancestor(used.id, |current| {
                let current_id = current.id();
                if current_id == decl_container {
                    return Some(false);
                } else if current.is_fn_like() {
                    return Some(true);
                } else if current.is_class_static_block_decl() {
                    return Some(self.p.node(decl).span().lo() < used.span.lo());
                }

                let parent_node = self.p.node(self.parent(current_id)?);
                let prop_decl = parent_node.as_class_prop_elem()?;

                let init_of_prop = prop_decl
                    .init
                    .map(|init| init.id() == current_id)
                    .unwrap_or_default();
                if init_of_prop {
                    if parent_node.is_static() {
                        let n = self.p.node(decl);
                        if n.is_class_method_elem() {
                            return Some(true);
                        } else if let Some(prop_decl) = n.as_class_prop_elem()
                            && let Some(usage_class) = self
                                .node_query(used.id.module())
                                .get_containing_class(used.id)
                            && let Some(decl_class) =
                                self.node_query(decl.module()).get_containing_class(decl)
                            && usage_class == decl_class
                        {
                            let prop_name = prop_decl.name;
                            if let ast::PropNameKind::Ident(_) = prop_name.kind {
                                // TODO: is_private_name
                                // let ty = {
                                //     let s = self.get_symbol_of_decl(decl);
                                //     self.get_type_of_symbol(s)
                                // };
                                // let p = self.p.node(self.parent(decl).unwrap());
                                // let elems = match p {
                                //     ast::Node::ClassDecl(c) => c.elems,
                                //     ast::Node::ClassExpr(c) => c.elems,
                                //     _ => unreachable!(),
                                // };
                                // let static_blocks = elems.list.iter().filter_map(|n| {
                                //     if let ast::ClassElemKind::StaticBlockDecl(block) = n.kind {
                                //         Some(block)
                                //     } else {
                                //         None
                                //     }
                                // });
                                // let mut is_prop_initialized_in_static_block = false;
                                // let p_span = p.span();
                                // let current_span = current.span();
                                // for static_block in static_blocks {
                                //     if static_block.span.lo() > p_span.lo()
                                //         && static_block.span.lo() <= current_span.lo()
                                //     {
                                //         // TODO:
                                //     }
                                // }

                                // if is_prop_initialized_in_static_block {
                                //     return Some(true);
                                // }
                            }
                        }
                    } else {
                        let n = self.p.node(decl);
                        let is_decl_instance_prop = n.is_class_prop_elem() && !n.is_static();
                        if !is_decl_instance_prop {
                            return Some(true);
                        } else if let Some(usage_class) = self
                            .node_query(used.id.module())
                            .get_containing_class(used.id)
                            && let Some(decl_class) =
                                self.node_query(decl.module()).get_containing_class(decl)
                            && usage_class != decl_class
                        {
                            return Some(true);
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
        use ast::Node::*;
        if decl.module() != used.id.module() {
            return true;
        }
        let used_span = used.span;
        let decl_span = self.p.node(decl).span();
        let decl_pos = decl_span.lo();
        let nq = self.node_query(decl.module());
        let decl_container = nq.get_enclosing_blockscope_container(decl);

        if decl_pos < used_span.lo() {
            let n = self.p.node(decl);
            return match n {
                VarDecl(decl) => !self
                    .node_query(decl.id.module())
                    .is_immediately_used_in_init_or_block_scoped_var(decl, used.id, decl_container),
                ObjectBindingElem(_) => {
                    match nq.find_ancestor(used.id, |n| n.is_object_binding_elem().then_some(true))
                    {
                        Some(error_binding_element) => {
                            n.span().lo() < self.p.node(error_binding_element).span().lo() || {
                                nq.find_ancestor(error_binding_element, |n| {
                                    matches!(n, ObjectBindingElem(_) | ArrayBinding(_))
                                        .then_some(true)
                                }) != nq.find_ancestor(decl, |n| {
                                    matches!(n, ObjectBindingElem(_) | ArrayBinding(_))
                                        .then_some(true)
                                })
                            }
                        }
                        None => {
                            let decl = nq
                                .find_ancestor(decl, |n| n.is_var_decl().then_some(true))
                                .unwrap();
                            self.is_block_scoped_name_declared_before_use(decl, used)
                        }
                    }
                }
                ArrayBinding(_) => {
                    match nq.find_ancestor(used.id, |n| n.is_array_binding().then_some(true)) {
                        Some(error_binding_element) => {
                            n.span().lo() < self.p.node(error_binding_element).span().lo() || {
                                nq.find_ancestor(error_binding_element, |n| {
                                    matches!(n, ObjectBindingElem(_) | ArrayBinding(_))
                                        .then_some(true)
                                }) != nq.find_ancestor(decl, |n| {
                                    matches!(n, ObjectBindingElem(_) | ArrayBinding(_))
                                        .then_some(true)
                                })
                            }
                        }
                        None => {
                            let decl = nq
                                .find_ancestor(decl, |n| n.is_var_decl().then_some(true))
                                .unwrap();
                            self.is_block_scoped_name_declared_before_use(decl, used)
                        }
                    }
                }
                _ => true,
            };
        }

        if self.is_used_in_fn_or_instance_prop(used, decl, decl_container) {
            return true;
        }

        false
    }

    fn check_resolved_block_scoped_var(&mut self, ident: &'cx ast::Ident, id: SymbolID) {
        let s = self.binder.symbol(id);
        let Some(decl) = s.opt_decl() else {
            return;
        };

        if !self
            .p
            .node_flags(decl)
            .contains(bolt_ts_ast::NodeFlags::AMBIENT)
            && !self.is_block_scoped_name_declared_before_use(decl, ident)
        {
            let (decl_span, kind) = match self.p.node(decl) {
                ast::Node::VarDecl(decl) => (decl.span, errors::DeclKind::BlockScopedVariable),
                ast::Node::ObjectBindingElem(elem) => {
                    (elem.span, errors::DeclKind::BlockScopedVariable)
                }
                ast::Node::ClassDecl(class) => (class.name.unwrap().span, errors::DeclKind::Class),
                ast::Node::EnumDecl(decl) => {
                    if s.flags.contains(SymbolFlags::REGULAR_ENUM) {
                        (decl.span, errors::DeclKind::Enum)
                    } else {
                        return;
                    }
                }
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
        // TODO: dont duplicate check more than once.
        if self.symbol(symbol).flags.intersects(
            SymbolFlags::CLASS
                .union(SymbolFlags::BLOCK_SCOPED_VARIABLE)
                .union(SymbolFlags::ENUM),
        ) {
            self.check_resolved_block_scoped_var(ident, symbol);
        }

        let ty = self.get_type_of_symbol(symbol);
        let assignment_kind = self
            .node_query(ident.id.module())
            .get_assignment_target_kind(ident.id);

        if assignment_kind != AssignmentKind::None && symbol != Symbol::ERR {
            let symbol = self.binder.symbol(symbol);
            if !symbol.flags.intersects(SymbolFlags::VARIABLE) {
                let ty = if symbol.flags.contains(SymbolFlags::CLASS) {
                    "class"
                } else if symbol.flags.contains(SymbolFlags::FUNCTION) {
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

        let mut decl = if symbol == self.global_this_symbol {
            return ty;
        } else if let Some(decl) = symbol.opt_decl(self.binder) {
            decl
        } else {
            return ty;
        };

        let s = self.binder.symbol(symbol);
        let is_alias = s.flags.contains(SymbolFlags::ALIAS);

        if s.flags.intersects(SymbolFlags::VARIABLE) {
            if assignment_kind == AssignmentKind::Definite {
                // TODO: is_in_compound_like_assignment
                return ty;
            }
        } else if is_alias {
            let Some(d) = s.get_decl_of_alias_symbol(self.p) else {
                return ty;
            };
            decl = d;
        } else {
            return ty;
        }

        let immediate_decl = decl;

        let decl_container = self
            .node_query(decl.module())
            .get_control_flow_container(decl);
        let flow_container = self
            .node_query(ident.id.module())
            .get_control_flow_container(ident.id);
        let is_outer_variable = flow_container != decl_container;

        let is_param = self
            .p
            .node(self.node_query(decl.module()).get_root_decl(decl))
            .is_param_decl();
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
        self.check_non_null_type(ty, expr.id())
    }

    fn check_non_null_assertion(&mut self, n: &'cx ast::NonNullExpr<'cx>) -> &'cx ty::Ty<'cx> {
        // TODO: NonNullChain
        let ty = self.check_expr(n.expr);
        self.get_non_nullable_ty(ty)
    }

    fn check_non_null_type(&mut self, ty: &'cx ty::Ty<'cx>, node: ast::NodeID) -> &'cx ty::Ty<'cx> {
        self.check_non_null_ty_with_reporter(
            ty,
            node,
            Self::report_object_possibly_null_or_undefined_error,
        )
    }

    fn check_non_null_ty_with_reporter(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        node: ast::NodeID,
        report: impl FnOnce(&mut Self, ast::NodeID, TypeFacts),
    ) -> &'cx ty::Ty<'cx> {
        if self.config.strict_null_checks() && ty.flags.intersects(TypeFlags::UNKNOWN) {
            let error = errors::ObjectIsOfTypeUnknown {
                span: self.p.node(node).span(),
            };
            self.push_error(Box::new(error));
            return self.error_ty;
        }
        let facts = self.get_type_facts(ty, TypeFacts::IS_UNDEFINED_OR_NULL);
        if facts.intersects(TypeFacts::IS_UNDEFINED_OR_NULL) {
            report(self, node, facts);
            let t = self.get_non_nullable_ty(ty);
            return if t
                .flags
                .intersects(TypeFlags::NULLABLE.union(TypeFlags::NEVER))
            {
                self.error_ty
            } else {
                t
            };
        }
        ty
    }

    fn check_non_null_non_void_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        node: ast::NodeID,
    ) -> &'cx ty::Ty<'cx> {
        let non_null_ty = self.check_non_null_type(ty, node);
        // TODO: report `The value cannot used here`
        non_null_ty
    }

    fn report_object_possibly_null_or_undefined_error(
        &mut self,
        node: ast::NodeID,
        facts: TypeFacts,
    ) {
        let n = self.p.node(node);
        if n.is_null_lit() {
            let error = errors::TheValueCannotBeUsedHere {
                span: n.span(),
                value: "null".to_string(),
            };
            self.push_error(Box::new(error));
            return;
        } else if n
            .as_ident()
            .is_some_and(|ident| ident.name == keyword::KW_UNDEFINED)
        {
            let error = errors::TheValueCannotBeUsedHere {
                span: n.span(),
                value: "undefined".to_string(),
            };
            self.push_error(Box::new(error));
            return;
        }

        let name = match n {
            ast::Node::Ident(ident) => self.atoms.get(ident.name).to_string(),
            _ => {
                // TODO:
                return;
            }
        };
        let kind = if facts.contains(TypeFacts::IS_UNDEFINED.union(TypeFacts::IS_NULL)) {
            errors::UndefinedOrNull::Both
        } else if facts.intersects(TypeFacts::IS_NULL) {
            errors::UndefinedOrNull::Null
        } else {
            errors::UndefinedOrNull::Undefined
        };
        let error = errors::XIsPossiblyNullOrUndefined {
            span: n.span(),
            name,
            kind,
        };
        self.push_error(Box::new(error));
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
        } else if self.is_type_any(left_ty) || self.is_type_any(right_ty) {
            Some(self.any_ty)
        } else {
            None
        }
    }

    fn check_for_disallowed_es_symbol_operation(
        &mut self,
        left: &'cx ast::Expr,
        left_ty: &'cx ty::Ty<'cx>,
        right: &'cx ast::Expr,
        right_ty: &'cx ty::Ty<'cx>,
        op: BinOp,
    ) -> bool {
        if let Some(offending_symbol_op) = if self
            .maybe_type_of_kind_considering_base_constraint(left_ty, TypeFlags::ES_SYMBOL_LIKE)
        {
            Some(left)
        } else if self
            .maybe_type_of_kind_considering_base_constraint(right_ty, TypeFlags::ES_SYMBOL_LIKE)
        {
            Some(right)
        } else {
            None
        } {
            // TODO: error
            false
        } else {
            true
        }
    }

    fn maybe_type_of_kind_considering_base_constraint(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        kind: TypeFlags,
    ) -> bool {
        if ty.maybe_type_of_kind(kind) {
            true
        } else {
            let base_constraint = self.get_base_constraint_or_ty(ty);
            base_constraint.maybe_type_of_kind(kind)
        }
    }

    fn report_op_error_unless(
        &mut self,
        left_ty: &'cx ty::Ty<'cx>,
        right_ty: &'cx ty::Ty<'cx>,
        error_span: Span,
        op: BinOp,
        f: impl Fn(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>) -> bool + Copy,
    ) -> bool {
        if !f(self, left_ty, right_ty) {
            self.report_op_error(left_ty, right_ty, error_span, op, Some(f));
            true
        } else {
            false
        }
    }

    fn report_op_error(
        &mut self,
        left_ty: &'cx ty::Ty<'cx>,
        right_ty: &'cx ty::Ty<'cx>,
        error_span: Span,
        op: BinOp,
        f: Option<impl Fn(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>) -> bool + Copy>,
    ) {
        let would_work_with_await = false;

        let mut effective_left_ty = left_ty;
        let mut effective_right_ty = right_ty;
        if !would_work_with_await && let Some(f) = f {
            let get_base_tys_if_unrelated = |this: &mut Self| {
                let left_base = this.get_base_ty_of_literal_ty(left_ty);
                let right_base = this.get_base_ty_of_literal_ty(right_ty);
                if !f(this, left_base, right_base) {
                    Some((left_base, right_base))
                } else {
                    None
                }
            };
            if let Some((l, r)) = get_base_tys_if_unrelated(self) {
                effective_left_ty = l;
                effective_right_ty = r;
            }
        }
        let error = errors::OperatorCannotBeAppliedToTypesXAndY {
            span: error_span,
            op: op.kind.to_string(),
            ty1: effective_left_ty.to_string(self),
            ty2: effective_right_ty.to_string(self),
        };
        self.push_error(Box::new(error));
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
                (match self.check_binary_like_expr_for_add(left_ty, right_ty) {
                    Some(ty) => ty,
                    None => {
                        let error = errors::OperatorCannotBeAppliedToTypesXAndY {
                            op: op.kind.to_string(),
                            ty1: left_ty.to_string(self),
                            ty2: right_ty.to_string(self),
                            span: node.span,
                        };
                        self.push_error(Box::new(error));
                        self.any_ty
                    }
                }) as _
            }
            Sub | Mul | Div | Mod => {
                if left_ty == self.silent_never_ty || right_ty == self.silent_never_ty {
                    return self.silent_never_ty;
                }
                let left_ty = self.check_non_null_type(left_ty, left.id());
                let right_ty = self.check_non_null_type(left_ty, left.id());
                if left_ty.flags.intersects(TypeFlags::BOOLEAN_LIKE)
                    && right_ty.flags.intersects(TypeFlags::BOOLEAN_LIKE)
                    && let Some(suggest) = get_suggestion_boolean_op(op.kind.as_str())
                {
                    let error = errors::TheOp1IsNotAllowedForBooleanTypesConsiderUsingOp2Instead {
                        span: op.span,
                        op1: op.kind.to_string(),
                        op2: suggest.to_string(),
                    };
                    self.push_error(Box::new(error));
                    self.number_ty
                } else {
                    let left_ok = self.check_arithmetic_op_ty(left_ty, true, |this| {
                        let error = errors::TheSideOfAnArithmeticOperationMustBeOfTypeAnyNumberBigintOrAnEnumType {
                            span: left.span(),
                            left_or_right: errors::LeftOrRight::Left
                        };
                        this.push_error(Box::new(error));
                    });
                    let right_ok = self.check_arithmetic_op_ty(right_ty, true, |this| {
                        let error = errors::TheSideOfAnArithmeticOperationMustBeOfTypeAnyNumberBigintOrAnEnumType {
                            span: right.span(),
                            left_or_right: errors::LeftOrRight::Right
                        };
                        this.push_error(Box::new(error));
                    });
                    self.number_ty
                }
            }
            BitOr => {
                let left = self.check_non_null_type(left_ty, left.id());
                let right = self.check_non_null_type(right_ty, right.id());
                self.number_ty
            }
            LogicalAnd => {
                if self.has_type_facts(left_ty, TypeFacts::TRUTHY) {
                    left_ty
                } else {
                    right_ty
                }
            }
            LogicalOr => {
                if self.has_type_facts(left_ty, TypeFacts::FALSY) {
                    let left_ty = self.remove_definitely_falsy_tys(left_ty);
                    let left_ty = self.get_non_nullable_ty(left_ty);
                    let tys = &[left_ty, right_ty];
                    self.get_union_ty(tys, ty::UnionReduction::Subtype, false, None, None)
                } else {
                    left_ty
                }
            }
            EqEq => self.boolean_ty(),
            EqEqEq => self.boolean_ty(),
            Less | LessEq | Great | GreatEq => {
                if self.check_for_disallowed_es_symbol_operation(left, left_ty, right, right_ty, op)
                {
                    let left_ty = {
                        let t = self.check_non_null_type(left_ty, left.id());
                        self.get_base_ty_of_literal_ty_for_comparison(t)
                    };
                    let right_ty = {
                        let t = self.check_non_null_type(right_ty, right.id());
                        self.get_base_ty_of_literal_ty_for_comparison(t)
                    };
                    self.report_op_error_unless(left_ty, right_ty, op.span, op, |this, l, r| {
                        if this.is_type_any(l) || this.is_type_any(r) {
                            true
                        } else {
                            let left_assignable_to_number =
                                this.is_type_assignable_to(l, this.number_or_bigint_ty());
                            let right_assignable_to_number =
                                this.is_type_assignable_to(l, this.number_or_bigint_ty());
                            left_assignable_to_number && right_assignable_to_number
                                || !left_assignable_to_number && !right_assignable_to_number && {
                                    this.is_type_related_to(
                                        l,
                                        r,
                                        relation::RelationKind::Comparable,
                                    ) || this.is_type_related_to(
                                        r,
                                        l,
                                        relation::RelationKind::Comparable,
                                    )
                                }
                        }
                    });
                }
                self.boolean_ty()
            }
            Shl => self.number_ty,
            Sar => self.number_ty,
            Shr => self.number_ty,
            BitAnd => self.number_ty,
            Instanceof => self.check_instanceof_expr(left, left_ty, right, right_ty),
            In => self.check_in_expr(left, left_ty, right, right_ty),
            Satisfies => todo!(),
            NEq => self.boolean_ty(),
            NEqEq => self.boolean_ty(),
            Comma => right_ty,
            BitXor => self.number_ty,
            Exp => self.number_ty,
        }
    }

    fn check_instanceof_expr(
        &mut self,
        left: &'cx ast::Expr,
        left_ty: &'cx ty::Ty<'cx>,
        right: &'cx ast::Expr,
        right_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if left_ty == self.silent_never_ty || right_ty == self.silent_never_ty {
            return self.silent_never_ty;
        }
        if !self.is_type_any(left_ty)
            && self.all_types_assignable_to_kind(left_ty, ty::TypeFlags::PRIMITIVE, false)
        {
            let error = errors::TheLeftHandSideOfAnInstanceofExpressionMustBeOfTypeAnyAnObjectTypeOrATypeParameter {
                span: left.span(),
            };
            self.push_error(Box::new(error));
        }

        self.boolean_ty()
    }

    fn check_in_expr(
        &mut self,
        left: &'cx ast::Expr,
        left_ty: &'cx ty::Ty<'cx>,
        right: &'cx ast::Expr,
        right_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        self.check_type_assignable_to(left_ty, self.string_number_symbol_ty(), Some(left.id()));
        if !self.check_type_assignable_to(right_ty, self.non_primitive_ty, Some(right.id())) {
            let right_ty = self.get_widened_literal_ty(right_ty);
            let error = bolt_ts_ecma_rules::TheRightValueOfTheInOperatorMustBeAnObjectButGotTy {
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
        let mut seen_default = false;
        for (i, ty_param) in ty_params.iter().enumerate() {
            self.check_ty_param(ty_param);

            if ty_param.default.is_some() {
                seen_default = true;
                // TODO: create_ty_from_ty_reference
            } else if seen_default {
                // TODO: Required_type_parameters_may_not_follow_optional_type_parameters
            }

            for b_ty_param in ty_params.iter().take(i) {
                if self.get_symbol_of_decl(b_ty_param.id) == self.get_symbol_of_decl(ty_param.id) {
                    self.push_error(Box::new(errors::DuplicateIdentifier {
                        span: ty_param.name.span,
                        ident: pprint_ident(ty_param.name, &self.atoms),
                    }));
                }
            }
        }
    }

    fn fn_has_implicit_return(&mut self, f: ast::NodeID) -> bool {
        debug_assert!(
            self.p.node(f).is_fn_like(),
            "fn_has_implicit_return: {:#?}",
            self.p.node(f)
        );
        let n = self.get_flow_in_node_of_node(f);
        use bolt_ts_binder::FlowInNode::*;
        match n {
            Noop => false,
            FnLike(f) => f
                .end_flow_node
                .is_some_and(|n| self.is_reachable_flow_node(n)),
        }
    }

    pub(super) fn check_and_aggregate_ret_expr_tys(
        &mut self,
        f: ast::NodeID,
        body: &'cx ast::BlockStmt<'cx>,
    ) -> Option<Vec<&'cx ty::Ty<'cx>>> {
        let mut has_ret_with_no_expr = self.fn_has_implicit_return(f);
        let mut has_ret_of_ty_never = false;

        fn t<'cx, T: Copy + Debug>(
            id: ast::NodeID,
            checker: &mut TyChecker<'cx>,
            f: impl Fn(&mut TyChecker<'cx>, &'cx ast::RetStmt<'cx>) -> T + Copy,
            v: &mut Vec<T>,
            has_ret_with_no_expr: &mut bool,
            has_ret_of_ty_never: &mut bool,
        ) {
            let node = checker.p.node(id);
            match node {
                ast::Node::RetStmt(n) => {
                    if let Some(ret_expr) = n.expr {
                        let expr = ast::Expr::skip_parens(ret_expr);
                        // TODO: async function and await call;
                        // TODO: const reference
                    } else {
                        *has_ret_with_no_expr = true;
                    }
                    let ty = f(checker, n);
                    v.push(ty)
                }
                ast::Node::BlockStmt(n) => {
                    for stmt in n.stmts {
                        t(
                            stmt.id(),
                            checker,
                            f,
                            v,
                            has_ret_with_no_expr,
                            has_ret_of_ty_never,
                        );
                    }
                }
                ast::Node::IfStmt(n) => {
                    t(
                        n.then.id(),
                        checker,
                        f,
                        v,
                        has_ret_with_no_expr,
                        has_ret_of_ty_never,
                    );
                    if let Some(else_then) = n.else_then {
                        t(
                            else_then.id(),
                            checker,
                            f,
                            v,
                            has_ret_with_no_expr,
                            has_ret_of_ty_never,
                        );
                    }
                }
                ast::Node::SwitchStmt(n) => {
                    for case in n.case_block.clauses {
                        let stmts = match case {
                            ast::CaseOrDefaultClause::Case(clause) => clause.stmts,
                            ast::CaseOrDefaultClause::Default(clause) => clause.stmts,
                        };
                        for stmt in stmts {
                            t(
                                stmt.id(),
                                checker,
                                f,
                                v,
                                has_ret_with_no_expr,
                                has_ret_of_ty_never,
                            );
                        }
                    }
                }
                _ => {}
            }
        }

        let mut aggregated_tys = Vec::with_capacity(8);
        t(
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
                let ty = this.check_expr_cached(expr);
                this.check_mode = old;
                ty
            },
            &mut aggregated_tys,
            &mut has_ret_with_no_expr,
            &mut has_ret_of_ty_never,
        );

        let may_return_never = || {
            let func = self.p.node(f);
            func.is_fn_expr() || func.is_arrow_fn_expr() || func.is_object_method_member()
        };

        if aggregated_tys.is_empty()
            && !has_ret_with_no_expr
            && (has_ret_of_ty_never || may_return_never())
        {
            return None;
        }
        if self.config.strict_null_checks() && !aggregated_tys.is_empty() && has_ret_with_no_expr {
            // TODO: !(isJSConstructor(func) && aggregatedTypes.some(t => t.symbol === func.symbol))
            aggregated_tys.push(self.undefined_ty);
        }
        Some(aggregated_tys)
    }

    fn is_array_or_tuple(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.kind.is_array(self) || ty.is_tuple()
    }

    fn is_array_or_tuple_intersection(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.kind
            .as_intersection()
            .map(|i| i.tys.iter().all(|t| self.is_array_or_tuple(t)))
            .unwrap_or_default()
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
            let flags = self.symbol(symbol).flags;
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

    pub(crate) fn is_empty_anonymous_object_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.kind.as_object_anonymous().is_some_and(|_| {
            if let Some(symbol) = ty.symbol() {
                let s = self.symbol(symbol);
                s.flags.intersects(SymbolFlags::TYPE_LITERAL)
                    && self.get_members_of_symbol(symbol).0.is_empty()
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

    fn is_type_any(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.flags.intersects(TypeFlags::ANY)
    }

    fn recombine_unknown_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty == self.unknown_union_ty() {
            self.unknown_ty
        } else {
            ty
        }
    }

    fn get_ty_facts(&mut self, ty: &'cx ty::Ty<'cx>, mask: TypeFacts) -> TypeFacts {
        let facts = self.get_type_facts_worker(ty, mask);
        facts & mask
    }

    fn remove_nullable_by_intersection(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        target_facts: TypeFacts,
        other_facts: TypeFacts,
        other_includes_facts: TypeFacts,
        other_ty: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let facts = self.get_ty_facts(
            ty,
            TypeFacts::EQ_UNDEFINED
                .union(TypeFacts::EQ_NULL)
                .union(TypeFacts::IS_UNDEFINED.union(TypeFacts::IS_NULL)),
        );
        if !facts.intersects(target_facts) {
            return ty;
        };
        self.map_ty(
            ty,
            |this, t| {
                Some(if this.has_type_facts(t, target_facts) {
                    let b = if !facts.intersects(other_includes_facts)
                        && this.has_type_facts(t, other_facts)
                    {
                        let empty_and_other_union = this.get_union_ty(
                            &[this.empty_object_ty(), other_ty],
                            ty::UnionReduction::Lit,
                            false,
                            None,
                            None,
                        );
                        empty_and_other_union
                    } else {
                        this.empty_object_ty()
                    };
                    this.get_intersection_ty(&[t, b], IntersectionFlags::None, None, None)
                } else {
                    t
                })
            },
            false,
        )
        .unwrap()
    }

    fn get_adjusted_ty_with_facts(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        facts: TypeFacts,
    ) -> &'cx ty::Ty<'cx> {
        let strict_null_checks = self.config.strict_null_checks();
        let ty = self.get_ty_with_facts(
            if strict_null_checks && ty.flags.contains(TypeFlags::UNKNOWN) {
                debug_assert!(self.unknown_union_ty().kind.is_union());
                self.unknown_union_ty()
            } else {
                ty
            },
            facts,
        );
        let reduced = self.recombine_unknown_ty(ty);
        if strict_null_checks {
            if facts == ty::TypeFacts::NE_UNDEFINED {
                return self.remove_nullable_by_intersection(
                    reduced,
                    TypeFacts::EQ_UNDEFINED,
                    TypeFacts::EQ_NULL,
                    TypeFacts::IS_NULL,
                    self.null_ty,
                );
            } else if facts == ty::TypeFacts::NE_NULL {
                return self.remove_nullable_by_intersection(
                    reduced,
                    TypeFacts::EQ_NULL,
                    TypeFacts::EQ_UNDEFINED,
                    TypeFacts::IS_UNDEFINED,
                    self.undefined_ty,
                );
            } else if facts == ty::TypeFacts::NE_UNDEFINED_OR_NULL || facts == ty::TypeFacts::TRUTHY
            {
                return self
                    .map_ty(
                        reduced,
                        |this, t| {
                            if this.has_type_facts(t, TypeFacts::EQ_UNDEFINED_OR_NULL) {
                                Some(this.get_global_non_nullable_ty_instantiation(t))
                            } else {
                                Some(t)
                            }
                        },
                        false,
                    )
                    .unwrap();
            }
        }

        reduced
    }

    fn get_non_nullable_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if self.config.strict_null_checks() {
            self.get_adjusted_ty_with_facts(ty, ty::TypeFacts::NE_UNDEFINED_OR_NULL)
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

    fn is_matching_reference(&mut self, source: ast::NodeID, target: ast::NodeID) -> bool {
        let t = self.p.node(target);
        match t {
            ParenExpr(t) => return self.is_matching_reference(source, t.expr.id()),
            NonNullExpr(t) => return self.is_matching_reference(source, t.expr.id()),
            _ => (),
        }
        let s = self.p.node(source);
        use bolt_ts_ast::Node::*;

        match s {
            Ident(s_ident) => {
                if self
                    .node_query(source.module())
                    .is_this_in_type_query(source)
                {
                    t.is_this_expr()
                } else if let Some(t_ident) = t.as_ident() {
                    self.resolve_symbol_by_ident(s_ident) == self.resolve_symbol_by_ident(t_ident)
                } else if let Some(t_v) = t.as_var_decl() {
                    match t_v.name.kind {
                        bolt_ts_ast::BindingKind::Ident(_) => {
                            self.resolve_symbol_by_ident(s_ident) == self.get_symbol_of_decl(t_v.id)
                        }
                        bolt_ts_ast::BindingKind::ObjectPat(_)
                        | bolt_ts_ast::BindingKind::ArrayPat(_) => {
                            unreachable!()
                        }
                    }
                } else if let Some(t) = t.as_object_binding_elem() {
                    let s = self.resolve_symbol_by_ident(s_ident);
                    self.get_export_symbol_of_value_symbol_if_exported(s)
                        == self.get_symbol_of_decl(t.id)
                } else if let Some(t) = t.as_array_binding() {
                    let s = self.resolve_symbol_by_ident(s_ident);
                    self.get_export_symbol_of_value_symbol_if_exported(s)
                        == self.get_symbol_of_decl(t.id)
                } else {
                    false
                }
            }
            ThisExpr(_) => t.is_this_expr(),
            _ => false,
        }
    }

    fn is_or_contain_matching_refer(&mut self, source: ast::NodeID, target: ast::NodeID) -> bool {
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

        if let ast::ExprKind::PropAccess(p) = expr.kind
            && self.is_or_contain_matching_refer(refer, p.expr.id())
        {
            return true;
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
                .intersects(TypeFlags::OBJECT.union(TypeFlags::NON_PRIMITIVE))
        } else if target == self.global_object_ty() {
            source
                .flags
                .intersects(TypeFlags::OBJECT.union(TypeFlags::NON_PRIMITIVE))
                && !self.is_empty_anonymous_object_ty(source)
        } else if target == self.global_fn_ty() {
            source.flags.intersects(TypeFlags::OBJECT) && self.is_fn_object_ty(source)
        } else {
            self.has_base_ty(
                source,
                if let Some(r) = target.kind.as_object_reference() {
                    if r.target.kind.is_object_interface() {
                        target
                    } else {
                        r.target
                    }
                } else {
                    target
                },
            ) || (target.kind.is_array(self)
                && !target.is_readonly_array(self)
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
            let atom = self.atoms.atom(i.to_string().as_str());
            self.get_string_literal_type_from_string(atom)
        }));
        let t = if ty.readonly {
            self.global_readonly_array_ty()
        } else {
            self.global_array_ty()
        };
        let t = self.get_index_ty(t, IndexFlags::empty());
        v.push(t);
        self.get_union_ty(&v, ty::UnionReduction::Lit, false, None, None)
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
            ty
        } else if ty.kind.is_union() {
            self.map_ty(ty, |this, t| Some(this.get_lower_bound_of_key_ty(t)), true)
                .unwrap()
        } else if let Some(i) = ty.kind.as_intersection() {
            let tys = i.tys;
            if tys.len() == 2
                && tys[0]
                    .flags
                    .intersects(TypeFlags::STRING | TypeFlags::NUMBER | TypeFlags::BIG_INT)
                && tys[1] == self.empty_ty_literal_ty()
            {
                ty
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
                    false,
                    None,
                    None,
                )
            })
        } else {
            None
        }
    }

    fn is_empty_literal_ty(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        if self.config.strict_null_checks() {
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
                    |_, _| unreachable!(),
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
            assigned_ty
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
            |_, t| t,
            None,
            None,
            None,
        )
        .unwrap()
    }

    fn get_nullable_ty(&mut self, ty: &'cx ty::Ty<'cx>, flags: TypeFlags) -> &'cx ty::Ty<'cx> {
        let missing = (flags & !ty.flags) & TypeFlags::UNDEFINED.union(TypeFlags::NULL);
        if missing.is_empty() {
            ty
        } else if missing == TypeFlags::UNDEFINED {
            self.get_union_ty(
                &[ty, self.undefined_ty],
                ty::UnionReduction::Lit,
                false,
                None,
                None,
            )
        } else if missing == TypeFlags::NULL {
            self.get_union_ty(
                &[ty, self.null_ty],
                ty::UnionReduction::Lit,
                false,
                None,
                None,
            )
        } else {
            self.get_union_ty(
                &[ty, self.undefined_ty, self.null_ty],
                ty::UnionReduction::Lit,
                false,
                None,
                None,
            )
        }
    }

    fn get_common_super_ty(&mut self, tys: &'cx [&'cx ty::Ty<'cx>]) -> &'cx ty::Ty<'cx> {
        if tys.len() == 1 {
            return tys[0];
        }

        let primary_tys = if self.config.strict_null_checks() {
            self.same_map_tys(Some(tys), |this, t, _| {
                this.filter_type(t, |_, u| !u.flags.intersects(TypeFlags::NULLABLE))
            })
            .unwrap()
        } else {
            tys
        };

        let literal_tys_with_same_base_ty =
            |this: &mut TyChecker<'cx>, tys: &[&'cx ty::Ty<'cx>]| {
                let mut common_base_ty = None;
                for t in tys {
                    if !t.flags.intersects(TypeFlags::NEVER) {
                        let base_ty = this.get_base_ty_of_literal_ty(t);
                        if common_base_ty.is_none() {
                            common_base_ty = Some(base_ty);
                        };
                        if common_base_ty == Some(t) || base_ty != common_base_ty.unwrap() {
                            return false;
                        }
                    }
                }
                true
            };

        let super_ty_or_union = if literal_tys_with_same_base_ty(self, primary_tys) {
            self.get_union_ty(primary_tys, ty::UnionReduction::Lit, false, None, None)
        } else {
            let candidate = self
                .reduced_left(
                    primary_tys,
                    |this, s, t, _| {
                        if this.is_type_related_to(s, t, relation::RelationKind::StrictSubtype) {
                            t
                        } else {
                            s
                        }
                    },
                    |_, t| t,
                    None,
                    None,
                    None,
                )
                .unwrap();
            if primary_tys.iter().all(|&t| {
                t == candidate
                    || self.is_type_related_to(t, candidate, relation::RelationKind::StrictSubtype)
            }) {
                candidate
            } else {
                self.reduced_left(
                    primary_tys,
                    |this, s, t, _| {
                        if this.is_type_related_to(s, t, relation::RelationKind::Subtype) {
                            t
                        } else {
                            s
                        }
                    },
                    |_, t| t,
                    None,
                    None,
                    None,
                )
                .unwrap()
            }
        };

        if primary_tys == tys {
            super_ty_or_union
        } else {
            let flags = self
                .get_combined_ty_flags(tys)
                .intersection(TypeFlags::NULLABLE);
            self.get_nullable_ty(super_ty_or_union, flags)
        }
    }

    fn get_combined_ty_flags(&mut self, tys: &[&'cx ty::Ty<'cx>]) -> TypeFlags {
        self.reduced_left(
            tys,
            |this, flags, t, _| {
                flags
                    | (if let Some(u) = t.kind.as_union() {
                        this.get_combined_ty_flags(u.tys)
                    } else {
                        t.flags
                    })
            },
            |_, _| unreachable!(),
            Some(TypeFlags::empty()),
            None,
            None,
        )
        .unwrap()
    }

    fn reduced_left<T: Copy, U>(
        &mut self,
        array: &[T],
        f: impl Fn(&mut Self, U, T, usize) -> U,
        t_into_u: impl Fn(&mut Self, &T) -> U,
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
                    result = t_into_u(self, &array[pos]);
                    pos += 1;
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
        if !self.get_check_flags(symbol).intersects(CheckFlags::MAPPED) {
            false
        } else {
            assert_eq!(symbol.module(), bolt_ts_span::ModuleID::TRANSIENT);
            self.transient_symbol_links[symbol.index_as_usize()]
                .get_ty()
                .is_none()
                && self
                    .find_resolution_cycle_start_index(ResolutionKey::Type(symbol))
                    .is_some()
        }
    }

    pub fn decl_modifier_flags_from_symbol(
        &self,
        symbol: SymbolID,
    ) -> enumflags2::BitFlags<ast::ModifierKind> {
        if let Some(decl) = self.symbol(symbol).value_decl {
            let flags = self
                .node_query(decl.module())
                .get_combined_modifier_flags(decl);

            return if self
                .symbol(symbol)
                .parent
                .is_some_and(|p| self.binder.symbol(p).flags.contains(SymbolFlags::CLASS))
            {
                flags
            } else {
                flags & !ast::ModifierKind::ACCESSIBILITY
            };
        }

        if self.symbol(symbol).flags.intersects(SymbolFlags::PROPERTY) {
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
                    && self
                        .checker
                        .node_query(n.id.module())
                        .is_part_of_ty_node(n.id)
                    && self
                        .checker
                        .node_query(n.id.module())
                        .maybe_ty_param_reference(n.id)
                    && self.checker.get_ty_from_ident(n) == self.tp
                {
                    self.contain_reference = true;
                }
            }
        }

        let tp = ty.kind.expect_param();
        let decl = {
            let Some(decls) = &self.symbol(tp.symbol).decls else {
                return true;
            };
            if decls.len() != 1 {
                return true;
            }
            decls[0]
        };
        let container = self.parent(decl);
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
                let Some(next) = self.parent(n) else {
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
        self.filter_type(ty, |this, t| this.has_type_facts(t, TypeFacts::TRUTHY))
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
                let val_ty = self.get_union_ty(&tys, ty::UnionReduction::Lit, false, None, None);
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
        let prop_flags = self.symbol(prop).flags;
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
            let flags = SymbolFlags::PROPERTY.union(SymbolFlags::TRANSIENT)
                | prop_flags.intersection(SymbolFlags::OPTIONAL);
            let name = self.symbol(prop).name;
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
            let decls = self.symbol(prop).decls.clone();
            self.create_transient_symbol(name, flags, links, decls, None)
        }
    }

    fn check_decl_init(
        &mut self,
        decl: &impl VarLike<'cx>,
        contextual_ty: Option<&'cx ty::Ty<'cx>>,
    ) -> &'cx ty::Ty<'cx> {
        let init = decl.init().unwrap();
        // TODO: get_quick_ty_of_expr

        if let Some(contextual_ty) = contextual_ty {
            let check_mode = self.check_mode.unwrap_or(CheckMode::empty());
            self.check_expr_with_contextual_ty(init, contextual_ty, None, check_mode)
        } else {
            self.check_expr_cached(init)
        }
    }

    pub fn check_external_module_exports(&mut self, node: &'cx ast::Program<'cx>) {
        let module_symbol = self.get_symbol_of_decl(node.id);
        if self
            .get_symbol_links(module_symbol)
            .get_exports_checked()
            .is_some_and(|checked| checked)
        {
            return;
        }
        let exports = self.get_exports_of_symbol(module_symbol);
        let redeclared_exports = exports
            .0
            .iter()
            .filter_map(|(id, symbol)| {
                if matches!(id, SymbolName::ExportStar) {
                    return None;
                }
                let s = self.symbol(*symbol);
                if s.flags
                    .intersects(SymbolFlags::ALIAS.union(SymbolFlags::ENUM))
                {
                    return None;
                }
                let Some(decls) = s.decls.as_ref() else {
                    return None;
                };
                let exported_declarations_count = decls
                    .iter()
                    .filter(|decl| {
                        let d = self.p.node(**decl);
                        d.is_not_overload() && d.is_not_accessor() && !d.is_interface_decl()
                    })
                    .count();
                if exported_declarations_count <= 1 {
                    return None;
                }
                // TODO: !is_duplicated_common_js_expxort
                Some(
                    decls
                        .iter()
                        .filter_map(|decl| {
                            let d = self.p.node(*decl);
                            if d.is_not_overload() {
                                Some(*decl)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>(),
                )
            })
            .flatten()
            .collect::<Vec<_>>();
        for decl in redeclared_exports {
            let decl = self.p.node(decl);
            let span = if let Some(name) = decl.ident_name() {
                name.span
            } else {
                decl.span()
            };
            let error = errors::CannotRedeclareExportedVariableX {
                span,
                name: "default".to_string(),
            };
            self.push_error(Box::new(error));
        }

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
        self.filter_type(ty, |this, t| this.has_type_facts(t, include))
    }

    fn remove_missing_or_undefined_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if self.config.exact_optional_property_types() {
            self.filter_type(ty, |this, t| t != this.missing_ty)
        } else {
            self.get_ty_with_facts(ty, TypeFacts::NE_UNDEFINED)
        }
    }

    fn has_skip_direct_inference_flags(&self, node: &ast::NodeID) -> bool {
        self.node_links
            .get(node)
            .is_some_and(|links| links.get_skip_direct_inference().unwrap_or_default())
    }

    fn enum_number_index_info(&self) -> &'cx ty::IndexInfo<'cx> {
        self.enum_number_index_info.get().unwrap()
    }

    fn is_from_inference_block_source(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.symbol()
            .and_then(|s| self.symbol(s).decls.as_ref())
            .is_some_and(|decls| {
                decls
                    .iter()
                    .any(|decl| self.has_skip_direct_inference_flags(decl))
            })
    }

    fn infer_ty_for_homomorphic_map_ty(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        constraint_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        // TODO: cache
        self.create_reverse_mapped_ty(source, target, constraint_ty)
    }

    fn is_const_enum_object_ty(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.get_object_flags().contains(ObjectFlags::ANONYMOUS)
            && ty
                .symbol()
                .is_some_and(|symbol| self.symbol(symbol).flags.contains(SymbolFlags::CONST_ENUM))
    }

    #[inline(always)]
    pub fn ty(&self, id: TyID) -> &'cx ty::Ty<'cx> {
        debug_assert!(id.as_usize() < self.tys.len());
        unsafe { self.tys.get_unchecked(id.as_usize()) }
    }

    #[inline(always)]
    pub fn ty_len(&self) -> usize {
        self.tys.len()
    }

    fn get_constructor_for_ty_args(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
        loc: ast::NodeID,
    ) -> impl Iterator<Item = &'cx ty::Sig<'cx>> {
        let ty_arg_count = ty_args.map_or(0, |t| t.list.len());
        let sigs = self.get_signatures_of_type(ty, ty::SigKind::Constructor);
        // TODO: is_javascript
        sigs.iter().filter_map(move |sig| {
            let min = self.get_min_ty_arg_count(sig.ty_params);
            if ty_arg_count >= min
                && sig
                    .ty_params
                    .is_none_or(|ty_params| ty_arg_count <= ty_params.len())
            {
                Some(*sig)
            } else {
                None
            }
        })
    }

    fn get_instantiated_constructors_for_ty_args(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        ty_args: Option<&'cx ast::Tys<'cx>>,
        loc: ast::NodeID,
    ) -> ty::Sigs<'cx> {
        let sigs = self
            .get_constructor_for_ty_args(ty, ty_args, loc)
            .collect::<Vec<_>>();
        let ty_args = ty_args.map(|t| {
            let ty_args = t
                .list
                .iter()
                .map(|ty| self.get_ty_from_type_node(ty))
                .collect::<Vec<_>>();
            self.alloc(ty_args) as ty::Tys<'cx>
        });
        let sigs = sigs
            .iter()
            .map(|sig| {
                if sig.ty_params.is_some() {
                    self.get_sig_instantiation(sig, ty_args, false, None)
                } else {
                    sig
                }
            })
            .collect::<Vec<_>>();
        self.alloc(sigs)
    }

    fn get_target_symbol(&mut self, symbol: SymbolID) -> SymbolID {
        if self
            .get_check_flags(symbol)
            .contains(CheckFlags::INSTANTIATED)
        {
            self.transient_symbol_links[symbol.index_as_usize()].expect_target()
        } else {
            symbol
        }
    }

    fn apply_to_param_tys(
        &mut self,
        source: &'cx ty::Sig<'cx>,
        target: &'cx ty::Sig<'cx>,
        callback: impl Fn(&mut Self, &'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>),
    ) {
        let source_count = source.get_param_count(self);
        let target_count = target.get_param_count(self);
        let source_rest_ty = source.get_rest_ty(self);
        let target_rest_ty = target.get_rest_ty(self);
        let target_non_rest_count = if target_rest_ty.is_some() {
            target_count - 1
        } else {
            target_count
        };
        let param_count = if source_rest_ty.is_some() {
            target_count
        } else {
            usize::min(source_count, target_non_rest_count)
        };
        // TODO: `source_this_ty`
        for i in 0..param_count {
            let source_ty = self.get_ty_at_pos(source, i);
            let target_ty = self.get_ty_at_pos(target, i);
            callback(self, source_ty, target_ty);
        }
        if let Some(target_rest_ty) = target_rest_ty {
            let readonly = false;
            let rest_ty = self.get_rest_ty_at_pos(source, param_count, readonly);
            callback(self, rest_ty, target_rest_ty);
        }
    }

    fn get_ty_without_sig(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if ty.kind.is_object() {
            self.resolve_structured_type_members(ty);
            let resolved = self.get_ty_links(ty.id).expect_structured_members();
            if !resolved.ctor_sigs.is_empty() || !resolved.call_sigs.is_empty() {
                let a = self.create_anonymous_ty(ty.symbol(), ObjectFlags::empty(), None);
                let s = self.alloc(ty::StructuredMembers {
                    members: resolved.members,
                    call_sigs: self.empty_array(),
                    ctor_sigs: self.empty_array(),
                    index_infos: self.empty_array(),
                    props: resolved.props,
                });
                let prev = self
                    .ty_links
                    .insert(a.id, TyLinks::default().with_structured_members(s));
                debug_assert!(prev.is_none());
                return a;
            }
        } else if let Some(i) = ty.kind.as_intersection() {
            let tys = i
                .tys
                .iter()
                .map(|ty| self.get_ty_without_sig(ty))
                .collect::<Vec<_>>();
            let tys = self.alloc(tys);
            return self.get_intersection_ty(tys, IntersectionFlags::None, None, None);
        }
        ty
    }

    fn is_const_mapped_ty(&mut self, ty: &'cx ty::Ty<'cx>, depth: u8) -> bool {
        debug_assert!(ty.kind.is_object_mapped());
        self.get_homomorphic_ty_var(ty)
            .is_some_and(|ty_var| self._is_const_ty_variable(ty_var, depth))
    }

    fn _is_const_ty_variable(&mut self, ty: &'cx ty::Ty<'cx>, depth: u8) -> bool {
        if depth >= 5 {
            return false;
        }
        use ty::TyKind::*;
        match ty.kind {
            Param(t) => self.symbol(t.symbol).decls.as_ref().is_some_and(|decls| {
                decls.iter().any(|decl| {
                    self.p
                        .node(*decl)
                        .modifiers()
                        .is_some_and(|ms| ms.flags.contains(ast::ModifierKind::Const))
                })
            }),
            Union(ty::UnionTy { tys, .. }) | Intersection(ty::IntersectionTy { tys, .. }) => {
                tys.iter().any(|ty| self._is_const_ty_variable(ty, depth))
            }
            IndexedAccess(ty) => self._is_const_ty_variable(ty.object_ty, depth + 1),
            Cond(_) => self
                .get_constraint_of_cond_ty(ty)
                .is_some_and(|ty| self._is_const_ty_variable(ty, depth + 1)),
            Substitution(ty) => self._is_const_ty_variable(ty.base_ty, depth),
            Object(object_ty) => {
                if let ty::ObjectTyKind::Mapped(_) = object_ty.kind {
                    self.is_const_mapped_ty(ty, depth)
                } else if let Some(tup) = object_ty.kind.as_generic_tuple_type() {
                    let tys = self.get_element_tys(ty);
                    tys.iter().enumerate().any(|(i, t)| {
                        tup.element_flags[i].contains(ElementFlags::VARIADIC)
                            && self._is_const_ty_variable(t, depth)
                    })
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn is_const_ty_variable(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        self._is_const_ty_variable(ty, 0)
    }

    fn is_valid_const_assertion_argument(&self, node: ast::NodeID) -> bool {
        let n = self.p.node(node);
        use ast::Node::*;
        match n {
            StringLit(_)
            | NoSubstitutionTemplateLit(_)
            | NumLit(_)
            | BigIntLit(_)
            | BoolLit(_)
            | ArrayLit(_)
            | ObjectLit(_)
            | TemplateExpr(_) => true,
            ParenExpr(n) => self.is_valid_const_assertion_argument(n.expr.id()),
            PrefixUnaryExpr(n) => {
                use ast::ExprKind::*;
                use ast::PrefixUnaryOp::*;
                match n.op {
                    Minus if matches!(n.expr.kind, NumLit(_) | BigIntLit(_)) => true,
                    Plus if matches!(n.expr.kind, NumLit(_)) => true,
                    _ => false,
                }
            }
            PropAccessExpr(ast::PropAccessExpr { expr, .. })
            | EleAccessExpr(ast::EleAccessExpr { expr, .. }) => {
                let expr = ast::Expr::skip_parens(expr);
                let Some(symbol) = expr
                    .is_entity_name_expr()
                    .then(|| self.final_res(expr.id()))
                else {
                    return false;
                };
                self.symbol(symbol).flags.contains(SymbolFlags::ENUM)
            }
            _ => false,
        }
    }

    fn is_const_context(&mut self, node: ast::NodeID) -> bool {
        let Some(parent) = self.parent(node) else {
            return false;
        };
        let p = self.p.node(parent);
        if p.is_assertion_expr() {
            let ty = match p {
                ast::Node::AsExpr(n) => n.ty,
                _ => unreachable!(),
            };
            ty.is_const_ty_refer()
            // TODO: is_js_doc_type_assertion
        }
        // TODO: is_valid_const_assertion_argument
        // else if self.is_valid_const_assertion_argument(node) {
        //     let ty = self.get_contextual_ty(node, Some(ContextFlags::empty()));
        //     ty.is_some_and(|ty| self.is_const_ty_variable(ty))
        // }
        else if p.is_array_lit() || p.is_paren_expr() || p.is_spread_element() {
            self.is_const_context(parent)
        } else if p.is_object_prop_assignment()
            || p.is_object_shorthand_member()
            || p.is_template_span()
        {
            let parent_parent = self.parent(parent).unwrap();
            self.is_const_context(parent_parent)
        } else {
            false
        }
    }

    fn has_type_facts(&mut self, ty: &'cx ty::Ty<'cx>, mark: TypeFacts) -> bool {
        !self.get_type_facts(ty, mark).is_empty()
    }

    fn get_type_facts(&mut self, ty: &'cx ty::Ty<'cx>, mark: TypeFacts) -> TypeFacts {
        self.get_type_facts_worker(ty, mark) & mark
    }

    fn get_type_facts_worker(
        &mut self,
        mut ty: &'cx ty::Ty<'cx>,
        caller_only_needs: TypeFacts,
    ) -> TypeFacts {
        if ty
            .flags
            .intersects(TypeFlags::INTERSECTION.union(TypeFlags::INSTANTIABLE))
        {
            ty = self
                .get_base_constraint_of_ty(ty)
                .unwrap_or(self.unknown_ty);
        }

        let flags = ty.flags;
        let strict_null_checks = self.config.strict_null_checks();

        if flags.intersects(TypeFlags::STRING.union(TypeFlags::STRING_MAPPING)) {
            if strict_null_checks {
                TypeFacts::STRING_STRICT_FACTS
            } else {
                TypeFacts::STRING_FACTS
            }
        } else if flags.intersects(TypeFlags::STRING_LITERAL.union(TypeFlags::TEMPLATE_LITERAL)) {
            let is_empty = if flags.contains(TypeFlags::STRING_LITERAL)
                && let Some(lit) = ty.kind.as_string_lit()
            {
                lit.val == keyword::IDENT_EMPTY
            } else {
                false
            };

            if strict_null_checks {
                if is_empty {
                    TypeFacts::ZERO_NUMBER_STRICT_FACTS
                } else {
                    TypeFacts::NON_ZERO_NUMBER_STRICT_FACTS
                }
            } else if is_empty {
                TypeFacts::ZERO_NUMBER_FACTS
            } else {
                TypeFacts::NON_ZERO_NUMBER_FACTS
            }
        } else if flags.intersects(TypeFlags::NUMBER.union(TypeFlags::ENUM)) {
            if strict_null_checks {
                TypeFacts::NUMBER_STRICT_FACTS
            } else {
                TypeFacts::NUMBER_FACTS
            }
        } else if let ty::TyKind::NumberLit(lit) = ty.kind {
            let is_zero = lit.is(0.);
            if strict_null_checks {
                if is_zero {
                    TypeFacts::ZERO_NUMBER_STRICT_FACTS
                } else {
                    TypeFacts::NON_ZERO_NUMBER_STRICT_FACTS
                }
            } else if is_zero {
                TypeFacts::ZERO_NUMBER_FACTS
            } else {
                TypeFacts::NON_ZERO_NUMBER_FACTS
            }
        } else if flags.contains(TypeFlags::BIG_INT) {
            if strict_null_checks {
                TypeFacts::BIGINT_STRICT_FACTS
            } else {
                TypeFacts::BIGINT_FACTS
            }
        } else if flags.contains(TypeFlags::BIG_INT_LITERAL) {
            todo!()
        } else if flags.contains(TypeFlags::BOOLEAN) {
            if strict_null_checks {
                TypeFacts::BOOLEAN_STRICT_FACTS
            } else {
                TypeFacts::BOOLEAN_FACTS
            }
        } else if flags.intersects(TypeFlags::BOOLEAN_LIKE) {
            let is_false = ty == self.false_ty || ty == self.regular_false_ty;
            if strict_null_checks {
                if is_false {
                    TypeFacts::FALSE_STRICT_FACTS
                } else {
                    TypeFacts::TRUE_STRICT_FACTS
                }
            } else if is_false {
                TypeFacts::FALSE_FACTS
            } else {
                TypeFacts::TRUE_FACTS
            }
        } else if flags.contains(TypeFlags::OBJECT) {
            let possible_facts = if strict_null_checks {
                TypeFacts::EMPTY_OBJECT_STRICT_FACTS
                    .union(TypeFacts::FUNCTION_STRICT_FACTS)
                    .union(TypeFacts::OBJECT_STRICT_FACTS)
            } else {
                TypeFacts::EMPTY_OBJECT_FACTS
                    .union(TypeFacts::FUNCTION_FACTS)
                    .union(TypeFacts::OBJECT_FACTS)
            };
            if !caller_only_needs.intersects(possible_facts) {
                return TypeFacts::empty();
            }
            let object_flags = ty.get_object_flags();
            if object_flags.contains(ObjectFlags::ANONYMOUS) && self.is_empty_object_ty(ty) {
                if strict_null_checks {
                    TypeFacts::EMPTY_OBJECT_STRICT_FACTS
                } else {
                    TypeFacts::EMPTY_OBJECT_FACTS
                }
            } else if self.is_fn_object_ty(ty) {
                if strict_null_checks {
                    TypeFacts::FUNCTION_STRICT_FACTS
                } else {
                    TypeFacts::FUNCTION_FACTS
                }
            } else if strict_null_checks {
                TypeFacts::OBJECT_STRICT_FACTS
            } else {
                TypeFacts::OBJECT_FACTS
            }
        } else if flags.contains(TypeFlags::VOID) {
            TypeFacts::VOID_FACTS
        } else if flags.contains(TypeFlags::UNDEFINED) {
            TypeFacts::UNDEFINED_FACTS
        } else if flags.contains(TypeFlags::NULL) {
            TypeFacts::NULL_FACTS
        } else if flags.contains(TypeFlags::ES_SYMBOL_LIKE) {
            if strict_null_checks {
                TypeFacts::SYMBOL_STRICT_FACTS
            } else {
                TypeFacts::SYMBOL_FACTS
            }
        } else if flags.contains(TypeFlags::NON_PRIMITIVE) {
            if strict_null_checks {
                TypeFacts::OBJECT_STRICT_FACTS
            } else {
                TypeFacts::OBJECT_FACTS
            }
        } else if flags.contains(TypeFlags::NEVER) {
            TypeFacts::empty()
        } else if let Some(u) = ty.kind.as_union() {
            self.reduced_left(
                u.tys,
                |this, facts, t, _| facts.union(this.get_type_facts_worker(t, caller_only_needs)),
                |_, _| unreachable!(),
                Some(TypeFacts::empty()),
                None,
                None,
            )
            .unwrap()
        } else {
            // TODO:  intersection
            TypeFacts::UNKNOWN_FACTS
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
    unknown_union_ty,
    unknown_empty_object_ty,
    string_or_number_ty,
    string_number_symbol_ty,
    number_or_bigint_ty,
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
    global_tpl_strings_array_ty,
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

fn resolve_external_module_name(
    mg: &ModuleGraph,
    module_spec: ast::NodeID,
    p: &bolt_ts_parser::ParsedMap<'_>,
) -> Option<bolt_ts_binder::SymbolID> {
    let from = module_spec.module();
    let name = match p.node(module_spec) {
        ast::Node::StringLit(lit) => lit.val,
        ast::Node::NoSubstitutionTemplateLit(lit) => lit.val,
        _ => unreachable!(),
    };
    let Some(dep) = mg.get_dep(from, name) else {
        unreachable!()
    };

    match dep {
        ModuleRes::Err => None,
        ModuleRes::Res(module_id) => Some(bolt_ts_binder::SymbolID::container(module_id)),
    }
}
