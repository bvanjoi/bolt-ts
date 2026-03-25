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
mod discriminant;
mod elaborate_error;
pub mod errors;
mod eval;
mod expect;
mod flow;
mod fn_mapper;
mod get_base_ty;
mod get_cond_ty_info;
mod get_context;
mod get_contextual;
mod get_declared_ty;
mod get_effective_node;
mod get_global;
mod get_iteration_tys;
mod get_mapped_ty_info;
mod get_sig;
mod get_simplified_ty;
mod get_symbol;
mod get_syntactic_semantics;
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
mod unwrap_ty;
mod utils;

use std::fmt::Debug;

use bolt_ts_ast::r#trait::VarLike;
use bolt_ts_ast::{self as ast, pprint_elem_access_expr, pprint_prop_access_expr};
use bolt_ts_ast::{BinOp, pprint_ident};
use bolt_ts_ast::{FnFlags, keyword};
use bolt_ts_atom::{Atom, AtomIntern};
use bolt_ts_binder::param_index_in_parameter_list;
use bolt_ts_binder::{AccessKind, AssignmentKind, NodeQuery, prop_name};
use bolt_ts_binder::{FlowID, FlowInNodes, FlowNodes};
use bolt_ts_binder::{GlobalSymbols, MergedSymbols, ResolveResult, SymbolTable, Symbols};
use bolt_ts_binder::{Symbol, SymbolFlags, SymbolID, SymbolName};
use bolt_ts_config::NormalizedCompilerOptions;
use bolt_ts_middle::F64Represent;
use bolt_ts_module_graph::{ModuleGraph, ModuleRes};
use bolt_ts_parser::ParsedMap;
use bolt_ts_span::ModuleID;
use bolt_ts_utils::FxIndexSet;
use bolt_ts_utils::{fx_hashmap_with_capacity, no_hashmap_with_capacity, no_hashset_with_capacity};

use enumflags2::BitFlag;
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};

use self::check_expr::IterationUse;
use self::check_type_related_to::RecursionFlags;
use self::create_ty::IntersectionFlags;
use self::cycle_check::ResolutionKey;
use self::flow::FlowCacheKey;
use self::flow::FlowTy;
use self::fn_mapper::{PermissiveMapper, RestrictiveMapper};
use self::get_context::{InferenceContextual, TyContextual};
use self::get_contextual::ContextFlags;
use self::get_iteration_tys::IterationTypeKind;
use self::get_simplified_ty::SimplifiedKind;
use self::get_variances::VarianceFlags;
use self::infer::InferenceContext;
use self::infer::{InferenceFlags, InferencePriority};
use self::instantiation_ty_map::InstantiationTyMap;
use self::instantiation_ty_map::SubstitutionKey;
use self::instantiation_ty_map::TyAliasInstantiationMap;
use self::instantiation_ty_map::TyInstantiationMap;
use self::instantiation_ty_map::UnionMap;
use self::instantiation_ty_map::UnionOfUnionTysKey;
use self::instantiation_ty_map::{IndexedAccessTyMap, IntersectionMap, StringMappingTyMap};
use self::instantiation_ty_map::{TyCacheTrait, TyKey};
use self::links::NodeLinks;
use self::links::SymbolLinks;
use self::links::{SigLinks, TyLinks};
pub use self::merge::MergeModuleAugmentationResult;
pub use self::merge::merge_module_augmentation_list_for_global;
use self::node_check_flags::NodeCheckFlags;
use self::relation::EnumRelationMap;
use self::relation::RelationComparisonResult;
use self::relation::RelationKey;
pub use self::resolve::ExpectedArgsCount;
pub(crate) use self::symbol_info::SymbolInfo;
use self::transient_symbol::create_transient_symbol;
use self::type_predicate::TyPred;
use self::utils::contains_ty;

use super::ty::TyMapper;
use super::ty::typeof_ne_facts;
use super::ty::{self, AccessFlags};
use super::ty::{CheckFlags, IndexFlags, TYPEOF_NE_FACTS};
use super::ty::{ElementFlags, ObjectFlags, Sig, SigFlags, SigID, TyID, TypeFacts, TypeFlags};

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

struct FlowLoopTypesArena<'cx> {
    arena: bolt_ts_arena::la_arena::Arena<Vec<&'cx ty::Ty<'cx>>>,
}

impl<'cx> FlowLoopTypesArena<'cx> {
    fn new() -> Self {
        Self {
            arena: bolt_ts_arena::la_arena::Arena::new(),
        }
    }
    fn alloc(&mut self, value: Vec<&'cx ty::Ty<'cx>>) -> FlowLoopTypesArenaId<'cx> {
        FlowLoopTypesArenaId(self.arena.alloc(value))
    }
    fn get(&self, id: FlowLoopTypesArenaId<'cx>) -> &Vec<&'cx ty::Ty<'cx>> {
        &self.arena[id.0]
    }
    fn get_mut(&mut self, id: FlowLoopTypesArenaId<'cx>) -> &mut Vec<&'cx ty::Ty<'cx>> {
        &mut self.arena[id.0]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct FlowLoopTypesArenaId<'cx>(bolt_ts_arena::la_arena::Idx<Vec<&'cx ty::Ty<'cx>>>);

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
    union_of_union_tys: FxHashMap<UnionOfUnionTysKey<'cx>, &'cx ty::Ty<'cx>>,
    substitution_tys: FxHashMap<SubstitutionKey, &'cx ty::Ty<'cx>>,
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
    iteration_tys_map: nohash_hasher::IntMap<TyKey, &'cx ty::IterationTys<'cx>>,

    instantiation_ty_map: InstantiationTyMap<'cx>,
    ty_alias_instantiation_map: TyAliasInstantiationMap<'cx>,
    ty_instantiation_map: TyInstantiationMap<'cx>,
    enum_relation: EnumRelationMap,
    is_inference_partially_blocked: bool,

    mark_tys: nohash_hasher::IntSet<TyID>,
    shared_flow_info: Vec<(FlowID, FlowTy<'cx>)>,
    common_ty_links_arena: ty::CommonTyLinksArena<'cx>,
    fresh_ty_links_arena: ty::FreshTyLinksArena<'cx>,
    union_ty_links_arena: ty::UnionTyLinksArena<'cx>,
    constituent_map_for_union_ty: FxHashMap<TyID, FxHashMap<&'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>>>,
    promise_or_awaitable_links_arena: ty::PromiseOrAwaitableTyLinksArena<'cx>,
    union_ty_constituent_map:
        nohash_hasher::IntMap<TyID, FxHashMap<&'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>>>,
    interface_ty_links_arena: ty::InterfaceTyLinksArena<'cx>,
    object_mapped_ty_links_arena: ty::ObjectMappedTyLinksArena<'cx>,
    conditional_links_arena: ty::ConditionalLinksArena<'cx>,
    // === ast ===
    pub p: &'cx ParsedMap<'cx>,
    pub mg: &'cx ModuleGraph,
    // === global ===
    // === intrinsic types ===
    pub any_ty: &'cx ty::Ty<'cx>,
    pub auto_ty: &'cx ty::Ty<'cx>,
    pub wildcard_ty: &'cx ty::Ty<'cx>,
    pub blocked_string_ty: &'cx ty::Ty<'cx>,
    pub error_ty: &'cx ty::Ty<'cx>,
    pub unknown_ty: &'cx ty::Ty<'cx>,
    pub undefined_ty: &'cx ty::Ty<'cx>,
    pub missing_ty: &'cx ty::Ty<'cx>,
    pub undefined_or_missing_ty: &'cx ty::Ty<'cx>,
    pub undefined_widening_ty: &'cx ty::Ty<'cx>,
    pub never_ty: &'cx ty::Ty<'cx>,
    /// These paths actually will not reach but should not throw error, for example:
    /// ```typescript
    /// function foo(cond: boolean) {
    ///     let x: string | number = 0;
    ///     while (cond) {
    ///         if (typeof x === "string") {
    ///             x = x.slice();
    ///              // ~ the type of `x` is `silent_never_ty` when during the `x` in `typeof x`.
    ///         } else {
    ///             x = "abc";
    ///         }
    ///     }
    /// }
    /// ```
    pub silent_never_ty: &'cx ty::Ty<'cx>,
    pub implicit_never_ty: &'cx ty::Ty<'cx>,
    pub unreachable_never_ty: &'cx ty::Ty<'cx>,
    pub void_ty: &'cx ty::Ty<'cx>,
    pub optional_ty: &'cx ty::Ty<'cx>,
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
    any_base_type_index_info: std::cell::OnceCell<&'cx ty::IndexInfo<'cx>>,

    any_sig: std::cell::OnceCell<&'cx Sig<'cx>>,
    unknown_sig: std::cell::OnceCell<&'cx Sig<'cx>>,
    resolving_sig: std::cell::OnceCell<&'cx Sig<'cx>>,

    any_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    any_fn_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    any_readonly_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    array_variances: std::cell::OnceCell<&'cx [VarianceFlags]>,
    auto_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    boolean_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    circular_constraint_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    empty_generic_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    empty_object_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    empty_ty_literal_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_object_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_fn_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_callable_fn_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_newable_fn_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_readonly_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_number_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_string_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_boolean_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    global_regexp_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    mark_super_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    mark_sub_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    mark_other_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    no_constraint_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    no_ty_pred: std::cell::OnceCell<&'cx TyPred<'cx>>,
    number_or_bigint_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    resolving_default_type: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    string_or_number_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    string_number_symbol_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    typeof_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    template_constraint_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    unknown_union_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    unknown_empty_object_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,

    deferred_global_typed_property_descriptor_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_template_strings_array_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_import_meta_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_import_meta_expression_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_import_call_optionals_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_import_attributes_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_es_symbol_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_promise_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_promise_like_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_promise_constructor_like_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_async_iterable_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_async_iterator_object_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_async_generator_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_async_iterable_iterator_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_async_iterator_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_async_disposable_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_iterable_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_iterator_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_iterator_object_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_iterable_iterator_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_generator_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_iterator_yield_result_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_iterator_return_result_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_disposable_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_bigint_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_class_decorator_context_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_class_method_decorator_context_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_class_getter_decorator_context_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_class_setter_decorator_context_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_class_accessor_decorator_context_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_class_accessor_decorator_target_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_class_accessor_decorator_return_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,
    deferred_global_class_field_decorator_context_ty: std::cell::OnceCell<&'cx ty::Ty<'cx>>,

    deferred_global_builtin_iterator_tys: std::cell::OnceCell<ty::Tys<'cx>>,
    deferred_global_builtin_async_iterator_tys: std::cell::OnceCell<ty::Tys<'cx>>,

    deferred_global_extract_symbol: std::cell::OnceCell<Option<SymbolID>>,
    deferred_global_omit_symbol: std::cell::OnceCell<Option<SymbolID>>,
    deferred_global_awaited_symbol: std::cell::OnceCell<Option<SymbolID>>,
    deferred_global_es_symbol_constructor_symbol: std::cell::OnceCell<Option<SymbolID>>,
    deferred_global_promise_constructor_symbol: std::cell::OnceCell<Option<SymbolID>>,
    deferred_global_non_nullable_type_alias: std::cell::OnceCell<Option<SymbolID>>,
    deferred_global_nan_symbol: std::cell::OnceCell<Option<SymbolID>>,
    deferred_global_record_symbol: std::cell::OnceCell<Option<SymbolID>>,

    empty_array: &'cx [u8; 0],
    structure_members_placeholder: &'cx ty::StructuredMembers<'cx>,
    never_intersection_tys: nohash_hasher::IntMap<ty::TyID, bool>,

    any_iteration_tys: std::cell::OnceCell<&'cx ty::IterationTys<'cx>>,
    no_iteration_tys: std::cell::OnceCell<&'cx ty::IterationTys<'cx>>,
    iteration_tys_of_iterable: nohash_hasher::IntMap<ty::TyID, &'cx ty::IterationTys<'cx>>,
    iteration_tys_of_iterator: nohash_hasher::IntMap<ty::TyID, &'cx ty::IterationTys<'cx>>,
    iteration_tys_of_async_iterable: nohash_hasher::IntMap<ty::TyID, &'cx ty::IterationTys<'cx>>,
    iteration_tys_of_async_iterator: nohash_hasher::IntMap<ty::TyID, &'cx ty::IterationTys<'cx>>,
    iteration_tys_of_iterator_result: nohash_hasher::IntMap<ty::TyID, &'cx ty::IterationTys<'cx>>,

    flow_loop_start: u32,
    flow_loop_nodes: Vec<FlowID>,
    flow_loop_keys: Vec<FlowCacheKey>,
    flow_loop_types_arena: FlowLoopTypesArena<'cx>,
    flow_loop_types: Vec<FlowLoopTypesArenaId<'cx>>,
    flow_loop_caches: FxHashMap<FlowID, FxHashMap<FlowCacheKey, &'cx ty::Ty<'cx>>>,
    flow_ty_cache: Option<Vec<&'cx ty::Ty<'cx>>>,

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
    awaited_ty_stack: Vec<&'cx ty::Ty<'cx>>,

    // ==== relation ====
    relations: FxHashMap<RelationKey<'cx>, RelationComparisonResult>,
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
        let cap = p.module_count() * 1024;
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
            (blocked_string_ty,     keyword::IDENT_ANY,     TypeFlags::ANY,             ObjectFlags::empty()),
            (error_ty,              keyword::IDENT_ERROR,   TypeFlags::ANY,             ObjectFlags::empty()),
            (non_inferrable_any_ty, keyword::IDENT_ANY,     TypeFlags::ANY,             ObjectFlags::CONTAINS_WIDENING_TYPE),
            (intrinsic_marker_ty,   keyword::KW_INTRINSIC,  TypeFlags::ANY,             ObjectFlags::empty()),
            (unknown_ty,            keyword::IDENT_UNKNOWN, TypeFlags::UNKNOWN,         ObjectFlags::empty()),
            (undefined_ty,          keyword::KW_UNDEFINED,  TypeFlags::UNDEFINED,       ObjectFlags::empty()),
            (missing_ty,            keyword::KW_UNDEFINED,  TypeFlags::UNDEFINED,       ObjectFlags::empty()),
            (es_symbol_ty,          keyword::IDENT_SYMBOL,  TypeFlags::ES_SYMBOL,       ObjectFlags::empty()),
            (void_ty,               keyword::KW_VOID,       TypeFlags::VOID,            ObjectFlags::empty()),
            (never_ty,              keyword::IDENT_NEVER,   TypeFlags::NEVER,           ObjectFlags::empty()),
            (optional_ty,           keyword::KW_UNDEFINED,  TypeFlags::UNDEFINED,       ObjectFlags::empty()),
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

            num_lit_tys: no_hashmap_with_capacity(1024),
            string_lit_tys: fx_hashmap_with_capacity(1024),
            bigint_lit_tys: fx_hashmap_with_capacity(512),
            union_tys: UnionMap::new(1024),
            union_of_union_tys: fx_hashmap_with_capacity(1024),
            substitution_tys: fx_hashmap_with_capacity(1024),
            intersection_tys: IntersectionMap::new(1024),
            indexed_access_tys: IndexedAccessTyMap::new(1024),
            string_mapping_tys: StringMappingTyMap::new(1024),
            instantiation_ty_map: InstantiationTyMap::new(1024),
            ty_alias_instantiation_map: TyAliasInstantiationMap::new(1024),
            ty_instantiation_map: TyInstantiationMap::new(1024),
            iteration_tys_map: no_hashmap_with_capacity(1024),
            mark_tys: no_hashset_with_capacity(1024),

            shared_flow_info: Vec::with_capacity(1024),
            flow_node_reachable: fx_hashmap_with_capacity(flow_nodes.len()),
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
            blocked_string_ty,
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
            optional_ty,
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

            is_inference_partially_blocked: false,
            enum_number_index_info: Default::default(),
            any_base_type_index_info: Default::default(),

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
            no_iteration_tys: Default::default(),
            iteration_tys_of_iterable: Default::default(),
            iteration_tys_of_iterator: Default::default(),
            iteration_tys_of_async_iterable: Default::default(),
            iteration_tys_of_async_iterator: Default::default(),
            iteration_tys_of_iterator_result: Default::default(),

            unknown_empty_object_ty: Default::default(),

            deferred_global_typed_property_descriptor_ty: Default::default(),
            deferred_global_template_strings_array_ty: Default::default(),
            deferred_global_import_meta_ty: Default::default(),
            deferred_global_import_meta_expression_ty: Default::default(),
            deferred_global_import_call_optionals_ty: Default::default(),
            deferred_global_import_attributes_ty: Default::default(),
            deferred_global_es_symbol_ty: Default::default(),
            deferred_global_promise_ty: Default::default(),
            deferred_global_promise_like_ty: Default::default(),
            deferred_global_promise_constructor_like_ty: Default::default(),
            deferred_global_async_iterable_ty: Default::default(),
            deferred_global_async_iterator_ty: Default::default(),
            deferred_global_async_iterator_object_ty: Default::default(),
            deferred_global_async_disposable_ty: Default::default(),
            deferred_global_async_iterable_iterator_ty: Default::default(),
            deferred_global_async_generator_ty: Default::default(),
            deferred_global_iterable_ty: Default::default(),
            deferred_global_iterator_ty: Default::default(),
            deferred_global_iterator_object_ty: Default::default(),
            deferred_global_iterable_iterator_ty: Default::default(),
            deferred_global_generator_ty: Default::default(),
            deferred_global_iterator_yield_result_ty: Default::default(),
            deferred_global_iterator_return_result_ty: Default::default(),
            deferred_global_disposable_ty: Default::default(),
            deferred_global_bigint_ty: Default::default(),
            deferred_global_class_decorator_context_ty: Default::default(),
            deferred_global_class_method_decorator_context_ty: Default::default(),
            deferred_global_class_getter_decorator_context_ty: Default::default(),
            deferred_global_class_setter_decorator_context_ty: Default::default(),
            deferred_global_class_accessor_decorator_context_ty: Default::default(),
            deferred_global_class_accessor_decorator_target_ty: Default::default(),
            deferred_global_class_accessor_decorator_return_ty: Default::default(),
            deferred_global_class_field_decorator_context_ty: Default::default(),
            deferred_global_builtin_iterator_tys: Default::default(),
            deferred_global_builtin_async_iterator_tys: Default::default(),
            deferred_global_extract_symbol: Default::default(),
            deferred_global_omit_symbol: Default::default(),
            deferred_global_awaited_symbol: Default::default(),
            deferred_global_es_symbol_constructor_symbol: Default::default(),
            deferred_global_promise_constructor_symbol: Default::default(),
            deferred_global_non_nullable_type_alias: Default::default(),
            deferred_global_nan_symbol: Default::default(),
            deferred_global_record_symbol: Default::default(),

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
            conditional_links_arena: ty::ConditionalLinksArena::with_capacity(cap),
            union_ty_links_arena: ty::UnionTyLinksArena::with_capacity(cap),
            constituent_map_for_union_ty: fx_hashmap_with_capacity(cap),
            promise_or_awaitable_links_arena: ty::PromiseOrAwaitableTyLinksArena::with_capacity(
                cap,
            ),
            union_ty_constituent_map: no_hashmap_with_capacity(32),
            enum_relation: EnumRelationMap::new(),

            resolution_tys: thin_vec::ThinVec::with_capacity(128),
            resolution_res: thin_vec::ThinVec::with_capacity(128),
            resolution_start: 0,

            flow_loop_start: 0,
            flow_loop_nodes: Vec::new(),
            flow_loop_keys: Vec::new(),
            flow_loop_types: Vec::new(),
            flow_loop_types_arena: FlowLoopTypesArena::new(),
            flow_loop_caches: Default::default(),
            flow_ty_cache: None,

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
            activity_ty_mapper: Vec::with_capacity(128),
            activity_ty_mapper_caches: Vec::with_capacity(128),
            instantiation_count: 0,
            awaited_ty_stack: Vec::with_capacity(8),

            relations: fx_hashmap_with_capacity(128),
        };

        macro_rules! make_global {
            ( { $( ($name: ident, $make_ty: expr) ),* $(,)? } ) => {
                $(
                    let $name = $make_ty;
                    this.$name.set($name).unwrap();
                )*
            };
        }
        // TODO: lazy
        make_global!({
            (boolean_ty,                    this.get_union_ty::<false>(&[regular_false_ty, regular_true_ty],                 ty::UnionReduction::Lit, None, None, None)),
            (string_or_number_ty,           this.get_union_ty::<false>(&[this.string_ty, this.number_ty],                    ty::UnionReduction::Lit, None, None, None)),
            (string_number_symbol_ty,       this.get_union_ty::<false>(&[this.string_ty, this.number_ty, this.es_symbol_ty], ty::UnionReduction::Lit, None, None, None)),
            (number_or_bigint_ty,           this.get_union_ty::<false>(&[this.number_ty, this.bigint_ty],                    ty::UnionReduction::Lit, None, None, None)),
            (global_number_ty,              this.get_global_type::<0, true>(SymbolName::Atom(keyword::IDENT_NUMBER_CLASS))),
            (global_boolean_ty,             this.get_global_type::<0, true>(SymbolName::Atom(keyword::IDENT_BOOLEAN_CLASS))),
            (global_string_ty,              this.get_global_type::<0, true>(SymbolName::Atom(keyword::IDENT_STRING_CLASS))),
            (global_array_ty,               this.get_global_type::<1, true>(SymbolName::Atom(keyword::IDENT_ARRAY_CLASS))),
            (global_regexp_ty,              this.get_global_type::<0, true>(SymbolName::Atom(keyword::IDENT_REGEXP_CLASS))),
            (global_readonly_array_ty,      this.get_global_type::<0, true>(SymbolName::Atom(keyword::IDENT_READONLY_ARRAY_CLASS))),
            (global_object_ty,              this.get_global_type::<0, true>(SymbolName::Atom(keyword::IDENT_OBJECT_CLASS))),
            (global_fn_ty,                  this.get_global_type::<0, true>(SymbolName::Atom(keyword::IDENT_FUNCTION_CLASS))),
            (global_callable_fn_ty,         if this.config.strict_bind_call_apply() { this.get_global_type::<0, true>(SymbolName::Atom(keyword::IDENT_CALLABLE_FUNCTION_CLASS)) } else { global_fn_ty }),
            (global_newable_fn_ty,          if this.config.strict_bind_call_apply() { this.get_global_type::<0, true>(SymbolName::Atom(keyword::IDENT_NEWABLE_FUNCTION_CLASS)) } else { global_fn_ty }),
            (any_array_ty,                  this.create_array_ty(this.any_ty, false)),
            (any_readonly_array_ty,         this.any_array_ty()),
            (typeof_ty,                 {
                                            let tys = TYPEOF_NE_FACTS.iter().map(|(key, _)| this.get_string_literal_type_from_string(*key)).collect::<Vec<_>>();
                                            this.get_union_ty::<false>(&tys, ty::UnionReduction::Lit, None, None, None)
                                        }),
            (any_fn_ty,                     this.create_anonymous_ty_with_resolved(None, ObjectFlags::NON_INFERRABLE_TYPE, this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (no_constraint_ty,              this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (circular_constraint_ty,        this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (resolving_default_type,        this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (empty_generic_ty,              this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (empty_object_ty,               this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (empty_ty_literal_ty,           this.create_anonymous_ty_with_resolved(Some(empty_ty_literal_symbol), Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (unknown_empty_object_ty,       this.create_anonymous_ty_with_resolved(None, Default::default(), this.alloc(Default::default()), Default::default(), Default::default(), Default::default(), None)),
            (mark_sub_ty,                   this.create_param_ty(Symbol::ERR, None, false)),
            (mark_other_ty,                 this.create_param_ty(Symbol::ERR, None, false)),
            (mark_super_ty,                 this.create_param_ty(Symbol::ERR, None, false)),
            (template_constraint_ty,        this.get_union_ty::<false>(&[string_ty, number_ty, boolean_ty, bigint_ty, null_ty, undefined_ty], ty::UnionReduction::Lit, None, None, None)),
            (any_iteration_tys,             this.create_iteration_tys(any_ty, any_ty, any_ty)),
            (no_iteration_tys,              this.alloc(ty::IterationTys {yield_ty: error_ty, return_ty: error_ty, next_ty: error_ty})),
            (any_sig,                       this.new_sig(Sig { flags: SigFlags::empty(), this_param: None, params: cast_empty_array(empty_array), min_args_count: 0, ret: None, node_id: None, target: None, mapper: None, id: SigID::dummy(), class_decl: None, composite_sigs: None, composite_kind: None })),
            (unknown_sig,                   this.new_sig(Sig { flags: SigFlags::empty(), this_param: None, params: cast_empty_array(empty_array), min_args_count: 0, ret: None, node_id: None, target: None, mapper: None, id: SigID::dummy(), class_decl: None, composite_sigs: None, composite_kind: None })),
            (resolving_sig,                 this.new_sig(Sig { flags: SigFlags::empty(), this_param: None, params: cast_empty_array(empty_array), min_args_count: 0, ret: None, node_id: None, target: None, mapper: None, id: SigID::dummy(), class_decl: None, composite_sigs: None, composite_kind: None })),
            (array_variances,               this.alloc([VarianceFlags::COVARIANT])),
            (no_ty_pred,                    this.create_ident_ty_pred(keyword::IDENT_EMPTY, 0, any_ty)),
            (enum_number_index_info,        this.alloc(ty::IndexInfo { symbol: Symbol::ERR, key_ty: number_ty, val_ty: string_ty, is_readonly: true })),
            (any_base_type_index_info,      this.alloc(ty::IndexInfo { symbol: Symbol::ERR, key_ty: string_ty, val_ty: any_ty, is_readonly: false })),
        });

        let unknown_union_ty = if this.config.strict_null_checks() {
            this.get_union_ty::<false>(
                &[this.undefined_ty, this.null_ty, unknown_empty_object_ty],
                ty::UnionReduction::Lit,
                None,
                None,
                None,
            )
        } else {
            unknown_ty
        };
        this.unknown_union_ty.set(unknown_union_ty).unwrap();

        debug_assert!(this.global_readonly_array_ty().is_readonly_array(&this));

        this.type_name.insert(boolean_ty.id, "boolean".to_string());

        let iarguments =
            this.get_global_type::<0, true>(SymbolName::Atom(keyword::IDENT_IARGUMENTS_CLASS));
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

    pub fn any_iteration_tys(&self) -> &'cx ty::IterationTys<'cx> {
        self.any_iteration_tys.get().copied().unwrap()
    }

    pub fn no_iteration_tys(&self) -> &'cx ty::IterationTys<'cx> {
        self.no_iteration_tys.get().copied().unwrap()
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
        if object_flags.contains(ObjectFlags::MAPPED) {
            self.get_apparent_ty_of_mapped_ty(t)
        } else if object_flags.contains(ObjectFlags::REFERENCE) && t != ty {
            self.get_ty_with_this_arg(t, Some(ty), false)
        } else if flags.contains(TypeFlags::INTERSECTION) {
            self.get_apparent_ty_of_intersection_ty(t, ty)
        } else if flags.intersects(TypeFlags::NUMBER_LIKE) {
            self.global_number_ty()
        } else if flags.intersects(TypeFlags::BIG_INT_LIKE) {
            self.get_global_bigint_ty()
        } else if flags.intersects(TypeFlags::STRING_LIKE) {
            self.global_string_ty()
        } else if flags.intersects(TypeFlags::BOOLEAN_LIKE) {
            self.global_boolean_ty()
        } else if flags.intersects(TypeFlags::ES_SYMBOL_LIKE) {
            self.get_global_es_symbol_ty()
        } else if flags.contains(TypeFlags::NON_PRIMITIVE) {
            self.empty_object_ty()
        } else if flags.contains(TypeFlags::INDEX) {
            self.string_number_symbol_ty()
        } else if flags.contains(TypeFlags::UNKNOWN) && !self.config.strict_null_checks() {
            self.empty_object_ty()
        } else {
            t
        }
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
        } else if ty.flags.contains(TypeFlags::INTERSECTION) {
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
        let value_decl = self.symbol(prop).value_decl;
        let name = value_decl.and_then(|d| self.node_query(d.module()).get_name_of_decl(d));
        if let Some(name) = name
            && matches!(name, ast::DeclarationName::PrivateIdent(_))
        {
            return;
        }

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
                    ast::PropNameKind::Computed(_) => "[...]".to_string(),
                    ast::PropNameKind::PrivateIdent(_) => todo!(),
                    ast::PropNameKind::BigIntLit(_) => todo!(),
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

    fn get_literal_ty_from_prop_name(
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
            ast::PropNameKind::PrivateIdent(ident) => {
                self.get_string_literal_type_from_string(ident.name)
            }
            ast::PropNameKind::BigIntLit(lit) => todo!(),
        }
    }

    pub(super) fn get_declaration_modifier_flags_from_symbol(
        &self,
        symbol: SymbolID,
        is_write: Option<bool>,
    ) -> enumflags2::BitFlags<bolt_ts_ast::ModifierKind> {
        let is_write = is_write.unwrap_or(false);
        let s = self.symbol(symbol);
        fn find_decls<'cx>(
            this: &TyChecker<'cx>,
            decls: &[ast::NodeID],
            f: impl Fn(ast::Node<'cx>) -> bool,
        ) -> Option<ast::NodeID> {
            decls.iter().find(|id| f(this.p.node(**id))).copied()
        }
        if let Some(value_declaration) = s.value_decl {
            let decl = if is_write
                && let Some(decls) = s.decls.as_ref()
                && let Some(decl) = find_decls(self, decls, |n| n.is_setter_decl())
            {
                decl
            } else if s.flags.contains(SymbolFlags::GET_ACCESSOR)
                && let Some(decls) = s.decls.as_ref()
                && let Some(decl) = find_decls(self, decls, |n| n.is_getter_decl())
            {
                decl
            } else {
                value_declaration
            };
            let flags = self
                .node_query(decl.module())
                .get_combined_modifier_flags(decl);
            return if let Some(p) = s.parent
                && let p = self.symbol(p)
                && p.flags.contains(SymbolFlags::CLASS)
            {
                flags
            } else {
                flags & !ast::ModifierKind::ACCESSIBILITY
            };
        }
        let check_flags = self.get_check_flags(symbol);
        if check_flags.intersects(CheckFlags::SYNTHETIC) {
            let access_modifier: enumflags2::BitFlags<_> =
                if check_flags.contains(CheckFlags::CONTAINS_PRIVATE) {
                    ast::ModifierKind::Private
                } else if check_flags.contains(CheckFlags::CONTAINS_PUBLIC) {
                    ast::ModifierKind::Public
                } else {
                    ast::ModifierKind::Protected
                }
                .into();
            let static_modifier = if check_flags.contains(CheckFlags::CONTAINS_STATIC) {
                ast::ModifierKind::Static.into()
            } else {
                ast::ModifierKind::empty()
            };
            static_modifier | access_modifier
        } else if s.flags.contains(SymbolFlags::PROPERTY) {
            use ast::ModifierKind;
            enumflags2::make_bitflags!(ModifierKind::{Public | Static})
        } else {
            Default::default()
        }
    }

    fn get_literal_ty_from_prop(
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
                                            ast::DeclarationName::PrivateIdent(n) => {
                                                ast::PropNameKind::PrivateIdent(n)
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
                                            ast::DeclarationName::BigIntLit(lit) => todo!(),
                                        };
                                        ast::PropName { kind }
                                    })
                            })
                            .map(|name| self.get_literal_ty_from_prop_name(&name.kind))
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
                let prop_name_ty = self.get_literal_ty_from_prop(
                    *prop,
                    TypeFlags::STRING_OR_NUMBER_LITERAL_OR_UNIQUE,
                    true,
                );
                let prop_ty = self.get_non_missing_type_of_symbol(*prop);
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
        let Some(ty_params) = self.get_sig_links(sig.id).get_ty_params() else {
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
        if self
            .get_sig_links(contextual_sig.id)
            .get_ty_params()
            .is_some()
        {
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
            && self.get_sig_links(ret_sig.id).get_ty_params().is_none()
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
        self.get_or_create_ty_from_sig(sig)
    }

    fn get_or_create_ty_from_sig(&mut self, sig: &'cx ty::Sig<'cx>) -> &'cx ty::Ty<'cx> {
        //TODO: cache `isolated_sig_ty`
        let is_constructor = sig.node_id.is_none_or(|node_id| {
            use bolt_ts_ast::Node::*;
            matches!(self.p.node(node_id), ClassCtor(_) | CtorSigDecl(_))
        });
        let ty = self.create_single_sig_ty(ty::SingleSigTy {
            symbol: sig.node_id.map(|node_id| self.final_res(node_id)),
            target: None,
            mapper: None,
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
        let Some(ty_params) = self.get_sig_links(sig.id).get_ty_params() else {
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
        // TODO: is_js
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
        if check_mode.contains(CheckMode::INFERENTIAL) {
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
        // TODO: in_js

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
                return Some(self.get_flow_ty_of_reference(node, this_ty, None, None, None));
            }
        }

        if let Some(parent) = self.parent(container_id) {
            if self.p.node(parent).is_class_like() {
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

        if container.is_program() {
            let container = self.p.get(node.module());
            if container.commonjs_module_indicator.is_some() {
                todo!()
            } else if container.external_module_indicator.is_some() {
                return Some(self.undefined_ty);
            } else if include_global_this {
                return Some(self.get_type_of_symbol(self.global_this_symbol));
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
        name: bolt_ts_atom::Atom,
        containing_ty: &'cx ty::Ty<'cx>,
    ) -> bool {
        let Some(symbol) = containing_ty.symbol() else {
            return false;
        };
        let ty = self.get_type_of_symbol(symbol);
        let name = SymbolName::Atom(name);
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

        if self.type_has_static_prop(prop_node.name, containing_ty) {
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
        let is_any_like =
            self.is_type_any(apparent_left_ty) || apparent_left_ty == self.silent_never_ty;
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
        let skip_object_function_property_augment = self.is_const_enum_object_ty(apparent_left_ty);
        let include_type_only_members = self.p.node(node).is_qualified_name();
        let prop = self.get_prop_of_ty(apparent_left_ty, name);
        let prop_ty = if let Some(prop) = prop {
            self.check_prop_not_used_before_declaration(prop, node, right);
            // TODO: mark_prop_as_referenced
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
            let index_info = if assignment_kind == AssignmentKind::None
                || !self.is_generic_object_ty(left_ty)
                || left_ty.kind.is_this_ty_param()
            {
                self.get_applicable_index_info_for_name(apparent_left_ty, name)
            } else {
                None
            };
            let Some(index_info) = index_info else {
                if left_ty.symbol() == Some(self.global_this_symbol) {
                    // TODO: report error
                    return self.any_ty;
                }
                if name
                    .as_atom()
                    .is_some_and(|atom| atom != keyword::IDENT_EMPTY)
                {
                    self.report_non_existent_prop(right, left_ty);
                }
                return self.error_ty;
            };
            let mut prop_ty = index_info.val_ty;
            if self.config.no_unchecked_indexed_access()
                && self
                    .node_query(node.module())
                    .get_assignment_target_kind(node)
                    != AssignmentKind::Definite
            {
                let tys = &[prop_ty, self.missing_ty];
                prop_ty =
                    self.get_union_ty::<false>(tys, ty::UnionReduction::Lit, None, None, None);
            }

            prop_ty
        };
        self.get_flow_type_of_prop_access_expr(node, prop, prop_ty, Some(right.id), self.check_mode)
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
            && !self.is_block_scoped_name_declared_before_use(value_decl, right.id, right.span)
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

            if !flags.contains(ast::ModifierKind::Static)
                && let prop_symbol = self.symbol(prop)
                && let Some(decls) = prop_symbol.decls.as_ref()
                && decls.iter().any(|decl| {
                    self.node_query(decl.module())
                        .is_class_instance_property(*decl)
                })
            {
                if let Some(error_node) = error_node {
                    let error = errors::AbstractMethod0InClass1CannotBeAccessedViaSuperExpression {
                        span: self.p.node(error_node).span(),
                        field: prop_symbol.name.to_string(&self.atoms),
                    };
                    self.push_error(Box::new(error));
                }
                return false;
            }
        }

        if flags.contains(ast::ModifierKind::Private) {
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
        if self
            .p
            .node_flags(node.id)
            .contains(ast::NodeFlags::OPTIONAL_CHAIN)
        {
            self.check_prop_access_chain(node)
        } else {
            let left_ty = self.check_non_null_expr(node.expr);
            self.check_prop_access_expr_or_qualified_name(
                node.id,
                node.expr.id(),
                left_ty,
                node.name,
            )
        }
    }

    fn remove_optional_ty_marker(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if self.config.strict_null_checks() {
            self.remove_ty(ty, self.optional_ty)
        } else {
            ty
        }
    }

    fn get_optional_expression_ty(
        &mut self,
        expr_ty: &'cx ty::Ty<'cx>,
        expr: &'cx ast::Expr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let expr_id = expr.id();
        let expr_id_module = expr_id.module();
        let nq = self.node_query(expr_id_module);
        if nq.is_expression_of_optional_chain_root(expr_id) {
            self.get_non_nullable_ty(expr_ty)
        } else if nq.is_optional_chain(expr_id) {
            self.remove_optional_ty_marker(expr_ty)
        } else {
            expr_ty
        }
    }

    fn add_optional_ty_marker(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if self.config.strict_null_checks() {
            let tys = &[ty, self.optional_ty];
            self.get_union_ty::<false>(tys, ty::UnionReduction::Lit, None, None, None)
        } else {
            ty
        }
    }

    fn propagate_optional_ty_marker(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        node: ast::NodeID,
        was_optional: bool,
    ) -> &'cx ty::Ty<'cx> {
        if was_optional {
            if self.node_query(node.module()).is_optional_chain(node) {
                self.get_optional_ty::<false>(ty)
            } else {
                self.add_optional_ty_marker(ty)
            }
        } else {
            ty
        }
    }

    fn check_prop_access_chain(&mut self, n: &'cx ast::PropAccessExpr<'cx>) -> &'cx ty::Ty<'cx> {
        let left_ty = self.check_expr(n.expr);
        let non_optional_ty = self.get_optional_expression_ty(left_ty, n.expr);
        let expr_id = n.expr.id();
        let non_null_ty = self.check_non_null_type(non_optional_ty, expr_id);
        let ty = self.check_prop_access_expr_or_qualified_name(n.id, expr_id, non_null_ty, n.name);
        self.propagate_optional_ty_marker(ty, n.id, non_optional_ty != left_ty)
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
                        None,
                        None,
                    );
                    // TODO: assigned_ty
                }
            }
        }
    }

    fn check_object_prop_assignment(
        &mut self,
        member: &'cx ast::ObjectPropAssignment<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if let ast::PropNameKind::Computed(n) = member.name.kind {
            self.check_computed_prop_name(n);
        }
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
        let in_tuple_context = if let Some(contextual_ty) = contextual_ty {
            self.some_type(contextual_ty, |this, ty| {
                if this.is_tuple_like(ty) {
                    true
                } else if this.is_generic_mapped_ty(ty) {
                    let m = ty.kind.expect_object_mapped();
                    this.get_name_ty_from_mapped_ty(m).is_none()
                        && this
                            .get_homomorphic_ty_var(
                                m.target
                                    .map_or(m, |target| target.kind.expect_object_mapped()),
                            )
                            .is_some()
                } else {
                    false
                }
            })
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
                element_types.push(self.add_optionality::<true>(ty, has_omitted_expr));
                let flags = if has_omitted_expr {
                    ElementFlags::OPTIONAL
                } else {
                    ElementFlags::REQUIRED
                };
                element_flags.push(flags);
                if in_tuple_context
                    && self.check_mode.is_some_and(|check_mode| {
                        check_mode.contains(CheckMode::INFERENTIAL)
                            && !check_mode.contains(CheckMode::SKIP_CONTEXT_SENSITIVE)
                    })
                    && self.is_context_sensitive(elem.id())
                {
                    let inference = self.get_inference_context(lit.id).unwrap();
                    // TODO: add_intra_expression_inference_site
                }
            }
        }

        self.pop_type_context();

        if force_tuple || is_const_context || in_tuple_context {
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
                        if element_flags[i].contains(ElementFlags::VARIADIC) {
                            this.get_indexed_access_ty_or_undefined(
                                t,
                                this.number_ty,
                                None,
                                None,
                                None,
                                None,
                            )
                            .unwrap_or(this.any_ty)
                        } else {
                            t
                        }
                    })
                    .unwrap();
                self.get_union_ty::<false>(tys, ty::UnionReduction::Subtype, None, None, None)
            };
            let array_ty = self.create_array_ty(ty, false);
            self.create_array_literal_ty(array_ty)
        }
    }

    fn create_array_literal_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if !ty.get_object_flags().contains(ObjectFlags::REFERENCE) {
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
        used_id: ast::NodeID,
        used_span: bolt_ts_span::Span,
        decl: ast::NodeID,
        decl_container: ast::NodeID,
    ) -> bool {
        assert_eq!(used_id.module(), decl.module());
        self.node_query(used_id.module())
            .find_ancestor(used_id, |current| {
                let current_id = current.id();
                if current_id == decl_container {
                    return Some(false);
                } else if current.is_fn_like() {
                    return Some(true);
                } else if current.is_class_static_block_decl() {
                    return Some(self.p.node(decl).span().lo() < used_span.lo());
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
                                .node_query(used_id.module())
                                .get_containing_class(used_id)
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
                            .node_query(used_id.module())
                            .get_containing_class(used_id)
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
        used_id: ast::NodeID,
        used_span: bolt_ts_span::Span,
    ) -> bool {
        use ast::Node::*;
        if decl.module() != used_id.module() {
            return true;
        }
        let nq = self.node_query(decl.module());
        if nq.is_in_type_query(used_id) {
            return true;
        }

        let decl_span = self.p.node(decl).span();
        let decl_pos = decl_span.lo();
        let decl_container = nq.get_enclosing_blockscope_container(decl);

        if decl_pos < used_span.lo() {
            let n = self.p.node(decl);
            return match n {
                VarDecl(decl) => !self
                    .node_query(decl.id.module())
                    .is_immediately_used_in_init_or_block_scoped_var(decl, used_id, decl_container),
                ObjectBindingElem(_) => {
                    match nq.find_ancestor(used_id, |n| n.is_object_binding_elem().then_some(true))
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
                            self.is_block_scoped_name_declared_before_use(decl, used_id, used_span)
                        }
                    }
                }
                ArrayBinding(_) => {
                    match nq.find_ancestor(used_id, |n| n.is_array_binding().then_some(true)) {
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
                            self.is_block_scoped_name_declared_before_use(decl, used_id, used_span)
                        }
                    }
                }
                _ => true,
            };
        }

        if self.is_used_in_fn_or_instance_prop(used_id, used_span, decl, decl_container) {
            return true;
        }

        false
    }

    fn check_resolved_block_scoped_var(&mut self, ident: &'cx ast::Ident, id: SymbolID) {
        debug_assert!(
            self.symbol(id).flags.intersects(
                SymbolFlags::BLOCK_SCOPED_VARIABLE
                    .union(SymbolFlags::CLASS)
                    .union(SymbolFlags::ENUM)
            )
        );
        let s = self.binder.symbol(id);
        let Some(decl) = s.opt_decl() else {
            unreachable!()
        };

        if !self
            .p
            .node_flags(decl)
            .contains(bolt_ts_ast::NodeFlags::AMBIENT)
            && !self.is_block_scoped_name_declared_before_use(decl, ident.id, ident.span)
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

    fn get_narrowed_ty_of_symbol(
        &mut self,
        symbol: SymbolID,
        location: &'cx ast::Ident,
    ) -> &'cx ty::Ty<'cx> {
        let ty = self.get_type_of_symbol(symbol);
        if let Some(declaration) = self.symbol(symbol).value_decl {
            let use_declaration = match self.p.node(declaration) {
                ast::Node::ObjectBindingElem(n) if n.init.is_none() && n.dotdotdot.is_none() => {
                    let parent = self.parent(declaration).unwrap();
                    if self.p.node(parent).expect_object_pat().elems.len() >= 2 {
                        true
                    } else {
                        false
                    }
                }
                _ => false,
            };
            if use_declaration {
                let root_declaration = self
                    .node_query(declaration.module())
                    .get_root_decl(declaration);
                let root_node = self.p.node(root_declaration);
                if ((root_node.is_var_decl()
                    && self
                        .node_query(root_declaration.module())
                        .get_combined_node_flags(root_declaration)
                        .intersects(ast::NodeFlags::CONSTANT))
                    || root_node.is_param_decl())
                    && let flags = self.get_node_links(declaration).flags()
                    && !flags.contains(NodeCheckFlags::IN_CHECK_IDENTIFIER)
                {
                    self.get_mut_node_links(declaration)
                        .override_flags(flags | NodeCheckFlags::IN_CHECK_IDENTIFIER);
                    let parent = self.parent(declaration).unwrap();
                    let parent_ty = self.get_ty_for_binding_element_parent(parent);
                    let parent_ty_constraint = parent_ty.map(|ty| {
                        self.map_ty(
                            ty,
                            |this, ty| Some(this.get_base_constraint_or_ty(ty)),
                            false,
                        )
                        .unwrap()
                    });
                    self.get_mut_node_links(declaration).override_flags(flags);
                    if let Some(parent_ty_constraint) = parent_ty_constraint
                        && parent_ty_constraint.flags.contains(TypeFlags::UNION)
                    {
                        // TODO: !(root_node.is_param_decl() && is_some_symbol_assigned)
                        let flow_node = self.get_flow_node_of_node(location.id);
                        let narrow_ty = self.get_flow_ty_of_reference(
                            parent,
                            parent_ty_constraint,
                            Some(parent_ty_constraint),
                            None,
                            flow_node,
                        );
                        if narrow_ty.flags.contains(TypeFlags::NEVER) {
                            return self.never_ty;
                        }
                        // TODO:
                    }
                }
            }
        }
        ty
    }

    fn check_ident(&mut self, ident: &'cx ast::Ident) -> &'cx ty::Ty<'cx> {
        match ident.name {
            keyword::KW_UNDEFINED => return self.undefined_widening_ty,
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

        let local_or_export_symbol = self.get_export_symbol_of_value_symbol_if_exported(symbol);
        // TODO: move into name resolution.
        // TODO: dont duplicate check more than once.
        if self.symbol(local_or_export_symbol).flags.intersects(
            SymbolFlags::BLOCK_SCOPED_VARIABLE
                .union(SymbolFlags::CLASS)
                .union(SymbolFlags::ENUM),
        ) {
            self.check_resolved_block_scoped_var(ident, local_or_export_symbol);
        }

        let ty = self.get_narrowed_ty_of_symbol(local_or_export_symbol, ident);
        let assignment_kind = self
            .node_query(ident.id.module())
            .get_assignment_target_kind(ident.id);

        if assignment_kind != AssignmentKind::None && local_or_export_symbol != Symbol::ERR {
            let flags = self.binder.symbol(local_or_export_symbol).flags;
            if !flags.intersects(SymbolFlags::VARIABLE) {
                let ty = if flags.contains(SymbolFlags::CLASS) {
                    "class"
                } else if flags.contains(SymbolFlags::FUNCTION) {
                    "function"
                } else if flags.intersects(SymbolFlags::ENUM) {
                    "enum"
                } else if flags.intersects(SymbolFlags::NAMESPACE) {
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
        } else if let Some(decl) = local_or_export_symbol.opt_decl(self.binder) {
            decl
        } else {
            return ty;
        };

        let s = self.binder.symbol(local_or_export_symbol);
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
        let mut flow_container = self
            .node_query(ident.id.module())
            .get_control_flow_container(ident.id);
        let is_outer_variable = flow_container != decl_container;

        let is_param = self
            .p
            .node(self.node_query(decl.module()).get_root_decl(decl))
            .is_param_decl();
        // let is_outer_variable = flow_container != decl_container;
        let type_is_auto = ty == self.auto_ty || ty == self.auto_array_ty();
        let is_automatic_ty_is_non_null = type_is_auto
            && self
                .p
                .node(self.parent(ident.id).unwrap())
                .is_non_null_expr();

        loop {
            if flow_container != decl_container
                && (matches!(
                    self.p.node(flow_container),
                    ast::Node::FnExpr(_) | ast::Node::ArrowFnExpr(_)
                ) || self
                    .node_query(flow_container.module())
                    .is_object_lit_or_class_expr_method_or_accessor(flow_container))
                && (
                    (self.is_constant_variable(s) && ty != self.auto_array_ty())
                        || (self.is_parameter_or_mutable_local_variable(s))
                    // TODO: is_past_assignment
                )
            {
                flow_container = self
                    .node_query(flow_container.module())
                    .get_control_flow_container(flow_container);
            } else {
                break;
            }
        }
        let is_never_initialized = if let Some(v) = self.p.node(immediate_decl).as_var_decl() {
            v.init.is_none()
        } else {
            false
        };
        let assume_initialized = is_param
            || is_alias
            || (is_outer_variable && !is_never_initialized)
            || self.p.node_flags(decl).contains(ast::NodeFlags::AMBIENT)
            || (ty != self.auto_ty
                && ty != self.auto_array_ty()
                && (!self.config.strict_null_checks()
                    || ty
                        .flags
                        .intersects(TypeFlags::ANY_OR_UNKNOWN.union(TypeFlags::VOID))
                    || self
                        .node_query(ident.id.module())
                        .is_in_type_query(ident.id)
                    || self
                        .node_query(ident.id.module())
                        .is_in_ambient_or_type_node(ident.id)
                    || self
                        .p
                        .node(self.parent(ident.id).unwrap())
                        .is_export_named_spec()))
            || self
                .parent(ident.id)
                .is_some_and(|p| self.p.node(p).is_non_null_expr());
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
            self.get_optional_ty::<false>(ty)
        };

        let flow_ty =
            self.get_flow_ty_of_reference(ident.id, ty, Some(init_ty), Some(flow_container), None);
        let flow_ty = if is_automatic_ty_is_non_null {
            self.get_non_nullable_ty(flow_ty)
        } else {
            flow_ty
        };

        if (ty == self.auto_ty || ty == self.auto_array_ty())
            && self.is_evolving_array_op_target(ident.id)
        {
            if flow_ty == self.auto_ty || flow_ty == self.auto_array_ty() {
                if self.config.no_implicit_any() {
                    todo!()
                }
                return self.convert_auto_to_any(flow_ty);
            }
        } else if !assume_initialized
            && !ty.contains_undefined_ty()
            && flow_ty.contains_undefined_ty()
        {
            let error = errors::VariableXIsUsedBeforeBeingAssigned {
                span: ident.span,
                name: self.atoms.get(ident.name).to_string(),
            };
            self.push_error(Box::new(error));
            return ty;
        }
        if assignment_kind != AssignmentKind::None {
            self.get_base_ty_of_literal_ty(flow_ty)
        } else {
            flow_ty
        }
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
            ast::Node::ThisExpr(_) => "this".to_string(),
            ast::Node::PropAccessExpr(n) => pprint_prop_access_expr(n, &self.atoms),
            ast::Node::EleAccessExpr(n) => pprint_elem_access_expr(n, &self.atoms),
            _ => {
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
        let object_index_ty = self.get_index_ty(indexed_access_ty.object_ty, IndexFlags::empty());
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
        use bolt_ts_binder::FlowInNode::*;
        if let FnLike(n) = self.get_flow_in_node_of_node(f)
            && let Some(n) = n.end_flow_node
        {
            self.is_reachable_flow_node(n)
        } else {
            false
        }
    }

    fn get_yielded_ty_of_yield_expr(
        &mut self,
        n: &'cx ast::YieldExpr<'cx>,
        expr_ty: &'cx ty::Ty<'cx>,
        sent_ty: &'cx ty::Ty<'cx>,
        is_async: bool,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let error_node = n.expr.map(|expr| expr.id()).unwrap_or(n.id);
        let yield_ty = if n.asterisk.is_some() {
            let mode = if is_async {
                IterationUse::ASYNC_YIELD_STAR
            } else {
                IterationUse::YIELD_STAR
            };
            self.check_iterated_ty_or_element_ty(mode, expr_ty, sent_ty, Some(error_node))
        } else {
            expr_ty
        };
        if !is_async {
            Some(yield_ty)
        } else {
            // TODO: error_node
            self.get_awaited_ty(yield_ty)
        }
    }

    fn check_and_aggregate_yield_operand_tys(
        &mut self,
        f: ast::NodeID,
        body: &'cx ast::BlockStmt<'cx>,
        is_async: bool,
    ) -> (Vec<&'cx ty::Ty<'cx>>, Vec<&'cx ty::Ty<'cx>>) {
        let mut yield_tys = vec![];
        let mut next_tys = vec![];

        struct YieldExprVisitor<'a, 'cx> {
            checker: &'a mut TyChecker<'cx>,
            yield_tys: &'a mut Vec<&'cx ty::Ty<'cx>>,
            next_tys: &'a mut Vec<&'cx ty::Ty<'cx>>,
            is_async: bool,
        }

        impl<'a, 'cx> bolt_ts_ast_visitor::Visitor<'cx> for YieldExprVisitor<'a, 'cx> {
            fn visit_yield_expr(&mut self, y: &'cx bolt_ts_ast::YieldExpr) {
                // ======
                let mut yield_expr_ty = if let Some(expr) = y.expr {
                    let old_check_mode = self.checker.check_mode;
                    self.checker.check_mode = if let Some(check_mode) = old_check_mode {
                        Some(check_mode & !CheckMode::SKIP_GENERIC_FUNCTIONS)
                    } else {
                        None
                    };
                    let ret = self.checker.check_expr(expr);
                    self.checker.check_mode = old_check_mode;
                    ret
                } else {
                    self.checker.undefined_widening_ty
                };
                if y.expr
                    .is_some_and(|expr| self.checker.is_const_context(expr.id()))
                {
                    yield_expr_ty = self.checker.get_regular_ty_of_literal_ty(yield_expr_ty)
                }
                if let Some(ty) = self.checker.get_yielded_ty_of_yield_expr(
                    y,
                    yield_expr_ty,
                    self.checker.any_ty,
                    self.is_async,
                ) && !self.yield_tys.contains(&ty)
                {
                    self.yield_tys.push(ty);
                }
                let next_ty = if y.asterisk.is_some() {
                    let mode = if self.is_async {
                        IterationUse::ASYNC_YIELD_STAR
                    } else {
                        IterationUse::YIELD_STAR
                    };
                    let iteration_tys = self.checker.get_iteration_tys_of_iterable(
                        yield_expr_ty,
                        mode,
                        y.expr.map(|e| e.id()),
                    );
                    iteration_tys.map(|tys| tys.next_ty)
                } else {
                    self.checker.get_contextual_ty(y.id, None)
                };
                if let Some(next_ty) = next_ty
                    && !self.next_tys.contains(&next_ty)
                {
                    self.next_tys.push(next_ty);
                }
                // ======
                let Some(expr) = y.expr else {
                    return;
                };
                self.visit_expr(expr);
            }

            fn visit_enum_decl(&mut self, _: &'cx bolt_ts_ast::EnumDecl<'cx>) {}
            fn visit_interface_decl(&mut self, _: &'cx bolt_ts_ast::InterfaceDecl<'cx>) {}
            fn visit_module_decl(&mut self, _: &'cx bolt_ts_ast::ModuleDecl<'cx>) {}
            fn visit_type_alias_decl(&mut self, _: &'cx bolt_ts_ast::TypeAliasDecl<'cx>) {}
            // TODO: is_fn_like
            // TODO: exclude type
        }

        let mut visitor = YieldExprVisitor {
            checker: self,
            yield_tys: &mut yield_tys,
            next_tys: &mut next_tys,
            is_async,
        };

        bolt_ts_ast_visitor::visit_block_stmt(&mut visitor, body);
        drop(visitor);
        (yield_tys, next_tys)
    }

    fn check_and_aggregate_ret_expr_tys(
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

    fn is_array_or_tuple_or_intersection(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.kind
            .as_intersection()
            .map(|i| i.tys.iter().all(|t| self.is_array_or_tuple(t)))
            .unwrap_or_default()
    }

    fn is_known_prop(
        &mut self,
        target: &'cx ty::Ty<'cx>,
        name: SymbolName,
        is_comparing_jsx_attributes: bool,
    ) -> bool {
        match target.kind {
            ty::TyKind::Object(_) => {
                if self.get_prop_of_object_ty(target, name).is_some()
                    || self
                        .get_applicable_index_info_for_name(target, name)
                        .is_some()
                    || name.is_late_bound()
                        && self.get_index_info_of_ty(target, self.string_ty).is_some()
                {
                    return true;
                }
            }
            ty::TyKind::Substitution(n) => {
                return self.is_known_prop(n.base_ty, name, is_comparing_jsx_attributes);
            }
            ty::TyKind::Union(ty::UnionTy { tys, .. })
            | ty::TyKind::Intersection(ty::IntersectionTy { tys, .. }) => {
                if Self::is_excess_property_check_target(target) {
                    for t in *tys {
                        if self.is_known_prop(t, name, is_comparing_jsx_attributes) {
                            return true;
                        }
                    }
                }
            }
            _ => {}
        }

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

    fn has_common_props(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
        is_comparing_jsx_attributes: bool,
    ) -> bool {
        for prop in self.get_props_of_ty(source) {
            let name = self.symbol(*prop).name;
            if self.is_known_prop(target, name, is_comparing_jsx_attributes) {
                return true;
            }
        }
        false
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
            ) && !flags.contains(SymbolFlags::CLASS)
                && !self.ty_has_call_or_ctor_sigs(ty)
        } else {
            let object_flags = ty.get_object_flags();
            // TODO: reversed type
            object_flags.contains(ObjectFlags::OBJECT_REST_TYPE)
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

    fn get_key_prop_name(&mut self, ty: &'cx ty::Ty<'cx>) -> Option<SymbolName> {
        let u = ty.kind.expect_union();
        let tys = u.tys;
        if tys.len() < 10
            || ty.get_object_flags().contains(ObjectFlags::PRIMITIVE_UNION)
            || tys.iter().fold(0, |count, ty| {
                let flags = ty.flags;
                if flags.intersects(TypeFlags::OBJECT.union(TypeFlags::INSTANTIABLE_NON_PRIMITIVE))
                {
                    count + 1
                } else {
                    count
                }
            }) < 10
        {
            return None;
        }

        if let Some(ret) = self.union_ty_links_arena[u.union_ty_links].get_key_prop_name() {
            return Some(ret);
        }
        let key_prop_name = tys.iter().find_map(|ty| {
            if ty
                .flags
                .intersects(TypeFlags::OBJECT.union(TypeFlags::INSTANTIABLE_NON_PRIMITIVE))
            {
                let props = self.get_props_of_ty(ty);
                props.iter().find_map(|prop| {
                    let ty = self.get_type_of_symbol(*prop);
                    ty.is_unit().then(|| self.symbol(*prop).name)
                })
            } else {
                None
            }
        });
        let map_by_key_prop = key_prop_name.and_then(|name| self.map_tys_by_key_prop(tys, name));
        // TODO:
        // let map_by_key_prop = key_prop_name.map(|name| {

        // })

        None
    }

    fn map_tys_by_key_prop(
        &mut self,
        tys: ty::Tys<'cx>,
        key_prop_name: SymbolName,
    ) -> Option<FxHashMap<SymbolID, &'cx ty::Ty<'cx>>> {
        None
        // TODO:
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
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        debug_assert!(ty.kind.is_union());
        let key_prop_name = self.get_key_prop_name(ty);
        let prop_ty = key_prop_name.and_then(|name| self.get_ty_of_prop_of_ty(ty, name));
        prop_ty.and_then(|prop_ty| self.get_constituent_ty_for_key_ty(ty, prop_ty))
    }

    fn empty_array<T>(&self) -> &'cx [T] {
        cast_empty_array(self.empty_array)
    }

    fn create_ty_mapper(
        &self,
        sources: ty::Tys<'cx>,
        targets: ty::Tys<'cx>,
    ) -> &'cx ty::TyMapper<'cx> {
        debug_assert!(sources.len() >= targets.len(), "{sources:#?} {targets:#?}",);
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
        assert!(
            sources.len() >= targets.map(|t| t.len()).unwrap_or_default(),
            "{sources:#?} {targets:#?}",
        );
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
        ty.flags.contains(TypeFlags::ANY)
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
                        let empty_and_other_union = this.get_union_ty::<false>(
                            &[this.empty_object_ty(), other_ty],
                            ty::UnionReduction::Lit,
                            None,
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
        if t == self.missing_ty {
            s == t
        } else {
            (t.flags.contains(TypeFlags::STRING) && s.flags.contains(TypeFlags::STRING_LITERAL))
                || (t.flags.contains(TypeFlags::NUMBER)
                    && s.flags.contains(TypeFlags::NUMBER_LITERAL))
                || self.is_type_identical_to(s, t)
        }
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
        if s.kind.is_object()
            && t.kind.is_object()
            && s.symbol()
                .is_some_and(|s_symbol| Some(s_symbol) == t.symbol())
        {
            true
        } else if let Some(s_alias_symbol) = s.alias_symbol()
            && s.alias_ty_arguments().is_some()
        {
            Some(s_alias_symbol) == t.alias_symbol()
        } else {
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
            ast::Node::ParenExpr(t) => return self.is_matching_reference(source, t.expr.id()),
            ast::Node::NonNullExpr(t) => return self.is_matching_reference(source, t.expr.id()),
            _ => (),
        }

        let is_same = |source: &'cx ast::Ident, target: ast::NodeID| {
            let s = self.resolve_symbol_by_ident(source);
            let s = self.get_export_symbol_of_value_symbol_if_exported(s);
            s == self.get_symbol_of_decl(target)
        };

        match self.p.node(source) {
            ast::Node::Ident(s_ident) => {
                if self
                    .node_query(source.module())
                    .is_this_in_type_query(source)
                {
                    t.is_this_expr()
                } else if let Some(t_ident) = t.as_ident()
                    && !keyword::is_prim_value_name(t_ident.name)
                {
                    self.resolve_symbol_by_ident(s_ident) == self.resolve_symbol_by_ident(t_ident)
                } else if let Some(t_v) = t.as_var_decl() {
                    match t_v.name.kind {
                        ast::BindingKind::Ident(_) => is_same(s_ident, t_v.id),
                        ast::BindingKind::ObjectPat(_) | ast::BindingKind::ArrayPat(_) => {
                            unreachable!()
                        }
                    }
                } else if let Some(t) = t.as_object_binding_elem() {
                    is_same(s_ident, t.id)
                } else if let Some(t) = t.as_array_binding() {
                    is_same(s_ident, t.id)
                } else {
                    false
                }
            }
            ast::Node::ThisExpr(_) => t.is_this_expr(),
            ast::Node::SuperExpr(_) => t.is_super_expr(),
            ast::Node::PropAccessExpr(s_prop_access) => {
                let source_prop_name = SymbolName::Atom(s_prop_access.name.name);
                let (target_access_expr, target_prop_name) = match t {
                    ast::Node::PropAccessExpr(t_prop_access) => (
                        Some(t_prop_access.expr.id()),
                        Some(SymbolName::Atom(t_prop_access.name.name)),
                    ),
                    ast::Node::EleAccessExpr(t_element_access) => (
                        Some(t_element_access.expr.id()),
                        self.try_get_element_access_name(t_element_access),
                    ),
                    _ => (None, None),
                };
                if let Some(target_prop_name) = target_prop_name {
                    return source_prop_name == target_prop_name
                        && self.is_matching_reference(
                            s_prop_access.expr.id(),
                            target_access_expr.unwrap(),
                        );
                }
                false
            }
            ast::Node::EleAccessExpr(s_element_access) => {
                if let Some(source_prop_name) = self.try_get_element_access_name(s_element_access) {
                    let (target_access_expr, target_prop_name) = match t {
                        ast::Node::PropAccessExpr(t_prop_access) => (
                            Some(t_prop_access.expr.id()),
                            Some(SymbolName::Atom(t_prop_access.name.name)),
                        ),
                        ast::Node::EleAccessExpr(t_element_access) => (
                            Some(t_element_access.expr.id()),
                            self.try_get_element_access_name(t_element_access),
                        ),
                        _ => (None, None),
                    };
                    if let Some(target_prop_name) = target_prop_name {
                        return source_prop_name == target_prop_name
                            && self.is_matching_reference(
                                s_element_access.expr.id(),
                                target_access_expr.unwrap(),
                            );
                    }

                    if let ast::Node::EleAccessExpr(t_element_access) = t
                        && let ast::ExprKind::Ident(s_element_access_arg) =
                            s_element_access.arg.kind
                        && let ast::ExprKind::Ident(t_element_access_arg) =
                            t_element_access.arg.kind
                    {
                        let s_symbol = self.resolve_symbol_by_ident(s_element_access_arg);
                        let t_symbol = self.resolve_symbol_by_ident(t_element_access_arg);
                        if s_symbol == t_symbol
                            && let s_s = self.binder.symbol(s_symbol)
                            && self.is_constant_variable(s_s)
                        {
                            // is_parameter_or_mutable_local_variable(s_) && !self.is_symbol_assign(s_s)
                            return self.is_matching_reference(
                                s_element_access.expr.id(),
                                t_element_access.expr.id(),
                            );
                        }
                    }
                }

                false
            }
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

    fn is_prototype_prop(&self, symbol: SymbolID) -> bool {
        let flags = self.symbol(symbol).flags;
        if flags.contains(SymbolFlags::METHOD)
            || self
                .get_check_flags(symbol)
                .contains(CheckFlags::SYNTHETIC_METHOD)
        {
            true
        } else {
            // TODO: is_in_js_file
            false
        }
    }

    fn is_ty_subset_of(&mut self, source: &'cx ty::Ty<'cx>, target: &'cx ty::Ty<'cx>) -> bool {
        source == target
            || source.flags.intersects(TypeFlags::NEVER)
            || target
                .kind
                .as_union()
                .is_some_and(|target| self.is_ty_subset_of_union(source, target))
    }

    fn is_ty_subset_of_union(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::UnionTy<'cx>,
    ) -> bool {
        if let Some(source) = source.kind.as_union() {
            for s in source.tys {
                if !target.tys.contains(s) {
                    return false;
                }
            }
            return true;
        }
        // if (source.flags & TypeFlags.EnumLike && getBaseTypeOfEnumLikeType(source as LiteralType) === target) {
        //     return true;
        // }
        target.tys.contains(&source)
    }

    fn get_constituent_ty_for_key_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        key_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        debug_assert!(ty.kind.is_union());
        let id = self.get_regular_ty_of_literal_ty(key_ty);
        let result = self
            .constituent_map_for_union_ty
            .get(&ty.id)
            .and_then(|map| map.get(id))?;

        if self.unknown_ty.eq(result) {
            None
        } else {
            Some(result)
        }
    }

    fn create_flow_ty(&self, ty: &'cx ty::Ty<'cx>, incomplete: bool) -> FlowTy<'cx> {
        if incomplete {
            let ty = if ty.flags.contains(TypeFlags::NEVER) {
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
        &self,
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
            || (self.is_array_like_ty(ty)
                && self
                    .get_ty_of_prop_of_ty(ty, SymbolName::Atom(keyword::IDENT_LENGTH))
                    .is_some_and(|length_ty| {
                        self.every_type(length_ty, |_, t| {
                            t.flags.intersects(TypeFlags::NUMBER_LITERAL)
                        })
                    }))
            || self
                .get_prop_of_ty(ty, SymbolName::EleNum((0.).into()))
                .is_some()
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
            let prop = self.get_literal_ty_from_prop(*prop, include, false);
            cb(self, prop)
        }
        if ty.flags.contains(TypeFlags::ANY) {
            cb(self, self.string_ty);
        } else {
            let infos = self.get_index_infos_of_ty(ty);
            for info in infos {
                if !string_only
                    || info
                        .key_ty
                        .flags
                        .intersects(TypeFlags::STRING.union(TypeFlags::TEMPLATE_LITERAL))
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
        self.get_union_ty::<false>(&v, ty::UnionReduction::Lit, None, None, None)
    }

    fn get_lower_bound_of_key_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if let Some(index_ty) = ty.kind.as_index_ty() {
            let t = self.get_apparent_ty(index_ty.ty);
            if t.kind.is_generic_tuple_type() {
                let tuple = t.as_tuple().unwrap();
                self.get_known_keys_of_tuple_ty(tuple)
            } else {
                self.get_index_ty(t, ty::IndexFlags::empty())
            }
        } else if let Some(c) = ty.kind.as_cond_ty() {
            if c.root.is_distributive
                && let constraint = self.get_lower_bound_of_key_ty(c.check_ty)
                && constraint != c.check_ty
            {
                let mapper = self.prepend_ty_mapping(c.root.check_ty, constraint, c.mapper);
                self.get_cond_ty_instantiation::<false>(ty, mapper, None, None)
            } else {
                ty
            }
        } else if ty.kind.is_union() {
            self.map_ty(ty, |this, t| Some(this.get_lower_bound_of_key_ty(t)), true)
                .unwrap()
        } else if let Some(i) = ty.kind.as_intersection() {
            let tys = i.tys;
            if tys.len() == 2
                && tys[0].flags.intersects(
                    TypeFlags::STRING
                        .union(TypeFlags::NUMBER)
                        .union(TypeFlags::BIG_INT),
                )
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
        ty == self.error_ty || (ty.flags.contains(TypeFlags::ANY) && ty.alias_symbol().is_some())
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
                    self.get_indexed_access_ty(t, self.number_ty, None, None, None, None)
                } else {
                    t
                };
                element_tys.push(t);
            }
            Some(if writing {
                self.get_intersection_ty(&element_tys, IntersectionFlags::None, None, None)
            } else {
                self.get_union_ty::<false>(
                    &element_tys,
                    if no_reductions {
                        ty::UnionReduction::None
                    } else {
                        ty::UnionReduction::Lit
                    },
                    None,
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
        ty.kind.as_object_mapped().is_some_and(|m| {
            let constraint_ty = self.get_constraint_ty_from_mapped_ty(m);
            if self.is_generic_index_ty(constraint_ty) {
                true
            } else {
                self.get_name_ty_from_mapped_ty(m).is_some_and(|name_ty| {
                    let source = self.get_ty_param_from_mapped_ty(m);
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

    fn is_generic_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        !self.get_generic_object_flags(ty).is_empty()
    }

    fn get_normalized_ty(
        &mut self,
        mut ty: &'cx ty::Ty<'cx>,
        kind: SimplifiedKind,
    ) -> &'cx ty::Ty<'cx> {
        let should_normalize_intersection = |this: &mut Self, ty: &'cx ty::IntersectionTy<'cx>| {
            let mut has_instantiable = false;
            let mut has_nullable_or_empty = false;
            for t in ty.tys {
                has_instantiable |= t.flags.intersects(TypeFlags::INSTANTIABLE);
                has_nullable_or_empty |=
                    t.flags.intersects(TypeFlags::NULLABLE) || this.is_empty_anonymous_object_ty(t);
                if has_instantiable && has_nullable_or_empty {
                    return true;
                }
            }
            false
        };
        loop {
            let t = if self.is_fresh_literal_ty(ty) {
                self.get_regular_ty(ty).unwrap()
            } else if ty.kind.is_generic_tuple_type() {
                // get normalized tuple type
                let reduced = self.get_reduced_ty(ty);
                if reduced != ty {
                    reduced
                } else if let Some(i) = ty.kind.as_intersection()
                    && should_normalize_intersection(self, i)
                    && let normalized_tys = self
                        .same_map_tys(Some(i.tys), |this, t, _| this.get_normalized_ty(t, kind))
                        .unwrap()
                    && !std::ptr::eq(normalized_tys, i.tys)
                {
                    self.get_intersection_ty(normalized_tys, IntersectionFlags::None, None, None)
                } else {
                    ty
                }
            } else if ty.get_object_flags().contains(ObjectFlags::REFERENCE) {
                let (node, target) = match ty.kind.expect_object().kind {
                    ty::ObjectTyKind::Reference(t) => (t.node, t.target),
                    ty::ObjectTyKind::Tuple(tup) => (None, tup.ty),
                    _ => unreachable!(),
                };
                if node.is_some() {
                    let type_arguments = self.get_ty_arguments(ty);
                    self.create_reference_ty(target, Some(type_arguments), ObjectFlags::empty())
                } else {
                    self.get_single_base_for_non_augmenting_subtype(ty)
                        .unwrap_or(ty)
                }
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
            self.get_union_ty::<false>(
                &[ty, self.undefined_ty],
                ty::UnionReduction::Lit,
                None,
                None,
                None,
            )
        } else if missing == TypeFlags::NULL {
            self.get_union_ty::<false>(
                &[ty, self.null_ty],
                ty::UnionReduction::Lit,
                None,
                None,
                None,
            )
        } else {
            self.get_union_ty::<false>(
                &[ty, self.undefined_ty, self.null_ty],
                ty::UnionReduction::Lit,
                None,
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
            self.get_union_ty::<false>(primary_tys, ty::UnionReduction::Lit, None, None, None)
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
        let source = self.get_ty_param_from_mapped_ty(mapped_ty);
        let mapper = self.alloc(TyMapper::make_unary(source, index_ty));
        let template_mapper = self.combine_ty_mappers(mapped_ty.mapper, mapper);
        let instantiated_template_ty = {
            let m = mapped_ty
                .target
                .map(|t| t.kind.expect_object_mapped())
                .unwrap_or(mapped_ty);
            let template_ty = self.get_template_ty_from_mapped_ty(m);
            self.instantiate_ty_worker(template_ty, template_mapper)
        };
        // TODO: is_optional
        let is_optional = false;
        self.add_optionality::<true>(instantiated_template_ty, is_optional)
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
        if ms.contains(ast::MappedTyModifiers::EXCLUDE_OPTIONAL) {
            -1
        } else if ms.contains(ast::MappedTyModifiers::INCLUDE_OPTIONAL) {
            1
        } else {
            0
        }
    }

    fn get_combined_mapped_ty_optionality(&mut self, ty: &'cx ty::Ty<'cx>) -> i32 {
        if ty.get_object_flags().contains(ObjectFlags::MAPPED) {
            let m = ty.kind.expect_object_mapped();
            let n = self.get_mapped_ty_optionality(m);
            if n != 0 {
                n
            } else {
                let t = self.get_modifiers_ty_from_mapped_ty(m);
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
            && (self.is_ty_param_possibly_referenced(root.check_ty, root.node.true_ty.id())
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
                debug_assert!(ty.kind.is_param());
                Self {
                    tp: ty,
                    checker,
                    contain_reference: false,
                }
            }
        }
        impl<'cx> bolt_ts_ast_visitor::Visitor<'cx> for ContainReferenceVisitor<'cx, '_> {
            fn visit_this_ty(&mut self, _: &'cx bolt_ts_ast::ThisTy) {
                let t = self.tp.kind.expect_param();
                if t.is_this_ty {
                    self.contain_reference = true;
                }
            }
            fn visit_ident(&mut self, n: &'cx bolt_ts_ast::Ident) {
                if self.contain_reference {
                    return;
                }
                let t = self.tp.kind.expect_param();
                let nq = self.checker.node_query(n.id.module());
                if !t.is_this_ty
                    && nq.is_part_of_ty_node(n.id)
                    && nq.maybe_ty_param_reference(n.id)
                    && self.checker.get_ty_from_ident(n) == self.tp
                {
                    self.contain_reference = true;
                }
            }
            fn visit_typeof_ty(&mut self, n: &'cx bolt_ts_ast::TypeofTy<'cx>) {
                if self.contain_reference {
                    return;
                }
                let entity_name = n.name;
                let first_identifier = entity_name.get_first_identifier();
                if first_identifier.name != keyword::KW_THIS {
                    let first_identifier_symbol = self.checker.final_res(first_identifier.id);
                    let tp_symbol = self.checker.symbol(self.tp.symbol().unwrap());
                    let tp_decls = tp_symbol.decls.as_ref().unwrap();
                    debug_assert_eq!(tp_decls.len(), 1);
                    let tp_decl = tp_decls[0];
                    let tp_scope = if self.checker.p.node(tp_decl).is_ty_param() {
                        self.checker.parent(tp_decl)
                    } else if self.tp.kind.is_this_ty_param() {
                        Some(tp_decl)
                    } else {
                        None
                    };
                    if let Some(tp_scope) = tp_scope
                        && let s = self.checker.symbol(first_identifier_symbol)
                        && let Some(decls) = s.decls.as_ref()
                    {
                        if decls.into_iter().any(|id_decl| {
                            self.checker
                                .node_query(id_decl.module())
                                .is_descendant_of(*id_decl, tp_scope)
                        }) || n.ty_args.is_some_and(|ty_args| {
                            ty_args.list.iter().any(|ty| {
                                let mut v = ContainReferenceVisitor::new(self.tp, self.checker);
                                bolt_ts_ast_visitor::visit_ty(&mut v, ty);
                                v.contain_reference
                            })
                        }) {
                            self.contain_reference = true;
                        }
                        return;
                    }
                }

                self.contain_reference = true;
            }
            fn visit_method_signature(&mut self, n: &'cx bolt_ts_ast::MethodSignature<'cx>) {
                if self.contain_reference {
                    return;
                }
                if n.ty_params.is_some_and(|ty_params| {
                    ty_params.iter().any(|ty_param| {
                        let mut v = ContainReferenceVisitor::new(self.tp, self.checker);
                        bolt_ts_ast_visitor::visit_ty_param(&mut v, ty_param);
                        v.contain_reference
                    })
                }) || n.params.iter().any(|param| {
                    let mut v = ContainReferenceVisitor::new(self.tp, self.checker);
                    bolt_ts_ast_visitor::visit_param_decl(&mut v, param);
                    v.contain_reference
                }) || n.ty.is_some_and(|ty| {
                    let mut v = ContainReferenceVisitor::new(self.tp, self.checker);
                    bolt_ts_ast_visitor::visit_ty(&mut v, ty);
                    v.contain_reference
                }) {
                    self.contain_reference = true;
                }
            }
            fn visit_class_method_elem(&mut self, n: &'cx bolt_ts_ast::ClassMethodElem<'cx>) {
                if self.contain_reference {
                    return;
                }
                if n.ty.is_none() && n.body.is_some()
                    || n.ty_params.is_some_and(|ty_params| {
                        ty_params.iter().any(|ty_param| {
                            let mut v = ContainReferenceVisitor::new(self.tp, self.checker);
                            bolt_ts_ast_visitor::visit_ty_param(&mut v, ty_param);
                            v.contain_reference
                        })
                    })
                    || n.params.iter().any(|param| {
                        let mut v = ContainReferenceVisitor::new(self.tp, self.checker);
                        bolt_ts_ast_visitor::visit_param_decl(&mut v, param);
                        v.contain_reference
                    })
                    || n.ty.is_some_and(|ty| {
                        let mut v = ContainReferenceVisitor::new(self.tp, self.checker);
                        bolt_ts_ast_visitor::visit_ty(&mut v, ty);
                        v.contain_reference
                    })
                {
                    self.contain_reference = true;
                }
            }
            fn visit_object_method_member(&mut self, n: &'cx bolt_ts_ast::ObjectMethodMember<'cx>) {
                if self.contain_reference {
                    return;
                }
                if n.ty_params.is_some_and(|ty_params| {
                    ty_params.iter().any(|ty_param| {
                        let mut v = ContainReferenceVisitor::new(self.tp, self.checker);
                        bolt_ts_ast_visitor::visit_ty_param(&mut v, ty_param);
                        v.contain_reference
                    })
                }) || n.params.iter().any(|param| {
                    let mut v = ContainReferenceVisitor::new(self.tp, self.checker);
                    bolt_ts_ast_visitor::visit_param_decl(&mut v, param);
                    v.contain_reference
                }) || n.ty.is_some_and(|ty| {
                    let mut v = ContainReferenceVisitor::new(self.tp, self.checker);
                    bolt_ts_ast_visitor::visit_ty(&mut v, ty);
                    v.contain_reference
                }) {
                    self.contain_reference = true;
                }
            }
        }

        let tp = ty.kind.expect_param();
        let Some(decls) = &self.symbol(tp.symbol).decls else {
            return true;
        };
        if decls.len() != 1 {
            return true;
        }
        let decl = decls[0];
        let Some(container) = self.parent(decl) else {
            return true;
        };
        let mut n = node;
        while n != container {
            let node = self.p.node(n);
            if node.is_block_stmt()
                || node.as_cond_ty().is_some_and(|c| {
                    let mut v = ContainReferenceVisitor::new(ty, self);
                    bolt_ts_ast_visitor::visit_ty(&mut v, c.extends_ty);
                    v.contain_reference
                })
            {
                return true;
            };
            let Some(next) = self.parent(n) else {
                return true;
            };
            n = next;
        }
        let n = self.p.node(node);
        let mut v = ContainReferenceVisitor::new(ty, self);
        bolt_ts_ast_visitor::visit_node(&mut v, &n);
        v.contain_reference
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
                let val_ty =
                    self.get_union_ty::<false>(&tys, ty::UnionReduction::Lit, None, None, None);
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
            self.create_transient_symbol(name, flags, links, decls, None, None)
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
            .unwrap_or_default()
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
                    .intersects(SymbolFlags::NAMESPACE.union(SymbolFlags::ENUM))
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
                if s.flags.contains(SymbolFlags::TYPE_ALIAS) && exported_declarations_count <= 2 {
                    return None;
                }
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

    fn any_base_type_index_info(&self) -> &'cx ty::IndexInfo<'cx> {
        self.any_base_type_index_info.get().unwrap()
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
            let ty_params = self.get_sig_links(sig.id).get_ty_params();
            let min = self.get_min_ty_arg_count(ty_params);
            if ty_arg_count >= min
                && ty_params.is_none_or(|ty_params| ty_arg_count <= ty_params.len())
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
                if self.get_sig_links(sig.id).get_ty_params().is_some() {
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
            target_non_rest_count
        } else {
            usize::min(source_count, target_non_rest_count)
        };
        if let Some(source_this_ty) = self.get_this_ty_of_sig(source)
            && let Some(target_this_ty) = self.get_this_ty_of_sig(target)
        {
            callback(self, source_this_ty, target_this_ty);
        }
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
                let a =
                    self.create_anonymous_ty(ty.symbol(), ObjectFlags::empty(), None, None, None);
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

    fn is_const_mapped_ty(&mut self, ty: &'cx ty::MappedTy<'cx>, depth: u8) -> bool {
        self.get_homomorphic_ty_var(ty)
            .is_some_and(|ty_var| self.is_const_ty_variable_worker(ty_var, depth))
    }

    fn is_const_ty_variable_worker(&mut self, ty: &'cx ty::Ty<'cx>, depth: u8) -> bool {
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
            Union(ty::UnionTy { tys, .. }) | Intersection(ty::IntersectionTy { tys, .. }) => tys
                .iter()
                .any(|ty| self.is_const_ty_variable_worker(ty, depth)),
            IndexedAccess(ty) => self.is_const_ty_variable_worker(ty.object_ty, depth + 1),
            Cond(_) => self
                .get_constraint_of_cond_ty(ty)
                .is_some_and(|ty| self.is_const_ty_variable_worker(ty, depth + 1)),
            Substitution(ty) => self.is_const_ty_variable_worker(ty.base_ty, depth),
            Object(object_ty) => {
                if let ty::ObjectTyKind::Mapped(m) = object_ty.kind {
                    self.is_const_mapped_ty(m, depth)
                } else if let Some(tup) = object_ty.kind.as_generic_tuple_type() {
                    let tys = self.get_element_tys(ty);
                    tys.iter().enumerate().any(|(i, t)| {
                        tup.element_flags[i].contains(ElementFlags::VARIADIC)
                            && self.is_const_ty_variable_worker(t, depth)
                    })
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn is_const_ty_variable(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        self.is_const_ty_variable_worker(ty, 0)
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

    fn try_get_element_access_name(
        &mut self,
        n: &'cx ast::EleAccessExpr<'cx>,
    ) -> Option<SymbolName> {
        match n.arg.kind {
            ast::ExprKind::StringLit(n) => Some(SymbolName::Atom(n.val)),
            ast::ExprKind::NoSubstitutionTemplateLit(n) => Some(SymbolName::Atom(n.val)),
            ast::ExprKind::NumLit(n) => Some(SymbolName::EleNum(n.val.into())),
            _ if n.arg.is_entity_name_expr() => self.try_get_name_from_entity_name_expr(n.arg),
            _ => None,
        }
    }

    fn try_get_name_from_entity_name_expr(&mut self, n: &'cx ast::Expr<'cx>) -> Option<SymbolName> {
        let (id, symbol) = match n.kind {
            ast::ExprKind::Ident(n) => (n.id, self.final_res(n.id)),
            ast::ExprKind::PropAccess(n) => (
                n.id,
                self.resolve_qualified_name_like::<true, false>(
                    n.expr.id(),
                    n.name,
                    SymbolFlags::VALUE,
                ),
            ),
            _ => unreachable!(),
        };
        if symbol == Symbol::ERR {
            return None;
        }
        let s = self.symbol(symbol);
        if !(self.is_constant_variable(s) || s.flags.contains(SymbolFlags::ENUM_MEMBER)) {
            return None;
        }
        let decl = s.value_decl?;
        let decl_node = self.p.node(decl);
        if let Some(ty) = decl_node.ty_anno().map(|ty| self.get_ty_from_type_node(ty))
            && let Some(name) = self.try_get_name_from_ty(ty)
        {
            return Some(name);
        }

        if decl_node.has_only_expr_init()
            && self.is_block_scoped_name_declared_before_use(decl, id, n.span())
        {
            if let Some(init) = decl_node.initializer() {
                let init_ty = match self.p.node(self.parent(decl).unwrap()) {
                    ast::Node::ArrayBinding(_) => {
                        // TODO:
                        return None;
                    }
                    ast::Node::ObjectBindingElem(_) => {
                        // TODO:
                        return None;
                    }
                    _ => self.get_ty_of_expr(init),
                };
                return self.try_get_name_from_ty(init_ty);
            } else if let Some(enum_member) = decl_node.as_enum_member() {
                return Some(prop_name(enum_member.name));
            }
        }

        None
    }

    fn get_accessed_prop_name(&mut self, access: ast::NodeID) -> Option<SymbolName> {
        match self.p.node(access) {
            ast::Node::PropAccessExpr(n) => Some(SymbolName::Atom(n.name.name)),
            ast::Node::EleAccessExpr(n) => self.try_get_element_access_name(n),
            ast::Node::ParamDecl(_) => {
                let parent = self.parent(access).unwrap();
                let params = self.p.node(parent).params().unwrap();
                Some(param_index_in_parameter_list(access, params))
            }
            ast::Node::ObjectBindingElem(n) => {
                // let parent = self.parent(access).unwrap();
                // let pat = self.p.node(parent).expect_object_pat();
                self.get_literal_prop_name(&n.name.name())
            }
            // TODO: array binding
            _ => None,
        }
    }

    fn get_literal_prop_name(&mut self, name: &ast::PropNameKind<'cx>) -> Option<SymbolName> {
        let ty = self.get_literal_ty_from_prop_name(name);
        match ty.kind {
            ty::TyKind::StringLit(lit) => Some(SymbolName::Atom(lit.val)),
            ty::TyKind::NumberLit(lit) => Some(SymbolName::EleNum(lit.val)),
            ty::TyKind::BigIntLit(lit) => todo!(),
            _ => None,
        }
    }

    fn get_ty_of_prop_or_index_sig_of_ty(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        name: SymbolName,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(prop) = self.get_ty_of_prop_of_ty(ty, name) {
            return Some(prop);
        }
        let prop_ty = self.get_applicable_index_info_for_name(ty, name)?;
        Some(self.add_optionality::<true>(prop_ty.val_ty, true))
    }

    fn extract_tys_of_kind(&mut self, ty: &'cx ty::Ty<'cx>, kind: TypeFlags) -> &'cx ty::Ty<'cx> {
        self.filter_type(ty, |_, t| t.flags.intersects(kind))
    }

    fn exclude_props(
        &self,
        props: &[SymbolID],
        excludes: &FxHashSet<SymbolName>,
    ) -> impl Iterator<Item = SymbolID> {
        props.into_iter().filter_map(|prop| {
            let name = self.symbol(*prop).name;
            if excludes.contains(&name) {
                None
            } else {
                Some(*prop)
            }
        })
    }

    fn insert_union_constituent_map(
        &mut self,
        union_ty: &'cx ty::Ty<'cx>,
        map_by_key_prop: FxHashMap<&'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>>,
    ) {
        debug_assert!(union_ty.kind.is_union());
        let prev = self
            .union_ty_constituent_map
            .insert(union_ty.id, map_by_key_prop);
        debug_assert!(prev.is_none());
    }

    fn get_union_constituent_map(
        &mut self,
        union_ty: &'cx ty::Ty<'cx>,
    ) -> Option<&FxHashMap<&'cx ty::Ty<'cx>, &'cx ty::Ty<'cx>>> {
        debug_assert!(union_ty.kind.is_union());
        self.union_ty_constituent_map.get(&union_ty.id)
    }

    fn replace_primitives_with_literals(
        &mut self,
        ty_with_primitives: &'cx ty::Ty<'cx>,
        ty_with_literals: &'cx ty::Ty<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        if ty_with_primitives.maybe_type_of_kind(
            TypeFlags::STRING
                .union(TypeFlags::TEMPLATE_LITERAL)
                .union(TypeFlags::NUMBER)
                .union(TypeFlags::BIG_INT),
        ) && ty_with_literals.maybe_type_of_kind(
            TypeFlags::STRING_LITERAL
                .union(TypeFlags::TEMPLATE_LITERAL)
                .union(TypeFlags::STRING_MAPPING)
                .union(TypeFlags::NUMBER_LITERAL)
                .union(TypeFlags::BIG_INT_LITERAL),
        ) {
            self.map_ty(
                ty_with_primitives,
                |this, t| {
                    Some(if t.flags.contains(TypeFlags::STRING) {
                        this.extract_tys_of_kind(
                            ty_with_literals,
                            TypeFlags::STRING
                                .union(TypeFlags::STRING_LITERAL)
                                .union(TypeFlags::TEMPLATE_LITERAL)
                                .union(TypeFlags::STRING_MAPPING),
                        )
                    } else if t.is_pattern_lit_ty()
                        && !ty_with_literals.maybe_type_of_kind(
                            TypeFlags::STRING
                                .union(TypeFlags::TEMPLATE_LITERAL)
                                .union(TypeFlags::STRING_MAPPING),
                        )
                    {
                        this.extract_tys_of_kind(ty_with_literals, TypeFlags::STRING_LITERAL)
                    } else if t.flags.contains(TypeFlags::NUMBER) {
                        this.extract_tys_of_kind(
                            ty_with_literals,
                            TypeFlags::NUMBER.union(TypeFlags::NUMBER_LITERAL),
                        )
                    } else if t.flags.contains(TypeFlags::BIG_INT) {
                        this.extract_tys_of_kind(
                            ty_with_literals,
                            TypeFlags::BIG_INT.union(TypeFlags::BIG_INT_LITERAL),
                        )
                    } else {
                        t
                    })
                },
                false,
            )
            .unwrap()
        } else {
            ty_with_primitives
        }
    }

    fn check_ty_param_lists_identical(&mut self, symbol: SymbolID) {
        let s = self.binder.symbol(symbol);
        let Some(decls) = s.decls.as_ref() else {
            return;
        };
        if decls.len() <= 1 {
            return;
        }
        if self
            .get_symbol_links(symbol)
            .get_ty_params_checked()
            .unwrap_or_default()
        {
            return;
        }
        self.get_mut_symbol_links(symbol)
            .set_ty_params_checked(true);
        let s = self.binder.symbol(symbol);
        let decls: Vec<ast::NodeID> = s
            .decls
            .as_ref()
            .unwrap()
            .iter()
            .filter_map(|&decl| {
                let n = self.p.node(decl);
                matches!(n, ast::Node::ClassDecl(_) | ast::Node::InterfaceDecl(_)).then_some(decl)
            })
            .collect();
        let ty = self.get_declared_ty_of_symbol(symbol);
        let Some(i) = ty.as_class_or_interface_ty() else {
            unreachable!()
        };
        if !self.are_ty_params_identical(
            &decls,
            i.local_ty_params,
            Self::get_effective_ty_param_decls,
        ) {
            let s = self.binder.symbol(symbol);
            for decl in decls {
                let decl_name = self.p.node(decl).name().unwrap();
                let error = errors::AllDeclarationsOfXMustHaveIdenticalTypeParameters {
                    span: decl_name.span(),
                    name: s.name.to_string(&self.atoms),
                };
                self.diags.push(bolt_ts_errors::Diag {
                    inner: Box::new(error),
                });
            }
        }
    }

    fn are_ty_params_identical(
        &mut self,
        decls: &[ast::NodeID],
        target_params: Option<ty::Tys<'cx>>,
        get_ty_param_decl: impl FnOnce(&Self, ast::NodeID) -> ast::TyParams<'cx> + Copy,
    ) -> bool {
        let max_ty_argument_count = target_params.map_or(0, |target_params| target_params.len());
        let min_ty_argument_count = self.get_min_ty_arg_count(target_params);
        for decl in decls {
            let source_params = get_ty_param_decl(self, *decl);
            let num_ty_params = source_params.len();
            if num_ty_params < min_ty_argument_count || num_ty_params > max_ty_argument_count {
                return false;
            }
            for i in 0..num_ty_params {
                let source = source_params[i];
                let Some(target) = target_params.map(|target_params| target_params[i]) else {
                    continue;
                };
                let target_param_ty = target.kind.expect_param();
                if source.name.name != self.symbol(target_param_ty.symbol).name.expect_atom() {
                    return false;
                }
                if let Some(source_constraint) = self.get_effective_constraint_of_ty_param(source)
                    && let source_constraint = self.get_ty_from_type_node(source_constraint)
                    && let Some(target_constraint) = self.get_constraint_of_ty_param(target)
                    && !self.is_type_identical_to(source_constraint, target_constraint)
                {
                    return false;
                }

                if let Some(source_default) = source.default.map(|t| self.get_ty_from_type_node(t))
                    && let Some(target_default) = self.get_default_ty_from_ty_param(target)
                    && !self.is_type_identical_to(source_default, target_default)
                {
                    return false;
                }
            }
        }

        true
    }

    fn is_exhaustive_switch_stmt(&mut self, n: &'cx ast::SwitchStmt<'cx>) -> bool {
        if let Some(is_exhaustive) = self.get_node_links(n.id).get_non_existent_prop_checked() {
            return is_exhaustive;
        }
        let is_exhaustive = self.compute_exhaustive_switch_stmt(n);
        self.get_mut_node_links(n.id)
            .set_non_existent_prop_checked(is_exhaustive);
        is_exhaustive
    }

    fn get_switch_clause_ty_of_witnesses(
        &mut self,
        n: &'cx ast::SwitchStmt<'cx>,
    ) -> Option<FxIndexSet<Option<bolt_ts_atom::Atom>>> {
        if n.case_block.clauses.iter().any(|clause| match clause {
            ast::CaseOrDefaultClause::Case(n) => !n.expr.is_string_lit_like(),
            ast::CaseOrDefaultClause::Default(_) => false,
        }) {
            return None;
        }
        let witnesses = n
            .case_block
            .clauses
            .iter()
            .map(|clause| match clause {
                ast::CaseOrDefaultClause::Case(n) => match n.expr.kind {
                    ast::ExprKind::StringLit(n) => Some(n.val),
                    ast::ExprKind::NoSubstitutionTemplateLit(n) => Some(n.val),
                    _ => unimplemented!(),
                },
                ast::CaseOrDefaultClause::Default(_) => None,
            })
            .collect();
        Some(witnesses)
    }

    fn compute_exhaustive_switch_stmt(&mut self, n: &'cx ast::SwitchStmt<'cx>) -> bool {
        if let ast::ExprKind::Typeof(expr) = n.expr.kind {
            let Some(witnesses) = self.get_switch_clause_ty_of_witnesses(n) else {
                return false;
            };
            let operand_constraint = {
                let ty = self.check_expr_cached(expr.expr);
                self.get_base_constraint_or_ty(ty)
            };
            let not_equal_facts = self.get_not_equal_facts_from_ty_of_switch(0, 0, &witnesses);
            return if operand_constraint.flags.contains(TypeFlags::ANY_OR_UNKNOWN) {
                (TypeFacts::ALL_TYPEOF_NE & not_equal_facts) == TypeFacts::ALL_TYPEOF_NE
            } else {
                !self.some_type(operand_constraint, |this, t| {
                    this.get_ty_facts(t, not_equal_facts) == not_equal_facts
                })
            };
        } else {
            let ty = {
                let ty = self.check_expr_cached(n.expr);
                self.get_base_constraint_or_ty(ty)
            };
            if !ty.is_lit_ty() {
                return false;
            }

            let switch_tys = self.get_switch_clause_tys(n);
            return if switch_tys.is_empty()
                || switch_tys
                    .iter()
                    .any(|ty| ty.is_neither_unit_ty_nor_never())
            {
                false
            } else {
                let ty = self
                    .map_ty(
                        ty,
                        |this, t| Some(this.get_regular_ty_of_literal_ty(t)),
                        false,
                    )
                    .unwrap();
                self.each_ty_contained_in(ty, switch_tys)
            };
        }
    }

    fn get_ty_of_switch_clause(
        &mut self,
        n: &'cx ast::CaseOrDefaultClause<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        match n {
            ast::CaseOrDefaultClause::Case(n) => {
                let ty = self.get_ty_of_expr(n.expr);
                self.get_regular_ty_of_literal_ty(ty)
            }
            ast::CaseOrDefaultClause::Default(_) => self.never_ty,
        }
    }

    pub(super) fn get_switch_clause_tys(&mut self, n: &'cx ast::SwitchStmt<'cx>) -> ty::Tys<'cx> {
        if let Some(tys) = self.get_node_links(n.id).get_switch_tys() {
            return tys;
        }
        let tys = n
            .case_block
            .clauses
            .iter()
            .map(|clause| self.get_ty_of_switch_clause(clause))
            .collect::<Vec<_>>();
        let tys = self.alloc(tys);
        self.get_mut_node_links(n.id).set_switch_tys(tys);
        tys
    }

    fn get_not_equal_facts_from_ty_of_switch(
        &self,
        start: usize,
        end: usize,
        witnesses: &FxIndexSet<Option<bolt_ts_atom::Atom>>,
    ) -> TypeFacts {
        let mut facts = TypeFacts::empty();
        for i in 0..witnesses.len() {
            let witness = if i < start || i >= end {
                witnesses[i]
            } else {
                continue;
            };
            if let Some(witness) = witness {
                facts |= typeof_ne_facts(witness).unwrap_or(TypeFacts::TYPEOF_NE_HOST_OBJECT);
            }
        }

        facts
    }

    fn remove_ty(&mut self, ty: &'cx ty::Ty<'cx>, target_ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        self.filter_type(ty, |_, t| t != target_ty)
    }

    fn remove_missing_ty(&mut self, ty: &'cx ty::Ty<'cx>, is_optional: bool) -> &'cx ty::Ty<'cx> {
        if is_optional && self.config.exact_optional_property_types() {
            self.remove_ty(ty, self.missing_ty)
        } else {
            ty
        }
    }

    fn contains_missing_ty(&self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty == self.missing_ty
            || ty
                .kind
                .as_union()
                .is_some_and(|u| u.tys[0] == self.missing_ty)
    }

    fn remove_from_each(&mut self, tys: &mut Vec<&'cx ty::Ty<'cx>>, flags: TypeFlags) {
        for i in 0..tys.len() {
            tys[i] = self.filter_type(tys[i], |_, t| !t.flags.intersects(flags))
        }
    }

    fn get_apparent_mapped_ty_keys(
        &mut self,
        name_ty: &'cx ty::Ty<'cx>,
        target_ty: &'cx ty::MappedTy<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        let modifiers_ty = {
            let t = self.get_modifiers_ty_from_mapped_ty(target_ty);
            self.get_apparent_ty(t)
        };
        let mut mapped_keys = vec![];
        self.for_each_mapped_ty_prop_key_ty_and_index_sig_key_ty(
            modifiers_ty,
            TypeFlags::STRING_OR_NUMBER_LITERAL_OR_UNIQUE,
            false,
            |this, t| {
                let source = this.get_ty_param_from_mapped_ty(target_ty);
                let mapper = this.append_ty_mapping(target_ty.mapper, source, t);
                let t = this.instantiate_ty_worker(name_ty, mapper);
                mapped_keys.push(t);
            },
        );
        self.get_union_ty::<false>(&mapped_keys, ty::UnionReduction::Lit, None, None, None)
    }

    fn is_weak_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if ty.flags.contains(TypeFlags::OBJECT) {
            self.resolve_structured_type_members(ty);
            let s = self.get_ty_links(ty.id).expect_structured_members();
            s.call_sigs.is_empty()
                && s.ctor_sigs.is_empty()
                && s.index_infos.is_empty()
                && s.props.len() > 0
                && s.props
                    .iter()
                    .all(|prop| self.symbol(*prop).flags.contains(SymbolFlags::OPTIONAL))
        } else {
            match ty.kind {
                ty::TyKind::Substitution(ty) => self.is_weak_ty(ty.base_ty),
                ty::TyKind::Intersection(i) => i.tys.iter().any(|ty| self.is_weak_ty(ty)),
                _ => false,
            }
        }
    }

    fn get_single_base_for_non_augmenting_subtype(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let links = self.get_ty_links(ty.id);
        if let Some(_) = links.get_identical_base_ty_constraint() {
            return links.get_cached_equivalent_base_ty();
        }
        self.get_mut_ty_links(ty.id)
            .set_identical_base_ty_constraint(true);
        // TODO: tuple
        let reference_ty = ty.kind.as_object_reference()?;
        let target = reference_ty.target;
        let target_i = target.as_class_or_interface_ty()?;
        if target.get_object_flags().contains(ObjectFlags::CLASS) {
            if let Some(base_ty_node) = self.get_base_type_node_of_class(ty)
                && !matches!(
                    base_ty_node.expr_with_ty_args.expr.kind,
                    ast::ExprKind::Ident(_) | ast::ExprKind::PropAccess(_)
                )
            {
                return None;
            }
        }
        let bases = self.get_base_tys(ty);
        if bases.len() != 1 {
            return None;
        }
        if !self.get_members_of_symbol(target_i.symbol).0.is_empty() {
            return None;
        }
        let len = target_i.ty_params.map(|ty_params| ty_params.len());
        let type_arguments = self.get_ty_arguments(ty);
        let mut instantiated_base = if let Some(len) = len
            && len > 0
        {
            let type_arguments = &type_arguments[0..len];
            let mapper = self.create_ty_mapper(target_i.ty_params.unwrap(), type_arguments);
            self.instantiate_ty_worker(bases[0], mapper)
        } else {
            bases[0]
        };

        if type_arguments.len() > len.unwrap_or(0) {
            let last = type_arguments.last().copied();
            instantiated_base = self.get_ty_with_this_arg(instantiated_base, last, false);
        }

        self.get_mut_ty_links(ty.id)
            .set_cached_equivalent_base_ty(instantiated_base);
        Some(instantiated_base)
    }

    fn check_generator_instantiation_assignability_to_return_ty(
        &mut self,
        return_ty: &'cx ty::Ty<'cx>,
        fn_flags: FnFlags,
        error_node: Option<ast::NodeID>,
    ) -> bool {
        let is_async = fn_flags.contains(FnFlags::ASYNC);
        let generator_yield_ty = self
            .get_iteration_ty_of_generator_fn_return_ty(
                IterationTypeKind::Yield,
                return_ty,
                is_async,
            )
            .unwrap_or(self.any_ty);
        let generator_return_ty = self
            .get_iteration_ty_of_generator_fn_return_ty(
                IterationTypeKind::Return,
                return_ty,
                is_async,
            )
            .unwrap_or(generator_yield_ty);
        let generator_next_ty = self
            .get_iteration_ty_of_generator_fn_return_ty(
                IterationTypeKind::Next,
                return_ty,
                is_async,
            )
            .unwrap_or(self.unknown_ty);
        let generator_instantiation = self.create_generator_ty(
            generator_yield_ty,
            generator_return_ty,
            generator_next_ty,
            is_async,
        );
        self.check_type_assignable_to(generator_instantiation, return_ty, error_node)
    }

    fn get_awaited_ty_of_promise(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        error_node: Option<ast::NodeID>,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let promised_ty = self.get_promised_ty_of_promise(ty)?;
        self.get_awaited_ty(promised_ty)
    }

    fn include_undefined_in_index_sig(&mut self, ty: &'cx ty::Ty<'cx>) -> &'cx ty::Ty<'cx> {
        if self.config.no_unchecked_indexed_access() {
            let tys = [ty, self.missing_ty];
            self.get_union_ty::<false>(&tys, ty::UnionReduction::Lit, None, None, None)
        } else {
            ty
        }
    }

    fn is_ty_reference_with_generic_arguments(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        ty.is_non_deferred_ty_reference()
            && self.get_ty_arguments(ty).iter().any(|t| {
                t.flags.contains(TypeFlags::TYPE_PARAMETER)
                    || self.is_ty_reference_with_generic_arguments(t)
            })
    }

    fn is_string_index_sig_only_ty(&mut self, ty: &'cx ty::Ty<'cx>) -> bool {
        if ty.flags.contains(TypeFlags::OBJECT)
            && !self.is_generic_mapped_ty(ty)
            && self.get_props_of_ty(ty).is_empty()
            && self.get_index_infos_of_ty(ty).len() == 1
            && self.get_index_info_of_ty(ty, self.string_ty).is_some()
        {
            true
        } else if let Some(tys) = ty.kind.tys_of_union_or_intersection() {
            tys.iter().all(|t| self.is_string_index_sig_only_ty(t))
        } else {
            false
        }
    }

    fn is_type_equality_comparable_to(
        &mut self,
        source: &'cx ty::Ty<'cx>,
        target: &'cx ty::Ty<'cx>,
    ) -> bool {
        target.flags.intersects(TypeFlags::NULLABLE)
            || self.is_type_related_to(source, target, self::relation::RelationKind::Comparable)
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
