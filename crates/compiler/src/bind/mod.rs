mod bind_break_or_continue;
mod bind_children;
mod bind_class_like;
mod bind_container;
mod bind_for_in_for_of;
mod bind_prop_or_ele_access;
mod bind_ret_or_throw;
mod bind_worker;
mod container_flags;
mod create;
pub(crate) mod errors;
mod flow;
mod flow_in_node;
mod merge;
mod node_query;
mod parent_map;
mod pprint;
mod symbol;

use rayon::prelude::*;
use rustc_hash::FxHashMap;

use bolt_ts_atom::AtomMap;
use bolt_ts_config::NormalizedTsConfig;
use bolt_ts_span::Module;
use bolt_ts_span::ModuleID;
use bolt_ts_utils::fx_hashmap_with_capacity;

pub(crate) use self::create::set_value_declaration;
pub use self::flow::{FlowFlags, FlowID, FlowNode, FlowNodeKind, FlowNodes};
pub use self::flow_in_node::{FlowInNode, FlowInNodes};
pub(crate) use self::merge::merge_global_symbol;
pub(crate) use self::merge::{MergeGlobalSymbolResult, MergeSymbol, MergedSymbols};
pub use self::node_query::NodeQuery;
pub use self::parent_map::ParentMap;
pub use self::symbol::{GlobalSymbols, Symbol, SymbolID, SymbolName, Symbols};
pub use self::symbol::{SymbolFlags, SymbolTable};

use crate::parser::ParseResult;
use crate::parser::Parser;
use bolt_ts_ast as ast;

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum ModuleInstanceState {
    NonInstantiated = 0,
    Instantiated = 1,
    ConstEnumOnly = 2,
}

pub struct ResolveResult {
    pub symbols: Symbols,
    // TODO: use `NodeId::index` is enough
    pub final_res: FxHashMap<ast::NodeID, SymbolID>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    // TODO: use `NodeId::index` is enough
    pub locals: FxHashMap<ast::NodeID, SymbolTable>,
}

pub struct Binder {
    pub(crate) bind_results: Vec<ResolveResult>,
}

impl Binder {
    pub fn new(bind_results: Vec<ResolveResult>) -> Self {
        Self { bind_results }
    }

    #[inline(always)]
    #[track_caller]
    pub(crate) fn get(&self, id: ModuleID) -> &ResolveResult {
        let index = id.as_usize();
        &self.bind_results[index]
    }

    #[inline(always)]
    #[track_caller]
    pub(super) fn symbol(&self, id: SymbolID) -> &Symbol {
        let m = id.module();
        self.get(m).symbols.get(id)
    }

    pub fn steal_errors(&mut self) -> Vec<bolt_ts_errors::Diag> {
        self.bind_results
            .iter_mut()
            .flat_map(|result| std::mem::take(&mut result.diags))
            .collect()
    }

    pub fn locals(&self, id: ast::NodeID) -> Option<&SymbolTable> {
        self.get(id.module()).locals.get(&id)
    }
}

struct BinderState<'cx, 'atoms, 'parser> {
    p: &'parser mut ParseResult<'cx>,
    atoms: &'atoms AtomMap<'cx>,
    diags: Vec<bolt_ts_errors::Diag>,
    symbols: Symbols,
    // TODO: use `NodeId::index` is enough
    locals: FxHashMap<ast::NodeID, SymbolTable>,

    container: Option<ast::NodeID>,
    this_parent_container: Option<ast::NodeID>,
    block_scope_container: Option<ast::NodeID>,
    last_container: Option<ast::NodeID>,
    seen_this_keyword: bool,

    current_flow: Option<FlowID>,
    in_strict_mode: bool,
    in_assignment_pattern: bool,
    parent: Option<ast::NodeID>,
    emit_flags: bolt_ts_ast::NodeFlags,
    has_explicit_return: bool,
    in_return_position: bool,
    has_flow_effects: bool,
    has_explicit_ret: bool,
    current_true_target: Option<FlowID>,
    current_false_target: Option<FlowID>,
    current_exception_target: Option<FlowID>,
    current_break_target: Option<FlowID>,
    current_continue_target: Option<FlowID>,
    current_return_target: Option<FlowID>,
    unreachable_flow_node: FlowID,
    report_unreachable_flow_node: FlowID,

    // TODO: use `NodeId::index` is enough
    container_chain: FxHashMap<ast::NodeID, ast::NodeID>,
    // TODO: use `NodeId::index` is enough
    final_res: FxHashMap<ast::NodeID, SymbolID>,
    flow_nodes: FlowNodes<'cx>,
    flow_in_nodes: FlowInNodes,
    parent_map: self::parent_map::ParentMap,
}

struct BinderNodeQuery<'cx, 'p> {
    parent_map: &'p ParentMap,
    parse_result: &'p ParseResult<'cx>,
}

impl<'cx, 'p> BinderNodeQuery<'cx, 'p> {
    fn new(parent_map: &'p ParentMap, parse_result: &'p ParseResult<'cx>) -> Self {
        Self {
            parent_map,
            parse_result,
        }
    }
}

impl<'cx> NodeQuery<'cx> for BinderNodeQuery<'cx, '_> {
    fn node(&self, id: bolt_ts_ast::NodeID) -> bolt_ts_ast::Node<'cx> {
        self.parse_result.node(id)
    }

    fn parent(&self, id: bolt_ts_ast::NodeID) -> Option<bolt_ts_ast::NodeID> {
        self.parent_map.parent_unfinished(id)
    }

    fn node_flags(&self, id: bolt_ts_ast::NodeID) -> bolt_ts_ast::NodeFlags {
        self.parse_result.node_flags(id)
    }

    fn is_external_or_commonjs_module(&self) -> bool {
        self.parse_result.external_module_indicator.is_some()
            || self.parse_result.commonjs_module_indicator.is_some()
    }

    fn is_external_module(&self) -> bool {
        self.parse_result.external_module_indicator.is_some()
    }
}

impl<'cx, 'atoms, 'parser> BinderState<'cx, 'atoms, 'parser> {
    fn new(
        atoms: &'atoms AtomMap<'cx>,
        parser: &'parser mut ParseResult<'cx>,
        module_id: ModuleID,
        options: &NormalizedTsConfig,
    ) -> Self {
        let symbols = Symbols::new(module_id);
        let mut flow_nodes = FlowNodes::new(module_id);
        let unreachable_flow_node = flow_nodes.create_flow_unreachable();
        let report_unreachable_flow_node = flow_nodes.create_flow_unreachable();

        let in_strict_mode = (!parser.is_declaration && options.compiler_options().always_strict())
            || parser.external_module_indicator.is_some();
        let parent_map = ParentMap::new(parser.node_len());
        let flow_in_nodes = FlowInNodes::new(parser.node_len());

        BinderState {
            atoms,
            p: parser,
            final_res: fx_hashmap_with_capacity(512),
            container_chain: fx_hashmap_with_capacity(128),
            locals: fx_hashmap_with_capacity(128),
            symbols,
            diags: Vec::new(),

            flow_nodes,
            flow_in_nodes,

            in_strict_mode,
            in_assignment_pattern: false,
            seen_this_keyword: false,
            emit_flags: bolt_ts_ast::NodeFlags::empty(),
            parent: None,
            current_flow: None,
            current_break_target: None,
            current_continue_target: None,
            current_return_target: None,
            current_exception_target: None,
            current_true_target: None,
            current_false_target: None,
            unreachable_flow_node,
            report_unreachable_flow_node,
            has_flow_effects: false,
            has_explicit_ret: false,
            in_return_position: false,
            has_explicit_return: false,

            container: None,
            this_parent_container: None,
            block_scope_container: None,
            last_container: None,
            parent_map,
        }
    }

    fn push_error(&mut self, error: crate::Diag) {
        let diag = bolt_ts_errors::Diag { inner: error };
        self.diags.push(diag);
    }

    fn node_query(&self) -> impl node_query::NodeQuery<'cx> {
        BinderNodeQuery::new(&self.parent_map, self.p)
    }
}

pub struct BinderResult<'cx> {
    pub(crate) diags: Vec<bolt_ts_errors::Diag>,
    pub(crate) symbols: Symbols,
    // TODO: use `NodeId::index` is enough
    pub(crate) locals: FxHashMap<ast::NodeID, SymbolTable>,
    // TODO: use `NodeId::index` is enough
    pub(crate) final_res: FxHashMap<ast::NodeID, SymbolID>,
    pub(crate) flow_nodes: FlowNodes<'cx>,
    pub(crate) flow_in_nodes: FlowInNodes,
}

impl<'cx> BinderResult<'cx> {
    fn new(state: BinderState<'cx, '_, '_>) -> Self {
        Self {
            diags: state.diags,
            symbols: state.symbols,
            locals: state.locals,
            final_res: state.final_res,
            flow_nodes: state.flow_nodes,
            flow_in_nodes: state.flow_in_nodes,
        }
    }
}

pub fn bind_parallel<'cx>(
    modules: &[Module],
    atoms: &AtomMap<'cx>,
    parser: Parser<'cx>,
    options: &NormalizedTsConfig,
) -> Vec<(BinderResult<'cx>, (ParseResult<'cx>, self::ParentMap))> {
    assert_eq!(parser.module_count(), modules.len());
    parser
        .map
        .into_par_iter()
        .zip(modules)
        .map(|(mut p, m)| {
            let module_id = m.id;
            let is_default_lib = m.is_default_lib;
            let root = p.root();
            let mut bind_state = bind(atoms, &mut p, root, module_id, options);
            let parent_map = std::mem::take(&mut bind_state.parent_map);
            let bind_result = BinderResult::new(bind_state);
            assert!(!is_default_lib || bind_result.diags.is_empty());
            (bind_result, (p, parent_map))
        })
        .collect()
}

fn bind<'cx, 'atoms, 'parser>(
    atoms: &'atoms AtomMap<'cx>,
    parser: &'parser mut ParseResult<'cx>,
    root: &'cx ast::Program,
    module_id: ModuleID,
    options: &NormalizedTsConfig,
) -> BinderState<'cx, 'atoms, 'parser> {
    let mut state = BinderState::new(atoms, parser, module_id, options);
    state.bind(root.id);
    state.parent_map.finish();
    state
}

pub fn prop_name(name: &ast::PropName) -> SymbolName {
    match name.kind {
        ast::PropNameKind::Ident(ident) => SymbolName::Atom(ident.name),
        ast::PropNameKind::NumLit(num) => SymbolName::EleNum(num.val.into()),
        ast::PropNameKind::StringLit { key, .. } => SymbolName::Atom(key),
        ast::PropNameKind::Computed(c) => {
            use bolt_ts_ast::ExprKind::*;
            match c.expr.kind {
                Ident(n) => SymbolName::Atom(n.name),
                StringLit(n) => SymbolName::Atom(n.val),
                _ => unreachable!("name: {name:#?}"),
            }
        }
    }
}
