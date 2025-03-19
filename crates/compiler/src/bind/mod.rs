mod bind_call_like;
mod bind_class_like;
mod bind_fn_like;
mod bind_visitor;
mod container_flags;
mod create;
pub(crate) mod errors;
mod flow;
mod pprint;
mod symbol;

use bolt_ts_atom::AtomMap;
use bolt_ts_config::NormalizedTsConfig;
use bolt_ts_span::Module;
use bolt_ts_span::ModuleID;

use rayon::prelude::*;
use rustc_hash::FxHashMap;

pub use self::flow::{FlowFlags, FlowID, FlowNode, FlowNodeKind, FlowNodes};
pub use self::symbol::SymbolFlags;
pub use self::symbol::{GlobalSymbols, Symbol, SymbolID, SymbolName, Symbols};

use crate::late_resolve::ResolveResult;
use crate::parser::ParseResult;
use crate::parser::Parser;
use bolt_ts_ast as ast;

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum ModuleInstanceState {
    NonInstantiated = 0,
    Instantiated = 1,
    ConstEnumOnly = 2,
}

bolt_ts_utils::module_index!(ScopeID);

pub struct Binder {
    binder_result: Vec<ResolveResult>,
}

impl Binder {
    pub fn new(p: &Parser) -> Self {
        Self {
            binder_result: Vec::with_capacity(p.module_count() + 1),
        }
    }

    pub fn insert(&mut self, id: ModuleID, result: ResolveResult) {
        assert_eq!(self.binder_result.len(), id.as_usize());
        self.binder_result.push(result);
    }

    #[inline(always)]
    #[track_caller]
    pub(crate) fn get(&self, id: ModuleID) -> &ResolveResult {
        let index = id.as_usize();
        &self.binder_result[index]
    }

    #[inline(always)]
    #[track_caller]
    pub(super) fn symbol(&self, id: SymbolID) -> &Symbol {
        let m = id.module();
        self.get(m).symbols.get(id)
    }

    pub fn steal_errors(&mut self) -> Vec<bolt_ts_errors::Diag> {
        self.binder_result
            .iter_mut()
            .flat_map(|result| std::mem::take(&mut result.diags))
            .collect()
    }

    pub fn locals(&self, id: ast::NodeID) -> Option<&FxHashMap<SymbolName, SymbolID>> {
        self.get(id.module()).locals.get(&id)
    }
}

struct BinderState<'cx, 'atoms, 'parser> {
    scope_id: ScopeID,
    p: &'parser mut ParseResult<'cx>,
    atoms: &'atoms AtomMap<'cx>,
    diags: Vec<bolt_ts_errors::Diag>,
    scope_id_parent_map: Vec<Option<ScopeID>>,
    // TODO: use `NodeId::index` is enough
    node_id_to_scope_id: FxHashMap<ast::NodeID, ScopeID>,
    symbols: Symbols,
    // TODO: use `NodeId::index` is enough
    locals: FxHashMap<ast::NodeID, FxHashMap<SymbolName, SymbolID>>,

    container: Option<ast::NodeID>,
    this_parent_container: Option<ast::NodeID>,
    block_scope_container: Option<ast::NodeID>,
    last_container: Option<ast::NodeID>,
    seen_this_keyword: bool,

    current_flow: Option<FlowID>,
    in_strict_mode: bool,
    emit_flags: bolt_ts_ast::NodeFlags,
    has_explicit_return: bool,
    in_return_position: bool,
    has_flow_effects: bool,
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
    res: FxHashMap<(ScopeID, SymbolName), SymbolID>,
    // TODO: use `NodeId::index` is enough
    final_res: FxHashMap<ast::NodeID, SymbolID>,
    flow_nodes: FlowNodes<'cx>,
}

pub struct BinderResult<'cx> {
    pub(crate) diags: Vec<bolt_ts_errors::Diag>,
    pub(crate) scope_id_parent_map: Vec<Option<ScopeID>>,
    // TODO: use `NodeId::index` is enough
    pub(crate) node_id_to_scope_id: FxHashMap<ast::NodeID, ScopeID>,
    pub(crate) symbols: Symbols,
    // TODO: use `NodeId::index` is enough
    pub(crate) locals: FxHashMap<ast::NodeID, FxHashMap<SymbolName, SymbolID>>,
    pub(super) res: FxHashMap<(ScopeID, SymbolName), SymbolID>,
    // TODO: use `NodeId::index` is enough
    pub(crate) final_res: FxHashMap<ast::NodeID, SymbolID>,
    pub(crate) flow_nodes: FlowNodes<'cx>,
}

impl<'cx> BinderResult<'cx> {
    fn new(state: BinderState<'cx, '_, '_>) -> Self {
        Self {
            diags: state.diags,
            scope_id_parent_map: state.scope_id_parent_map,
            node_id_to_scope_id: state.node_id_to_scope_id,
            symbols: state.symbols,
            locals: state.locals,
            res: state.res,
            final_res: state.final_res,
            flow_nodes: state.flow_nodes,
        }
    }
}

pub fn bind_parallel<'cx>(
    modules: &[Module],
    atoms: &AtomMap<'cx>,
    parser: Parser<'cx>,
    options: &NormalizedTsConfig,
) -> Vec<(BinderResult<'cx>, ParseResult<'cx>)> {
    assert_eq!(parser.module_count(), modules.len());
    parser
        .map
        .into_par_iter()
        .zip(modules)
        .map(|(mut p, m)| {
            let module_id = m.id;
            let is_global = m.global;
            let root = p.root();
            let bind_state = bind(atoms, &mut p, root, module_id, options);
            let bind_result = BinderResult::new(bind_state);
            assert!(!is_global || bind_result.diags.is_empty());
            (bind_result, p)
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
    let mut state = BinderState::new(atoms, parser, root, module_id, options);
    state.bind_program(root);
    state
}

pub fn prop_name(name: &ast::PropName) -> SymbolName {
    match name.kind {
        ast::PropNameKind::Ident(ident) => SymbolName::Ele(ident.name),
        ast::PropNameKind::NumLit(num) => SymbolName::EleNum(num.val.into()),
        ast::PropNameKind::StringLit { key, .. } => SymbolName::Ele(key),
        ast::PropNameKind::Computed(c) => {
            use bolt_ts_ast::ExprKind::*;
            match c.expr.kind {
                Ident(n) => SymbolName::Ele(n.name),
                StringLit(n) => SymbolName::Ele(n.val),
                _ => unreachable!(),
            }
        }
    }
}
