mod bind_call_like;
mod bind_class_like;
mod bind_fn_like;
mod bind_visitor;
mod container_flags;
mod create;
mod errors;
mod flow;
mod pprint;
mod symbol;

use bolt_ts_atom::AtomMap;
use bolt_ts_span::Module;
use bolt_ts_span::ModuleID;

use rustc_hash::FxHashMap;

pub use self::flow::{FlowFlags, FlowID, FlowNode, FlowNodeKind, FlowNodes};
pub use self::symbol::BlockContainerSymbol;
use self::symbol::ClassSymbol;
pub(crate) use self::symbol::SymbolKind;
use self::symbol::TyLitSymbol;
pub use self::symbol::{GlobalSymbols, Symbol, SymbolID, SymbolName, Symbols};
pub use self::symbol::{SymbolFlags, SymbolFnKind};

use crate::late_resolve::ResolveResult;
use crate::parser::Parser;
use bolt_ts_ast as ast;

bolt_ts_utils::module_index!(ScopeID);

impl ScopeID {
    pub const fn is_root(&self) -> bool {
        self.index == 0
    }
}

pub struct Binder<'cx> {
    p: &'cx Parser<'cx>,
    binder_result: Vec<ResolveResult>,
}

impl<'cx> Binder<'cx> {
    pub fn new(p: &'cx Parser<'cx>) -> Self {
        Self {
            p,
            binder_result: Vec::with_capacity(p.module_count() + 1),
        }
    }

    pub fn insert(&mut self, id: ModuleID, result: ResolveResult) {
        assert_eq!(self.binder_result.len(), id.as_usize());
        self.binder_result.push(result);
    }

    #[inline(always)]
    pub(crate) fn get(&self, id: ModuleID) -> &ResolveResult {
        let index = id.as_usize();
        &self.binder_result[index]
    }

    #[inline(always)]
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

pub struct BinderState<'cx, 'atoms> {
    scope_id: ScopeID,
    p: &'cx Parser<'cx>,
    pub(crate) diags: Vec<bolt_ts_errors::Diag>,
    pub(crate) atoms: &'atoms AtomMap<'cx>,
    pub(crate) scope_id_parent_map: Vec<Option<ScopeID>>,
    // TODO: use `NodeId::index` is enough
    pub(crate) node_id_to_scope_id: FxHashMap<ast::NodeID, ScopeID>,
    pub(crate) symbols: Symbols,
    // TODO: use `NodeId::index` is enough
    pub(crate) locals: FxHashMap<ast::NodeID, FxHashMap<SymbolName, SymbolID>>,

    pub(crate) flow_nodes: FlowNodes<'cx>,
    current_flow: Option<FlowID>,
    has_flow_effects: bool,
    in_return_position: bool,
    current_true_target: Option<FlowID>,
    current_false_target: Option<FlowID>,
    current_exception_target: Option<FlowID>,
    unreachable_flow_node: FlowID,
    report_unreachable_flow_node: FlowID,

    pub(super) res: FxHashMap<(ScopeID, SymbolName), SymbolID>,
    // TODO: use `NodeId::index` is enough
    pub(crate) final_res: FxHashMap<ast::NodeID, SymbolID>,
}

pub fn bind_parallel<'cx, 'atoms>(
    modules: &[Module],
    atoms: &'atoms AtomMap<'cx>,
    parser: &'cx Parser<'cx>,
) -> Vec<BinderState<'cx, 'atoms>> {
    modules
        // .into_par_iter()
        .into_iter()
        .map(|m| {
            let module_id = m.id;
            let is_global = m.global;
            let root = parser.root(module_id);
            let bind_result = bind(atoms, parser, root, module_id);
            assert!(!is_global || bind_result.diags.is_empty());
            bind_result
        })
        .collect()
}

fn bind<'cx, 'atoms>(
    atoms: &'atoms AtomMap<'cx>,
    parser: &'cx Parser<'cx>,
    root: &'cx ast::Program,
    module_id: ModuleID,
) -> BinderState<'cx, 'atoms> {
    let mut state = BinderState::new(atoms, parser, module_id);
    state.bind_program(root);
    state
}

pub fn prop_name(name: &ast::PropName) -> SymbolName {
    match name.kind {
        ast::PropNameKind::Ident(ident) => SymbolName::Ele(ident.name),
        ast::PropNameKind::NumLit(num) => SymbolName::EleNum(num.val.into()),
        ast::PropNameKind::StringLit { key, .. } => SymbolName::Ele(key),
    }
}
