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

use rayon::prelude::*;
use rustc_hash::FxHashMap;

pub use self::flow::{FlowFlags, FlowID, FlowNode, FlowNodeKind, FlowNodes};
pub use self::symbol::BlockContainerSymbol;
use self::symbol::ClassSymbol;
pub(crate) use self::symbol::SymbolKind;
use self::symbol::TyLitSymbol;
pub use self::symbol::{GlobalSymbols, Symbol, SymbolID, SymbolName, Symbols};
pub use self::symbol::{SymbolFlags, SymbolFnKind};

use crate::ast;
use crate::late_resolve::ResolveResult;
use crate::parser::Parser;

bolt_ts_utils::module_index!(ScopeID);

impl ScopeID {
    pub const fn is_root(&self) -> bool {
        self.index == 0
    }
}

pub struct Binder<'cx> {
    p: &'cx Parser<'cx>,
    atoms: &'cx AtomMap<'cx>,
    binder_result: Vec<ResolveResult>,
}

impl<'cx> Binder<'cx> {
    pub fn new(p: &'cx Parser<'cx>, atoms: &'cx AtomMap) -> Self {
        Self {
            p,
            atoms,
            binder_result: Vec::with_capacity(p.module_count() + 1),
        }
    }

    pub fn insert(&mut self, id: ModuleID, result: ResolveResult) {
        assert_eq!(self.binder_result.len(), id.as_usize());
        self.binder_result.push(result);
    }

    #[inline(always)]
    fn get(&self, id: ModuleID) -> &ResolveResult {
        let index = id.as_usize();
        &self.binder_result[index]
    }

    #[inline(always)]
    pub fn final_res(&self, id: ast::NodeID) -> SymbolID {
        self.get(id.module())
            .final_res
            .get(&id)
            .copied()
            .unwrap_or_else(|| {
                let n = self.p.node(id);
                let Some(node) = n.as_ident() else {
                    unreachable!("final_res not found for {:#?}", n);
                };
                let name = self.atoms.get(node.name);
                let span = self.p.node(id).span();
                panic!("The resolution of `{name}({span})` is not found.");
            })
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

pub struct BinderState<'cx> {
    scope_id: ScopeID,
    p: &'cx Parser<'cx>,
    pub(crate) diags: Vec<bolt_ts_errors::Diag>,
    pub(crate) atoms: &'cx AtomMap<'cx>,
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
    unreachable_flow_node: FlowID,
    report_unreachable_flow_node: FlowID,

    pub(super) res: FxHashMap<(ScopeID, SymbolName), SymbolID>,
    // TODO: use `NodeId::index` is enough
    pub(crate) final_res: FxHashMap<ast::NodeID, SymbolID>,
}

pub fn bind_parallel<'cx>(
    modules: &[Module],
    atoms: &'cx AtomMap<'cx>,
    parser: &'cx Parser<'cx>,
) -> Vec<BinderState<'cx>> {
    modules
        .into_par_iter()
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

fn bind<'cx>(
    atoms: &'cx AtomMap<'cx>,
    parser: &'cx Parser<'cx>,
    root: &'cx ast::Program,
    module_id: ModuleID,
) -> BinderState<'cx> {
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
