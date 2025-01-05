mod errors;

use crate::bind::{BinderState, GlobalSymbols, SymbolID, Symbols};
use crate::graph::ModuleGraph;
use crate::{ast, parser};
use bolt_ts_span::ModuleID;
use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

pub struct ResolveResult<'cx> {
    pub symbols: Symbols<'cx>,
    pub final_res: FxHashMap<ast::NodeID, SymbolID>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    pub deep_res: FxHashMap<ast::NodeID, SymbolID>,
}

pub fn late_resolve<'cx>(
    mut state: BinderState<'cx>,
    module_id: ModuleID,
    mg: &ModuleGraph,
    p: &'cx parser::Parser<'cx>,
    global: &'cx GlobalSymbols,
) -> ResolveResult<'cx> {
    let deep_res = fx_hashmap_with_capacity(state.res.len());
    let root = p.root(module_id);
    let mut resolver = Resolver {
        p,
        mg,
        module_id,
        global,
        deep_res,
        state: &state,
        diags: vec![],
    };
    let deep_res = resolver.deep_res;

    state.diags.extend(resolver.diags);
    ResolveResult {
        deep_res,
        diags: std::mem::take(&mut state.diags),
        final_res: std::mem::take(&mut state.final_res),
        symbols: state.symbols,
    }
}

struct Resolver<'cx> {
    mg: &'cx ModuleGraph,
    module_id: ModuleID,
    state: &'cx BinderState<'cx>,
    p: &'cx parser::Parser<'cx>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    global: &'cx GlobalSymbols,
    deep_res: FxHashMap<ast::NodeID, SymbolID>,
}

impl<'cx> ast::Visitor<'cx> for Resolver<'cx> {}
