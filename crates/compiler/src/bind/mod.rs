mod bind_call_like;
mod bind_class_like;
mod bind_fn_like;
mod bind_visitor;
mod create;
mod errors;
mod merged_symbol;
mod pprint;
mod symbol;

use bolt_ts_atom::AtomMap;
use bolt_ts_span::Module;
use bolt_ts_span::ModuleID;

use rayon::prelude::*;
use rustc_hash::FxHashMap;

pub use self::symbol::BlockContainerSymbol;
use self::symbol::ClassSymbol;
pub(crate) use self::symbol::SymbolKind;
use self::symbol::TransientSymbol;
use self::symbol::TyLitSymbol;
pub use self::symbol::{GlobalSymbols, Symbol, SymbolID, SymbolName, Symbols};
pub use self::symbol::{SymbolFlags, SymbolFnKind};

use crate::ast;
use crate::late_resolve::ResolveResult;
use crate::parser::Parser;

bolt_ts_utils::index_with_module!(ScopeID);

impl ScopeID {
    pub const fn is_root(&self) -> bool {
        self.index == 0
    }
}

pub struct Binder<'cx> {
    p: &'cx Parser<'cx>,
    atoms: &'cx AtomMap<'cx>,
    binder_result: Vec<ResolveResult<'cx>>,
    transient_binder: Symbols<'cx>,
}

impl<'cx> Binder<'cx> {
    pub fn new(p: &'cx Parser<'cx>, atoms: &'cx AtomMap) -> Self {
        Self {
            p,
            atoms,
            binder_result: Vec::with_capacity(p.module_count() + 1),
            transient_binder: Symbols::new(ModuleID::MOCK),
        }
    }

    pub fn insert(&mut self, id: ModuleID, result: ResolveResult<'cx>) {
        assert_eq!(self.binder_result.len(), id.as_usize());
        self.binder_result.push(result);
    }

    #[inline(always)]
    fn get(&self, id: ModuleID) -> &ResolveResult<'cx> {
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
                let node = self.p.node(id).expect_ident();
                let name = self.atoms.get(node.name);
                panic!("The resolution of `{name}` is not found.");
            })
    }

    #[inline(always)]
    pub fn symbols(&self, id: ModuleID) -> &Symbols<'cx> {
        if id == ModuleID::MOCK {
            &self.transient_binder
        } else {
            &self.get(id).symbols
        }
    }

    #[inline(always)]
    pub fn symbol(&self, id: SymbolID) -> &Symbol<'cx> {
        let m = id.module();
        if m == ModuleID::MOCK {
            self.transient_binder.get(id)
        } else {
            self.get(m).symbols.get(id)
        }
    }

    pub(crate) fn get_transient(&self, symbol: SymbolID) -> Option<&TransientSymbol<'cx>> {
        match &self.symbol(symbol).kind.0 {
            SymbolKind::Transient(ty) => Some(ty),
            _ => None,
        }
    }

    pub(crate) fn get_mut_transient(
        &mut self,
        symbol: SymbolID,
    ) -> Option<&mut TransientSymbol<'cx>> {
        if symbol.module() != ModuleID::MOCK {
            return None;
        }
        match &mut self.transient_binder.get_mut(symbol).kind.0 {
            SymbolKind::Transient(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn create_transient_symbol(
        &mut self,
        name: SymbolName,
        symbol_flags: SymbolFlags,
        origin: Option<SymbolID>,
        links: crate::check::SymbolLinks<'cx>,
    ) -> SymbolID {
        let symbol_flags = symbol_flags | SymbolFlags::TRANSIENT;
        let symbol = Symbol::new(
            name,
            symbol_flags,
            SymbolKind::Transient(TransientSymbol { links, origin }),
        );
        self.transient_binder.insert(symbol)
    }

    pub fn steal_errors(&mut self) -> Vec<bolt_ts_errors::Diag> {
        self.binder_result
            .iter_mut()
            .flat_map(|result| std::mem::take(&mut result.diags))
            .collect()
    }
}

pub struct BinderState<'cx> {
    scope_id: ScopeID,
    p: &'cx Parser<'cx>,
    pub(crate) diags: Vec<bolt_ts_errors::Diag>,
    pub(crate) atoms: &'cx AtomMap<'cx>,
    pub(crate) scope_id_parent_map: Vec<Option<ScopeID>>,
    pub(crate) node_id_to_scope_id: FxHashMap<ast::NodeID, ScopeID>,
    pub(crate) symbols: Symbols<'cx>,
    pub(super) res: FxHashMap<(ScopeID, SymbolName), SymbolID>,
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
            let bind_result = bind(&atoms, parser, root, module_id);
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

fn prop_name(name: &ast::PropName) -> SymbolName {
    match name.kind {
        ast::PropNameKind::Ident(ident) => SymbolName::Ele(ident.name),
        ast::PropNameKind::NumLit(num) => SymbolName::EleNum(num.val.into()),
        ast::PropNameKind::StringLit(lit) => SymbolName::Ele(lit.val),
    }
}
