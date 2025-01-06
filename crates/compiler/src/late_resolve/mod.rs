mod errors;

use crate::ast::Visitor;
use crate::bind::{
    BinderState, BlockContainerSymbol, GlobalSymbols, SymbolFnKind, SymbolID, SymbolName, Symbols,
};
use crate::graph::ModuleGraph;
use crate::{ast, parser};
use bolt_ts_atom::AtomMap;
use bolt_ts_span::{Module, ModuleID};
use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

pub struct ResolveResult<'cx> {
    pub symbols: Symbols<'cx>,
    pub final_res: FxHashMap<ast::NodeID, SymbolID>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    pub deep_res: FxHashMap<ast::NodeID, SymbolID>,
}

pub fn late_resolve<'cx>(
    mut states: FxHashMap<ModuleID, BinderState<'cx>>,
    modules: &[Module],
    mg: &'cx ModuleGraph,
    p: &'cx parser::Parser<'cx>,
    global: &'cx GlobalSymbols,
    atoms: &'cx AtomMap<'cx>,
) -> Vec<(ModuleID, ResolveResult<'cx>)> {
    let mut temp = fx_hashmap_with_capacity(modules.len());
    for module in modules {
        let module_id = module.id;
        let state = states.get_mut(&module.id).unwrap();
        let deep_res = fx_hashmap_with_capacity(state.res.len());
        let root = p.root(module.id);
        let mut resolver = Resolver {
            mg,
            p,
            module_id,
            global,
            deep_res,
            state,
            diags: vec![],
            atoms,
        };
        resolver.visit_program(root);
        let deep_res = std::mem::take(&mut resolver.deep_res);
        let resolver_diags = std::mem::take(&mut resolver.diags);
        drop(resolver);
        state.diags.extend(resolver_diags);
        let diags = std::mem::take(&mut state.diags);
        let prev = temp.insert(module_id, (deep_res, diags));
        assert!(prev.is_none());
    }

    let mut res = Vec::with_capacity(modules.len());
    for module in modules {
        let state = states.get_mut(&module.id).unwrap();
        let symbols = std::mem::take(&mut state.symbols);
        let final_res = std::mem::take(&mut state.final_res);
        let old = std::mem::take(temp.get_mut(&module.id).unwrap());
        res.push((
            module.id,
            ResolveResult {
                symbols,
                final_res,
                diags: old.1,
                deep_res: old.0,
            },
        ));
    }
    res
}

struct Resolver<'cx, 'r> {
    mg: &'cx ModuleGraph,
    module_id: ModuleID,
    state: &'r mut BinderState<'cx>,
    p: &'cx parser::Parser<'cx>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    global: &'cx GlobalSymbols,
    deep_res: FxHashMap<ast::NodeID, SymbolID>,
    atoms: &'cx AtomMap<'cx>,
}

impl<'cx, 'r> Resolver<'cx, 'r> {
    fn symbol_decl(&self, symbol_id: SymbolID) -> ast::NodeID {
        use crate::bind::SymbolKind::*;
        match &self.state.symbols.get(symbol_id).kind.0 {
            Fn(f) => f.decls[0],
            Transient(_) => unreachable!(),
            _ => todo!(),
        }
    }

    fn push_error(&mut self, module_id: ModuleID, diag: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag {
            module_id,
            inner: diag,
        })
    }

    fn container(&self, module_id: ModuleID) -> &BlockContainerSymbol {
        self.state
            .symbols
            .get_container(module_id)
            .expect_block_container()
    }
}

impl<'cx, 'r> ast::Visitor<'cx> for Resolver<'cx, 'r> {
    fn visit_import_decl(&mut self, node: &'cx ast::ImportDecl<'cx>) {
        let Some(dep) = self.mg.get_dep(self.module_id, node.id) else {
            unreachable!()
        };

        if let Some(clause) = node.clause.kind {
            use ast::ImportClauseKind::*;
            match clause {
                Ns(n) => {}
                Specs(specs) => {
                    for spec in specs {
                        use ast::ImportSpecKind::*;
                        match spec.kind {
                            ShortHand(n) => {
                                let name = SymbolName::Normal(n.name.name);
                                if !self.container(dep).exports.contains_key(&name) {
                                    if let Some(local) =
                                        self.container(dep).locals.get(&name).copied()
                                    {
                                        let name = self.state.symbols.get(local).name.expect_atom();
                                        let symbol_span = self
                                            .p
                                            .node(self.symbol_decl(local))
                                            .ident_name()
                                            .unwrap()
                                            .span;
                                        let error =
                                            errors::ModuleADeclaresBLocallyButItIsNotExported {
                                                span: n.span,
                                                module_name: self
                                                    .atoms
                                                    .get(node.module.val)
                                                    .to_string(),
                                                symbol_name: self.atoms.get(name).to_string(),
                                                symbol_span,
                                            };
                                        self.push_error(n.span.module, Box::new(error));
                                    }
                                }
                            }
                            Named(n) => {}
                        }
                    }
                }
            }
        }
    }
}
