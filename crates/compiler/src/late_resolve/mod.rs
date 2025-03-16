mod errors;

use crate::bind::{
    BinderResult, BlockContainerSymbol, GlobalSymbols, SymbolFlags, SymbolID, SymbolName, Symbols,
};
use crate::graph::{ModuleGraph, ModuleRes};
use crate::parser;
use bolt_ts_ast as ast;
use bolt_ts_ast::Visitor;
use bolt_ts_atom::AtomMap;
use bolt_ts_span::{Module, ModuleID};
use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

pub struct ResolveResult {
    pub symbols: Symbols,
    // TODO: use `NodeId::index` is enough
    pub final_res: FxHashMap<ast::NodeID, SymbolID>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    // TODO: use `NodeId::index` is enough
    pub deep_res: FxHashMap<ast::NodeID, SymbolID>,
    // TODO: use `NodeId::index` is enough
    pub locals: FxHashMap<ast::NodeID, FxHashMap<SymbolName, SymbolID>>,
}

pub fn late_resolve<'cx>(
    mut states: Vec<BinderResult<'cx>>,
    modules: &[Module],
    mg: &'cx ModuleGraph,
    p: &'cx parser::Parser<'cx>,
    global: &'cx GlobalSymbols,
    atoms: &'cx AtomMap<'cx>,
) -> Vec<(ModuleID, ResolveResult)> {
    let mut temp = Vec::with_capacity(modules.len());
    for module in modules {
        let module_id = module.id;
        let deep_res = fx_hashmap_with_capacity(states[module.id.as_usize()].res.len());
        let root = p.root(module.id);
        let mut resolver = Resolver {
            mg,
            p,
            module_id,
            global,
            deep_res,
            states: &mut states,
            diags: vec![],
            atoms,
        };
        resolver.visit_program(root);
        let deep_res = std::mem::take(&mut resolver.deep_res);
        let resolver_diags = std::mem::take(&mut resolver.diags);
        temp.push((deep_res, resolver_diags));
    }

    modules
        .iter()
        .zip(states)
        .zip(temp)
        .map(|((module, mut state), (deep_res, diags))| {
            let symbols = state.symbols;
            let final_res = state.final_res;
            state.diags.extend(diags);
            let diags = state.diags;
            let result = ResolveResult {
                symbols,
                final_res,
                diags,
                deep_res,
                locals: state.locals,
            };
            (module.id, result)
        })
        .collect::<Vec<_>>()
}

struct Resolver<'cx, 'r, 'atoms> {
    mg: &'cx ModuleGraph,
    module_id: ModuleID,
    states: &'r mut Vec<BinderResult<'cx>>,
    p: &'cx parser::Parser<'cx>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    global: &'cx GlobalSymbols,
    deep_res: FxHashMap<ast::NodeID, SymbolID>,
    atoms: &'atoms AtomMap<'cx>,
}

impl Resolver<'_, '_, '_> {
    fn symbol_decl(&self, symbol_id: SymbolID) -> ast::NodeID {
        let s = self.symbol(symbol_id);
        if let Some(s) = &s.kind.1 {
            s.decls[0]
        } else {
            unreachable!()
        }
    }

    fn symbol(&self, symbol_id: SymbolID) -> &crate::bind::Symbol {
        self.states[symbol_id.module().as_usize()]
            .symbols
            .get(symbol_id)
    }

    fn push_error(&mut self, diag: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag { inner: diag })
    }

    fn container(&self, m: ModuleRes) -> &BlockContainerSymbol {
        match m {
            ModuleRes::Err => unreachable!(),
            ModuleRes::Res(module_id) => self.states[module_id.as_usize()]
                .symbols
                .get_container(module_id)
                .expect_block_container(),
        }
    }
}

impl<'cx> ast::Visitor<'cx> for Resolver<'cx, '_, '_> {
    fn visit_import_decl(&mut self, node: &'cx ast::ImportDecl<'cx>) {
        let Some(dep) = self.mg.get_dep(self.module_id, node.id) else {
            unreachable!()
        };

        if let Some(clause) = node.clause.kind {
            use bolt_ts_ast::ImportClauseKind::*;
            match clause {
                Ns(_) => {}
                Specs(specs) => {
                    for spec in specs {
                        use bolt_ts_ast::ImportSpecKind::*;
                        match spec.kind {
                            Shorthand(n) => {
                                let name = SymbolName::Normal(n.name.name);
                                if !self.container(dep).exports.contains_key(&name) {
                                    let module_name = self.atoms.get(node.module.val).to_string();
                                    let symbol_name = self.atoms.get(n.name.name);
                                    let decl = self.container(dep).exports.values().find_map(|export| {
                                        let s = self.symbol(*export);
                                        if s.flags == SymbolFlags::ALIAS {
                                            let decl = s.kind.1.as_ref().unwrap().decls[0];
                                            if let Some(spec) = self.p.node(decl).as_export_named_spec() {
                                                match spec.prop_name.kind {
                                                    bolt_ts_ast::ModuleExportNameKind::Ident(ident) if ident.name == n.name.name => return Some(decl),
                                                    _ => (),
                                                }
                                            }
                                        } 
                                        None
                                    });
                                    if let Some(decl) = decl {
                                        let mut helper = vec![];
                                        if let Some(local) = self.container(dep).locals.get(&name) {
                                            let symbol_span = self
                                                .p
                                                .node(self.symbol_decl(*local))
                                                .ident_name()
                                                .unwrap()
                                                .span;
                                            helper.push(
                                                errors::ModuleADeclaresBLocallyButItIsExportedAsCHelperKind::NameIsDeclaredHere(
                                                    errors::NameIsDeclaredHere {
                                                        span: symbol_span,
                                                        name: symbol_name.to_string()
                                                    }
                                                ));
                                        }
                                        let name = match self.p.node(decl) {
                                            ast::Node::ExportNamedSpec(n) => match n.name.kind {
                                                bolt_ts_ast::ModuleExportNameKind::Ident(ident) => {
                                                    ident.name
                                                }
                                                bolt_ts_ast::ModuleExportNameKind::StringLit(
                                                    lit,
                                                ) => lit.val,
                                            },
                                            _ => unreachable!(),
                                        };
                                        helper.push(
                                            errors::ModuleADeclaresBLocallyButItIsExportedAsCHelperKind::ExportedAliasHere(
                                                errors::ExportedAliasHere {
                                                    span: self.p.node(decl).span(),
                                                    name: self.atoms.get(name).to_string()
                                                }
                                            )
                                        );

                                        let error =
                                            errors::ModuleADeclaresBLocallyButItIsExportedAsC {
                                                span: n.span,
                                                module_name,
                                                symbol_name: symbol_name.to_string(),
                                                target_name: self.atoms.get(name).to_string(),
                                                related: helper,
                                            };
                                        self.push_error(Box::new(error));
                                    } else if let Some(local) =
                                        self.container(dep).locals.get(&name).copied()
                                    {
                                        let symbol_span = self
                                            .p
                                            .node(self.symbol_decl(local))
                                            .ident_name()
                                            .unwrap()
                                            .span;
                                        let error =
                                            errors::ModuleADeclaresBLocallyButItIsNotExported {
                                                span: n.span,
                                                module_name,
                                                symbol_name: symbol_name.to_string(),
                                                related: [errors::NameIsDeclaredHere {
                                                    span: symbol_span,
                                                    name: symbol_name.to_string(),
                                                }],
                                            };
                                        self.push_error(Box::new(error));
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
