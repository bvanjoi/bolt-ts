mod errors;

use crate::bind::{
    BinderResult, GlobalSymbols, SymbolFlags, SymbolID, SymbolName, SymbolTable, Symbols,
};
use crate::graph::{ModuleGraph, ModuleRes};
use crate::parser;
use bolt_ts_ast as ast;
use bolt_ts_ast::Visitor;
use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_span::{Module, ModuleID};
use bolt_ts_utils::fx_hashmap_with_capacity;
use rustc_hash::FxHashMap;

pub struct ResolveResult {
    pub symbols: Symbols,
    // TODO: use `NodeId::index` is enough
    pub final_res: FxHashMap<ast::NodeID, SymbolID>,
    pub diags: Vec<bolt_ts_errors::Diag>,
    // TODO: use `NodeId::index` is enough
    pub alias_target: FxHashMap<ast::NodeID, SymbolID>,
    // TODO: use `NodeId::index` is enough
    pub locals: FxHashMap<ast::NodeID, SymbolTable>,
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
        let alias_target = fx_hashmap_with_capacity(states[module.id.as_usize()].final_res.len());
        let root = p.root(module.id);
        let mut resolver = Resolver {
            mg,
            p,
            module_id,
            global,
            alias_target,
            states: &mut states,
            diags: vec![],
            atoms,
        };
        resolver.visit_program(root);
        let alias_target = std::mem::take(&mut resolver.alias_target);
        let resolver_diags = std::mem::take(&mut resolver.diags);
        temp.push((alias_target, resolver_diags));
    }

    modules
        .iter()
        .zip(states)
        .zip(temp)
        .map(|((module, mut state), (alias_target, diags))| {
            let symbols = state.symbols;
            let final_res = state.final_res;
            state.diags.extend(diags);
            let diags = state.diags;
            let result = ResolveResult {
                symbols,
                final_res,
                diags,
                alias_target,
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
    // TODO: use `NodeId::index` is enough
    alias_target: FxHashMap<ast::NodeID, SymbolID>,
    atoms: &'atoms AtomMap<'cx>,
}

impl Resolver<'_, '_, '_> {
    fn symbol_decl(&self, symbol_id: SymbolID) -> ast::NodeID {
        let s = self.symbol(symbol_id);
        s.decls[0]
    }

    fn symbol_of_decl(&self, id: ast::NodeID) -> SymbolID {
        self.states[id.module().as_usize()].final_res[&id]
    }

    fn symbol(&self, symbol_id: SymbolID) -> &crate::bind::Symbol {
        self.states[symbol_id.module().as_usize()]
            .symbols
            .get(symbol_id)
    }

    fn push_error(&mut self, diag: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag { inner: diag })
    }

    fn get_target_of_alias_decl(
        &mut self,
        node: ast::NodeID,
        dont_recur_resolve: bool,
    ) -> Option<SymbolID> {
        use ast::Node::*;
        match self.p.node(node) {
            ImportNamedSpec(_) | ShorthandSpec(_) => {
                self.get_target_of_import_named_spec(node, dont_recur_resolve)
            }
            _ => todo!(),
        }
    }

    fn resolve_external_module_name(
        &mut self,
        node: ast::NodeID,
        _module_spec: bolt_ts_atom::AtomId,
    ) -> Option<SymbolID> {
        let Some(dep) = self.mg.get_dep(self.module_id, node) else {
            unreachable!()
        };

        match dep {
            ModuleRes::Err => None,
            ModuleRes::Res(module_id) => Some(SymbolID::container(module_id)),
        }
    }

    fn error_no_module_member_symbol(
        &mut self,
        module_symbol: SymbolID,
        module_name: AtomId,
        target_symbol: SymbolID,
        node: ast::NodeID,
        name: ast::NodeID,
    ) {
        let decl_name = match self.p.node(name) {
            ast::Node::ImportNamedSpec(n) => match n.prop_name.kind {
                bolt_ts_ast::ModuleExportNameKind::Ident(ident) => ident.name,
                bolt_ts_ast::ModuleExportNameKind::StringLit(lit) => lit.val,
            },
            ast::Node::ShorthandSpec(n) => n.name.name,
            _ => unreachable!(),
        };
        self.report_non_exported_member(node, name, decl_name, module_symbol, module_name);
    }

    fn report_non_exported_member(
        &mut self,
        node: ast::NodeID,
        name: ast::NodeID,
        decl_name: AtomId,
        module_symbol: SymbolID,
        module_name: AtomId,
    ) {
        let s = self.symbol(module_symbol);
        let local_symbol = if let Some(value_decl) = s.value_decl {
            self.states[value_decl.module().as_usize()]
                .locals
                .get(&value_decl)
                .unwrap()
                .0
                .get(&SymbolName::Normal(decl_name))
                .copied()
        } else {
            None
        };
        if let Some(local_symbol) = local_symbol {
            let decl = s.exports.0.values().find_map(|export| {
                // TODO: use `get_symbol_if_same_reference`
                let s = self.symbol(*export);
                if s.flags == SymbolFlags::ALIAS {
                    let decl = s.decls[0];
                    if let Some(spec) = self.p.node(decl).as_export_named_spec() {
                        match spec.prop_name.kind {
                            bolt_ts_ast::ModuleExportNameKind::Ident(ident)
                                if ident.name == decl_name =>
                            {
                                return Some(decl);
                            }
                            _ => (),
                        }
                    }
                }
                None
            });
            let symbol_span = self
                .p
                .node(self.symbol_decl(local_symbol))
                .ident_name()
                .unwrap()
                .span;
            let error: crate::Diag = if let Some(decl) = decl {
                let mut helper = vec![];
                helper.push(
                    errors::ModuleADeclaresBLocallyButItIsExportedAsCHelperKind::NameIsDeclaredHere(
                        errors::NameIsDeclaredHere {
                            span: symbol_span,
                            name: self.atoms.get(decl_name).to_string(),
                        },
                    ),
                );
                let target_name = match self.p.node(decl) {
                    ast::Node::ExportNamedSpec(n) => match n.name.kind {
                        bolt_ts_ast::ModuleExportNameKind::Ident(ident) => ident.name,
                        bolt_ts_ast::ModuleExportNameKind::StringLit(lit) => lit.val,
                    },
                    _ => unreachable!(),
                };
                helper.push(
                    errors::ModuleADeclaresBLocallyButItIsExportedAsCHelperKind::ExportedAliasHere(
                        errors::ExportedAliasHere {
                            span: self.p.node(decl).span(),
                            name: self.atoms.get(decl_name).to_string(),
                        },
                    ),
                );
                let error = errors::ModuleADeclaresBLocallyButItIsExportedAsC {
                    span: self.p.node(name).span(),
                    module_name: self.atoms.get(module_name).to_string(),
                    symbol_name: self.atoms.get(decl_name).to_string(),
                    target_name: self.atoms.get(target_name).to_string(),
                    related: helper,
                };
                Box::new(error)
            } else {
                Box::new(errors::ModuleADeclaresBLocallyButItIsNotExported {
                    span: self.p.node(name).span(),
                    module_name: self.atoms.get(module_name).to_string(),
                    symbol_name: self.atoms.get(decl_name).to_string(),
                    related: [errors::NameIsDeclaredHere {
                        span: symbol_span,
                        name: self.atoms.get(decl_name).to_string(),
                    }],
                })
            };
            self.push_error(error);
        } else {
            todo!("error: module x has no exported member y")
        }
    }

    fn get_external_module_member(
        &mut self,
        node: ast::NodeID,
        spec: ast::NodeID,
        dont_recur_resolve: bool,
    ) -> Option<SymbolID> {
        let module_spec = match self.p.node(node) {
            ast::Node::ImportDecl(n) => n.module.val,
            ast::Node::ShorthandSpec(n) => n.name.name,
            _ => todo!("node: {:#?}", self.p.node(node)),
        };
        let module_symbol = self.resolve_external_module_name(node, module_spec);
        if let Some(module_symbol) = module_symbol {
            let name = match self.p.node(spec) {
                ast::Node::ImportNamedSpec(n) => match n.prop_name.kind {
                    bolt_ts_ast::ModuleExportNameKind::Ident(ident) => ident.name,
                    bolt_ts_ast::ModuleExportNameKind::StringLit(lit) => lit.val,
                },
                ast::Node::ShorthandSpec(n) => n.name.name,
                _ => todo!(),
            };
            let symbol_name = SymbolName::Normal(name);
            let symbol = self
                .symbol(module_symbol)
                .exports
                .0
                .get(&symbol_name)
                .copied();
            if symbol.is_none() {
                self.error_no_module_member_symbol(
                    module_symbol,
                    module_spec,
                    module_symbol,
                    node,
                    spec,
                );
            }
            symbol
        } else {
            None
        }
    }

    fn get_target_of_import_named_spec(
        &mut self,
        node: ast::NodeID,
        dont_recur_resolve: bool,
    ) -> Option<SymbolID> {
        let root = if self.p.node(node).is_binding() {
            self.p.get_root_decl(node)
        } else {
            let p = self.p.parent(node).unwrap();
            let p = self.p.parent(p).unwrap();
            p
        };
        let resolved = self.get_external_module_member(root, node, dont_recur_resolve);
        resolved
    }

    fn resolve_alias(&mut self, symbol: SymbolID) {
        let s = self.symbol(symbol);
        assert!(s.flags.intersects(SymbolFlags::ALIAS));
        let node = s.get_decl_of_alias_symbol(self.p).unwrap();
        self.get_target_of_alias_decl(node, false);
    }

    fn check_alias_symbol(&mut self, node: ast::NodeID) {
        let symbol = self.symbol_of_decl(node);
        self.resolve_alias(symbol);
    }

    fn check_import_binding(&mut self, node: ast::NodeID) {
        self.check_alias_symbol(node);
    }
}

impl<'cx> ast::Visitor<'cx> for Resolver<'cx, '_, '_> {
    fn visit_import_decl(&mut self, node: &'cx ast::ImportDecl<'cx>) {
        if let Some(clause) = node.clause.kind {
            use bolt_ts_ast::ImportClauseKind::*;
            match clause {
                Ns(_) => {
                    // import * as ns from 'xxxx'
                }
                Specs(specs) => {
                    // import { a, b as c } from 'xxxx'
                    for spec in specs {
                        use bolt_ts_ast::ImportSpecKind::*;
                        match spec.kind {
                            Shorthand(n) => {
                                // import { a } from 'xxxx'
                                self.check_import_binding(n.id);
                            }
                            Named(_) => {
                                // import { a as b } from 'xxxx'
                            }
                        }
                    }
                }
            }
        }
    }
}
