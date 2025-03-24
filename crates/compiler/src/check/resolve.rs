use super::{TyChecker, errors};
use crate::bind::SymbolID;
use crate::bind::{Symbol, SymbolFlags, SymbolName};
use crate::graph::ModuleRes;
use bolt_ts_ast as ast;
use bolt_ts_atom::AtomId;

#[derive(Debug, Clone, Copy)]
pub enum ExpectedArgsCount {
    Count(usize),
    Range { lo: usize, hi: usize },
}

impl std::fmt::Display for ExpectedArgsCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedArgsCount::Count(c) => write!(f, "{c}"),
            ExpectedArgsCount::Range { lo, hi } => write!(f, "{lo}-{hi}"),
        }
    }
}

impl<'cx> TyChecker<'cx> {
    #[inline]
    pub(super) fn resolve_symbol_by_ident(&self, ident: &'cx ast::Ident) -> SymbolID {
        let symbol = self.final_res(ident.id);
        symbol
    }

    #[inline]
    pub(super) fn final_res(&self, id: ast::NodeID) -> SymbolID {
        self.binder
            .get(id.module())
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

    pub(super) fn resolve_entity_name(&mut self, name: &'cx ast::EntityName<'cx>) -> SymbolID {
        use bolt_ts_ast::EntityNameKind::*;
        match name.kind {
            Ident(n) => self.resolve_symbol_by_ident(n),
            Qualified(n) => self.final_res(n.id),
        }
    }

    fn get_target_of_alias_decl(
        &mut self,
        node: ast::NodeID,
        dont_recur_resolve: bool,
    ) -> Option<SymbolID> {
        use ast::Node::*;
        match self.p.node(node) {
            ImportNamedSpec(_) => self.get_target_of_import_named_spec(node, dont_recur_resolve),
            ShorthandSpec(_) if self.p.node(self.p.parent(node).unwrap()).is_import_clause() => {
                self.get_target_of_import_named_spec(node, dont_recur_resolve)
            }
            ShorthandSpec(_) => {
                assert!(self.p.node(self.p.parent(node).unwrap()).is_specs_export());
                const MEANING: SymbolFlags = SymbolFlags::VALUE
                    .union(SymbolFlags::TYPE)
                    .union(SymbolFlags::NAMESPACE);
                self.get_target_of_export_spec(node, MEANING, dont_recur_resolve)
            }
            NsImport(n) => self.get_target_of_ns_import(n, dont_recur_resolve),
            _ => todo!(),
        }
    }

    fn get_target_of_ns_import(
        &mut self,
        n: &'cx ast::NsImport<'cx>,
        dont_resolve_alias: bool,
    ) -> Option<SymbolID> {
        let p = self.p.parent(n.id).unwrap();
        let p = self.p.parent(p).unwrap();
        let import_decl = self.p.node(p).expect_import_decl();
        let immediate = self.resolve_external_module_name(p, import_decl.module.val);
        // TODO: resolve_es_module;
        let resolved = immediate;
        resolved
    }

    fn get_target_of_export_spec(
        &mut self,
        node: ast::NodeID,
        meaning: SymbolFlags,
        dont_recur_resolve: bool,
    ) -> Option<SymbolID> {
        let n = self.p.node(node);
        let name = match n {
            ast::Node::ShorthandSpec(n) => n.name.name,
            ast::Node::ExportNamedSpec(n) => match n.prop_name.kind {
                ast::ModuleExportNameKind::Ident(n) => n.name,
                ast::ModuleExportNameKind::StringLit(_) => todo!(),
            },
            _ => unreachable!(),
        };

        // TODO: default
        let resolved = match n {
            ast::Node::ShorthandSpec(n) => {
                let p_id = self.p.parent(node).unwrap();
                let p = self.p.node(p_id).expect_specs_export();
                if let Some(_) = p.module {
                    self.get_external_module_member(p_id, node, dont_recur_resolve)
                } else {
                    self.binder
                        .get(n.id.module())
                        .final_res
                        // COMMENT: pay attention to `n.name.id`, `n.id` refers to itself, but `n.name.id` refers to the result in early_resolve.
                        .get(&n.name.id)
                        .copied()
                }
            }
            ast::Node::ExportNamedSpec(n) => match n.prop_name.kind {
                ast::ModuleExportNameKind::Ident(ident) => {
                    let p = self.p.parent(node).unwrap();
                    let p = self.p.node(p).expect_specs_export();
                    if let Some(_) = p.module {
                        todo!()
                    } else {
                        self.binder.get(n.id.module()).final_res.get(&n.id).copied()
                    }
                }
                ast::ModuleExportNameKind::StringLit(_) => todo!(),
            },
            _ => unreachable!(),
        };

        resolved
    }

    fn resolve_external_module_name(
        &mut self,
        node: ast::NodeID,
        _module_spec: bolt_ts_atom::AtomId,
    ) -> Option<SymbolID> {
        let Some(dep) = self.mg.get_dep(node) else {
            dbg!(node.module());
            dbg!(self.p.node(node).span());
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
        let s = self.binder.symbol(module_symbol);
        let local_symbol = if let Some(value_decl) = s.value_decl {
            self.binder
                .get(value_decl.module())
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
                let s = self.binder.symbol(*export);
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
                .node(local_symbol.decl(self.binder))
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
            ast::Node::SpecsExport(n) => n.module.unwrap().val,
            _ => todo!("node: {:#?}", self.p.node(node)),
        };
        let module_symbol = self.resolve_external_module_name(node, module_spec);
        // TODO: target_symbol
        let target_symbol = module_symbol;
        if let Some(target_symbol) = target_symbol {
            let name = match self.p.node(spec) {
                ast::Node::ImportNamedSpec(n) => match n.prop_name.kind {
                    bolt_ts_ast::ModuleExportNameKind::Ident(ident) => ident.name,
                    bolt_ts_ast::ModuleExportNameKind::StringLit(_) => todo!(),
                },
                ast::Node::ShorthandSpec(n) => n.name.name,
                ast::Node::ExportNamedSpec(n) => match n.prop_name.kind {
                    ast::ModuleExportNameKind::Ident(n) => n.name,
                    ast::ModuleExportNameKind::StringLit(_) => todo!(),
                },
                _ => todo!(),
            };
            let symbol_name = SymbolName::Normal(name);
            let symbol_from_module =
                self.get_export_of_module(target_symbol, symbol_name, dont_recur_resolve);
            if symbol_from_module.is_none() {
                self.error_no_module_member_symbol(
                    module_symbol.unwrap(),
                    module_spec,
                    target_symbol,
                    node,
                    spec,
                );
            }
            symbol_from_module
        } else {
            None
        }
    }

    fn get_export_of_module(
        &mut self,
        symbol: SymbolID,
        name: SymbolName,
        dont_resolve_alias: bool,
    ) -> Option<SymbolID> {
        let s = self.binder.symbol(symbol);
        if s.flags.intersects(SymbolFlags::MODULE) {
            let export_symbol = s.exports.0.get(&name).copied();
            let resolved = export_symbol
                .map(|export_symbol| self.resolve_symbol(export_symbol, dont_resolve_alias));
            resolved
        } else {
            None
        }
    }

    fn is_non_local_alias(&self, symbol: SymbolID, excludes: Option<SymbolFlags>) -> bool {
        const DEFAULT: SymbolFlags = SymbolFlags::VALUE
            .union(SymbolFlags::TYPE)
            .union(SymbolFlags::NAMESPACE);
        let excludes = excludes.unwrap_or(DEFAULT);
        let flags = self.symbol(symbol).flags();
        (flags.intersection(SymbolFlags::ALIAS | excludes) == SymbolFlags::ALIAS)
            || (flags.intersects(SymbolFlags::ALIAS.union(SymbolFlags::ASSIGNMENT)))
    }

    fn resolve_symbol(&mut self, symbol: SymbolID, dont_resolve_alias: bool) -> SymbolID {
        if !dont_resolve_alias && self.is_non_local_alias(symbol, None) {
            self.resolve_alias(symbol)
        } else {
            symbol
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

    pub(super) fn resolve_alias(&mut self, symbol: SymbolID) -> SymbolID {
        if let Some(alias_target) = self.get_symbol_links(symbol).get_alias_target() {
            return if alias_target == Symbol::RESOLVING {
                self.get_mut_symbol_links(symbol)
                    .override_alias_target(Symbol::ERR);
                Symbol::ERR
            } else {
                alias_target
            };
        };
        self.get_mut_symbol_links(symbol)
            .set_alias_target(Symbol::RESOLVING);
        let s = self.binder.symbol(symbol);
        assert!(s.flags.intersects(SymbolFlags::ALIAS), "symbol: {:#?}", s);
        let node = s.get_decl_of_alias_symbol(self.p).unwrap();
        let target = self.get_target_of_alias_decl(node, false);
        if self
            .get_symbol_links(symbol)
            .get_alias_target()
            .is_none_or(|alias_target| alias_target == Symbol::RESOLVING)
        {
            let v = target.unwrap_or(Symbol::ERR);
            self.get_mut_symbol_links(symbol).override_alias_target(v);
        } else {
            // TODO: report circle error
        }
        self.get_symbol_links(symbol).expect_alias_target()
    }

    fn check_alias_symbol(&mut self, node: ast::NodeID) {
        let symbol = self.get_symbol_of_decl(node);
        self.resolve_alias(symbol);
    }

    pub(super) fn check_import_binding(&mut self, node: ast::NodeID) {
        self.check_alias_symbol(node);
    }
}
