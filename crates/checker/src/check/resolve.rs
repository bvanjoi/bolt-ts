use bolt_ts_ast as ast;
use bolt_ts_binder::{Symbol, SymbolFlags, SymbolID};

use super::TyChecker;
use super::errors;

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

impl TyChecker<'_> {
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
                    unreachable!("final_res not found for {:#?}({:#?})", n.span(), id);
                };
                let name = self.atoms.get(node.name);
                let span = self.p.node(id).span();
                panic!("The resolution of `{name}({span})` is not found.");
            })
    }

    pub(super) fn check_alias_symbol(&mut self, node: ast::NodeID) {
        let symbol = self.get_symbol_of_decl(node);
        let target = self.resolve_alias(symbol);
        if target == Symbol::ERR {
            return;
        }
        let symbol = self.symbol(symbol).export_symbol.unwrap_or(symbol);
        let symbol = self.get_merged_symbol(symbol);
        if self
            .p
            .node_flags(node)
            .contains(ast::NodeFlags::JAVASCRIPT_FILE)
        {
            todo!()
        }

        let target_flags = self.symbol(target).flags;
        let s = self.symbol(symbol);
        let excluded_meanings = if s
            .flags
            .intersects(SymbolFlags::VALUE.union(SymbolFlags::EXPORT_VALUE))
        {
            SymbolFlags::VALUE
        } else {
            SymbolFlags::empty()
        }
        .union(if s.flags.intersects(SymbolFlags::TYPE) {
            SymbolFlags::TYPE
        } else {
            SymbolFlags::empty()
        })
        .union(if s.flags.intersects(SymbolFlags::NAMESPACE) {
            SymbolFlags::NAMESPACE
        } else {
            SymbolFlags::empty()
        });
        let n = self.p.node(node);
        if target_flags.intersects(excluded_meanings) {
            if n.is_export_shorthand_spec() {
                let error = errors::ExportDeclarationConflictsWithExportedDeclarationOfX {
                    name: s.name.to_string(&self.atoms),
                    span: n.span(),
                };
                self.push_error(Box::new(error));
            } else {
                let error = errors::ImportDeclarationConflictsWithLocalDeclarationOfX {
                    name: s.name.to_string(&self.atoms),
                    span: n.span(),
                };
                self.push_error(Box::new(error));
            }
        } else if n.is_export_shorthand_spec() {
            // TODO:
        }
    }

    pub(super) fn check_import_binding(&mut self, node: ast::NodeID) {
        self.check_alias_symbol(node);
    }
}
