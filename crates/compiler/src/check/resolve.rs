use super::TyChecker;
use crate::ast;
use crate::bind::SymbolID;

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
        self.binder.final_res(ident.id)
    }

    pub(super) fn resolve_entity_name(&mut self, name: &'cx ast::EntityName<'cx>) -> SymbolID {
        use ast::EntityNameKind::*;
        match name.kind {
            Ident(n) => self.resolve_symbol_by_ident(n),
            Qualified(n) => self.binder.final_res(n.id),
        }
    }

    // fn resolve_qualified_name(
    //     &mut self,
    //     left: &'cx ast::EntityName<'cx>,
    //     right: &'cx ast::Ident,
    // ) -> SymbolID {
    //     let left_symbol = self.resolve_entity_name(left);
    //     let left_s = self.binder.symbol(left_symbol);
    //     let ns = if left_s.flags.intersects(SymbolFlags::NAMESPACE) {
    //         left_s.expect_ns()
    //     } else {
    //         return Symbol::ERR;
    //     };
    //     let name = SymbolName::Normal(right.name);
    //     ns.exports.get(&name)
    // }
}
