use super::TyChecker;
use crate::{ast, bind::SymbolKind};

pub(super) trait FnLikeDecl<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>>;
}

impl<'cx> FnLikeDecl<'cx> for ast::FnDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> FnLikeDecl<'cx> for ast::ClassMethodEle<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_fn_like_decl(&mut self, expr: &impl FnLikeDecl<'cx>) {
        let symbol = self.get_symbol_of_decl(expr.id());
        let SymbolKind::Function { decls, .. } = &self.symbols.get(symbol).kind else {
            unreachable!()
        };
        if decls[0] == expr.id() {
            self.check_fn_like_symbol(symbol);
        }

        if let Some(body) = expr.body() {
            self.check_block(body)
        }
    }
}
