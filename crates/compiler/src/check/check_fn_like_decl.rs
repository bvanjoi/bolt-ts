use super::TyChecker;
use crate::ast;
use crate::bind::SymbolKind;

pub(super) trait FnLikeDecl<'cx>: Copy + std::fmt::Debug {
    fn id(&self) -> ast::NodeID;
    fn params(&self) -> ast::ParamsDecl<'cx>;
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>>;
}

impl<'cx> FnLikeDecl<'cx> for ast::FnDecl<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> FnLikeDecl<'cx> for ast::ClassMethodEle<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> FnLikeDecl<'cx> for ast::ClassCtor<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn params(&self) -> ast::ParamsDecl<'cx> {
        self.params
    }
    fn body(&self) -> Option<&'cx ast::BlockStmt<'cx>> {
        self.body
    }
}

impl<'cx> TyChecker<'cx> {
    fn check_param_decl(&mut self, param: &'cx ast::ParamDecl<'cx>) {
        if let Some(init) = param.init {
            self.check_expr(init);
        }
    }

    pub(super) fn check_fn_like_decl(&mut self, decl: &impl FnLikeDecl<'cx>) {
        let symbol = self.get_symbol_of_decl(decl.id());
        let SymbolKind::Function { decls, .. } = &self.binder.symbol(symbol).kind else {
            unreachable!()
        };
        if decls[0] == decl.id() {
            self.check_fn_like_symbol(symbol);
        }

        for param in decl.params() {
            self.check_param_decl(param)
        }

        if let Some(body) = decl.body() {
            self.check_block(body)
        }
    }
}
