use super::TyChecker;
use crate::{ast, ir};

impl<'cx> TyChecker<'cx> {
    fn check_param_decl(&mut self, param: &'cx ast::ParamDecl<'cx>) {
        self.check_var_like_decl(param);
    }

    pub(super) fn check_fn_like_decl(&mut self, decl: &impl ir::FnDeclLike<'cx>) {
        let id = decl.id();
        let symbol = self.get_symbol_of_decl(id);
        let f = &self.binder.symbol(symbol).expect_fn();
        if f.decls[0] == id {
            self.check_fn_like_symbol(symbol);
        }

        for param in decl.params() {
            self.check_param_decl(param)
        }

        if let Some(body) = ir::FnDeclLike::body(decl) {
            self.check_block(body)
        }
    }
}
