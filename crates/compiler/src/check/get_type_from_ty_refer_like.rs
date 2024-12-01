use crate::{ast, bind::{Symbol, SymbolKind}, ty};

use super::TyChecker;

pub(super) trait GetTypeFromTyReferLike<'cx> {
    fn id(&self) -> ast::NodeID;
    fn name(&self) -> Option<&'cx ast::Ident>; // TODO: support member name
    fn args(&self) -> Option<ast::Tys<'cx>>;
}

impl<'cx> GetTypeFromTyReferLike<'cx> for ast::Expr<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id()
    }
    fn name(&self) -> Option<&'cx ast::Ident> {
        if let ast::ExprKind::Ident(ident) = self.kind {
            Some(ident)
        } else {
            None
        }
    }
    fn args(&self) -> Option<ast::Tys<'cx>> {
        None
    }
}

impl<'cx> GetTypeFromTyReferLike<'cx> for ast::ReferTy<'cx> {
    fn id(&self) -> ast::NodeID {
        self.id
    }
    fn name(&self) -> Option<&'cx ast::Ident> {
        Some(self.name)
    }
    fn args(&self) -> Option<ast::Tys<'cx>> {
        self.args
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_type_from_ty_reference(
        &mut self,
        node: &impl GetTypeFromTyReferLike<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        // TODO: cache
        if let Some(name) = node.name() {
            let symbol = self.resolve_symbol_by_ident(name, SymbolKind::is_type);
            if symbol == Symbol::ERR {
                if let Some(error) = self.check_using_type_as_value(&name) {
                    self.push_error(name.span.module, error);
                }
                return self.error_ty();
            }
            self.get_declared_ty_of_symbol(node.id().module(), symbol)
                .unwrap()
        } else {
            // TODO:
            self.undefined_ty()
        }
    }
}
